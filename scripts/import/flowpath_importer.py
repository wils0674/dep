"""Process provided GeoJSON files into the database.

Brian Gelder provides me some archive of processed HUC12s, this archive is
dumped to disk and then glob'd by this script.  It then creates a `myhucs.txt`
file, which provides the downstream scripts the data domain to limit processing
to.

    python flowpath_importer.py <scenario> <path to geojsons in ../../data/>

"""
import glob
import logging
import os
import re
import sys

import geopandas as gpd
import numpy as np
import pandas as pd
import pyproj
from pydep.util import get_cli_fname
from pyiem.util import get_dbconn, logger
from shapely.geometry import LineString
from tqdm import tqdm

LOG = logger()
print(" * BE CAREFUL!  The GeoJSON files may not be 5070, but 26915")
print(" * VERIFY that the GeoJSON is the 5070 grid value")
print(" * This will generate a `myhucs.txt` file with found HUCs")

SOILFY = 2023
SOILCOL = f"SOL_FY_{SOILFY}"
PREFIX = "fp"
TRUNC_GRIDORDER_AT = 4
MAX_POINTS_OFE = 97
GENLU_CODES = {}
PROCESSING_COUNTS = {
    "flowpaths_good": 0,
    "flowpaths_deduped": 0,
    "flowpaths_tooshort": 0,
    "flowpaths_toosteep": 0,
    "flowpaths_toomanydupes": 0,
    "flowpaths_invalidgeom": 0,
    "flowpaths_lenzero_reset": 0,
    "flowpaths_inconsistent_gridorder": 0,
    "fields_deleted": 0,
    "fields_inserted": 0,
}
TRANSFORMER = pyproj.Transformer.from_crs(
    "epsg:5070", "epsg:4326", always_xy=True
)
# First call returns inf for unknown reasons
TRANSFORMER.transform(223279, 2071344)
MAX_SLOPE_RATIO = 0.9
HUC12RE = re.compile("^fp[0-9]{12}")


def create_flowpath_id(cursor, scenario, huc12, fpath) -> int:
    """Create a database flowpath identifier

    Args:
      cursor (psycopg.cursor): database cursor
      huc12 (str): HUC12 identifier
      fpath (int): the flowpath id value for this HUC12

    Returns:
      int the value of this huc12 flowpath
    """
    cursor.execute(
        "INSERT into flowpaths(huc_12, fpath, scenario) "
        "values (%s, %s, %s) RETURNING fid",
        (huc12, fpath, scenario),
    )
    return cursor.fetchone()[0]


def fillout_codes(df):
    """ "Get the right full-string codes."""
    col = "CropRotatn_CY_2022"
    s = df[col].str
    # TODO, this isn't right!
    df["landuse"] = s[1] + s[0] + s[1] + s + s[-2] + s[-1]
    s = df["Management_CY_2022"].str
    df["management"] = s[1] + s[0] + s[1] + s + s[-2] + s[-1]


def get_data(filename):
    """Converts a GeoJSON file into a pandas dataframe

    Args:
      filename (str): The geojson filename to process

    Returns:
      gpd.DataFrame with the geojson data included.
    """
    df = gpd.read_file(filename, engine="pyogrio")
    huc12 = None
    for col in df.columns:
        if HUC12RE.match(col):
            huc12 = col[len(PREFIX) : (len(PREFIX) + 12)]
            break

    if "irrigated" not in df.columns:
        LOG.info("%s had no irrigated column", filename)
        df["irrigated"] = 0
    # Rename columns to make things simplier
    rename = {
        f"{PREFIX}Len{huc12}": "len",
        f"ep3m{huc12}": "elev",
        f"gord_{huc12}": "gorder",
    }
    df = df.rename(columns=rename)
    # Ensure that len is not an object
    if df["len"].dtype == object:
        df["len"] = pd.to_numeric(df["len"])
    # Can't deal with int32 dtypes
    for col in df.select_dtypes(np.int32).columns:
        df[col] = df[col].astype(int)

    # Any null FBndID (mostly forest?) get set to -1
    df["FBndID"] = df["FBndID"].fillna("FUNUSED_-1")
    fillout_codes(df)
    fld_df = None
    fldfn = filename.replace("smpl3m_mean18", "FB")
    if os.path.isfile(fldfn):
        # Get the field bounds dataframe as well
        fld_df = gpd.read_file(fldfn, engine="pyogrio")
        # Check for bad geometries
        if not fld_df["geometry"].is_valid.all():
            LOG.info(
                "Found %s invalid geomtries, calling buffer(0)",
                len(fld_df.index) - fld_df["geometry"].is_valid.sum(),
            )
            fld_df["geometry"] = fld_df["geometry"].buffer(0)
        fillout_codes(fld_df)
    else:
        LOG.warning("Missing %s", fldfn)
    return df, fld_df


def delete_previous(cursor, scenario, huc12):
    """This file is the authority, so we cull previous content."""
    cursor.execute(
        "DELETE from flowpath_ofes p USING flowpaths f WHERE "
        "p.flowpath = f.fid and f.huc_12 = %s and f.scenario = %s",
        (huc12, scenario),
    )
    cursor.execute(
        "DELETE from flowpaths WHERE scenario = %s and huc_12 = %s",
        (scenario, huc12),
    )


def load_genlu_codes(cursor):
    """Populate dict."""
    cursor.execute("SELECT id, label from general_landuse")
    for row in cursor:
        GENLU_CODES[row[1]] = row[0]


def get_genlu_code(cursor, label):
    """Find or create the code for this label."""
    if label not in GENLU_CODES:
        cursor.execute("SELECT max(id) from general_landuse")
        row = cursor.fetchone()
        newval = 0 if row[0] is None else row[0] + 1
        LOG.info("Inserting new general landuse code: %s [%s]", newval, label)
        cursor.execute(
            "INSERT into general_landuse(id, label) values (%s, %s)",
            (newval, label),
        )
        GENLU_CODES[label] = newval
    return GENLU_CODES[label]


def dedupe(df):
    """Deduplicate by removing duplicated points."""
    dups = df["len"].duplicated(keep=False)
    if dups.sum() > 7:
        PROCESSING_COUNTS["flowpaths_toomanydupes"] += 1
        return None
    PROCESSING_COUNTS["flowpaths_deduped"] += 1
    df = df[~dups].copy()
    # Ensure we are zero based.
    if df["len"].min() > 0:
        df["len"] = df["len"] - df["len"].min()
    return df


def compute_ofe(df):
    """Figure out what the OFE should be after prj2wepp does magic."""
    # Default
    df["ofe"] = 1
    # OFEs are breaks in management, landuse and or soils
    cols = ["FBndID", SOILCOL, "management", "landuse"]
    locs = df[cols].ne(df[cols].shift()).apply(lambda x: x.index[x].tolist())
    indices = locs.apply(pd.Series).stack().unique()
    ofeid = 0
    values = df.index.to_list()
    for idx in values:
        # Last index should not bump OFE, it is quasi ignored
        if idx in indices and idx != values[-1]:
            ofeid += 1
        df.at[idx, "ofe"] = ofeid
    return df


def simplify(df):
    """WEPP can only handle 100 slope points per OFE."""
    df["useme"] = False
    for _ofe, gdf in df.groupby("ofe"):
        if len(gdf.index) < MAX_POINTS_OFE:
            df.loc[gdf.index, "useme"] = True
            continue
        # Any hack is a good hack here, take the first and last point
        df.at[gdf.index[0], "useme"] = True
        df.at[gdf.index[-1], "useme"] = True
        # Take the top 16 values by slope
        df.loc[
            gdf.sort_values("slope", ascending=False).index[:MAX_POINTS_OFE],
            "useme",
        ] = True

    df = df[df["useme"]].copy()

    return df


def compute_slope(df):
    """Figure out the slope values."""
    # Compute slope
    df["slope"] = (df["elev"] - df["elev"].shift()) / (
        df["len"].shift() - df["len"]
    )
    # Duplicate the first value, for now?
    df.iat[0, df.columns.get_loc("slope")] = df["slope"].values[1]
    return df


def insert_ofe(cursor, gdf, db_fid, ofe, ofe_starts):
    """Add database entry."""
    pts = list(zip(gdf.geometry.x, gdf.geometry.y, gdf.elev.values / 100.0))
    next_ofe = ofe + 1
    firstpt = gdf.iloc[0]
    lastpt = gdf.iloc[-1]
    if next_ofe in ofe_starts.index:
        lastpt = ofe_starts.loc[next_ofe]
        # append on next
        pts.extend(
            [(lastpt.geometry.x, lastpt.geometry.y, lastpt.elev / 100.0)]
        )
    line = LineString(pts)

    cursor.execute(
        """
        INSERT into flowpath_ofes (flowpath, ofe, geom, bulk_slope, max_slope,
        gssurgo_id, fbndid, management, landuse, real_length)
        values (%s, %s, %s, %s, %s,
        (select id from gssurgo where fiscal_year = %s and mukey = %s),
        %s, %s, %s, %s)
        """,
        (
            db_fid,
            ofe,
            f"SRID=5070;{line.wkt}",
            (
                (firstpt["elev"] - lastpt["elev"])
                / (lastpt["len"] - firstpt["len"])
            ),
            gdf["slope"].max(),
            SOILFY,
            int(firstpt[SOILCOL]),
            firstpt["FBndID"].split("_")[1],
            firstpt["management"],
            firstpt["landuse"],
            (lastpt["len"] - firstpt["len"]) / 100.0,
        ),
    )


def process_flowpath(cursor, scenario, db_fid, df) -> pd.DataFrame:
    """Do one flowpath please."""

    # Sort along the length column, which orders the points from top
    # to bottom, rename columns to simplify further code.
    df = df.sort_values("len", ascending=True)
    if df.iloc[0]["len"] > 0:
        PROCESSING_COUNTS["flowpaths_lenzero_reset"] += 1
        df["len"] = df["len"] - df.iloc[0]["len"]
    # remove duplicate points due to a bkgelder sampling issue whereby some
    # points exist in two fields
    if df["len"].duplicated().any():
        df = dedupe(df)
        if df is None:
            return

    # Ensure that the gorder field is monotonic
    if not df["gorder"].is_monotonic_increasing:
        PROCESSING_COUNTS["flowpaths_inconsistent_gridorder"] += 1
        return

    # Truncate at grid order
    df = df[df["gorder"] <= TRUNC_GRIDORDER_AT]

    if len(df.index) < 3:
        PROCESSING_COUNTS["flowpaths_tooshort"] += 1
        return

    compute_slope(df)

    # Compute the OFE value
    compute_ofe(df)

    # WEPP has a 100 point per OFE limit, so if we detect this, simplification
    # is necessary, we are cautious to be well below 100
    if (
        df[["ofe", "landuse"]].groupby("ofe").count().max()["landuse"]
        > MAX_POINTS_OFE
    ):
        df = simplify(df)
        # Need to recompute slopes
        compute_slope(df)

    if df["slope"].max() > MAX_SLOPE_RATIO:
        PROCESSING_COUNTS["flowpaths_toosteep"] += 1
        return

    if len(df.index) < 3:
        PROCESSING_COUNTS["flowpaths_tooshort"] += 1
        return

    # Need OFE start points in order to create the end point for the previous
    # OFE
    ofe_starts = df.groupby("ofe").first()

    # reset_index to get a sequential id
    for ofe, gdf in df.reset_index().groupby("ofe"):
        # Insert OFE information
        insert_ofe(cursor, gdf, db_fid, ofe, ofe_starts)

    # Update flowpath info
    ls = LineString(zip(df.geometry.x, df.geometry.y))
    if not ls.is_valid:
        PROCESSING_COUNTS["flowpaths_invalidgeom"] += 1
        return
    # pylint: disable=not-an-iterable
    cursor.execute(
        """
        UPDATE flowpaths SET geom = %s, irrigated = %s,
        max_slope = %s, bulk_slope = %s, ofe_count = %s, climate_file = %s,
        real_length = %s
        WHERE fid = %s
        """,
        (
            f"SRID=5070;{ls.wkt}",
            bool(df["irrigated"].sum() > 0),
            df["slope"].max(),
            (df["elev"].max() - df["elev"].min())
            / (df["len"].max() - df["len"].min()),
            df["ofe"].max(),
            get_cli_fname(
                *TRANSFORMER.transform(
                    df.iloc[0].geometry.x, df.iloc[0].geometry.y
                ),
                scenario,
            ),
            df["len"].max() / 100.0,
            db_fid,
        ),
    )
    PROCESSING_COUNTS["flowpaths_good"] += 1
    return df


def delete_flowpath(cursor, fid):
    """Delete the flowpath."""
    cursor.execute("DELETE from flowpath_ofes where flowpath = %s", (fid,))
    cursor.execute("DELETE from flowpaths where fid = %s", (fid,))


def process_fields(cursor, scenario, huc12, fld_df):
    """Database update this information."""
    cursor.execute(
        """
        DELETE from field_operations o USING fields f where
        o.field_id = f.field_id and f.scenario = %s and f.huc12 = %s
        """,
        (scenario, huc12),
    )
    cursor.execute(
        "DELETE from fields where scenario = %s and huc12 = %s",
        (scenario, huc12),
    )
    PROCESSING_COUNTS["fields_deleted"] += cursor.rowcount

    for _, row in fld_df.iterrows():
        if row["geometry"] is None:
            print("null geom", row)
            continue
        cursor.execute(
            """
            INSERT into fields (scenario, huc12, fbndid, acres, isag, geom,
            management, landuse, genlu)
            VALUES (%s, %s, %s, %s, %s, st_multi(%s), %s, %s, %s)
            """,
            (
                scenario,
                huc12,
                row["FBndID"].split("_")[1],
                row["Acres"],
                bool(row["isAG"]),
                row["geometry"].wkt,
                row["management"],
                row["landuse"],
                get_genlu_code(cursor, row["GenLU"]),
            ),
        )
    PROCESSING_COUNTS["fields_inserted"] += len(fld_df.index)


def process(cursor, scenario, huc12df, fld_df):
    """Processing of a HUC12's data into the database

    Args:
      cursor (psycopg.cursor): database cursor
        scenario (int): the scenario to process
      huc12df (pd.DataFrame): the dataframe containing the data
      fld_df (gpd.GeoDataFrame): The associated fields.

    Returns:
      None
    """
    # Hack compute the huc12 by finding the fp field name
    huc12 = None
    fpcol = None
    for col in huc12df.columns:
        if col.startswith(PREFIX):
            fpcol = col
            huc12 = col[len(PREFIX) :].replace("_tif", "")
            break
    if huc12 is None or len(huc12) != 12:
        raise ValueError(f"Could not find huc12 from {huc12df.columns}")
    if fld_df is not None:
        process_fields(cursor, scenario, huc12, fld_df)
    newid_schema = f"fp_id_{huc12}" in huc12df.columns
    if newid_schema:
        fpcol = f"fp_id_{huc12}"

    delete_previous(cursor, scenario, huc12)
    # the inbound dataframe has lots of data, one row per flowpath point
    # We group the dataframe by the column which uses a PREFIX and the huc8
    for flowpath_num, df in huc12df.groupby(fpcol):
        # These are upstream errors I should ignore
        if flowpath_num == 0 or len(df.index) < 3:
            continue
        if newid_schema:
            # Condition the flowpath_num to fit in database
            flowpath_num = int(flowpath_num[4:])
        # Create the flowpathid in the database
        db_fid = create_flowpath_id(cursor, scenario, huc12, flowpath_num)
        try:
            if process_flowpath(cursor, scenario, db_fid, df) is None:
                delete_flowpath(cursor, db_fid)
        except Exception as exp:
            LOG.info("flowpath_num: %s hit exception", flowpath_num)
            logging.error(exp, exc_info=True)
            delete_flowpath(cursor, db_fid)
            # print(df)
            # sys.exit()
    return huc12


def main(argv):
    """Our main function, the starting point for code execution"""
    pgconn = get_dbconn("idep")
    cursor = pgconn.cursor()
    load_genlu_codes(cursor)
    scenario = int(argv[1])
    datadir = os.path.join("..", "..", "data", argv[2])
    # collect up the GeoJSONs in that directory
    fns = glob.glob(f"{datadir}/smpl3m_*.json")
    fns.sort()
    i = 0
    # Allow for processing to restart
    done = []
    if os.path.isfile("myhucs.txt-done"):
        with open("myhucs.txt-done", encoding="utf8") as fh:
            done = [x.strip() for x in fh]

    progress = tqdm(fns)
    # track our work
    with open("myhucs.txt", "w", encoding="utf8") as fh:
        for fn in progress:
            progress.set_description(os.path.basename(fn))
            huc12 = fn[-17:-5]
            if huc12 in done:
                continue
            # Save our work every 100 HUC12s,
            # so to keep the database transaction
            # at a reasonable size
            if i > 0 and i % 100 == 0:
                pgconn.commit()
                cursor = pgconn.cursor()
            fp_df, fld_df = get_data(fn)
            # Sometimes we get no flowpaths, so skip writing those
            huc12 = process(cursor, scenario, fp_df, fld_df)
            if not fp_df.empty:
                fh.write(f"{huc12}\n")
            i += 1

    # Commit the database changes
    cursor.close()
    pgconn.commit()

    print("Processing accounting:")
    for key, val in PROCESSING_COUNTS.items():
        print(f"    {key}: {val}")


if __name__ == "__main__":
    main(sys.argv)
