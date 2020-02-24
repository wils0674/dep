"""Process provided GeoJSON files into the database.

Brian Gelder provides me some archive of processed HUC12s, this archive is
dumped to disk and then glob'd by this script.  It then creates a `myhucs.txt`
file, which provides the downstream scripts the data domain to limit processing
to.

    python flowpath_importer.py <scenario> <path to geojsons in ../../data/>

"""
from __future__ import print_function
import glob
import os
import sys

from tqdm import tqdm
import geopandas as gpd
import pandas as pd
from pyiem.util import get_dbconn

print(" * BE CAREFUL!  The GeoJSON files may not be 5070, but 26915")
print(" * VERIFY that the GeoJSON is the 5070 grid value")
print(" * This will generate a `myhucs.txt` file with found HUCs")

SCENARIO = int(sys.argv[1])
PREFIX = "fp"
TRUNC_GRIDORDER_AT = 4

PGCONN = get_dbconn("idep")
INSERT_SQL = """
    INSERT into flowpath_points(flowpath, segid,
    elevation, length,  surgo, management, slope, geom,
    landuse, scenario, gridorder)
    values(%s, %s, %s, %s, %s, %s, %s, 'SRID=5070;POINT(%s %s)',
    %s, %s, %s)
"""


def get_flowpath(cursor, huc12, fpath):
    """Get or create a database flowpath identifier

    Args:
      cursor (psycopg2.cursor): database cursor
      huc12 (str): HUC12 identifier
      fpath (int): the flowpath id value for this HUC12

    Returns:
      int the value of this huc12 flowpath
    """
    cursor.execute(
        """
        SELECT fid from flowpaths where huc_12 = %s and fpath = %s
        and scenario = %s
    """,
        (huc12, fpath, SCENARIO),
    )
    if cursor.rowcount == 0:
        cursor.execute(
            """
            INSERT into flowpaths(huc_12, fpath, scenario)
            values (%s, %s, %s) RETURNING fid
        """,
            (huc12, fpath, SCENARIO),
        )
    return cursor.fetchone()[0]


def get_data(filename):
    """Converts a GeoJSON file into a pandas dataframe

    Args:
      filename (str): The geojson filename to process

    Returns:
      gpd.DataFrame with the geojson data included.
    """
    df = gpd.read_file(filename, index="OBJECTID")
    # Compute full rotation string
    # OK, be careful here. Presently, the 8 char field covers
    # 2010 thru 2017, so we rotate to cover the first and last years
    # 2007 2011[1]
    # 2008 2010[0]
    # 2009 2011[1]
    # 2018 2016[6]
    # 2019 2017[7]
    # 2020 2018[6]
    s = df["CropRotatn_CY_2017"]
    df["landuse"] = (
        s.str[1] + s.str[0] + s.str[1] + s + s.str[6] + s.str[7] + s.str[6]
    )
    s = df["Management_CY_2017"]
    df["management"] = (
        s.str[1] + s.str[0] + s.str[1] + s + s.str[6] + s.str[7] + s.str[6]
    )
    return df


def delete_previous(cursor, huc12):
    """This file is the authority for the HUC12, so we cull previous content.
    """
    cursor.execute(
        """
        DELETE from flowpath_points p USING flowpaths f WHERE
        p.scenario = %s and p.flowpath = f.fid and f.huc_12 = %s
        and f.scenario = %s
    """,
        (SCENARIO, huc12, SCENARIO),
    )
    cursor.execute(
        """
        DELETE from flowpaths WHERE
        scenario = %s and huc_12 = %s
    """,
        (SCENARIO, huc12),
    )


def process_flowpath(cursor, huc12, db_fid, df):
    """Do one flowpath please."""
    lencolname = "%sLen%s" % (PREFIX, huc12)
    elevcolname = "ep3m%s" % (huc12,)
    gordcolname = "gord_%s" % (huc12,)
    # Sort along the length column, which orders the points from top
    # to bottom
    df = df.sort_values(lencolname, ascending=True)
    # Remove any previous data for this flowpath
    cursor.execute(
        "DELETE from flowpath_points WHERE flowpath = %s", (db_fid,)
    )
    linestring = []
    sz = len(df.index)
    maxslope = 0
    elev_change = 0
    x_change = 0
    for segid, (_, row) in enumerate(df.iterrows()):
        if (segid + 1) == sz:  # Last row!
            # This effectively repeats the slope of the previous point
            row2 = df.iloc[segid - 1]
        else:
            row2 = df.iloc[segid + 1]
        dy = abs(row[elevcolname] - row2[elevcolname])
        elev_change += dy
        dx = abs(row2[lencolname] - row[lencolname])
        x_change += dx
        gridorder = row[gordcolname]
        if gridorder > TRUNC_GRIDORDER_AT or pd.isnull(gridorder):
            continue
        slope = dy / dx

        if slope > maxslope:
            maxslope = slope
        args = (
            db_fid,
            segid,
            row[elevcolname] / 100.0,
            row[lencolname] / 100.0,
            row["SOL_FY_2018"],
            row["management"],
            slope,
            row["geometry"].x,
            row["geometry"].y,
            row["landuse"],
            SCENARIO,
            gridorder,
        )
        cursor.execute(INSERT_SQL, args)

        linestring.append("%s %s" % (row["geometry"].x, row["geometry"].y))

    # Line string must have at least 2 points
    if len(linestring) > 1:
        if x_change == 0:
            print()
            print(df)
            sys.exit()
        sql = """
            UPDATE flowpaths SET geom = 'SRID=5070;LINESTRING(%s)',
            max_slope = %s, bulk_slope = %s
            WHERE fid = %s
        """ % (
            ",".join(linestring),
            maxslope,
            elev_change / x_change,
            db_fid,
        )
        cursor.execute(sql)
    else:
        # Cull our work above if this flowpath is too short
        cursor.execute(
            "DELETE from flowpath_points where flowpath = %s", (db_fid,)
        )
        cursor.execute("DELETE from flowpaths where fid = %s", (db_fid,))


def process(cursor, filename, huc12df):
    """Processing of a HUC12's data into the database

    Args:
      cursor (psycopg2.cursor): database cursor
      filename (str): the geojson filename
      huc12df (pd.DataFrame): the dataframe containing the data

    Returns:
      None
    """
    # We get the huc12 code based on the filename
    huc12 = filename.split(".")[0].split("_")[-1]
    delete_previous(cursor, huc12)
    # the inbound dataframe has lots of data, one row per flowpath point
    # We group the dataframe by the column which uses a PREFIX and the huc8
    for flowpath_num, df in huc12df.groupby("%s%s" % (PREFIX, huc12)):
        # These are upstream errors I should ignore
        if flowpath_num == 0 or len(df.index) < 2:
            continue
        # Get or create the flowpathid from the database
        db_fid = get_flowpath(cursor, huc12, flowpath_num)
        process_flowpath(cursor, huc12, db_fid, df)
    return huc12


def main():
    """Our main function, the starting point for code execution"""
    cursor = PGCONN.cursor()
    # track our work
    with open("myhucs.txt", "w") as fh:
        # Change the working directory to where we have data files
        os.chdir("../../data/%s" % (sys.argv[2],))
        # collect up the GeoJSONs in that directory
        fns = glob.glob("smpl3m_*.json")
        fns.sort()
        i = 0

        progress = tqdm(fns)
        for fn in progress:
            progress.set_description(fn)
            # Save our work every 100 HUC12s,
            # so to keep the database transaction
            # at a reasonable size
            if i > 0 and i % 100 == 0:
                PGCONN.commit()
                cursor = PGCONN.cursor()
            df = get_data(fn)
            huc12 = process(cursor, fn, df)
            fh.write("%s\n" % (huc12,))
            i += 1

    # Commit the database changes
    cursor.close()
    PGCONN.commit()


if __name__ == "__main__":
    main()
