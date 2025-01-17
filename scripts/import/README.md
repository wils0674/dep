Gelder Data Import Procedure
============================

Brian Gelder provides me a 7zip file with individual flowpaths included.  There
is one file per HUC12.

1. `python flowpath_importer.py <scenario> <dirname in ../../data/>`
1. `python clear_files.py <scenario>`
1. go to ../util and run `python make_dirs.py <scenario>`
1. cd to ../import and run `python flowpath2prj.py <scenario>`
1. `python prj2wepp.py <scenario>`
1. `python dbset_ofe.py <scenario>`
1. `python package_myhucs.py <scenario>`
1. go to ../cligen and run `python assign_climate_file.py <scenario>`
1. If new HUC12s are present, get an updated simplified HUC12 from Dave.
1. Copy laptop database tables `huc12`, `flowpaths` and `flowpath_points` to IEMDB
1. copy `myhucs.txt` up to IEM and run `python clear_files.py`
1. extract the `dep.tar` file on IEM
1. On IEM run `cligen/locate_clifile.py <scenario>`
1. On IEM run `util/make_dirs.py <scenario>`

This query finds any new HUC12s and inserts the geometry into a table.

    insert into huc12
    (states, hu_12_name, huc_8, huc_12, simple_geom, geom, scenario)
    select states, name, huc_8, huc_12, st_geometryn(geom, 1),
    geom, 0 from p200 where huc_12 in
    (select distinct huc_12 from flowpaths where huc_12 not in
    (select huc_12 from huc12 where scenario = 0) and scenario = 0);

    insert into huc12
    (states, hu_12_name, huc_8, huc_12, simple_geom, geom, scenario)
    select states, name, substr(huc12, 1, 8), huc12,
    ST_Transform(st_geometryn(simple_geom, 1), 5070),
    ST_Transform(geom, 5070), 0 from wbd_huc12 where huc12 in
    (select distinct huc_12 from flowpaths where huc_12 not in
    (select huc_12 from huc12 where scenario = 0) and scenario = 0);

We should also check that we don't have unknown tables.

    select distinct huc_12 from flowpaths where scenario = 0 and huc_12 not in (select huc_12 from huc12 where scenario = 0) ORDER by huc_12;
