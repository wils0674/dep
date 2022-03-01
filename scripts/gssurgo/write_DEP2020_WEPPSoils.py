# Write_DEP2020_WEPPsoils.py
#
# Using data tables generated from the SDM Soils Data version 10.15.2012
# write the WEPP soils files (.SOL) in the 2006.2 format.
#
# Output files will be written using the SSURGO MUKEY as the name descriptor.
#
# original coding: DE James 03.2013
#  04/3/2013 - add 'HrzCount > 0' to selection criteria
#  09/2014 - update to use FY2014 data - no changes except you must hand-edit
#            those SOL files with more than 9 horizons...should be just 1
#  06/2015 - update to use 8-state MWDEP data
#  04/2018 - Use the subset -- MWDEP_SoilParameters table for reduced file count
#            Set and use zCount to write no more than 9 horizon records -- 185 records or so
#  11/2019 - modify to trap >9 recortds and write the correct hrzCount
#  10/28/2019 - Modify write statement for Kr from f6.2 to f6.4
#  12/16/2019 - add comment status to MUKEY declaration as per DHerzmann and his new reader
#  05/26/2021 - Add 'Is Not Null' test for KI, TC, KR after failed record for udorthents
#               Add test for file exists so I didn't to do 200,000 records again, because
#                this a national extraction (300,000+ MUKEYs)


## modules
import os
from pyiem.util import get_dbconn

##
outPath = "test"

## SQL Server connect
cxn = get_dbconn("idep")

Pcursor = cxn.cursor()
Pcursor.execute(
    """Select mukey, compname, Albedo, Texture, HrzCount, KI, KR, TC, KB
    From DEP_SoilParameters
    Where HrzCount > 0 AND Albedo Is Not Null AND compname != 'Aquolls' 
    AND KI Is Not Null AND KR Is Not Null AND TC Is Not Null
    AND KB Is Not Null
    """
)

MURows = Pcursor.fetchall()
del Pcursor

for row in MURows:
    fname = outPath + "/DEP_" + str(row[0]) + ".SOL"

    if not os.path.exists(fname):
        outf = open(fname, "w")
        print("writing: " + fname + "  - " + row[1])

        outf.write("2006.2\n")
        outf.write("#\n")
        outf.write("# DEP SOL\n")
        outf.write("# DE James 05_2021\n")
        outf.write("# Source: US gSSURGO CONUS Soil DB 07/2020\n")
        outf.write("#\n")
        outf.write("SSURGO MUKEY: " + str(row[0]) + "\n")
        outf.write("1 1\n")

        if row[4] > 9:
            row[4] = 9

        outf.write(
            "'{0}' '{1}' {2} {3} {4} {5:.2f} {6:.4f} {7:.2f} {8:.2f}\n".format(
                row[1],
                row[3],
                row[4],
                row[2],
                "0.75",
                row[5],
                row[6],
                row[7],
                row[8],
            )
        )

        HrzCursor = cxn.cursor()
        zCount = 0
        HrzCursor.execute(
            """SELECT DepthTo_mm, Sand, Clay, OM, CEC7, FragTot
                                  FROM DEP_SoilFractions
                                  WHERE mukey = %s
                                  ORDER by DepthTo_mm""",
            (row[0],),
        )
        for row2 in HrzCursor:
            zCount += 1
            if zCount < 10:
                outf.write(
                    "       {0} {1} {2} {3} {4} {5}\n".format(
                        row2[0],
                        row2[1],
                        row2[2],
                        row2[3],
                        row2[4],
                        row2[5],
                    )
                )

        del HrzCursor
        outf.write("0 0 0 0\n")
        outf.write("255 255 255\n")

        outf.close()
