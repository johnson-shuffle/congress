#!/bin/bash  

# ------------------------------------------------------------------------------
# r script to standardize csv formats (add POP column when missing)
# ------------------------------------------------------------------------------
# setwd('~/Desktop/cffr/')
# for (d in list.dirs()) {
#   for (f in list.files(file.path(d), pattern = '*.csv')) {
#     x <- read.csv(file.path(d,f))
#     if (!'POP' %in% names(x)) { x$POP <- NA }
#     x <- x[match(n, names(x))]
#     write.csv(x, file = file.path(d,f), row.names = F)
#   }
# }

# ------------------------------------------------------------------------------
# locations
# ------------------------------------------------------------------------------

# database
db=~/Projects/congress/congress.sqlite

# zip files
cd ~/Desktop/cffr/

# ------------------------------------------------------------------------------
# create empty tables in the database
# ------------------------------------------------------------------------------

# agencies
echo -e "CREATE TABLE cffr_agency (
    YEAR INTEGER,
    AGENCY TEXT,
    AGENCY_DSCR TEXT
    );\n.exit" | sqlite3 $db

# programs
echo -e "CREATE TABLE cffr_program (
    YEAR INTEGER,
    PROG_ID TEXT,
    PROG_DSCR TEXT
    );\n.exit" | sqlite3 $db

# data
echo -e "CREATE TABLE cffr (
    FIPSST INTEGER,
    FIPSCO INTEGER,
    FIPSPLAC INTEGER,
    STATE TEXT,
    COUNTY TEXT,
    PLACE TEXT,
    POP TEXT,
    CONGDIST TEXT,
    PROG_ID TEXT,
    OBJ_TYPE TEXT,
    AGENCY INTEGER,
    AMOUNT REAL,
    YEAR INTEGER
);\n.exit" | sqlite3 $db

# ------------------------------------------------------------------------------
# loop the zip files
# ------------------------------------------------------------------------------

# array of years
y=($(seq 1993 1 2010))

# array of tables
for i in ${y[@]}; do

    cd ~/Desktop/cffr/

    # agencies and programs
    a=${i:2:3}"agen.txt"
    b=${i:2:3}"prog.txt"

    # sub ; for ,
    sed -i.bak 's/,/;/g' $a
    sed -i.bak 's/,/;/g' $b

    # import
    echo -e "CREATE TABLE agency (RAW TEXT); \
        CREATE TABLE program (RAW TEXT); \
        \n.mode csv \
        \n.import $a agency \
        \n.import $b program\n.exit" | sqlite3 $db

    # directory for state csv files
    cd ${i:2:3}"CFFIND"

    # import
    for f in $ls *.csv; do

        echo -e ".mode csv\n.import $f temp" | sqlite3 $db

    done

    # add year, update, insert - data
    echo -e "ALTER TABLE temp ADD COLUMN YEAR INTEGER; \
        UPDATE temp SET YEAR=$i; \
        INSERT INTO cffr SELECT * FROM temp;\n.exit" | sqlite3 $db
    
    # add year, update, insert - agencies
    echo -e "ALTER TABLE agency ADD COLUMN YEAR INTEGER; \
        UPDATE agency SET YEAR=$i; \
        INSERT INTO cffr_agency \
        SELECT \
            substr(RAW, 1, 4) AS AGENCY, \
            trim(substr(RAW, 5, length(RAW))) AS AGENCY_DSCR, \
            YEAR \
        FROM agency;\n.exit" | sqlite3 $db

    # add year, update, insert - programs
    echo -e "ALTER TABLE program ADD COLUMN YEAR INTEGER; \
        UPDATE program SET YEAR=$i; \
        INSERT INTO cffr_program \
        SELECT \
            substr(RAW, 1, 7) AS PROG_ID, \
            trim(substr(RAW, 8, length(RAW))) AS PROG_DSCR, \
            YEAR AS YEAR \
        FROM program;\n.exit" | sqlite3 $db

    # clean up
    echo -e "DROP TABLE temp;\n.exit" | sqlite3 $db
    echo -e "DROP TABLE agency;\n.exit" | sqlite3 $db
    echo -e "DROP TABLE program;\n.exit" | sqlite3 $db

done
