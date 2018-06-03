#!/bin/bash

# ------------------------------------------------------------------------------
# locations
# ------------------------------------------------------------------------------

# database
db=opensecrets

# zip files
cd /Users/JRJ/Desktop/opensecrets/

# ------------------------------------------------------------------------------
# create empty tables in the database
# ------------------------------------------------------------------------------

# candidates
echo -e "DROP TABLE IF EXISTS cands;
    CREATE TABLE cands (
    cycle INTEGER,
    fecid TEXT,
    osid TEXT,
    firstlast TEXT,
    party TEXT,
    distid_run TEXT,
    distid_cur TEXT,
    cand_cur TEXT,
    cand_cyc TEXT,
    incumb TEXT,
    recipcode TEXT,
    nopacs TEXT,
    PRIMARY KEY(cycle, fecid));" | psql -d $db

# pacs to candidates
echo -e "DROP TABLE IF EXISTS pacs;
    CREATE TABLE pacs (
    cycle INTEGER,
    fecrecno TEXT,
    pacid TEXT,
    osid TEXT,
    amount REAL,
    date DATE,
    realcode TEXT,
    type TEXT,
    direct_indirect TEXT,
    fecid TEXT,
    PRIMARY KEY(fecrecno));" | psql -d $db

# pac to pac
echo -e "DROP TABLE IF EXISTS pac_other;
    CREATE TABLE pac_other (
    cycle INTEGER,
    fecrecno TEXT,
    filerid TEXT,
    donorcmte TEXT,
    contriblendtrans TEXT,
    city TEXT,
    state TEXT,
    zip TEXT,
    fec_occ_emp TEXT,
    primcode TEXT,
    date DATE,
    amount REAL,
    recipid TEXT,
    party TEXT,
    otherid TEXT,
    recipcode TEXT,
    recipprimcode TEXT,
    amend TEXT,
    report TEXT,
    prim_gen TEXT,
    microfilm TEXT,
    type TEXT,
    realcode TEXT,
    source TEXT,
    PRIMARY KEY(fecrecno, microfilm));" | psql -d $db

# individuals
echo -e "DROP TABLE IF EXISTS indivs;
    CREATE TABLE indivs (
    cycle INTEGER,
    fecrecno TEXT,
    contribid TEXT,
    contrib TEXT,
    recipid TEXT,
    orgname TEXT,
    ultorg TEXT,
    realcode TEXT,
    date DATE,
    amount REAL,
    street TEXT,
    city TEXT,
    state TEXT,
    zip TEXT,
    recipcode TEXT,
    type TEXT,
    cmteid TEXT,
    otherid TEXT,
    gender TEXT,
    microfilm TEXT,
    occ TEXT,
    emp TEXT,
    source TEXT,
    PRIMARY KEY(fecrecno, contribid, microfilm));" | psql -d $db

# committees
echo -e "DROP TABLE IF EXISTS cmtes;
    CREATE TABLE cmtes (
    cycle INTEGER,
    cmteid TEXT,
    pacshort TEXT,
    affiliate TEXT,
    ultorg TEXT,
    recipid TEXT,
    recipcode TEXT,
    fecid TEXT,
    party TEXT,
    primcode TEXT,
    source TEXT,
    sensitive TEXT,
    notdomestic TEXT,
    active TEXT,
    PRIMARY KEY(cycle, cmteid));" | psql -d $db

# ------------------------------------------------------------------------------
# loop the zip files
# ------------------------------------------------------------------------------

# array of years
y=($(seq 90 2 98) $(seq -f "%02g" 0 2 16))

# array of tables
t=("cands" "cmtes" "pac_other") #"indivs" "pacs")

for i in ${y[@]}; do
    for j in ${t[@]}; do

        # filename and zip file
        f=$j$i".txt"
        z="CampaignFin"$i
        unzip $z $f

        # clear out any trailing whitespace
        LANG=C sed -i.bak -E 's/ +\|/\|/g' $f
        LANG=C sed -i.bak -E 's/\| +/\|/g' $f
        LANG=C sed -i.bak -E 's/ +\,/\,/g' $f

        # change quote character to " (double quote "s in the data first)
        LANG=C sed -i.bak 's/"/""/g' $f
        LANG=C sed -i.bak 's/|/"/g' $f

        # convert blank dates to 01/01/0001
        if [[ $j == "indivs" ]];then
            csvfix map -f 9 -fv "" -tv "01/01/0001" -sqf 1:23 $f > "out1.txt"
        elif [[ $j == "pacs" ]];then
            csvfix map -f 6 -fv "" -tv "01/01/0001" -sqf 1:10 $f > "out1.txt"
        elif [[ $j == "pac_other" ]];then
            csvfix map -f 11 -fv "" -tv "01/01/0001" -sqf 1:24 $f > "out1.txt"
        else
            cp $f "out1.txt"
        fi
        rm $f

        # convert to utf-8 encoding
        iconv -f ISO-8859-1 -t utf-8 'out1.txt' > "out2.txt"
        rm "out1.txt"

        # remove any duplicates
        csvfix unique 'out2.txt' > "out3.txt"
        rm "out2.txt"

        # copy to database
        echo "\copy $j from 'out3.txt' csv;" | psql -d $db

        # clean up
        rm "out3.txt"
        rm $f".bak"
        rm -rf $z

    done
done

# indivs 04: duplicate 4110520041045135963 (373570), 
# indivs 12: duplicate 4122020121176554351 (3659686)

# ------------------------------------------------------------------------------
# add industry codes
# ------------------------------------------------------------------------------

echo "DROP TABLE IF EXISTS codes;
    CREATE TABLE codes (
    code TEXT,
    name TEXT,
    grouping TEXT,
    industry TEXT,
    sector_short TEXT,
    sector_long TEXT,
    grouping_alt TEXT,
    PRIMARY KEY(code));" | psql -d $db

echo "\copy codes from 'codes_industries.csv' csv;" | psql -d $db

96, 98, 00, 02 indivs problems
