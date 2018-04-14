#!/bin/bash  

# ------------------------------------------------------------------------------
# locations
# ------------------------------------------------------------------------------

# database
db=~/Projects/congress/opensecrets.sqlite

# zip files
cd ~/Desktop/opensecrets/

# ------------------------------------------------------------------------------
# create empty tables in the database
# ------------------------------------------------------------------------------

# candidates
echo -e "CREATE TABLE cands (
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
    PRIMARY KEY(cycle, fecid, osid)
);\n.exit" | sqlite3 $db

# pacs to candidates
echo -e "CREATE TABLE pacs (
    cycle INTEGER,
    fecrecno TEXT,
    pacid TEXT,
    osid TEXT,
    amount REAL,
    date TEXT,
    realcode TEXT,
    type TEXT,
    direct_indirect TEXT,
    fecid TEXT
);\n.exit" | sqlite3 $db

# pac to pac
echo -e "CREATE TABLE pac_other (
    cycle INTEGER,
    fecrecno TEXT,
    filerid TEXT,
    donorcmte TEXT,
    contriblendtrans TEXT,
    city TEXT,
    state TEXT,
    zipcode INTEGER,
    fec_occ_emp TEXT,
    primcode TEXT,
    date TEXT,
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
    source TEXT
);\n.exit" | sqlite3 $db

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
    date TEXT,
    amount REAL,
    street TEXT,
    city TEXT,
    state TEXT,
    zip INTEGER,
    recipcode TEXT,
    type TEXT,
    cmteid TEXT,
    otherid TEXT,
    gender TEXT,
    microfilm TEXT,
    occ TEXT,
    emp TEXT,
    source TEXT
);\n.exit" | sqlite3 $db

# committees
echo -e "CREATE TABLE cmtes (
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
    active TEXT
);\n.exit" | sqlite3 $db


# ------------------------------------------------------------------------------
# loop the zip files
# ------------------------------------------------------------------------------

# array of years
y=($(seq 90 2 98) $(seq -f "%02g" 0 2 16))

# array of tables
t=("cands" "cmtes" "indivs" "pacs" "pac_other")

for i in ${y[@]}; do
    for j in ${t[2]}; do

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

        # add to sqlite db
        echo -e ".mode csv\n.import" $f $j | sqlite3 $db

        # clean up
        rm $f
        rm $f".bak"
        rm -rf $z
        
    done
done
