#!/bin/bash  

# ----- locations ---------------------------------------------------------

# database
db=congress

# destination for files
temp=~/temp


# ----- congressional district maps ---------------------------------------

# loop
for i in `seq 103 1 114`; do

    stem="districts"$i
    
    # zip file
    curl -O "http://cdmaps.polisci.ucla.edu/shp/"$stem".zip"
    
    # unzip
    unzip $stem".zip" -d $temp

    # table
    t="cd_"$i
    echo "DROP TABLE IF EXISTS cd_$i" | psql -d $db

    # file
    f=$temp"/districtShapes/"$stem".shp"

    # copy to database
    shp2pgsql -s 4269 $f $t | psql -h localhost -d $db

    # clean up
    rm $stem".zip"
    rm -rf $temp"/districtShapes"

done
