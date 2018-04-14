#!/bin/bash  

# ------------------------------------------------------------------------------
# locations
# ------------------------------------------------------------------------------

# database
db=~/Projects/congress/congress_gis.sqlite

# destination for files
mkdir ~/Desktop/cd_maps/
cd ~/Desktop/cd_maps/

# ------------------------------------------------------------------------------
# zip files
#   http://cdmaps.polisci.ucla.edu/
# ------------------------------------------------------------------------------
url="http://cdmaps.polisci.ucla.edu/shp/"

for i in `seq 103 1 114`; do
    f="districts"$i
    curl -O $url$f".zip"
    unzip $f".zip"
    echo -e .loadshp "districtShapes/"$f "cd"$i utf-8 4269 | spatialite $db
    rm -rf "districtShapes" $f".zip"
done
