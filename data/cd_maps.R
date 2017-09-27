# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
load_tidy()
library(rgdal)

db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

td <- tempdir()

# ------------------------------------------------------------------------------
# zip files
#   http://cdmaps.polisci.ucla.edu/
# ------------------------------------------------------------------------------
cd_maps_fun <- function(congress, dir = NULL) {
  
  # download
  download.file(
    str_c('http://cdmaps.polisci.ucla.edu/shp/districts', congress, '.zip'),
    destfile = str_c(td, '/cd.zip')
  )
  
  # unzip
  unzip(str_c(td, '/cd.zip'), exdir = td)
  
  # extract shape object
  readOGR(str_c(td, '/districtShapes/'), str_c('districts', congress))

}

cd_maps <- map(103:114, cd_maps_fun, dir = td)

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
save(cd_maps, file = '~/GoogleDrive/Projects/congress/cd_maps.Rda')
