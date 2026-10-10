# ----- Preample ---------------------------------------------------------

source("config.R")

td <- tempdir()
raw_cd_dir <- file.path(path_raw, "cd-maps")
dir.create(raw_cd_dir, showWarnings = FALSE, recursive = TRUE)


# ----- Zip Files --------------------------------------------------------

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


# ----- Save Output ------------------------------------------------------

# Processed with sf in Tier 2 pipeline

