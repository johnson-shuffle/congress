# ==============================================================================
# UCLA Congressional District Shapefiles Ingestion & Curation
# Source: Jeffrey B. Lewis et al., UCLA Congressional District Shapefiles
#   http://cdmaps.polisci.ucla.edu/
# Target: raw/cd-maps/districtShapes103 through districtShapes113
# ==============================================================================

source("config.R")
library(sf)
library(dplyr)
library(stringr)

# ----- raw data destination ---------------------------------------------------

raw_cd_dir <- file.path(path_raw, "cd-maps")
dir.create(raw_cd_dir, showWarnings = FALSE, recursive = TRUE)

# ----- download and unpack UCLA shapefiles if not present ---------------------

get_congress_shapefile <- function(congress, target_dir = raw_cd_dir) {
  stopifnot(congress >= 103 && congress <= 114)

  # 114th Congress uses 113th Congress decennial redistricting plan
  shp_cong <- if_else(congress == 114, 113, congress)
  stem <- str_c("districts", shp_cong)
  folder_path <- file.path(target_dir, str_c("districtShapes", shp_cong))
  shp_file <- file.path(folder_path, str_c(stem, ".shp"))

  if (!file.exists(shp_file)) {
    message("Downloading UCLA shapefiles for Congress ", shp_cong, "...")
    td <- tempdir()
    zip_path <- file.path(td, str_c(stem, ".zip"))
    url <- str_c("http://cdmaps.polisci.ucla.edu/shp/", stem, ".zip")

    download.file(url, destfile = zip_path, mode = "wb")
    unzip(zip_path, exdir = folder_path)
    unlink(zip_path)
  }

  cd <- st_read(shp_file, quiet = TRUE)
  if (is.na(st_crs(cd))) st_crs(cd) <- 4269
  cd
}

# ----- verification across 103rd-114th Congresses -----------------------------

message("Verifying UCLA Congressional District shapefiles (103rd-114th)...")
for (cg in 103:114) {
  shp <- get_congress_shapefile(cg)
  message("  Congress ", cg, ": ", nrow(shp), " districts loaded (CRS: EPSG:", st_crs(shp)$epsg, ").")
}
message("UCLA CD maps verification completed successfully.")
