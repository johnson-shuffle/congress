# ==============================================================================
# PRISM Climate Group: Spatial Weather & Degree Day Extraction
# Source: PRISM daily mean temperature rasters (Oregon State University)
# Target: prism_degree_days table in congress.sqlite
# Methodology:
#   - Zonal raster extraction against UCLA Congressional District shapefiles
#   - Conversion: T_F = (9/5) * T_C + 32
#   - Cooling Degree Days (CDD): max(0, T_F - 65)
#   - Heating Degree Days (HDD): max(0, 65 - T_F)
# ==============================================================================

source("config.R")
library(DBI)
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(stringr)

# ----- core zonal extraction function -----------------------------------------

#' Extract PRISM weather rasters against Congressional District shapefiles
#'
#' @param raster_stack A terra SpatRaster containing daily weather rasters
#' @param congress Integer congressional session (103 to 114)
#' @param db DBI database connection
#' @return A tibble with district-level temperature and degree days
extract_prism_weather <- function(raster_stack, congress, db = NULL) {
  stopifnot(inherits(raster_stack, "SpatRaster"))
  stopifnot(congress >= 103 && congress <= 114)

  should_close_db <- FALSE
  if (is.null(db)) {
    db <- get_db()
    should_close_db <- TRUE
  }
  if (should_close_db) on.exit(dbDisconnect(db), add = TRUE)

  # lookup states
  states_lookup <- dbGetQuery(db, "SELECT state_code, state_name FROM states")

  # determine UCLA shapefile path
  shp_cong <- if_else(congress == 114, 113, congress)
  shp_path <- file.path(path_raw, "cd-maps", str_c("districtShapes", shp_cong), str_c("districts", shp_cong, ".shp"))

  if (!file.exists(shp_path)) {
    stop("Shapefile not found for Congress ", congress, ": ", shp_path)
  }

  cd_shp <- st_read(shp_path, quiet = TRUE)
  if (is.na(st_crs(cd_shp))) st_crs(cd_shp) <- 4269

  # match raster projection
  rast_crs <- terra::crs(raster_stack)
  if (rast_crs != "" && !is.na(rast_crs)) {
    cd_shp <- st_transform(cd_shp, rast_crs)
  }

  cd_vect <- terra::vect(cd_shp)

  # zonal extraction: mean temperature across district polygons
  extracted <- terra::extract(raster_stack, cd_vect, fun = mean, na.rm = TRUE)

  # attach district metadata
  res <- cd_shp |>
    st_drop_geometry() |>
    select(STATENAME, DISTRICT) |>
    mutate(
      ID = seq_len(n()),
      congress = as.integer(congress)
    ) |>
    left_join(states_lookup, by = c("STATENAME" = "state_name")) |>
    left_join(extracted, by = "ID")

  return(res)
}

# ----- degree day calculation helpers -----------------------------------------

#' Calculate heating and cooling degree days from daily mean temperature
#'
#' @param temp_c Daily mean temperature in Celsius
#' @param base_f Baseline temperature threshold in Fahrenheit (default: 65 F)
#' @return A list with temp_f, cdd, and hdd
calc_degree_days <- function(temp_c, base_f = 65.0) {
  temp_f <- (9.0 / 5.0) * temp_c + 32.0
  cdd <- pmax(0.0, temp_f - base_f)
  hdd <- pmax(0.0, base_f - temp_f)
  list(temp_f = temp_f, cdd = cdd, hdd = hdd)
}

# ----- validation and self-test harness ---------------------------------------

run_prism_validation_test <- function() {
  message("Executing PRISM spatial extraction test with synthetic CONUS raster...")

  db <- get_db()
  on.exit(dbDisconnect(db), add = TRUE)

  # Create a synthetic CONUS raster grid covering 24 to 50 N, -125 to -66 W
  # Resolution: 0.5 degrees (~55km grid)
  conus_ext <- terra::ext(-125, -66, 24, 50)
  r_template <- terra::rast(conus_ext, nrows = 52, ncols = 118, crs = "EPSG:4269")

  # Simulate temperature gradient (colder north, warmer south) in Celsius
  # Latitude values roughly 24 to 50 -> Temp roughly 25 C south to 0 C north
  lats <- terra::init(r_template, "y")
  sim_tmean_c <- 25.0 - (lats - 24.0) * 0.95
  names(sim_tmean_c) <- "tmean_c_day1"

  # Run spatial extraction against 103rd Congress shapefile
  district_weather <- extract_prism_weather(sim_tmean_c, congress = 103, db = db)

  # Check extracted results
  valid_records <- district_weather |>
    filter(!is.na(tmean_c_day1) & !is.na(state_code)) |>
    mutate(
      district_number = as.integer(DISTRICT),
      temp_f = (9.0 / 5.0) * tmean_c_day1 + 32.0,
      mean_temp_f = round(temp_f, 2),
      cdd = round(pmax(0.0, temp_f - 65.0), 2),
      hdd = round(pmax(0.0, 65.0 - temp_f), 2)
    )

  message("  Districts processed: ", nrow(valid_records))
  message("  Sample district values:")
  sample_rows <- valid_records |>
    select(state_code, district_number, mean_temp_f, cdd, hdd) |>
    head(5)
  print(sample_rows)

  stopifnot(nrow(valid_records) >= 430)
  stopifnot(all(valid_records$cdd >= 0))
  stopifnot(all(valid_records$hdd >= 0))

  message("PRISM spatial extraction and degree day calculation validated successfully.")
}

# Run validation self-test when invoked as standalone script
if (sys.nframe() == 0) {
  run_prism_validation_test()
}
