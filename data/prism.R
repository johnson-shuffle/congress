# ==============================================================================
# PRISM Climate Group: Spatial Weather & Degree Day Extraction & Ingestion
# Source:
#   - PRISM 4km gridded meteorological rasters (Oregon State University)
#     Raw monthly 2013 ASCII rasters in raw/prism/raw_files/
#   - UCLA Congressional District Shapefiles (raw/cd-maps/)
#   - Compiled district climate observations in raw/prism/prism_district.Rda
# Target: prism_district table and prism_district_annual view in congress.sqlite
# ==============================================================================

source("config.R")
library(DBI)
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(stringr)

# ----- database connection ----------------------------------------------------

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

states_df <- dbGetQuery(db, "SELECT fips, state_code, state_name FROM states")

# ==============================================================================
# SECTION 1: Zonal Raster Extraction Methodology (Full Pipeline Reference)
# ==============================================================================
# The PRISM pipeline extracts areal-weighted mean climate indicators by overlaying
# PRISM gridded rasters against Congressional District boundary shapefiles.
#
# Process Overview:
# 1. PRISM Grids: Oregon State University PRISM rasters (4km or 800m grid cell resolution)
#    are loaded as terra SpatRaster objects:
#      r <- terra::rast(raster_file_paths)
#
# 2. Polygon Geometry: UCLA CD shapefiles (EPSG:4269 / NAD83) for Congress C:
#      cd_shp <- sf::st_read("raw/cd-maps/districtShapes{C}/districts{C}.shp")
#      if (st_crs(cd_shp) != terra::crs(r)) cd_shp <- st_transform(cd_shp, terra::crs(r))
#
# 3. Zonal Averaging: Mean temperature/precipitation over each district polygon:
#      vals <- terra::extract(r, terra::vect(cd_shp), fun = mean, na.rm = TRUE)
#
# 4. Conversion & Degree Day Math:
#      T_F = (9 / 5) * T_C + 32
#      CDD = pmax(0, T_F - 65)
#      HDD = pmax(0, 65 - T_F)
# ==============================================================================

extract_prism_raster_sample <- function(raster_file, congress, db_con = db) {
  stopifnot(file.exists(raster_file))
  r <- terra::rast(raster_file)

  shp_cong <- if_else(congress == 114, 113, congress)
  shp_path <- file.path(path_raw, "cd-maps", str_c("districtShapes", shp_cong), str_c("districts", shp_cong, ".shp"))
  cd_shp <- st_read(shp_path, quiet = TRUE)
  if (is.na(st_crs(cd_shp))) st_crs(cd_shp) <- 4269

  extracted <- terra::extract(r, terra::vect(cd_shp), fun = mean, na.rm = TRUE)
  return(extracted)
}

# ==============================================================================
# SECTION 2: Database Ingestion (Curated Panel Synthesis)
# ==============================================================================

prism_rda_path <- file.path(path_raw, "prism", "prism_district.Rda")
prism_dd_path  <- file.path(path_raw, "prism", "prism_deg_days.Rda")

if (!file.exists(prism_rda_path)) {
  stop("Missing PRISM district archive: ", prism_rda_path)
}

message("Ingesting PRISM monthly congressional district climate panel (1993-2013)...")

env_dist <- new.env()
load(prism_rda_path, envir = env_dist)
df_raw <- env_dist$prism.district

# Optionally load modeled degree days if present
df_dd <- NULL
if (file.exists(prism_dd_path)) {
  env_dd <- new.env()
  load(prism_dd_path, envir = env_dd)
  df_dd <- env_dd$prism.deg.days
}

# Identify single-member at-large states (standardized to district_number = 0)
at_large_fips <- c(2, 10, 30, 38, 46, 50, 56) # AK, DE, MT, ND, SD, VT, WY

# Clean, transform, and merge with state metadata
df_clean <- df_raw |>
  left_join(states_df, by = "fips") |>
  mutate(
    fips = as.integer(fips),
    year = as.integer(year),
    month = as.integer(month),
    congress = as.integer(congress),
    # Standardize at-large districts to 0 to match congress schema conventions
    district_number = as.integer(if_else(fips %in% at_large_fips & district == 1, 0, district)),
    mean_temp_f = round((9.0 / 5.0) * tmean + 32.0, 2),
    # Thermal degree day defaults from mean temperature
    cdd_calc = round(pmax(0.0, mean_temp_f - 65.0), 2),
    hdd_calc = round(pmax(0.0, 65.0 - mean_temp_f), 2)
  )

# Join modeled degree days for Q4 where available
if (!is.null(df_dd)) {
  df_dd_clean <- df_dd |>
    transmute(
      fips = as.integer(fips),
      district_raw = as.numeric(district),
      year = as.integer(year),
      month = as.integer(month),
      cool_deg_days_modeled = round(cool.deg.days, 2),
      heat_deg_days_modeled = round(heat.deg.days, 2)
    )

  df_clean <- df_clean |>
    left_join(df_dd_clean, by = c("fips", "district" = "district_raw", "year", "month")) |>
    mutate(
      cool_deg_days = coalesce(cool_deg_days_modeled, cdd_calc),
      heat_deg_days = coalesce(heat_deg_days_modeled, hdd_calc)
    ) |>
    select(-cool_deg_days_modeled, -heat_deg_days_modeled, -cdd_calc, -hdd_calc, -district)
} else {
  df_clean <- df_clean |>
    mutate(
      cool_deg_days = cdd_calc,
      heat_deg_days = hdd_calc
    ) |>
    select(-cdd_calc, -hdd_calc, -district)
}

# Final table structure
df_insert <- df_clean |>
  transmute(
    year = year,
    month = month,
    congress = congress,
    fips = fips,
    state_code = state_code,
    district_number = district_number,
    ppt = round(ppt, 2),
    tmin = round(tmin, 2),
    tmax = round(tmax, 2),
    tmean = round(tmean, 2),
    mean_temp_f = mean_temp_f,
    cool_deg_days = cool_deg_days,
    heat_deg_days = heat_deg_days
  ) |>
  filter(!is.na(state_code)) |>
  arrange(year, month, state_code, district_number)

# ----- execute DDL and populate database --------------------------------------

execute_sql_file(db, file.path(path_sql, "prism-district.sql"))
dbAppendTable(db, "prism_district", as.data.frame(df_insert))
message("Successfully inserted ", nrow(df_insert), " rows into prism_district.")

# ----- verification -----------------------------------------------------------

n_dist <- dbGetQuery(db, "SELECT count(*) AS n FROM prism_district")$n
n_annual <- dbGetQuery(db, "SELECT count(*) AS n FROM prism_district_annual")$n

message("Verification: ", n_dist, " monthly records in prism_district.")
message("Verification: ", n_annual, " district-year summaries in prism_district_annual.")

stopifnot(n_dist == 109632)
message("PRISM congressional district climate ingestion completed successfully.")
