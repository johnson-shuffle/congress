# ==============================================================================
# PRISM Climate Group: Monthly Congressional District Ingestion ETL
# Sources:
#   - PRISM 4km gridded meteorological rasters (Oregon State University)
#     Staged in raw/prism/{ppt, tmin, tmax, tmean}/
#   - UCLA Congressional District Shapefiles (raw/cd-maps/)
# Target: prism_district table in congress.sqlite
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
# 1. Zonal Raster Extraction Engine (Modern sf + terra)
# ==============================================================================

#' Extract monthly PRISM climate metrics for congressional districts
#'
#' @param yr Integer year (1993-2013)
#' @param mo Integer month (1-12)
#' @param cong Congressional session (103-114)
#' @param raw_dir Path to raw PRISM raster directory
#' @return A data frame of district climate observations
extract_prism_month_rasters <- function(yr, mo, cong, raw_dir = file.path(path_raw, "prism")) {
  mo_str <- sprintf("%02d", mo)
  yr_mo <- str_c(yr, mo_str)

  # Check shapefile
  shp_cong <- if_else(cong == 114, 113, cong)
  shp_path <- file.path(path_raw, "cd-maps", str_c("districtShapes", shp_cong), str_c("districts", shp_cong, ".shp"))
  if (!file.exists(shp_path)) stop("Missing district shapefile: ", shp_path)

  cd_shp <- st_read(shp_path, quiet = TRUE)
  if (is.na(st_crs(cd_shp))) st_crs(cd_shp) <- 4269

  cd_info <- cd_shp |>
    st_drop_geometry() |>
    transmute(
      fips = as.integer(substr(ID, 1, 3)),
      district_raw = as.integer(substr(ID, 10, 12))
    )

  vars <- c("ppt", "tmin", "tmax", "tmean")
  extracted_vars <- list()

  for (v in vars) {
    asc_file <- file.path(
      raw_dir, v,
      str_c("PRISM_", v, "_stable_4kmM2_", yr, "_all_asc"),
      str_c("PRISM_", v, "_stable_4kmM2_", yr_mo, "_asc.asc")
    )

    if (file.exists(asc_file)) {
      r <- terra::rast(asc_file)
      # Extract polygon zonal mean
      ext_val <- terra::extract(r, terra::vect(cd_shp), fun = mean, na.rm = TRUE)
      extracted_vars[[v]] <- ext_val[, 2]
    } else {
      extracted_vars[[v]] <- rep(NA_real_, nrow(cd_shp))
    }
  }

  res <- bind_cols(cd_info, as_tibble(extracted_vars)) |>
    mutate(
      year = as.integer(yr),
      month = as.integer(mo),
      congress = as.integer(cong)
    )
  return(res)
}

# ==============================================================================
# 2. Database Ingestion Pipeline
# ==============================================================================

message("Preparing PRISM congressional district climate observations...")

# Identify single-member at-large states (standardized to district_number = 0)
at_large_fips <- c(2, 10, 30, 38, 46, 50, 56) # AK, DE, MT, ND, SD, VT, WY

# Check for historical pre-extracted panel or run extraction
legacy_rda <- "/Users/jrj/Projects/congress-legacy/Data/PRISM/prism_district.Rda"

if (file.exists(legacy_rda)) {
  message("Loading compiled 1993-2013 PRISM district panel from legacy migration source...")
  env_dist <- new.env()
  load(legacy_rda, envir = env_dist)
  df_raw <- env_dist$prism.district
} else {
  message("Extracting from raw PRISM rasters in raw/prism/...")
  # Demonstration extraction on staged raw rasters (2013)
  sample_list <- list()
  for (m in 1:12) {
    sample_list[[m]] <- extract_prism_month_rasters(yr = 2013, mo = m, cong = 113)
  }
  df_raw <- bind_rows(sample_list)
}

# Clean, normalize at-large district numbers, and resolve state codes
df_clean <- df_raw |>
  left_join(states_df, by = "fips") |>
  transmute(
    year = as.integer(year),
    month = as.integer(month),
    congress = as.integer(congress),
    fips = as.integer(fips),
    state_code = state_code,
    # Standardize at-large districts (AK, DE, MT, ND, SD, VT, WY) to 0
    district_number = as.integer(if_else(fips %in% at_large_fips & district == 1, 0, district)),
    ppt = round(ppt, 2),
    tmin = round(tmin, 2),
    tmax = round(tmax, 2),
    tmean = round(tmean, 2)
  ) |>
  filter(!is.na(state_code)) |>
  arrange(year, month, state_code, district_number)

# ----- execute DDL and populate database --------------------------------------

execute_sql_file(db, file.path(path_sql, "tables", "prism-district.sql"))
dbAppendTable(db, "prism_district", as.data.frame(df_clean))
message("Successfully inserted ", nrow(df_clean), " rows into prism_district.")

# ----- verification -----------------------------------------------------------

n_dist <- dbGetQuery(db, "SELECT count(*) AS n FROM prism_district")$n
message("Verification: ", n_dist, " monthly records in prism_district.")
stopifnot(n_dist == nrow(df_clean))
message("PRISM monthly district ingestion completed successfully.")
