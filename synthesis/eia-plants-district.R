# ==============================================================================
# EIA: Power Plants Congressional District Spatial Matching
# Sources:
#   - eia_plants (Form 860 GPS locations in congress.sqlite)
#   - raw/cd-maps/districtShapes103 through districtShapes113 (UCLA shapefiles)
# Target: eia_plants_district table in congress.sqlite
# ==============================================================================

source("config.R")
library(DBI)
library(sf)
library(dplyr)
library(stringr)

# ----- database connection ----------------------------------------------------

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

states_lookup <- dbGetQuery(db, "SELECT state_code, state_name FROM states")

# ----- query active power plants with valid coordinates -----------------------

message("Querying EIA Form 860 power plants with GPS coordinates...")
plants <- dbGetQuery(db, "
  SELECT plant_code, state_code, latitude, longitude
  FROM eia_plants
  WHERE latitude IS NOT NULL 
    AND longitude IS NOT NULL 
    AND state_code IS NOT NULL
    AND state_code != 'DC'
  ORDER BY plant_code
") |>
  left_join(states_lookup, by = "state_code")

message("Loaded ", nrow(plants), " plants with valid coordinates.")

# convert plants to simple features (NAD83 / EPSG:4269)
plants_sf <- st_as_sf(plants, coords = c("longitude", "latitude"), crs = 4269, remove = FALSE)

# ----- loop through Congresses 103 to 114 -------------------------------------

all_district_matches <- list()

for (cong in 103:114) {
  message("Processing spatial join for Congress ", cong, "...")

  # Shapefile path (114th Congress uses 113th Congress decennial redistricting plan)
  shp_cong <- if_else(cong == 114, 113, cong)
  shp_path <- file.path(path_raw, "cd-maps", str_c("districtShapes", shp_cong), str_c("districts", shp_cong, ".shp"))

  if (!file.exists(shp_path)) {
    stop("Missing shapefile for Congress ", cong, ": ", shp_path)
  }

  cd_shp <- st_read(shp_path, quiet = TRUE)
  if (is.na(st_crs(cd_shp))) {
    st_crs(cd_shp) <- 4269
  } else {
    cd_shp <- st_transform(cd_shp, 4269)
  }

  # Point-in-polygon spatial join
  joined <- st_join(plants_sf, cd_shp[, c("STATENAME", "DISTRICT")])

  # Resolve borderline/coastal points with nearest district within the same state
  na_idx <- which(is.na(joined$DISTRICT))
  if (length(na_idx) > 0) {
    message("  Resolving ", length(na_idx), " boundary points using nearest state district...")
    for (i in na_idx) {
      target_statename <- joined$state_name[i]
      sub_cd <- cd_shp |> filter(STATENAME == target_statename)
      if (nrow(sub_cd) > 0) {
        nearest_idx <- st_nearest_feature(joined[i, ], sub_cd)
        joined$DISTRICT[i] <- sub_cd$DISTRICT[nearest_idx]
      }
    }
  }

  # Clean and format output columns
  cong_df <- joined |>
    st_drop_geometry() |>
    transmute(
      plant_code = as.integer(plant_code),
      congress = as.integer(cong),
      state_code = state_code,
      district_number = as.integer(if_else(is.na(DISTRICT), "0", DISTRICT)),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    )

  all_district_matches[[as.character(cong)]] <- cong_df
}

df_all <- bind_rows(all_district_matches)
message("Total plant-district records generated: ", nrow(df_all))

# ----- execute DDL and populate database --------------------------------------

execute_sql_file(db, file.path(path_sql, "tables", "eia-plants-district.sql"))
dbAppendTable(db, "eia_plants_district", as.data.frame(df_all))
message("Successfully inserted ", nrow(df_all), " rows into eia_plants_district.")

# ----- verification -----------------------------------------------------------

n_count <- dbGetQuery(db, "SELECT count(*) AS n FROM eia_plants_district")$n
n_plants_unique <- dbGetQuery(db, "SELECT count(DISTINCT plant_code) AS n FROM eia_plants_district")$n
n_missing_dist <- dbGetQuery(db, "SELECT count(*) AS n FROM eia_plants_district WHERE district_number IS NULL")$n

message("Verification: ", n_count, " total records across 12 Congresses (", n_plants_unique, " unique plants).")
stopifnot(n_count == nrow(df_all))
stopifnot(n_missing_dist == 0)
message("EIA plant spatial district resolution completed successfully.")
