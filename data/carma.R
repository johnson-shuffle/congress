# ==============================================================================
# CARMA: Carbon Monitoring for Action (Power Plant & District Emissions)
# Source: raw/carma/CARMAv30_PowerUser.zip (Region.csv and Plant.csv)
# ==============================================================================

source("config.R")
library(readr)
library(dplyr)
library(stringr)

# ----- load data & database ---------------------------------------------------

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

states <- dbGetQuery(db, "SELECT fips, state_code, state_name FROM states")

zip_path <- file.path(path_raw, "carma", "CARMAv30_PowerUser.zip")
if (!file.exists(zip_path)) {
  stop("Missing CARMA raw archive: ", zip_path)
}

# ----- process congressional districts ----------------------------------------

message("Ingesting CARMA congressional district emissions...")
regions <- read_csv(unz(zip_path, "Region.csv"), col_types = cols())

carma_district <- regions |>
  filter(region_type == "congressperson") |>
  mutate(
    id_str = as.character(region_id),
    fips = as.integer(substr(id_str, 5, 6)),
    district_code = as.integer(substr(id_str, 7, 8))
  ) |>
  left_join(select(states, fips, state_code), by = "fips") |>
  transmute(
    fips,
    state_code,
    district_code,
    representative = region_name,
    plant_count = as.integer(plantcount),
    carbon_2004 = carbon_past,
    carbon_2009 = carbon_present,
    carbon_future = carbon_future,
    energy_2004 = energy_past,
    energy_2009 = energy_present,
    energy_future = energy_future,
    intensity_2004 = intensity_past,
    intensity_2009 = intensity_present,
    intensity_future = intensity_future,
    share_fossil_2004 = share_fos_past,
    share_fossil_2009 = share_fos_present,
    share_hydro_2004 = share_hyd_past,
    share_hydro_2009 = share_hyd_present,
    share_nuclear_2004 = share_nuc_past,
    share_nuclear_2009 = share_nuc_present,
    share_renewable_2004 = share_ren_past,
    share_renewable_2009 = share_ren_present
  ) |>
  arrange(fips, district_code)

# Execute DDL
execute_sql_file(db, file.path(path_sql, "tables", "carma-district.sql"))

# Insert into carma_district
dbAppendTable(db, "carma_district", as.data.frame(carma_district))
message("Inserted ", nrow(carma_district), " rows into carma_district")

# ----- process power plants ---------------------------------------------------

message("Ingesting CARMA power plant emissions...")
plants <- read_csv(unz(zip_path, "Plant.csv"), col_types = cols())

carma_plants <- plants |>
  filter(iso3 == "USA") |>
  mutate(
    clean_region = case_when(
      region == "Washington, D.C." ~ "District of Columbia",
      TRUE ~ region
    )
  ) |>
  left_join(select(states, state_name, state_code), by = c("clean_region" = "state_name")) |>
  transmute(
    plant_id = as.integer(plant_id),
    plant_name = plant,
    company = company,
    city = city,
    county = county,
    state_code = state_code,
    district_code = as.integer(congdist),
    latitude = latitude,
    longitude = longitude,
    carbon_2004 = carbon_past,
    carbon_2009 = carbon_present,
    carbon_future = carbon_future,
    energy_2004 = energy_past,
    energy_2009 = energy_present,
    energy_future = energy_future,
    intensity_2004 = intensity_past,
    intensity_2009 = intensity_present,
    intensity_future = intensity_future
  ) |>
  arrange(plant_id)

# Execute DDL
execute_sql_file(db, file.path(path_sql, "tables", "carma-plants.sql"))

# Insert into carma_plants
dbAppendTable(db, "carma_plants", as.data.frame(carma_plants))
message("Inserted ", nrow(carma_plants), " rows into carma_plants")

# ----- sanity checks ----------------------------------------------------------

n_dist <- dbGetQuery(db, "SELECT count(*) AS n FROM carma_district")$n
n_plants <- dbGetQuery(db, "SELECT count(*) AS n FROM carma_plants")$n

message("Verification:")
message("  carma_district: ", n_dist, " districts")
message("  carma_plants:   ", n_plants, " US power plants")

stopifnot(n_dist == 435)
stopifnot(n_plants == 8690)
message("CARMA ingestion completed successfully.")
