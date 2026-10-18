# ==============================================================================
# EIA: Power Plants Form 860 Ingestion
# Source: raw/eia/eia_plants_gps.csv (EIA Form 860 GPS & Utility Metadata)
# Target: eia_plants table in congress.sqlite
# ==============================================================================

source("config.R")
library(readr)
library(dplyr)
library(stringr)

# ----- database connection ----------------------------------------------------

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

states <- dbGetQuery(db, "SELECT state_code FROM states")$state_code

# ----- read raw form 860 plant data -------------------------------------------

plants_csv <- file.path(path_raw, "eia", "eia_plants_gps.csv")
if (!file.exists(plants_csv)) {
  stop("Missing EIA plants raw file: ", plants_csv)
}

message("Ingesting EIA Form 860 power plants from raw CSV...")
raw_plants <- read_csv(plants_csv, col_types = cols())

df_plants <- raw_plants |>
  transmute(
    plant_code = as.integer(`Plant Code`),
    utility_id = as.integer(`Utility ID`),
    plant_name = `Plant Name`,
    street_address = `Street Address`,
    city = City,
    county = County,
    state_code = if_else(is.na(State) | State == "" | !State %in% states, NA_character_, State),
    zip = as.character(Zip),
    water_source = `Name of Water Source`,
    nerc_region = `NERC Region`,
    primary_naics = as.integer(`Primary Purpose (NAICS Code)`),
    system_owner = `Transmission or Distribution System Owner`,
    regulatory_status = `Regulatory Status`,
    sector_name = `Sector Name`,
    sector = as.integer(Sector),
    grid_voltage_kv = as.numeric(`Grid Voltage (kV)`),
    ferc_cogen_status = `FERC Cogeneration Status`,
    iso_rto = `ISO RTO`,
    iso_rto_code = `ISO RTO Code`,
    latitude = as.numeric(Latitude),
    longitude = as.numeric(Longitude)
  ) |>
  arrange(plant_code)

# ----- execute DDL and populate table -----------------------------------------

execute_sql_file(db, file.path(path_sql, "tables", "eia-plants.sql"))
dbAppendTable(db, "eia_plants", as.data.frame(df_plants))
message("Inserted ", nrow(df_plants), " rows into eia_plants")

# ----- verification -----------------------------------------------------------

n_plants <- dbGetQuery(db, "SELECT count(*) AS n FROM eia_plants")$n
message("Verification: ", n_plants, " facilities in eia_plants")
stopifnot(n_plants == 7289)
message("EIA Form 860 ingestion completed successfully.")
