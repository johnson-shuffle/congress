# ==============================================================================
# EIA: Electricity Generation & Emissions Panel Synthesis
# Queries: eia_survey_gen, eia_fuel_factors, eia_fuel_factors_old, states from SQLite
# Target: eia_generation table in congress.sqlite
# ==============================================================================

source("config.R")
library(DBI)
library(dplyr)
library(stringr)

# ----- database connection ----------------------------------------------------

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

states <- dbGetQuery(db, "SELECT state_code FROM states")$state_code

# ----- query factors from database --------------------------------------------

co2_factors <- dbGetQuery(db, "
  SELECT esource, kg_co2_per_mmbtu
  FROM eia_fuel_factors
")

co2_factors_old <- dbGetQuery(db, "
  SELECT esource, esource_other, mmbtu, kg_co2_per_mmbtu
  FROM eia_fuel_factors_old
")

# ----- 1. Process Form 759 Utility Records ------------------------------------

message("Processing Form 759 records from eia_survey_gen...")
f759_raw <- dbGetQuery(db, "
  SELECT plant_id, state_code, year, energy_source AS esource, fuel_quantity, net_generation_mwh
  FROM eia_survey_gen
  WHERE survey_form = 'F759'
")

p759 <- f759_raw |>
  left_join(co2_factors_old, by = "esource") |>
  mutate(
    fuel_mmbtu = fuel_quantity * mmbtu,
    co2_metric_tons = fuel_mmbtu * kg_co2_per_mmbtu / 1E9
  ) |>
  filter(!is.na(co2_metric_tons)) |>
  select(plant_id, state_code, year, energy_source = esource, net_generation_mwh, fuel_mmbtu, co2_metric_tons)

# ----- 2. Process Form 906 (1990-1998) ----------------------------------------

message("Processing Form 906 (1990-1998) records from eia_survey_gen...")
f906_98_raw <- dbGetQuery(db, "
  SELECT plant_id, state_code, year, energy_source AS esource, fuel_mmbtu, net_generation_mwh
  FROM eia_survey_gen
  WHERE survey_form = 'F906_NU' AND year <= 1998
")

p1998 <- f906_98_raw |>
  left_join(co2_factors, by = "esource") |>
  mutate(
    fuel_mmbtu = if_else(esource == "BIT", 17, fuel_mmbtu),
    co2_metric_tons = fuel_mmbtu * kg_co2_per_mmbtu / 1E9
  ) |>
  filter(!is.na(co2_metric_tons)) |>
  select(plant_id, state_code, year, energy_source = esource, net_generation_mwh, fuel_mmbtu, co2_metric_tons)

# ----- 3. Process Form 906 (1999-2000) ----------------------------------------

message("Processing Form 906 (1999-2000) records from eia_survey_gen...")
f906_00_raw <- dbGetQuery(db, "
  SELECT plant_id, state_code, year, energy_source AS esource_other, fuel_quantity, net_generation_mwh
  FROM eia_survey_gen
  WHERE survey_form = 'F906_NU' AND year >= 1999
")

p2000 <- f906_00_raw |>
  left_join(co2_factors_old, by = "esource_other") |>
  mutate(
    fuel_mmbtu = fuel_quantity * mmbtu,
    co2_metric_tons = fuel_mmbtu * kg_co2_per_mmbtu / 1E9
  ) |>
  filter(!is.na(co2_metric_tons)) |>
  select(plant_id, state_code, year, energy_source = esource, net_generation_mwh, fuel_mmbtu, co2_metric_tons)

# ----- 4. Process Form 923 (2001-2014) ----------------------------------------

message("Processing Form 923 records from eia_survey_gen...")
f923_raw <- dbGetQuery(db, "
  SELECT plant_id, state_code, year, energy_source AS esource, fuel_mmbtu, net_generation_mwh
  FROM eia_survey_gen
  WHERE survey_form = 'F923'
")

p923 <- f923_raw |>
  left_join(co2_factors, by = "esource") |>
  mutate(co2_metric_tons = fuel_mmbtu * kg_co2_per_mmbtu / 1E9) |>
  filter(!is.na(co2_metric_tons)) |>
  select(plant_id, state_code, year, energy_source = esource, net_generation_mwh, fuel_mmbtu, co2_metric_tons)

# ----- 5. Combine and Populate eia_generation ---------------------------------

message("Synthesizing eia_generation analytical table...")
all_gen <- bind_rows(p923, p2000, p1998, p759) |>
  mutate(
    state_code = if_else(is.na(state_code) | state_code == "" | !state_code %in% states, NA_character_, state_code)
  )

execute_sql_file(db, file.path(path_sql, "eia-generation.sql"))
dbAppendTable(db, "eia_generation", as.data.frame(all_gen))
message("Inserted ", nrow(all_gen), " rows into eia_generation")

# ----- verification -----------------------------------------------------------

n_gen <- dbGetQuery(db, "SELECT count(*) AS n FROM eia_generation")$n
message("Verification: ", n_gen, " records in eia_generation")
stopifnot(n_gen == 217971)
message("EIA electricity generation synthesis completed successfully.")
