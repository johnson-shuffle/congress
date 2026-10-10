# ==============================================================================
# EIA: Fuel Carbon and Emission Factors Ingestion
# Sources: raw/eia/eia_fuel_type_carbon.csv, raw/eia/eia_fuel_type_carbon_old.csv
# Target: eia_fuel_factors, eia_fuel_factors_old in congress.sqlite
# ==============================================================================

source("config.R")
library(readr)
library(dplyr)

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

message("Ingesting EIA fuel carbon factors...")

f1_path <- file.path(path_raw, "eia", "eia_fuel_type_carbon.csv")
f2_path <- file.path(path_raw, "eia", "eia_fuel_type_carbon_old.csv")

if (!file.exists(f1_path) || !file.exists(f2_path)) {
  stop("Missing fuel factors CSVs in raw/eia/")
}

f1 <- read_csv(f1_path, col_types = cols()) |>
  transmute(
    esource = esource,
    description = `esource.dscr`,
    physical_units = `physical.units`,
    kg_co2_per_mmbtu = as.numeric(`kg.co2.per.mmbtu`),
    g_ch4_per_mmbtu = as.numeric(`g.ch4.per.mmbtu`),
    g_n2o_per_mmbtu = as.numeric(`g.n2o.per.mmbtu`),
    notes = notes
  )

f2 <- read_csv(f2_path, col_types = cols()) |>
  transmute(
    esource = esource,
    esource_other = as.character(`esource.other`),
    description = `esource.dscr`,
    physical_units = `physical.units`,
    mmbtu = as.numeric(mmbtu),
    kg_co2_per_mmbtu = as.numeric(`kg.co2.per.mmbtu`),
    g_ch4_per_mmbtu = as.numeric(`g.ch4.per.mmbtu`),
    g_n2o_per_mmbtu = as.numeric(`g.n2o.per.mmbtu`),
    notes = notes
  )

execute_sql_file(db, file.path(path_sql, "eia-factors.sql"))
dbAppendTable(db, "eia_fuel_factors", as.data.frame(f1))
dbAppendTable(db, "eia_fuel_factors_old", as.data.frame(f2))

message("Inserted ", nrow(f1), " rows into eia_fuel_factors")
message("Inserted ", nrow(f2), " rows into eia_fuel_factors_old")
