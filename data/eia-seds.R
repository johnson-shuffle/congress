# ==============================================================================
# EIA: Raw SEDS Production and Prices Ingestion
# Sources: raw/eia/Complete_SEDS.csv, raw/eia/eia_prices.csv
# Target: eia_seds, eia_prices in congress.sqlite
# ==============================================================================

source("config.R")
library(readr)
library(dplyr)
library(tidyr)

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

# Execute DDL
execute_sql_file(db, file.path(path_sql, "tables", "eia-seds.sql"))

# ----- 1. Ingest Complete_SEDS.csv --------------------------------------------

seds_csv <- file.path(path_raw, "eia", "Complete_SEDS.csv")
if (!file.exists(seds_csv)) {
  stop("Missing Complete_SEDS.csv in raw/eia/")
}

message("Ingesting raw SEDS dataset into eia_seds...")
seds_raw <- read_csv(seds_csv, col_types = cols())

seds_clean <- seds_raw |>
  transmute(
    data_status = as.integer(Data_Status),
    msn = MSN,
    state_code = StateCode,
    year = as.integer(Year),
    data = as.numeric(Data)
  )

dbAppendTable(db, "eia_seds", as.data.frame(seds_clean))
message("Inserted ", nrow(seds_clean), " rows into eia_seds")

# ----- 2. Ingest eia_prices.csv -----------------------------------------------

prices_csv <- file.path(path_raw, "eia", "eia_prices.csv")
if (!file.exists(prices_csv)) {
  stop("Missing eia_prices.csv in raw/eia/")
}

message("Ingesting raw energy prices into eia_prices...")
pr_raw <- read_csv(prices_csv, col_types = cols())

pr_clean <- pr_raw |>
  select(-Data_Status) |>
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "price"
  ) |>
  transmute(
    state_code = State,
    msn = MSN,
    year = as.integer(year),
    price = as.numeric(price)
  )

dbAppendTable(db, "eia_prices", as.data.frame(pr_clean))
message("Inserted ", nrow(pr_clean), " rows into eia_prices")

message("EIA SEDS and prices raw ingestion completed successfully.")
