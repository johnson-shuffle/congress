# ============================================================================
# Ingest Reference State Metadata into SQLite
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(DBI)
  library(RSQLite)
})

source("config.R")

# ----- load data ------------------------------------------------------------

raw_file <- file.path(path_raw, "states", "us-state-info.csv")

raw_states <- read_csv(
  raw_file,
  col_types = cols(
    statecode = col_character(),
    fips = col_integer(),
    statedscr = col_character(),
    census.region = col_integer(),
    census.division = col_integer(),
    icpsr = col_integer(),
    adage.region = col_character(),
    recs.domain = col_integer(),
    recs.division = col_integer(),
    nerc.region = col_character(),
    interconnect = col_character()
  )
)

# ----- transform ------------------------------------------------------------

at_large_states <- c("AK", "DE", "MT", "ND", "SD", "VT", "WY")

states <- raw_states |>
  rename(
    state_code = statecode,
    state_name = statedscr,
    census_region = `census.region`,
    census_division = `census.division`,
    icpsr_code = icpsr,
    adage_region = `adage.region`,
    recs_domain = `recs.domain`,
    recs_division = `recs.division`,
    nerc_region = `nerc.region`
  ) |>
  mutate(
    at_large = if_else(state_code %in% at_large_states, 1L, 0L)
  ) |>
  select(
    fips,
    state_code,
    state_name,
    census_region,
    census_division,
    icpsr_code,
    adage_region,
    recs_domain,
    recs_division,
    nerc_region,
    interconnect,
    at_large
  ) |>
  arrange(fips)

# ----- database write -------------------------------------------------------

db <- get_db()

dbExecute(db, "PRAGMA foreign_keys = OFF;")
execute_sql_file(db, file.path(path_sql, "_states.sql"))
dbWriteTable(db, "states", states, append = TRUE)
dbExecute(db, "PRAGMA foreign_keys = ON;")

dbDisconnect(db)

message(sprintf("Successfully loaded %d records into 'states' table.", nrow(states)))
