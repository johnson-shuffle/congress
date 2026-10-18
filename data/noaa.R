# ==============================================================================
# NOAA CPC: Population-Weighted Daily Degree Days Ingestion (1993-2016)
# Source: NOAA Climate Prediction Center
#   https://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/daily_data/
# Target: noaa_degree_days table and noaa_state_degree_days view in congress.sqlite
# ==============================================================================

source("config.R")
library(DBI)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# ----- database connection ----------------------------------------------------

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

states_lookup <- dbGetQuery(db, "SELECT state_code FROM states")$state_code

# ----- raw data destination ---------------------------------------------------

raw_noaa_dir <- file.path(path_raw, "noaa")
dir.create(raw_noaa_dir, showWarnings = FALSE, recursive = TRUE)

base_url <- "https://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/daily_data"

# ----- helper function to download and parse NOAA text exports ----------------

fetch_noaa_file <- function(year, type = c("Cooling", "Heating")) {
  type <- match.arg(type)
  year_dir <- file.path(raw_noaa_dir, as.character(year))
  dir.create(year_dir, showWarnings = FALSE, recursive = TRUE)

  local_file <- file.path(year_dir, str_c("StatesCONUS.", type, ".txt"))
  remote_url <- str_c(base_url, "/", year, "/StatesCONUS.", type, ".txt")

  if (!file.exists(local_file)) {
    download.file(remote_url, destfile = local_file, quiet = TRUE)
  }

  raw_df <- read_delim(local_file, delim = "|", skip = 3, show_col_types = FALSE)
  date_cols <- names(raw_df)[-1]

  val_name <- if_else(type == "Cooling", "cdd", "hdd")

  raw_df |>
    pivot_longer(cols = all_of(date_cols), names_to = "date", values_to = val_name) |>
    transmute(
      state_code = Region,
      date = date,
      !!val_name := as.numeric(.data[[val_name]])
    )
}

# ----- process all study years (1993-2016) ------------------------------------

study_years <- 1993:2016
message("Ingesting NOAA degree days across ", length(study_years), " years (1993-2016)...")

all_records <- list()

for (yr in study_years) {
  message("  Processing year ", yr, "...")
  cdd_df <- fetch_noaa_file(yr, "Cooling")
  hdd_df <- fetch_noaa_file(yr, "Heating")

  merged_yr <- full_join(cdd_df, hdd_df, by = c("state_code", "date")) |>
    mutate(
      year = as.integer(yr),
      cdd = coalesce(cdd, 0),
      hdd = coalesce(hdd, 0)
    ) |>
    filter(state_code %in% states_lookup)

  all_records[[as.character(yr)]] <- merged_yr
}

df_noaa <- bind_rows(all_records)
message("Total daily state observations: ", nrow(df_noaa))

# ----- execute DDL and populate database --------------------------------------

execute_sql_file(db, file.path(path_sql, "tables", "noaa-degree-days.sql"))
dbAppendTable(db, "noaa_degree_days", as.data.frame(df_noaa))
message("Successfully inserted ", nrow(df_noaa), " rows into noaa_degree_days.")
execute_sql_file(db, file.path(path_sql, "views", "noaa-state-degree-days-vw.sql"))

# ----- verification -----------------------------------------------------------

n_daily <- dbGetQuery(db, "SELECT count(*) AS n FROM noaa_degree_days")$n
n_annual <- dbGetQuery(db, "SELECT count(*) AS n FROM noaa_state_degree_days_vw")$n
stopifnot(n_daily == nrow(df_noaa))
stopifnot(n_annual == length(study_years) * 48)

message("Verification: ", n_daily, " daily observations and ", n_annual, " annual state summaries in noaa_state_degree_days_vw.")
message("NOAA degree days ingestion completed successfully.")
