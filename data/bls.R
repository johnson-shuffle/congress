# ==============================================================================
# Ingestion: Bureau of Labor Statistics (BLS)
# Reads raw BLS files from raw/bls/ and populates `bls_unempl` and `bls_cpi`.
# Covers state annual unemployment rates (1976–2014) and CPI inflation (1913–2015).
# ==============================================================================

source("config.R")
library(readr)
library(dplyr)
library(stringr)
library(DBI)


# ----- unemployment -----------------------------------------------------------

unemp_file <- "raw/bls/unemp.txt"

if (!file.exists(unemp_file)) {
  stop("Missing raw file: ", unemp_file)
}

unemp_raw <- read_delim(
  unemp_file,
  delim = "\t",
  col_types = cols(.default = "c"),
  show_col_types = FALSE
)

# Clean whitespace and parse fields
# LASST = State-level series; measure code 03 = unemployment rate
unemp_raw$series_id <- str_trim(unemp_raw$series_id)
unemp_raw$value <- as.numeric(unemp_raw$value)
unemp_raw$year <- as.integer(unemp_raw$year)

bls_unempl <- unemp_raw |>
  filter(str_sub(series_id, 1, 5) == "LASST" & str_sub(series_id, 19, 20) == "03") |>
  mutate(fips = as.integer(str_sub(series_id, 6, 7))) |>
  filter(fips != 72 & !is.na(fips) & !is.na(value)) |>
  group_by(fips, year) |>
  summarise(avg_unempl = round(mean(value, na.rm = TRUE), 4), .groups = "drop") |>
  select(year, fips, avg_unempl)


# ----- inflation --------------------------------------------------------------

cpi_file <- "raw/bls/cpi.txt"

if (!file.exists(cpi_file)) {
  stop("Missing raw file: ", cpi_file)
}

cpi_raw <- read_delim(
  cpi_file,
  delim = "\t",
  col_types = cols(.default = "c"),
  show_col_types = FALSE
)

cpi_raw$value <- as.numeric(cpi_raw$value)
cpi_raw$year <- as.integer(cpi_raw$year)

bls_cpi <- cpi_raw |>
  filter(!is.na(year) & !is.na(value)) |>
  group_by(year) |>
  summarise(avg_cpi = round(mean(value, na.rm = TRUE), 4), .groups = "drop") |>
  select(year, avg_cpi)


# ----- insert-to-database -----------------------------------------------------

db <- get_db()

execute_sql_file(db, file.path(path_sql, "bls-unempl.sql"))
dbWriteTable(db, "bls_unempl", bls_unempl, append = TRUE)

execute_sql_file(db, file.path(path_sql, "bls-cpi.sql"))
dbWriteTable(db, "bls_cpi", bls_cpi, append = TRUE)

message("Successfully populated bls_unempl: ", dbGetQuery(db, "SELECT count(*) FROM bls_unempl")[[1]], " rows.")
message("Successfully populated bls_cpi: ", dbGetQuery(db, "SELECT count(*) FROM bls_cpi")[[1]], " rows.")
