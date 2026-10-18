# ============================================================================
# Consolidated Federal Funds Report (CFFR) ETL
# Ingests raw CFFR federal spending outlays, agency codes, and program codes
# Covers 1993–2010 across US States, Territories, and Congressional Districts
# Sources: raw/cffr/*agen.txt, raw/cffr/*prog.txt, raw/cffr/*CFFIND/*.csv
# Target tables: cffr_agency, cffr_program, cffr
# ============================================================================

source("config.R")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
})

raw_cffr_dir <- file.path(path_raw, "cffr")
db <- get_db()

# ----- 1. Ingest cffr_agency -------------------------------------------------

message("Ingesting cffr_agency (1993-2010)...")
execute_sql_file(db, file.path(path_sql, "tables", "cffr-agency.sql"))

agen_files <- list.files(raw_cffr_dir, pattern = "agen[.]txt$", full.names = TRUE) |>
  sort()

all_agencies <- map_dfr(agen_files, function(f) {
  yr_2d <- substr(basename(f), 1, 2)
  yr <- ifelse(as.integer(yr_2d) >= 90, 1900L + as.integer(yr_2d), 2000L + as.integer(yr_2d))
  lines <- readLines(f, warn = FALSE)
  lines <- lines[nchar(lines) >= 4]
  tibble(
    year = as.integer(yr),
    agency_code = substr(lines, 1, 4),
    agency_name = trimws(substr(lines, 5, nchar(lines)))
  ) |>
    distinct(year, agency_code, .keep_all = TRUE)
})

dbWithTransaction(db, {
  dbAppendTable(db, "cffr_agency", all_agencies)
})
message(sprintf("Loaded %d agency records into 'cffr_agency'.", nrow(all_agencies)))

# ----- 2. Ingest cffr_program ------------------------------------------------

message("Ingesting cffr_program (1993-2010)...")
execute_sql_file(db, file.path(path_sql, "tables", "cffr-program.sql"))

prog_files <- list.files(raw_cffr_dir, pattern = "prog[.]txt$", full.names = TRUE) |>
  sort()

all_programs <- map_dfr(prog_files, function(f) {
  yr_2d <- substr(basename(f), 1, 2)
  yr <- ifelse(as.integer(yr_2d) >= 90, 1900L + as.integer(yr_2d), 2000L + as.integer(yr_2d))
  lines <- readLines(f, warn = FALSE)
  lines <- lines[nchar(lines) >= 6]
  tibble(
    year = as.integer(yr),
    prog_id = substr(lines, 1, 6),
    prog_name = trimws(substr(lines, 7, nchar(lines)))
  ) |>
    distinct(year, prog_id, .keep_all = TRUE)
})

dbWithTransaction(db, {
  dbAppendTable(db, "cffr_program", all_programs)
})
message(sprintf("Loaded %d program records into 'cffr_program'.", nrow(all_programs)))

# ----- 3. Ingest cffr spending outlays ---------------------------------------

message("Ingesting cffr geographic spending outlays (1993-2010)...")
execute_sql_file(db, file.path(path_sql, "tables", "cffr.sql"))

cols_spec <- cols(
  FIPSST = col_character(),
  FIPSCO = col_character(),
  FIPSPLAC = col_character(),
  STATE = col_character(),
  COUNTY = col_character(),
  PLACE = col_character(),
  POP = col_double(),
  CONGDIST = col_character(),
  PROG_ID = col_character(),
  OBJ_TYPE = col_character(),
  AGENCY = col_character(),
  AMOUNT = col_double()
)

years <- 1993:2010
total_spending_records <- 0L

for (yr in years) {
  t0 <- Sys.time()
  yr_code <- sprintf("%02d", yr %% 100)
  dir_path <- file.path(raw_cffr_dir, paste0(yr_code, "CFFIND"))
  csv_files <- list.files(dir_path, pattern = "[.]csv$", full.names = TRUE) |>
    sort()
  
  if (length(csv_files) == 0) {
    warning(sprintf("No CSV files found in %s", dir_path))
    next
  }
  
  year_df <- map_dfr(csv_files, function(f) {
    x <- suppressWarnings(read_csv(f, col_types = cols_spec, progress = FALSE))
    if (!"POP" %in% names(x)) {
      x$POP <- NA_real_
    }
    x
  }) |>
    transmute(
      year = as.integer(yr),
      fips_state = as.integer(FIPSST),
      fips_county = as.integer(FIPSCO),
      fips_place = as.integer(FIPSPLAC),
      state_code = as.character(STATE),
      county_name = as.character(COUNTY),
      place_name = as.character(PLACE),
      population = as.integer(POP),
      congdist = as.character(CONGDIST),
      prog_id = as.character(PROG_ID),
      obj_type = as.character(OBJ_TYPE),
      agency_code = as.character(AGENCY),
      amount = as.numeric(AMOUNT)
    )
  
  dbWithTransaction(db, {
    dbAppendTable(db, "cffr", year_df)
  })
  
  t1 <- Sys.time()
  total_spending_records <- total_spending_records + nrow(year_df)
  message(sprintf("  Year %d: inserted %d records (%.2f s).", yr, nrow(year_df), as.numeric(t1 - t0, units = "secs")))
}

# ----- 4. Verification -------------------------------------------------------

cnt_agency  <- dbGetQuery(db, "SELECT count(*) FROM cffr_agency")[[1]]
cnt_program <- dbGetQuery(db, "SELECT count(*) FROM cffr_program")[[1]]
cnt_cffr    <- dbGetQuery(db, "SELECT count(*) FROM cffr")[[1]]

dbDisconnect(db)

message("\n============================================================")
message("CFFR ETL Ingestion Complete:")
message(sprintf("  cffr_agency:  %d rows", cnt_agency))
message(sprintf("  cffr_program: %d rows", cnt_program))
message(sprintf("  cffr:         %d rows", cnt_cffr))
message("============================================================\n")
