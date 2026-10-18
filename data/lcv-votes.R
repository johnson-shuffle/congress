# ==============================================================================
# Ingestion: League of Conservation Voters (LCV) Roll Call Vote Positions
# Populates `lcv_votes` table in congress.sqlite
# Covers selected environmental roll calls (1971–2015, 92nd–114th Congress)
# Source: raw/lcv/lcv-votes.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(DBI)
  library(RSQLite)
})

source("config.R")

votes_file <- file.path(path_raw, "lcv", "lcv-votes.csv")

if (!file.exists(votes_file)) {
  stop(sprintf("Raw file not found: %s", votes_file))
}

# ----- 1. read and clean raw file ---------------------------------------------

raw_votes <- read_csv(
  votes_file,
  col_types = cols(
    congress = col_integer(),
    year = col_integer(),
    chamber = col_character(),
    rollnumber = col_integer(),
    rollcall = col_character(),
    lcv_type = col_character(),
    lcv_dscr = col_character(),
    lcv_vote = col_integer(),
    lcv_note = col_character(),
    not_in_lcv = col_integer(),
    double_count = col_integer()
  )
)

lcv_votes <- raw_votes |>
  distinct(year, chamber, rollnumber, .keep_all = TRUE) |>
  arrange(year, chamber, rollnumber)

# ----- 2. write to database ---------------------------------------------------

db <- get_db()

execute_sql_file(db, file.path(path_sql, "tables", "lcv-votes.sql"))
dbWriteTable(db, "lcv_votes", lcv_votes, append = TRUE)

cnt_votes <- dbGetQuery(db, "SELECT count(*) FROM lcv_votes")[[1]]

dbDisconnect(db)

message(sprintf("Successfully populated 'lcv_votes': %d rows.", cnt_votes))
