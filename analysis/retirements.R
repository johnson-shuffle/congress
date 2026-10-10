# ==============================================================================
# Synthesis: Congressional Member Retirements & Election Year Status
# Combines member rosters from `crosswalk_members` with curated `raw/retirements.csv`
# and determines election year status (by Senate class) and retirement indicators.
# Populates `retirements` table (and `casualty`/`casualties` views) in congress.sqlite.
# Covers House and Senate (103rd–114th Congress, 1993–2016).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(DBI)
  library(RSQLite)
})

source("config.R")

raw_ret_file <- file.path(path_raw, "retirements.csv")
if (!file.exists(raw_ret_file)) {
  raw_ret_file <- file.path(path_raw, "legislators", "retirements.csv")
}
if (!file.exists(raw_ret_file)) {
  stop("raw/retirements.csv not found.")
}

# ----- 1. read curated retirements --------------------------------------------

raw_retirements <- read_csv(
  raw_ret_file,
  col_types = cols(
    govtrack_id = col_character(),
    congress = col_integer(),
    first_name = col_character(),
    last_name = col_character(),
    party = col_character(),
    state_code = col_character()
  )
)

ret_flags <- raw_retirements |>
  distinct(govtrack_id, congress) |>
  mutate(is_retired = 1L)

# ----- 2. extract member roster and senate classes from database --------------

db <- get_db()

# Full roster across 103rd to 114th Congress
members <- dbGetQuery(
  db,
  "SELECT congress, chamber, icpsr_id, district_number, govtrack_id, state_code
   FROM crosswalk_members
   WHERE congress BETWEEN 103 AND 114"
)

# Extract Senate classes mapped to congress session dates
senate_classes <- dbGetQuery(
  db,
  "WITH cong_dates AS (
       SELECT congress, min(session_start) AS start_date, max(session_end) AS end_date
       FROM congress_sessions
       WHERE chamber = 'Senate'
       GROUP BY congress
   )
   SELECT DISTINCT m.govtrack_id, m.congress, t.senate_class
   FROM crosswalk_members m
   JOIN cong_dates c ON m.congress = c.congress
   JOIN govtrack_term t ON m.govtrack_id = t.govtrack_id AND t.chamber = 'Senate'
     AND t.start_date <= c.end_date AND t.end_date >= c.start_date
   WHERE m.chamber = 'Senate' AND m.congress BETWEEN 103 AND 114"
)

# ----- 3. synthesize election year and retirement indicators -----------------

retirements <- members |>
  left_join(senate_classes, by = c("govtrack_id", "congress")) |>
  mutate(
    senate_class = coalesce(as.integer(senate_class), 0L),
    year = 1994L + (as.integer(congress) - 103L) * 2L,
    election_year = case_when(
      chamber == "House" ~ 1L,
      chamber == "Senate" & (as.integer(congress) - 103L) %% 3L + 1L == senate_class ~ 1L,
      TRUE ~ 0L
    )
  ) |>
  left_join(ret_flags, by = c("govtrack_id", "congress")) |>
  mutate(
    retire = coalesce(is_retired, 0L)
  ) |>
  select(
    congress, chamber, icpsr_id, district_number, govtrack_id,
    state_code, year, senate_class, election_year, retire
  ) |>
  distinct(congress, chamber, icpsr_id, district_number, .keep_all = TRUE) |>
  arrange(congress, chamber, state_code, district_number)

# ----- 4. write to database ---------------------------------------------------

init_table(db, "retirements")

dbWriteTable(db, "retirements", retirements, append = TRUE)

cnt_rows <- dbGetQuery(db, "SELECT count(*) FROM retirements")[[1]]
cnt_ret  <- dbGetQuery(db, "SELECT count(*) FROM retirements WHERE retire = 1")[[1]]
cnt_elec <- dbGetQuery(db, "SELECT count(*) FROM retirements WHERE election_year = 1")[[1]]

dbDisconnect(db)

message(sprintf("Successfully populated 'retirements': %d rows.", cnt_rows))
message(sprintf("  Retirements identified: %d members", cnt_ret))
message(sprintf("  Election year terms:    %d terms", cnt_elec))
