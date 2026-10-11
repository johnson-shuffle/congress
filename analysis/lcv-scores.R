# ==============================================================================
# Synthesis: LCV Environmental Voting Scores
# Synthesizes member annual and biennial LCV scores by joining `lcv_votes`
# with Voteview roll calls (`voteview_info`) and individual votes (`voteview_cast`).
# Populates `lcv_scores` and `lcv_congress_scores` tables in congress.sqlite.
# Covers House and Senate (103rd–114th Congress, 1993–2015).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(DBI)
  library(RSQLite)
})

source("config.R")

db <- get_db()

# ----- 1. extract and match lcv roll calls with voteview metadata -------------

message("Extracting LCV roll calls and matching with Voteview rollcall metadata...")

lcv_votes <- dbGetQuery(
  db,
  "SELECT congress, year, chamber, rollnumber AS clerk_rollnumber, lcv_vote, double_count
   FROM lcv_votes
   WHERE congress BETWEEN 103 AND 114"
)

# Extract Voteview rollcall identifiers with corresponding year and clerk numbers
vv_info <- dbGetQuery(
  db,
  "SELECT congress, chamber, rollnumber, clerk_rollnumber,
          CAST(strftime('%Y', date) AS INTEGER) AS year
   FROM voteview_info
   WHERE congress BETWEEN 103 AND 114"
)

# Join LCV scorecard votes to Voteview rollcall sequence number
matched_rolls <- inner_join(
  lcv_votes,
  vv_info,
  by = c("congress", "chamber", "clerk_rollnumber", "year")
) |>
  distinct(congress, chamber, rollnumber, .keep_all = TRUE)

message(sprintf("Matched %d LCV roll calls to Voteview rollcall records.", nrow(matched_rolls)))

# ----- 2. extract cast votes and compute scores -------------------------------

message("Querying individual member votes from voteview_cast...")

# Write matched rolls to a temp table to efficiently query voteview_cast
dbExecute(db, "DROP TABLE IF EXISTS temp_lcv_matched_rolls;")
dbWriteTable(db, "temp_lcv_matched_rolls", matched_rolls, temporary = TRUE)

query_votes <- "
SELECT c.congress, c.chamber, c.rollnumber, c.icpsr_id, c.cast_code,
       m.year, m.lcv_vote, m.double_count
FROM voteview_cast c
INNER JOIN temp_lcv_matched_rolls m
  ON c.congress = m.congress
 AND c.chamber = m.chamber
 AND c.rollnumber = m.rollnumber;
"

cast_data <- dbGetQuery(db, query_votes)
dbExecute(db, "DROP TABLE IF EXISTS temp_lcv_matched_rolls;")

message(sprintf("Loaded %d cast vote rows for scoring.", nrow(cast_data)))

# ----- 3. vectorized score calculation ---------------------------------------

yea_codes <- c(1L, 2L, 3L)
nay_codes <- c(4L, 5L, 6L)
eligible_codes <- 1L:9L

scored_votes <- cast_data |>
  mutate(
    weight = if_else(double_count == 1L, 2L, 1L),
    is_eligible = if_else(cast_code %in% eligible_codes, 1L, 0L),
    is_pro = case_when(
      cast_code %in% yea_codes & lcv_vote == 1L ~ 1L,
      cast_code %in% nay_codes & lcv_vote == 0L ~ 1L,
      TRUE ~ 0L
    ),
    eligible_val = weight * is_eligible,
    pro_val = weight * is_pro
  )

# ----- 4. annual score aggregation -------------------------------------------

annual_scores <- scored_votes |>
  group_by(icpsr_id, congress, year, chamber) |>
  summarise(
    eligible_votes = sum(eligible_val),
    pro_votes = sum(pro_val),
    .groups = "drop"
  ) |>
  mutate(
    # Adjust for Gabrielle Giffords (icpsr_id 20701) in 2011/2012 following shooting
    eligible_votes = if_else(icpsr_id == 20701L & year %in% c(2011L, 2012L), 0L, eligible_votes),
    pro_votes = if_else(icpsr_id == 20701L & year %in% c(2011L, 2012L), 0L, pro_votes),
    lcv_score = if_else(eligible_votes > 0L, pro_votes / eligible_votes, NA_real_)
  ) |>
  select(icpsr_id, congress, year, chamber, eligible_votes, lcv_score) |>
  arrange(congress, year, chamber, icpsr_id)

# ----- 5. congress-level biennial aggregation ---------------------------------

congress_scores <- scored_votes |>
  group_by(icpsr_id, congress, chamber) |>
  summarise(
    eligible_votes = sum(eligible_val),
    pro_votes = sum(pro_val),
    .groups = "drop"
  ) |>
  mutate(
    eligible_votes = if_else(icpsr_id == 20701L & congress == 112L, 0L, eligible_votes),
    pro_votes = if_else(icpsr_id == 20701L & congress == 112L, 0L, pro_votes),
    lcv_score = if_else(eligible_votes > 0L, pro_votes / eligible_votes, NA_real_)
  ) |>
  select(icpsr_id, congress, chamber, eligible_votes, lcv_score) |>
  arrange(congress, chamber, icpsr_id)

# ----- 6. write to database ---------------------------------------------------

execute_sql_file(db, file.path(path_sql, "lcv-scores.sql"))

dbWriteTable(db, "lcv_scores", annual_scores, append = TRUE)
dbWriteTable(db, "lcv_congress_scores", congress_scores, append = TRUE)

cnt_annual   <- dbGetQuery(db, "SELECT count(*) FROM lcv_scores")[[1]]
cnt_congress <- dbGetQuery(db, "SELECT count(*) FROM lcv_congress_scores")[[1]]

dbDisconnect(db)

message(sprintf("Successfully synthesized 'lcv_scores': %d annual rows.", cnt_annual))
message(sprintf("Successfully synthesized 'lcv_congress_scores': %d biennial rows.", cnt_congress))
