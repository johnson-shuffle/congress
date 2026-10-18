# ==============================================================================
# Synthesis: FEC 2014 General Election Margins & Unified Election Margins View
# Queries SQLite tables (`fec_elections`, `fec_special_elections`, `cq_margins`,
# `crosswalk_members`) to:
#   1. Populate `fec_margins_2014` (114th Congress general election margins)
#   2. Create view `election_margins_vw` (103rd–114th Congress general + special)
# ==============================================================================

source("config.R")
library(dplyr)
library(stringr)
library(DBI)

db <- get_db()

# ----- 1. Initialize DDL schemas ----------------------------------------------

execute_sql_file(db, file.path(path_sql, "tables", "fec-margins-2014.sql"))
execute_sql_file(db, file.path(path_sql, "views", "election-margins-vw.sql"))


# ----- 2. Load 114th Congress Crosswalk & 2014 FEC Results from SQLite --------

cw114 <- dbGetQuery(
  db,
  "SELECT congress, chamber, govtrack_id, icpsr_id, state_code, district_number, full_name, last_name
   FROM crosswalk_members
   WHERE congress = 114"
)

us_terr <- c("AS", "GU", "MP", "PR", "VI", "DC")

fec14 <- dbGetQuery(
  db,
  "SELECT chamber, state_code, district_number, is_unexpired, fec_id,
          candidate_first_name AS firstname, candidate_last_name AS lastname,
          general_pct AS ge, ge_runoff_pct AS ro
   FROM fec_elections
   WHERE cycle = 2014 AND is_unexpired = 0"
)


# ----- 3. House 2014 Margins --------------------------------------------------

hou14_clean <- fec14 |>
  filter(
    chamber == "House",
    !is.na(firstname),
    firstname != "",
    !state_code %in% us_terr
  ) |>
  mutate(
    district_number = ifelse(district_number == 0L, 1L, as.integer(district_number)),
    vote_pct = coalesce(ro, ge)
  ) |>
  group_by(state_code, district_number) |>
  arrange(desc(vote_pct)) |>
  mutate(rank = row_number()) |>
  filter(rank <= 2) |>
  ungroup()

hou14_win <- hou14_clean |> filter(rank == 1)
hou14_run <- hou14_clean |> filter(rank == 2)

hou14_margins <- hou14_win |>
  left_join(
    hou14_run |> select(state_code, district_number, runner_up_pct = vote_pct),
    by = c("state_code", "district_number")
  ) |>
  mutate(
    winner_pct = vote_pct,
    runner_up_pct = coalesce(runner_up_pct, 1.0 - winner_pct),
    margin = winner_pct - runner_up_pct,
    margin_pct = margin * 100.0,
    chamber = "House",
    congress = 114L
  )

cw114_hou <- cw114 |> filter(chamber == "House")

hou14_final <- hou14_margins |>
  left_join(
    cw114_hou |> select(govtrack_id, icpsr_id, state_code, district_number, last_name),
    by = c("state_code", "district_number")
  ) |>
  mutate(winner_name = paste(firstname, lastname)) |>
  select(
    congress, chamber, state_code, district_number, govtrack_id, icpsr_id,
    fec_id, winner_name, winner_pct, runner_up_pct, margin_pct, margin
  ) |>
  filter(!is.na(govtrack_id)) |>
  distinct(congress, chamber, state_code, district_number, govtrack_id, .keep_all = TRUE)


# ----- 4. Senate 2014 Margins -------------------------------------------------

sen14_clean <- fec14 |>
  filter(
    chamber == "Senate",
    !is.na(firstname),
    firstname != "",
    !state_code %in% us_terr,
    !is.na(ge) | !is.na(ro)
  ) |>
  mutate(vote_pct = coalesce(ro, ge)) |>
  group_by(state_code) |>
  arrange(desc(vote_pct)) |>
  mutate(rank = row_number()) |>
  filter(rank <= 2) |>
  ungroup()

sen14_win <- sen14_clean |> filter(rank == 1)
sen14_run <- sen14_clean |> filter(rank == 2)

sen14_margins <- sen14_win |>
  left_join(
    sen14_run |> select(state_code, runner_up_pct = vote_pct),
    by = "state_code"
  ) |>
  mutate(
    winner_pct = vote_pct,
    runner_up_pct = coalesce(runner_up_pct, 1.0 - winner_pct),
    margin = winner_pct - runner_up_pct,
    margin_pct = margin * 100.0,
    chamber = "Senate",
    congress = 114L,
    district_number = 0L
  )

cw114_sen <- cw114 |> filter(chamber == "Senate")

sen14_final <- sen14_margins |>
  inner_join(
    cw114_sen |> select(govtrack_id, icpsr_id, state_code, last_name),
    by = c("state_code", "lastname" = "last_name")
  ) |>
  mutate(winner_name = paste(firstname, lastname)) |>
  select(
    congress, chamber, state_code, district_number, govtrack_id, icpsr_id,
    fec_id, winner_name, winner_pct, runner_up_pct, margin_pct, margin
  ) |>
  distinct(congress, chamber, state_code, district_number, govtrack_id, .keep_all = TRUE)

margins_2014_all <- bind_rows(hou14_final, sen14_final)
dbWriteTable(db, "fec_margins_2014", margins_2014_all, append = TRUE)
message(sprintf(
  "fec_margins_2014: synthesized %d records (%d House, %d Senate).",
  nrow(margins_2014_all), nrow(hou14_final), nrow(sen14_final)
))

n_view <- dbGetQuery(db, "SELECT count(*) AS n FROM election_margins_vw")$n
message(sprintf("election_margins_vw view verified: %d rows.", n_view))

dbDisconnect(db)
