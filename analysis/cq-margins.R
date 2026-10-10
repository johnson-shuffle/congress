# ==============================================================================
# Analysis: Electoral Victory Margins (103rd–114th Congress)
# Joins cq_elections with crosswalk_members and govtrack_term to compute
# election victory margins and runner-up shares for sitting members.
# Populates `cq_margins` in SQLite.
# ==============================================================================

source("config.R")
library(dplyr)
library(tidyr)
library(DBI)


# ----- connect-and-extract ----------------------------------------------------

db <- get_db()

cq_elec <- dbGetQuery(db, "SELECT * FROM cq_elections")
cw <- dbGetQuery(db, "SELECT * FROM crosswalk_members")
terms <- dbGetQuery(
  db,
  "SELECT govtrack_id, state_code, senate_class, start_date, end_date
   FROM govtrack_term
   WHERE chamber = 'Senate' AND senate_class IS NOT NULL"
)


# ----- house-margins ----------------------------------------------------------

# Pick runoff if available for that district & cycle; otherwise general
hou_el <- cq_elec |>
  filter(office == "House") |>
  group_by(cycle, state_code, district_number) |>
  arrange(desc(election_type == "Runoff")) |>
  slice(1) |>
  ungroup()

# Candidate-level ranking
hou_cands <- bind_rows(
  hou_el |>
    select(cycle, state_code, district_number, candidate = dem_candidate, status = dem_status, votes = dem_votes, pct = dem_pct) |>
    mutate(party = "Dem"),
  hou_el |>
    select(cycle, state_code, district_number, candidate = rep_candidate, status = rep_status, votes = rep_votes, pct = rep_pct) |>
    mutate(party = "Rep"),
  hou_el |>
    select(cycle, state_code, district_number, candidate = third_candidate, status = third_status, votes = third_votes, pct = third_pct) |>
    mutate(party = "Third")
) |>
  filter(!is.na(candidate) | !is.na(pct) | !is.na(votes)) |>
  group_by(cycle, state_code, district_number) |>
  arrange(desc(coalesce(pct, 0)), desc(coalesce(votes, 0))) |>
  mutate(rank = row_number()) |>
  ungroup()

hou_winners <- hou_cands |> filter(rank == 1)
hou_runner_up <- hou_cands |> filter(rank == 2)

hou_res <- hou_winners |>
  select(cycle, state_code, district_number, winner_name = candidate, winner_status = status, winner_pct = pct) |>
  left_join(
    hou_runner_up |> select(cycle, state_code, district_number, runner_up_pct = pct),
    by = c("cycle", "state_code", "district_number")
  ) |>
  mutate(
    runner_up_pct = coalesce(runner_up_pct, 0),
    margin = ifelse(runner_up_pct == 0 & (is.na(winner_pct) | winner_pct == 100), 100, winner_pct - runner_up_pct)
  )

# Match with crosswalk_members (Congresses 103 to 114)
cw_hou <- cw |>
  filter(chamber == "House" & congress >= 103 & congress <= 114) |>
  mutate(cycle = 1992 + (congress - 103) * 2) |>
  # For party switchers with duplicate ICPSR IDs in same Congress, take primary ID (< 90000)
  group_by(congress, chamber, state_code, district_number) |>
  arrange(icpsr_id >= 90000) |>
  slice(1) |>
  ungroup()

hou_margins <- cw_hou |>
  inner_join(hou_res, by = c("cycle", "state_code", "district_number")) |>
  transmute(
    congress,
    cycle,
    chamber,
    state_code,
    district_number,
    senate_class = 0L,
    govtrack_id,
    icpsr_id,
    bioguide_id,
    winner_name,
    winner_status,
    winner_pct,
    runner_up_pct,
    margin
  )


# ----- senate-margins ---------------------------------------------------------

# Assign Senate class based on 6-year staggered cycle schedule
sen_el <- cq_elec |>
  filter(office == "Senate") |>
  mutate(
    senate_class = case_when(
      cycle %in% seq(1988, 2016, by = 6) ~ 1L,
      cycle %in% seq(1990, 2016, by = 6) ~ 2L,
      cycle %in% seq(1992, 2016, by = 6) ~ 3L
    )
  ) |>
  group_by(cycle, state_code, senate_class) |>
  arrange(desc(election_type == "Runoff")) |>
  slice(1) |>
  ungroup()

sen_cands <- bind_rows(
  sen_el |>
    select(cycle, state_code, senate_class, candidate = dem_candidate, status = dem_status, votes = dem_votes, pct = dem_pct) |>
    mutate(party = "Dem"),
  sen_el |>
    select(cycle, state_code, senate_class, candidate = rep_candidate, status = rep_status, votes = rep_votes, pct = rep_pct) |>
    mutate(party = "Rep"),
  sen_el |>
    select(cycle, state_code, senate_class, candidate = third_candidate, status = third_status, votes = third_votes, pct = third_pct) |>
    mutate(party = "Third")
) |>
  filter(!is.na(candidate) | !is.na(pct) | !is.na(votes)) |>
  group_by(cycle, state_code, senate_class) |>
  arrange(desc(coalesce(pct, 0)), desc(coalesce(votes, 0))) |>
  mutate(rank = row_number()) |>
  ungroup()

sen_winners <- sen_cands |> filter(rank == 1)
sen_runner_up <- sen_cands |> filter(rank == 2)

sen_res <- sen_winners |>
  select(cycle, state_code, senate_class, winner_name = candidate, winner_status = status, winner_pct = pct) |>
  left_join(
    sen_runner_up |> select(cycle, state_code, senate_class, runner_up_pct = pct),
    by = c("cycle", "state_code", "senate_class")
  ) |>
  mutate(
    runner_up_pct = coalesce(runner_up_pct, 0),
    margin = ifelse(runner_up_pct == 0 & (is.na(winner_pct) | winner_pct == 100), 100, winner_pct - runner_up_pct)
  )

# Match Senate members via exact Congress term date overlap
cw_sen <- cw |>
  filter(chamber == "Senate" & congress >= 103 & congress <= 114) |>
  mutate(
    start_year = 1789 + (congress - 1) * 2,
    end_year = start_year + 2,
    c_start = sprintf("%d-01-03", start_year),
    c_end = sprintf("%d-01-03", end_year)
  ) |>
  inner_join(terms, by = c("govtrack_id", "state_code"), relationship = "many-to-many") |>
  filter(start_date < c_end & end_date > c_start) |>
  group_by(congress, chamber, state_code, senate_class) |>
  arrange(icpsr_id >= 90000) |>
  slice(1) |>
  ungroup() |>
  mutate(
    cycle = case_when(
      congress == 103 & senate_class == 1 ~ 1988L,
      congress == 103 & senate_class == 2 ~ 1990L,
      congress == 103 & senate_class == 3 ~ 1992L,
      TRUE ~ as.integer(1992 + (congress - 103) * 2)
    ),
    is_election_class = case_when(
      congress == 103 ~ TRUE,
      cycle %in% seq(1988, 2016, by = 6) & senate_class == 1 ~ TRUE,
      cycle %in% seq(1990, 2016, by = 6) & senate_class == 2 ~ TRUE,
      cycle %in% seq(1992, 2016, by = 6) & senate_class == 3 ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  filter(is_election_class)

sen_margins <- cw_sen |>
  inner_join(sen_res, by = c("cycle", "state_code", "senate_class")) |>
  transmute(
    congress,
    cycle,
    chamber,
    state_code,
    district_number = 0L,
    senate_class,
    govtrack_id,
    icpsr_id,
    bioguide_id,
    winner_name,
    winner_status,
    winner_pct,
    runner_up_pct,
    margin
  )


# ----- combine-and-insert -----------------------------------------------------

margins_all <- bind_rows(hou_margins, sen_margins)

message("Total margin records to insert: ", nrow(margins_all))

init_table(db, "cq_margins")

dbWriteTable(db, "cq_margins", margins_all, append = TRUE)

message("Successfully populated cq_margins: ", dbGetQuery(db, "SELECT count(*) FROM cq_margins")[[1]], " rows.")
