# ==============================================================================
# ETL: Federal Election Commission (FEC) Results & Margins (1992–2014)
# Ingests:
#   1. Special election results (103rd–114th Congress) from curated FEC/Voteview sources
#   2. State presidential election margins (1992–2012)
#   3. Official FEC general election results (2004–2014) from results*.xls
#   4. 2014 General election victory margins (114th Congress)
# Creates view `election_margins` synthesizing crosswalk_members with margins.
# ==============================================================================

source("config.R")
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(DBI)

db <- get_db()

# ----- 1. Execute DDL schemas -------------------------------------------------

execute_sql_file <- function(db, path) {
  sql <- readLines(path, warn = FALSE) |> paste(collapse = "\n")
  statements <- strsplit(sql, ";")[[1]]
  for (stmt in statements) {
    clean_stmt <- str_trim(stmt)
    if (nchar(clean_stmt) > 0) {
      dbExecute(db, clean_stmt)
    }
  }
}

execute_sql_file(db, "sql/fec-special-elections.sql")
execute_sql_file(db, "sql/fec-margins-2014.sql")
execute_sql_file(db, "sql/fec-presidential-margins.sql")
execute_sql_file(db, "sql/fec-elections.sql")
execute_sql_file(db, "sql/election-margins.sql")

message("DDL schemas initialized.")

cw_all <- dbGetQuery(
  db,
  "SELECT congress, chamber, govtrack_id, icpsr_id, state_code, district_number, full_name, last_name
   FROM crosswalk_members"
)


# ----- 2. Special Elections (House & Senate) ----------------------------------

raw_spec_hou <- "raw/fec/special_elections_house.csv"
raw_spec_sen <- "raw/fec/special_elections_senate.csv"

hou_spec <- read_csv(raw_spec_hou, col_types = cols(.default = "c"), show_col_types = FALSE) |>
  mutate(
    congress = as.integer(congress),
    chamber = "House",
    govtrack_id = as.character(govtrackid),
    icpsr_id = as.integer(icpsrid),
    occupancy = as.integer(occupancy),
    means = as.integer(means),
    first_place_pct = as.numeric(first.place),
    second_place_pct = as.numeric(second.place),
    margin_pct = first_place_pct - second_place_pct,
    margin = margin_pct / 100.0
  ) |>
  left_join(
    cw_all |>
      filter(chamber == "House") |>
      select(congress, govtrack_id, cw_state = state_code, cw_dist = district_number, cw_name = full_name) |>
      distinct(congress, govtrack_id, .keep_all = TRUE),
    by = c("congress", "govtrack_id")
  ) |>
  mutate(
    state_code = coalesce(str_trim(statecode), cw_state),
    district_number = as.integer(coalesce(district, as.character(cw_dist))),
    candidate_name = coalesce(str_trim(name), cw_name)
  ) |>
  select(
    congress, chamber, state_code, district_number, govtrack_id, icpsr_id,
    candidate_name, occupancy, means, first_place_pct, second_place_pct,
    margin_pct, margin
  ) |>
  distinct(congress, chamber, state_code, district_number, govtrack_id, .keep_all = TRUE)

sen_spec <- read_csv(raw_spec_sen, col_types = cols(.default = "c"), show_col_types = FALSE) |>
  mutate(
    congress = as.integer(congress),
    chamber = "Senate",
    state_code = str_trim(statecode),
    district_number = 0L,
    govtrack_id = as.character(govtrackid),
    icpsr_id = as.integer(icpsrid),
    occupancy = as.integer(occupancy),
    means = as.integer(means),
    first_place_pct = as.numeric(first.place),
    second_place_pct = as.numeric(second.place),
    margin_pct = first_place_pct - second_place_pct,
    margin = margin_pct / 100.0
  ) |>
  left_join(
    cw_all |>
      filter(chamber == "Senate") |>
      select(congress, govtrack_id, cw_name = full_name) |>
      distinct(congress, govtrack_id, .keep_all = TRUE),
    by = c("congress", "govtrack_id")
  ) |>
  mutate(
    candidate_name = coalesce(str_trim(name), str_trim(lastname), cw_name)
  ) |>
  select(
    congress, chamber, state_code, district_number, govtrack_id, icpsr_id,
    candidate_name, occupancy, means, first_place_pct, second_place_pct,
    margin_pct, margin
  ) |>
  distinct(congress, chamber, state_code, district_number, govtrack_id, .keep_all = TRUE)

spec_all <- bind_rows(hou_spec, sen_spec) |>
  distinct(congress, chamber, state_code, district_number, govtrack_id, .keep_all = TRUE)

dbWriteTable(db, "fec_special_elections", spec_all, append = TRUE)
message(sprintf("fec_special_elections: ingested %d records.", nrow(spec_all)))


# ----- 3. Presidential Margins (1992–2012) -----------------------------------

raw_pres <- "raw/fec/margins_presidents.csv"

pres_df <- read_csv(raw_pres, col_types = cols(.default = "c"), show_col_types = FALSE) |>
  mutate(
    year = as.integer(year),
    state_code = str_trim(statecode),
    dem_pct = as.numeric(d.percent),
    rep_pct = as.numeric(r.percent),
    margin_pct = dem_pct - rep_pct,
    margin = margin_pct / 100.0
  ) |>
  select(year, state_code, dem_pct, rep_pct, margin_pct, margin) |>
  distinct(year, state_code, .keep_all = TRUE)

dbWriteTable(db, "fec_presidential_margins", pres_df, append = TRUE)
message(sprintf("fec_presidential_margins: ingested %d records.", nrow(pres_df)))


# ----- 4. 2014 General Election Margins (114th Congress) ----------------------

f14 <- "raw/fec/results14.xls"
us_terr <- c("AS", "GU", "MP", "PR", "VI", "DC")
cw114 <- cw_all |> filter(congress == 114)

# House
hou14_raw <- read_excel(f14, sheet = "2014 US House Results by State", col_types = "text")
hou14_clean <- hou14_raw |>
  select(
    state_code = `STATE ABBREVIATION`,
    district = D,
    fec_id = `FEC ID#`,
    firstname = `CANDIDATE NAME (First)`,
    lastname = `CANDIDATE NAME (Last)`,
    ge = `GENERAL %`,
    ro = `GE RUNOFF ELECTION % (LA)`
  ) |>
  filter(!grepl("UNEXPIRED", district, ignore.case = TRUE)) |>
  mutate(
    district = gsub(" - FULL TERM", "", district),
    district_number = as.integer(district),
    district_number = ifelse(district_number == 0L, 1L, district_number),
    fec_id = str_trim(fec_id),
    firstname = str_trim(firstname),
    lastname = str_trim(lastname),
    ge = as.numeric(gsub("%", "", ge)),
    ge = ifelse(ge > 1, ge / 100.0, ge),
    ro = as.numeric(gsub("%", "", ro)),
    ro = ifelse(ro > 1, ro / 100.0, ro)
  ) |>
  filter(!is.na(firstname), firstname != "", !is.na(district_number), !state_code %in% us_terr) |>
  mutate(vote_pct = coalesce(ro, ge)) |>
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

# Join House winners with crosswalk_members
cw114_hou <- cw114 |> filter(chamber == "House")
hou14_final <- hou14_margins |>
  left_join(
    cw114_hou |> select(govtrack_id, icpsr_id, state_code, district_number, last_name),
    by = c("state_code", "district_number")
  ) |>
  mutate(
    winner_name = paste(firstname, lastname)
  ) |>
  select(
    congress, chamber, state_code, district_number, govtrack_id, icpsr_id,
    fec_id, winner_name, winner_pct = winner_pct, runner_up_pct,
    margin_pct, margin
  ) |>
  filter(!is.na(govtrack_id)) |>
  distinct(congress, chamber, state_code, district_number, govtrack_id, .keep_all = TRUE)

# Senate
sen14_raw <- read_excel(f14, sheet = "2014 US Senate Results by State", col_types = "text")
sen14_clean <- sen14_raw |>
  select(
    state_code = `STATE ABBREVIATION`,
    district = D,
    fec_id = `FEC ID#`,
    firstname = `CANDIDATE NAME (First)`,
    lastname = `CANDIDATE NAME (Last)`,
    ge = `GENERAL %`,
    ro = `GE RUNOFF ELECTION % (LA)`
  ) |>
  filter(!grepl("UNEXPIRED", district, ignore.case = TRUE)) |>
  mutate(
    district = str_trim(district),
    fec_id = str_trim(fec_id),
    firstname = str_trim(firstname),
    lastname = str_trim(lastname),
    ge = as.numeric(gsub("%", "", ge)),
    ge = ifelse(ge > 1, ge / 100.0, ge),
    ro = as.numeric(gsub("%", "", ro)),
    ro = ifelse(ro > 1, ro / 100.0, ro)
  ) |>
  filter(!is.na(firstname), firstname != "", !is.na(district), district != "", !state_code %in% us_terr) |>
  filter(!is.na(ge) | !is.na(ro)) |>
  mutate(vote_pct = coalesce(ro, ge)) |>
  group_by(state_code, district) |>
  arrange(desc(vote_pct)) |>
  mutate(rank = row_number()) |>
  filter(rank <= 2) |>
  ungroup()

sen14_win <- sen14_clean |> filter(rank == 1)
sen14_run <- sen14_clean |> filter(rank == 2)

sen14_margins <- sen14_win |>
  left_join(
    sen14_run |> select(state_code, district, runner_up_pct = vote_pct),
    by = c("state_code", "district")
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

# Join Senate winners with crosswalk_members by state_code and last_name
cw114_sen <- cw114 |> filter(chamber == "Senate")
sen14_final <- sen14_margins |>
  inner_join(
    cw114_sen |> select(govtrack_id, icpsr_id, state_code, last_name),
    by = c("state_code", "lastname" = "last_name")
  ) |>
  mutate(
    winner_name = paste(firstname, lastname)
  ) |>
  select(
    congress, chamber, state_code, district_number, govtrack_id, icpsr_id,
    fec_id, winner_name, winner_pct, runner_up_pct,
    margin_pct, margin
  ) |>
  distinct(congress, chamber, state_code, district_number, govtrack_id, .keep_all = TRUE)

margins_2014_all <- bind_rows(hou14_final, sen14_final)
dbWriteTable(db, "fec_margins_2014", margins_2014_all, append = TRUE)
message(sprintf("fec_margins_2014: ingested %d records (%d House, %d Senate).",
                nrow(margins_2014_all), nrow(hou14_final), nrow(sen14_final)))


# ----- 5. Candidate Results (2004–2014) ----------------------------------------

parse_votes <- function(x) {
  if (is.null(x)) return(NA_integer_)
  clean <- gsub("[,*]", "", as.character(x))
  suppressWarnings(as.integer(clean))
}

parse_pct <- function(x) {
  if (is.null(x)) return(NA_real_)
  clean <- gsub("[%*]", "", as.character(x))
  val <- suppressWarnings(as.numeric(clean))
  ifelse(val > 1.0, val / 100.0, val)
}

years <- c("04", "06", "08", "10", "12", "14")
all_elections <- list()

for (yr in years) {
  cycle_val <- as.integer(paste0("20", yr))
  fpath <- sprintf("raw/fec/results%s.xls", yr)
  sheets <- excel_sheets(fpath)
  
  for (s in sheets) {
    if (grepl("Result|Senate|House", s, ignore.case = TRUE) && 
        !grepl("Publication|Party|Date|Convention|Endnote|Table", s, ignore.case = TRUE)) {
      
      df <- read_excel(fpath, sheet = s, col_types = "text")
      
      st_col <- names(df)[grepl("STATE.*ABBREV", names(df), ignore.case = TRUE)][1]
      if (is.na(st_col)) st_col <- names(df)[grepl("^STATE$", names(df), ignore.case = TRUE)][1]
      d_col <- names(df)[grepl("^D$|^DISTRICT$", names(df), ignore.case = TRUE)][1]
      fec_col <- names(df)[grepl("^FEC ID", names(df), ignore.case = TRUE)][1]
      inc_col <- names(df)[grepl("^INCUMBENT|\\\\(I\\\\)", names(df), ignore.case = TRUE)][1]
      fn_col <- names(df)[grepl("FIRST NAME|NAME.*FIRST", names(df), ignore.case = TRUE)][1]
      ln_col <- names(df)[grepl("LAST NAME|NAME.*LAST", names(df), ignore.case = TRUE)][1]
      full_col <- names(df)[grepl("^CANDIDATE NAME$|^LAST NAME, FIRST$", names(df), ignore.case = TRUE)][1]
      pty_col <- names(df)[grepl("^PARTY$", names(df), ignore.case = TRUE)][1]
      pv_col <- names(df)[grepl("^PRIMARY VOTES$|^PRIMARY$", names(df), ignore.case = TRUE)][1]
      pp_col <- names(df)[grepl("^PRIMARY %", names(df), ignore.case = TRUE)][1]
      rv_col <- names(df)[grepl("^RUNOFF VOTES$|^RUNOFF$", names(df), ignore.case = TRUE)][1]
      rp_col <- names(df)[grepl("^RUNOFF %", names(df), ignore.case = TRUE)][1]
      gv_col <- names(df)[grepl("^GENERAL VOTES$|^GENERAL$", names(df), ignore.case = TRUE)][1]
      gp_col <- names(df)[grepl("^GENERAL %", names(df), ignore.case = TRUE)][1]
      rgv_col <- names(df)[grepl("^GE RUNOFF", names(df), ignore.case = TRUE) & !grepl("%", names(df))][1]
      rgp_col <- names(df)[grepl("^GE RUNOFF.*%", names(df), ignore.case = TRUE)][1]
      win_col <- names(df)[grepl("WINNER", names(df), ignore.case = TRUE)][1]
      
      raw_st <- if (!is.na(st_col)) str_trim(df[[st_col]]) else rep(NA_character_, nrow(df))
      raw_dist <- if (!is.na(d_col)) str_trim(df[[d_col]]) else rep(NA_character_, nrow(df))
      raw_fn <- if (!is.na(fn_col)) str_trim(df[[fn_col]]) else rep(NA_character_, nrow(df))
      raw_ln <- if (!is.na(ln_col)) str_trim(df[[ln_col]]) else rep(NA_character_, nrow(df))
      raw_full <- if (!is.na(full_col)) str_trim(df[[full_col]]) else rep(NA_character_, nrow(df))
      
      parsed <- tibble(
        cycle = cycle_val,
        raw_state = raw_st,
        raw_district = raw_dist,
        fec_id = if (!is.na(fec_col)) str_trim(df[[fec_col]]) else NA_character_,
        incumbent_status = if (!is.na(inc_col)) str_trim(df[[inc_col]]) else NA_character_,
        candidate_first_name = raw_fn,
        candidate_last_name = raw_ln,
        candidate_name = raw_full,
        party = if (!is.na(pty_col)) str_trim(df[[pty_col]]) else NA_character_,
        primary_votes = if (!is.na(pv_col)) parse_votes(df[[pv_col]]) else NA_integer_,
        primary_pct = if (!is.na(pp_col)) parse_pct(df[[pp_col]]) else NA_real_,
        runoff_votes = if (!is.na(rv_col)) parse_votes(df[[rv_col]]) else NA_integer_,
        runoff_pct = if (!is.na(rp_col)) parse_pct(df[[rp_col]]) else NA_real_,
        general_votes = if (!is.na(gv_col)) parse_votes(df[[gv_col]]) else NA_integer_,
        general_pct = if (!is.na(gp_col)) parse_pct(df[[gp_col]]) else NA_real_,
        ge_runoff_votes = if (!is.na(rgv_col)) parse_votes(df[[rgv_col]]) else NA_integer_,
        ge_runoff_pct = if (!is.na(rgp_col)) parse_pct(df[[rgp_col]]) else NA_real_,
        winner_indicator = if (!is.na(win_col)) str_trim(df[[win_col]]) else NA_character_
      ) |>
        filter(
          !is.na(raw_state),
          nchar(raw_state) == 2,
          !is.na(raw_district),
          !raw_district %in% c("H", "Total", "TOTAL", "State Total", "State Total:"),
          (!is.na(candidate_last_name) & candidate_last_name != "") |
          (!is.na(candidate_first_name) & candidate_first_name != "") |
          (!is.na(candidate_name) & candidate_name != "")
        ) |>
        mutate(
          chamber = ifelse(grepl("^S|^Senate", raw_district, ignore.case = TRUE), "Senate", "House"),
          is_unexpired = ifelse(grepl("UNEXPIRED", raw_district, ignore.case = TRUE), 1L, 0L),
          district_num_clean = gsub("[^0-9]", "", raw_district),
          district_number = ifelse(chamber == "Senate", 0L, suppressWarnings(as.integer(district_num_clean))),
          district_number = coalesce(district_number, 0L),
          state_code = raw_state
        ) |>
        select(
          cycle, chamber, state_code, district_number, is_unexpired, fec_id,
          incumbent_status, candidate_first_name, candidate_last_name, candidate_name,
          party, primary_votes, primary_pct, runoff_votes, runoff_pct,
          general_votes, general_pct, ge_runoff_votes, ge_runoff_pct, winner_indicator
        )
      
      all_elections[[paste(yr, s)]] <- parsed
    }
  }
}

elections_df <- bind_rows(all_elections)
dbWriteTable(db, "fec_elections", elections_df, append = TRUE)
message(sprintf("fec_elections: ingested %d candidate election records (cycles 2004–2014).", nrow(elections_df)))


# ----- 6. Verification & Foreign Key Integrity --------------------------------

fk_check <- dbGetQuery(db, "PRAGMA foreign_key_check;")
if (nrow(fk_check) > 0) {
  warning("Foreign key check failed:")
  print(fk_check)
} else {
  message("Foreign key integrity: PASSED (0 violations).")
}

# Validation summary
cat("\n=== FEC Pipeline Ingestion Summary ===\n")
print(dbGetQuery(db, "
  SELECT 'fec_special_elections' AS table_name, count(*) AS rows FROM fec_special_elections
  UNION ALL
  SELECT 'fec_margins_2014', count(*) FROM fec_margins_2014
  UNION ALL
  SELECT 'fec_presidential_margins', count(*) FROM fec_presidential_margins
  UNION ALL
  SELECT 'fec_elections', count(*) FROM fec_elections
  UNION ALL
  SELECT 'election_margins (view)', count(*) FROM election_margins;
"))

dbDisconnect(db)
message("FEC ETL pipeline complete.")
