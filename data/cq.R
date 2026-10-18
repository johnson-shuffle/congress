# ==============================================================================
# Ingestion: Congressional Quarterly (CQ) Elections
# Reads raw CQ returns from raw/cq/ and populates `cq_elections` in SQLite.
# Covers House, Senate, President, and Governor races (1988–2016).
# ==============================================================================

source("config.R")
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(DBI)


# ----- helper-functions -------------------------------------------------------

clean_num <- function(x) {
  if (is.null(x)) return(NA_integer_)
  x <- gsub("[^0-9]", "", as.character(x))
  as.integer(ifelse(x == "", NA, x))
}

clean_pct <- function(x) {
  if (is.null(x)) return(NA_real_)
  x <- gsub("[^0-9.]", "", as.character(x))
  as.numeric(ifelse(x == "", NA, x))
}

clean_text <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- str_trim(as.character(x))
  ifelse(x %in% c("", "N/A", "NULL", "NA"), NA_character_, x)
}


# ----- load-state-lookup ------------------------------------------------------

db <- get_db()
states_df <- dbGetQuery(db, "SELECT state_code, state_name FROM states")
st_map <- setNames(states_df$state_code, states_df$state_name)


# ----- house ------------------------------------------------------------------

hou_fls <- list.files("raw/cq", pattern = "^hou_.*[.]csv$", full.names = TRUE)

hou_raw <- map_dfr(hou_fls, function(f) {
  read_csv(f, skip = 2, col_types = cols(.default = "c"), show_col_types = FALSE) |>
    filter(Office == "House")
})

# Drop blank lines in LA 2014 where all candidate and vote fields are N/A
hou <- hou_raw |>
  filter(!(State == "Louisiana" & raceYear == "2014" & RepVotes == "N/A" & DemVotes == "N/A"))

hou <- hou |>
  mutate(
    cycle = as.integer(raceYear),
    office = "House",
    state_code = st_map[State],
    district_number = ifelse(Area == "At Large", 0L, as.integer(gsub("[^0-9]", "", Area))),
    election_type = "General"
  )

# Tag runoff elections
hou <- hou |>
  mutate(
    election_type = case_when(
      state_code == "LA" & district_number == 2 & cycle == 2006 & DemVotes == "35,153" ~ "Runoff",
      state_code == "LA" & district_number == 3 & cycle == 2012 & RepVotes == "58,820" ~ "Runoff",
      state_code == "LA" & district_number == 5 & cycle == 2014 & RepVotes == "134,616" ~ "Runoff",
      state_code == "LA" & district_number == 6 & cycle == 2014 & RepVotes == "139,209" ~ "Runoff",
      state_code == "LA" & district_number == 3 & cycle == 2016 & RepVotes == "77,671" ~ "Runoff",
      state_code == "LA" & district_number == 4 & cycle == 2016 & RepVotes == "87,370" ~ "Runoff",
      state_code == "TX" & district_number == 23 & cycle == 2006 & RepVotes == "32,217" ~ "Runoff",
      TRUE ~ election_type
    )
  )

# Merge split Democratic and Republican general election lines for Louisiana 2006 District 2
la_gen_d <- hou |>
  filter(state_code == "LA" & district_number == 2 & cycle == 2006 & election_type == "General" & DemVotes == "28,283")
la_gen_r <- hou |>
  filter(state_code == "LA" & district_number == 2 & cycle == 2006 & election_type == "General" & RepVotes == "12,511")

hou <- hou |>
  filter(!(state_code == "LA" & district_number == 2 & cycle == 2006 & election_type == "General"))

if (nrow(la_gen_d) > 0 && nrow(la_gen_r) > 0) {
  merged_la <- la_gen_d[1, ]
  merged_la$RepCandidate <- la_gen_r$RepCandidate[1]
  merged_la$RepStatus <- la_gen_r$RepStatus[1]
  merged_la$RepVotes <- la_gen_r$RepVotes[1]
  merged_la$RepVotesMajorPercent <- "30.68"
  merged_la$DemVotesMajorPercent <- "69.32"
  hou <- bind_rows(hou, merged_la)
}

# Fix uncontested races and miscoded fields
hou <- hou |>
  mutate(
    DemCandidate = case_when(
      state_code == "FL" & district_number == 19 & cycle == 2004 ~ "Wexler, Robert",
      state_code == "FL" & district_number == 24 & cycle == 2012 ~ "Wilson, Frederica S.",
      state_code == "FL" & district_number == 24 & cycle == 2016 ~ "Wilson, Frederica S.",
      TRUE ~ DemCandidate
    ),
    DemStatus = case_when(
      state_code == "FL" & district_number == 19 & cycle == 2004 ~ "Incumbent",
      state_code == "FL" & district_number == 24 & cycle == 2012 ~ "Incumbent",
      state_code == "FL" & district_number == 24 & cycle == 2016 ~ "Incumbent",
      TRUE ~ DemStatus
    ),
    DemVotesMajorPercent = case_when(
      state_code == "FL" & district_number == 19 & cycle == 2004 ~ "100.00",
      state_code == "FL" & district_number == 24 & cycle == 2012 ~ "100.00",
      state_code == "FL" & district_number == 24 & cycle == 2016 ~ "100.00",
      state_code == "FL" & district_number == 23 & cycle == 2004 ~ "100.00",
      state_code == "AR" & district_number == 4 & cycle == 2004 ~ "100.00",
      TRUE ~ DemVotesMajorPercent
    ),
    RepCandidate = case_when(
      state_code == "FL" & district_number == 15 & cycle == 2012 ~ "Ross, Dennis A.",
      state_code == "OK" & district_number == 1 & cycle == 2016 ~ "Bridenstine, Jim",
      TRUE ~ RepCandidate
    ),
    RepStatus = case_when(
      state_code == "FL" & district_number == 15 & cycle == 2012 ~ "Incumbent",
      state_code == "OK" & district_number == 1 & cycle == 2016 ~ "Incumbent",
      state_code == "CA" & district_number == 12 & cycle == 2004 ~ "Challenger",
      TRUE ~ RepStatus
    ),
    RepVotesMajorPercent = case_when(
      state_code == "FL" & district_number == 15 & cycle == 2012 ~ "100.00",
      state_code == "OK" & district_number == 1 & cycle == 2016 ~ "100.00",
      state_code == "FL" & district_number == 7 & cycle == 2004 ~ "100.00",
      state_code == "FL" & district_number == 24 & cycle == 2004 ~ "100.00",
      state_code == "FL" & district_number == 25 & cycle == 2004 ~ "100.00",
      TRUE ~ RepVotesMajorPercent
    )
  )

hou_clean <- tibble(
  cycle = hou$cycle,
  office = hou$office,
  state_code = hou$state_code,
  district_number = hou$district_number,
  election_type = hou$election_type,
  rep_candidate = clean_text(hou$RepCandidate),
  rep_status = clean_text(hou$RepStatus),
  rep_votes = clean_num(hou$RepVotes),
  rep_pct = clean_pct(hou$RepVotesMajorPercent),
  dem_candidate = clean_text(hou$DemCandidate),
  dem_status = clean_text(hou$DemStatus),
  dem_votes = clean_num(hou$DemVotes),
  dem_pct = clean_pct(hou$DemVotesMajorPercent),
  third_party = clean_text(hou$ThirdParty),
  third_candidate = clean_text(hou$ThirdCandidate),
  third_status = clean_text(hou$ThirdStatus),
  third_votes = clean_num(hou$ThirdVotes),
  third_pct = clean_pct(hou$ThirdVotesTotalPercent),
  other_votes = clean_num(hou$OtherVotes),
  plurality_party = clean_text(hou$PluralityParty),
  plurality_votes = clean_num(hou$PluralityVotes)
)


# ----- senate -----------------------------------------------------------------

sen_fls <- list.files("raw/cq", pattern = "^sen_.*[.]csv$", full.names = TRUE)

sen_raw <- map_dfr(sen_fls, function(f) {
  d <- read_csv(f, skip = 2, col_types = cols(.default = "c"), show_col_types = FALSE) |>
    filter(Office == "Senate")
  d$cycle <- as.integer(str_extract(basename(f), "[0-9]{4}"))
  d
})

sen <- sen_raw |>
  mutate(
    office = "Senate",
    state_code = st_map[Area],
    district_number = 0L,
    election_type = "General"
  ) |>
  mutate(
    election_type = case_when(
      state_code == "GA" & cycle == 1992 & RepVotes == "635,114" ~ "Runoff",
      state_code == "GA" & cycle == 2008 & RepVotes == "1,228,033" ~ "Runoff",
      state_code == "LA" & cycle == 2014 & RepVotes == "712,379" ~ "Runoff",
      state_code == "LA" & cycle == 2016 & RepVotes == "536,191" ~ "Runoff",
      TRUE ~ election_type
    )
  )

sen_clean <- tibble(
  cycle = sen$cycle,
  office = sen$office,
  state_code = sen$state_code,
  district_number = sen$district_number,
  election_type = sen$election_type,
  rep_candidate = clean_text(sen$RepCandidate),
  rep_status = clean_text(sen$RepStatus),
  rep_votes = clean_num(sen$RepVotes),
  rep_pct = clean_pct(sen$RepVotesMajorPercent),
  dem_candidate = clean_text(sen$DemCandidate),
  dem_status = clean_text(sen$DemStatus),
  dem_votes = clean_num(sen$DemVotes),
  dem_pct = clean_pct(sen$DemVotesMajorPercent),
  third_party = clean_text(sen$ThirdParty),
  third_candidate = clean_text(sen$ThirdCandidate),
  third_status = clean_text(sen$ThirdStatus),
  third_votes = clean_num(sen$ThirdVotes),
  third_pct = clean_pct(sen$ThirdVotesTotalPercent),
  other_votes = clean_num(sen$OtherVotes),
  plurality_party = clean_text(sen$PluralityParty),
  plurality_votes = clean_num(sen$PluralityVotes)
)


# ----- president --------------------------------------------------------------

prez_fls <- list.files("raw/cq", pattern = "^prez_.*[.]csv$", full.names = TRUE)

prez_raw <- map_dfr(prez_fls, function(f) {
  d <- read_csv(f, skip = 2, col_types = cols(.default = "c"), show_col_types = FALSE) |>
    filter(Office == "President")
  d$cycle <- as.integer(str_extract(basename(f), "[0-9]{4}"))
  d
})

prez <- prez_raw |>
  mutate(
    office = "President",
    state_code = st_map[Area],
    district_number = 0L,
    election_type = "General"
  )

prez_clean <- tibble(
  cycle = prez$cycle,
  office = prez$office,
  state_code = prez$state_code,
  district_number = prez$district_number,
  election_type = prez$election_type,
  rep_candidate = clean_text(prez$RepCandidate),
  rep_status = clean_text(prez$RepStatus),
  rep_votes = clean_num(prez$RepVotes),
  rep_pct = clean_pct(prez$RepVotesMajorPercent),
  dem_candidate = clean_text(prez$DemCandidate),
  dem_status = clean_text(prez$DemStatus),
  dem_votes = clean_num(prez$DemVotes),
  dem_pct = clean_pct(prez$DemVotesMajorPercent),
  third_party = clean_text(prez$ThirdParty),
  third_candidate = clean_text(prez$ThirdCandidate),
  third_status = clean_text(prez$ThirdStatus),
  third_votes = clean_num(prez$ThirdVotes),
  third_pct = clean_pct(prez$ThirdVotesTotalPercent),
  other_votes = clean_num(prez$OtherVotes),
  plurality_party = clean_text(prez$PluralityParty),
  plurality_votes = clean_num(prez$PluralityVotes)
)


# ----- governor ---------------------------------------------------------------

gov_fls <- list.files("raw/cq", pattern = "^gov_.*[.]csv$", full.names = TRUE)

gov_raw <- map_dfr(gov_fls, function(f) {
  d <- read_csv(f, skip = 2, col_types = cols(.default = "c"), show_col_types = FALSE) |>
    filter(Office == "Governor")
  d$cycle <- as.integer(str_extract(basename(f), "[0-9]{4}"))
  d
})

gov <- gov_raw |>
  mutate(
    office = "Governor",
    state_code = st_map[Area],
    district_number = 0L,
    election_type = ifelse(
      grepl("Runoff", RaceTypeName) | (st_map[Area] == "LA" & cycle == 2015 & RepVotes == "505,940"),
      "Runoff",
      "General"
    )
  )

gov_clean <- tibble(
  cycle = gov$cycle,
  office = gov$office,
  state_code = gov$state_code,
  district_number = gov$district_number,
  election_type = gov$election_type,
  rep_candidate = clean_text(gov$RepCandidate),
  rep_status = clean_text(gov$RepStatus),
  rep_votes = clean_num(gov$RepVotes),
  rep_pct = clean_pct(gov$RepVotesMajorPercent),
  dem_candidate = clean_text(gov$DemCandidate),
  dem_status = clean_text(gov$DemStatus),
  dem_votes = clean_num(gov$DemVotes),
  dem_pct = clean_pct(gov$DemVotesMajorPercent),
  third_party = clean_text(gov$ThirdParty),
  third_candidate = clean_text(gov$ThirdCandidate),
  third_status = clean_text(gov$ThirdStatus),
  third_votes = clean_num(gov$ThirdVotes),
  third_pct = clean_pct(gov$ThirdVotesTotalPercent),
  other_votes = clean_num(gov$OtherVotes),
  plurality_party = clean_text(gov$PluralityParty),
  plurality_votes = clean_num(gov$PluralityVotes)
)


# ----- combine-and-insert -----------------------------------------------------

cq_all <- bind_rows(hou_clean, sen_clean, prez_clean, gov_clean) |>
  filter(!is.na(state_code))

message("Total CQ election records to insert: ", nrow(cq_all))

execute_sql_file(db, file.path(path_sql, "tables", "cq-elections.sql"))

dbWriteTable(db, "cq_elections", cq_all, append = TRUE)

message("Successfully populated cq_elections: ", dbGetQuery(db, "SELECT count(*) FROM cq_elections")[[1]], " rows.")
