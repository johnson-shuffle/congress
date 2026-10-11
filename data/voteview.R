# ==============================================================================
# Ingest Voteview Congressional Members, Roll Calls & Cast Votes (103rd–114th)
# Populates voteview_members, voteview_info, and voteview_cast in congress.sqlite
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(DBI)
  library(RSQLite)
})

source("config.R")

raw_vv_dir <- file.path(path_raw, "voteview")
dir.create(raw_vv_dir, showWarnings = FALSE, recursive = TRUE)

congresses <- 103:114

db <- get_db()

# ----- 1. voteview_members ----------------------------------------------------

execute_sql_file(db, file.path(path_sql, "voteview-members.sql"))

existing_members <- dbGetQuery(db, "SELECT count(*) FROM voteview_members")[[1]]

if (existing_members == 0) {
  message("Ingesting voteview_members...")
  fetch_voteview_members <- function(congress, chamber_code) {
    fname <- sprintf("%s%03d_members.csv", chamber_code, congress)
    local_path <- file.path(raw_vv_dir, fname)
    if (!file.exists(local_path)) {
      url <- sprintf("https://voteview.com/static/data/out/members/%s", fname)
      message(sprintf("Downloading %s...", fname))
      download.file(url, local_path, quiet = TRUE)
    }
    read_csv(local_path, col_types = cols(.default = "c"), na = c("", "NA", "N/A")) |>
      mutate(source_chamber = if_else(chamber_code == "H", "House", "Senate"))
  }

  house_list <- lapply(congresses, fetch_voteview_members, chamber_code = "H")
  senate_list <- lapply(congresses, fetch_voteview_members, chamber_code = "S")
  raw_members <- bind_rows(c(house_list, senate_list))

  voteview_members <- raw_members |>
    filter(!is.na(icpsr), as.integer(icpsr) < 99909) |>
    rename(
      icpsr_id = icpsr,
      state_code = state_abbrev,
      bio_name = bioname
    ) |>
    mutate(
      congress = as.integer(congress),
      chamber = source_chamber,
      icpsr_id = as.integer(icpsr_id),
      state_icpsr = as.integer(state_icpsr),
      district_code = coalesce(as.integer(district_code), 0L),
      state_code = if_else(state_code == "USA", "US", state_code),
      party_code = as.integer(party_code),
      occupancy = as.integer(occupancy),
      last_means = as.integer(last_means),
      born = as.integer(born),
      died = as.numeric(died),
      nominate_dim1 = as.numeric(nominate_dim1),
      nominate_dim2 = as.numeric(nominate_dim2),
      nominate_log_likelihood = as.numeric(nominate_log_likelihood),
      nominate_geo_mean_probability = as.numeric(nominate_geo_mean_probability),
      nominate_number_of_votes = as.integer(nominate_number_of_votes),
      nominate_number_of_errors = as.integer(nominate_number_of_errors),
      nokken_poole_dim1 = as.numeric(nokken_poole_dim1),
      nokken_poole_dim2 = as.numeric(nokken_poole_dim2)
    ) |>
    select(
      congress, chamber, icpsr_id, state_icpsr, district_code, state_code,
      party_code, occupancy, last_means, bio_name, bioguide_id, born, died,
      nominate_dim1, nominate_dim2, nominate_log_likelihood,
      nominate_geo_mean_probability, nominate_number_of_votes,
      nominate_number_of_errors, nokken_poole_dim1, nokken_poole_dim2
    ) |>
    distinct(congress, chamber, icpsr_id, district_code, .keep_all = TRUE) |>
    arrange(congress, chamber, state_code, district_code)

  dbWriteTable(db, "voteview_members", voteview_members, append = TRUE)
  message(sprintf("Loaded %d member records into 'voteview_members'.", nrow(voteview_members)))
} else {
  message(sprintf("'voteview_members' already contains %d records.", existing_members))
}

# ----- 2. voteview_info (roll call metadata) ----------------------------------

execute_sql_file(db, file.path(path_sql, "voteview-info.sql"))

fetch_rollcalls <- function(congress, chamber_code) {
  fname <- sprintf("%s%03d_rollcalls.csv", chamber_code, congress)
  local_path <- file.path(raw_vv_dir, fname)
  if (!file.exists(local_path)) {
    url <- sprintf("https://voteview.com/static/data/out/rollcalls/%s", fname)
    message(sprintf("Downloading %s...", fname))
    download.file(url, local_path, quiet = TRUE)
  }
  read_csv(
    local_path,
    col_types = cols(
      congress = col_integer(),
      chamber = col_character(),
      rollnumber = col_integer(),
      date = col_character(),
      session = col_integer(),
      clerk_rollnumber = col_integer(),
      majority_requirement = col_character(),
      yea_count = col_integer(),
      nay_count = col_integer(),
      nominate_mid_1 = col_double(),
      nominate_mid_2 = col_double(),
      nominate_spread_1 = col_double(),
      nominate_spread_2 = col_double(),
      nominate_log_likelihood = col_double(),
      bill_number = col_character(),
      vote_result = col_character(),
      vote_desc = col_character(),
      vote_question = col_character(),
      dtl_desc = col_character()
    ),
    na = c("", "NA", "N/A")
  )
}

message("Ingesting voteview_info across 103rd-114th Congress...")
h_rc_list <- lapply(congresses, fetch_rollcalls, chamber_code = "H")
s_rc_list <- lapply(congresses, fetch_rollcalls, chamber_code = "S")
all_rollcalls <- bind_rows(c(h_rc_list, s_rc_list)) |>
  distinct(congress, chamber, rollnumber, .keep_all = TRUE) |>
  arrange(congress, chamber, rollnumber)

dbWriteTable(db, "voteview_info", all_rollcalls, append = TRUE)
message(sprintf("Loaded %d roll call records into 'voteview_info'.", nrow(all_rollcalls)))

# ----- 3. voteview_cast (individual votes cast) ------------------------------

execute_sql_file(db, file.path(path_sql, "voteview-cast.sql"))

fetch_votes <- function(congress, chamber_code) {
  fname <- sprintf("%s%03d_votes.csv", chamber_code, congress)
  local_path <- file.path(raw_vv_dir, fname)
  if (!file.exists(local_path)) {
    url <- sprintf("https://voteview.com/static/data/out/votes/%s", fname)
    message(sprintf("Downloading %s...", fname))
    download.file(url, local_path, quiet = TRUE)
  }
  read_csv(
    local_path,
    col_types = cols(
      congress = col_integer(),
      chamber = col_character(),
      rollnumber = col_integer(),
      icpsr = col_double(),
      cast_code = col_integer(),
      prob = col_double()
    ),
    na = c("", "NA", "N/A")
  ) |>
    filter(!is.na(icpsr)) |>
    mutate(icpsr_id = as.integer(icpsr)) |>
    select(congress, chamber, rollnumber, icpsr_id, cast_code, prob)
}

message("Ingesting voteview_cast congress by congress...")
total_votes_loaded <- 0L

for (c_num in congresses) {
  h_v <- fetch_votes(c_num, "H")
  s_v <- fetch_votes(c_num, "S")
  cong_votes <- bind_rows(h_v, s_v) |>
    distinct(congress, chamber, rollnumber, icpsr_id, .keep_all = TRUE)

  dbWriteTable(db, "voteview_cast", cong_votes, append = TRUE)
  total_votes_loaded <- total_votes_loaded + nrow(cong_votes)
  message(sprintf("  Congress %d: inserted %d votes.", c_num, nrow(cong_votes)))
}

cnt_members <- dbGetQuery(db, "SELECT count(*) FROM voteview_members")[[1]]
cnt_info    <- dbGetQuery(db, "SELECT count(*) FROM voteview_info")[[1]]
cnt_cast    <- dbGetQuery(db, "SELECT count(*) FROM voteview_cast")[[1]]

dbDisconnect(db)

message("Voteview ETL Complete:")
message(sprintf("  voteview_members: %d rows", cnt_members))
message(sprintf("  voteview_info:    %d rows", cnt_info))
message(sprintf("  voteview_cast:    %d rows", cnt_cast))
