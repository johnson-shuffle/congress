# ==============================================================================
# Ingest Voteview Congressional Members, Roll Calls & Cast Votes (103rd–114th)
# Populates voteview_members, voteview_info, and voteview_cast in congress.sqlite
# Includes attainment corrections (occupancy & last_means) from legacy
# house_attain.R and senate_attain.R.
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

message("Ingesting voteview_members...")
fetch_voteview_members <- function(congress, chamber_code) {
  fname <- sprintf("%s%03d_members.csv", chamber_code, congress)
  local_path <- file.path(raw_vv_dir, fname)
  if (!file.exists(local_path)) {
    url <- sprintf("https://voteview.com/static/data/out/members/%s", fname)
    message(sprintf("Downloading %s...", fname))
    download.file(url, local_path, quiet = TRUE)
  }
  read_csv(local_path, col_types = cols(.default = "c"), na = c("", "NA", "N/A"), show_col_types = FALSE) |>
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
  distinct(congress, chamber, icpsr_id, district_code, .keep_all = TRUE)

# ----- 1b. corrections: occupancy & last_means (attain adjustments) -----------

# Default missing/zero attainment in 103rd, 104th, 106th, 114th Senate & territory delegates
voteview_members <- voteview_members |>
  mutate(
    occupancy = coalesce(occupancy, 0L),
    last_means = case_when(
      chamber == "Senate" & congress %in% c(103L, 104L, 114L) & (is.na(last_means) | last_means == 0L) ~ 1L,
      is.na(last_means) | last_means == 0L ~ 1L,
      TRUE ~ last_means
    )
  )

# House 103rd & 104th: Special elections and party switchers have occupancy = 0 in raw files
dup_103_104 <- voteview_members |>
  filter(chamber == "House", congress %in% c(103L, 104L)) |>
  count(congress, state_code, district_code) |>
  filter(n > 1L) |>
  mutate(is_multi_occ = TRUE)

voteview_members <- voteview_members |>
  left_join(dup_103_104 |> select(congress, state_code, district_code, is_multi_occ),
            by = c("congress", "state_code", "district_code")) |>
  mutate(
    occupancy = case_when(
      chamber == "House" & !is.na(is_multi_occ) & last_means == 1L ~ 1L,
      chamber == "House" & !is.na(is_multi_occ) & last_means == 2L ~ 2L,
      TRUE ~ occupancy
    )
  ) |>
  select(-is_multi_occ)

# Explicit member-level attainment corrections (House & Senate, 103rd–114th)
voteview_members <- voteview_members |>
  mutate(
    occupancy = case_when(
      # House 103: OH-02 Gradison/Portman flipped
      chamber == "House" & congress == 103L & icpsr_id == 14225L ~ 1L,
      chamber == "House" & congress == 103L & icpsr_id == 29386L ~ 2L,
      # House 106: CA-42 (Brown/Baca), LA-01 (Livingston/Vitter), party switchers
      chamber == "House" & congress == 106L & icpsr_id %in% c(10573L, 14469L, 29767L, 29542L, 14879L) ~ 1L,
      chamber == "House" & congress == 106L & icpsr_id %in% c(29942L, 29918L, 99767L, 99542L, 94879L) ~ 2L,
      # House 107: Goode party switch
      chamber == "House" & congress == 107L & icpsr_id == 99767L ~ 1L,
      chamber == "House" & congress == 107L & icpsr_id == 89767L ~ 2L,
      # House 108: HI-02 Case sole occupant; Hall party switch
      chamber == "House" & congress == 108L & icpsr_id == 20322L ~ 0L,
      chamber == "House" & congress == 108L & icpsr_id == 14828L ~ 1L,
      chamber == "House" & congress == 108L & icpsr_id == 94828L ~ 2L,
      # House 109: CA-05 Matsui & HI-02 Case sole occupants
      chamber == "House" & congress == 109L & icpsr_id %in% c(20538L, 20322L) ~ 0L,
      # House 111: IL-05 Quigley sole occupant; Griffith party switch
      chamber == "House" & congress == 111L & icpsr_id == 20954L ~ 0L,
      chamber == "House" & congress == 111L & icpsr_id == 20901L ~ 1L,
      chamber == "House" & congress == 111L & icpsr_id == 90901L ~ 2L,
      # House 112: Garamendi, Chu, Deutch sole occupants
      chamber == "House" & congress == 112L & icpsr_id %in% c(20958L, 20955L, 20959L) ~ 0L,
      # House 113: IL-02 Kelly sole occupant
      chamber == "House" & congress == 113L & icpsr_id == 21371L ~ 0L,
      # House 114: NY-11 Donovan sole occupant; MS-01 Nunnelee/Kelly; IL-18 Schock/LaHood
      chamber == "House" & congress == 114L & icpsr_id == 21560L ~ 0L,
      chamber == "House" & congress == 114L & icpsr_id %in% c(21147L, 20914L) ~ 1L,
      chamber == "House" & congress == 114L & icpsr_id %in% c(21561L, 21562L) ~ 2L,
      # Senate 103: Boren/Inhofe, Mathews, Krueger/Hutchison
      chamber == "Senate" & congress == 103L & icpsr_id %in% c(14700L, 14247L) ~ 1L,
      chamber == "Senate" & congress == 103L & icpsr_id %in% c(15424L, 49306L) ~ 2L,
      chamber == "Senate" & congress == 103L & icpsr_id == 49305L ~ 0L,
      # Senate 104: Dole/Frahm, Packwood/Wyden, Campbell party switch
      chamber == "Senate" & congress == 104L & icpsr_id %in% c(10513L, 12107L, 15407L) ~ 1L,
      chamber == "Senate" & congress == 104L & icpsr_id %in% c(49504L, 14871L, 95407L) ~ 2L,
      # Senate 106: Coverdell/Miller, J. Chafee/L. Chafee
      chamber == "Senate" & congress == 106L & icpsr_id %in% c(49301L, 14500L) ~ 1L,
      chamber == "Senate" & congress == 106L & icpsr_id %in% c(49904L, 49905L) ~ 2L,
      # Senate 107: Jeffords party switch
      chamber == "Senate" & congress == 107L & icpsr_id == 14240L ~ 1L,
      chamber == "Senate" & congress == 107L & icpsr_id == 94240L ~ 2L,
      # Senate 108: L. Murkowski
      chamber == "Senate" & congress == 108L & icpsr_id == 40300L ~ 1L,
      # Senate 111: Burris/Kirk, Specter party switch
      chamber == "Senate" & congress == 111L & icpsr_id %in% c(40903L, 14910L) ~ 1L,
      chamber == "Senate" & congress == 111L & icpsr_id %in% c(20115L, 94910L) ~ 2L,
      # Senate 112: S. Brown & DeMint sole occupants
      chamber == "Senate" & congress == 112L & icpsr_id %in% c(40913L, 29936L) ~ 0L,
      TRUE ~ occupancy
    ),
    last_means = case_when(
      # House explicit means adjustments
      chamber == "House" & congress == 103L & icpsr_id == 14225L ~ 1L,
      chamber == "House" & congress == 103L & icpsr_id == 29386L ~ 2L,
      chamber == "House" & congress == 106L & icpsr_id %in% c(10573L, 14469L) ~ 1L,
      chamber == "House" & congress == 106L & icpsr_id %in% c(29942L, 29918L) ~ 2L,
      chamber == "House" & congress == 109L & icpsr_id == 20322L ~ 1L,
      chamber == "House" & congress == 112L & icpsr_id %in% c(20958L, 20955L, 20959L) ~ 1L,
      chamber == "House" & congress == 114L & icpsr_id %in% c(21560L, 21561L, 21562L) ~ 2L,
      # Senate appointments (last_means = 5) and explicit general/special fixes
      chamber == "Senate" & congress == 103L & icpsr_id %in% c(49305L, 14247L) ~ 5L,
      chamber == "Senate" & congress == 103L & icpsr_id %in% c(15424L, 49306L) ~ 2L,
      chamber == "Senate" & congress == 104L & icpsr_id %in% c(10513L, 12107L) ~ 1L,
      chamber == "Senate" & congress == 104L & icpsr_id == 49504L ~ 5L,
      chamber == "Senate" & congress == 104L & icpsr_id == 14871L ~ 2L,
      chamber == "Senate" & congress == 106L & icpsr_id %in% c(49301L, 14500L) ~ 1L,
      chamber == "Senate" & congress == 106L & icpsr_id %in% c(49904L, 49905L) ~ 5L,
      chamber == "Senate" & congress == 107L & icpsr_id == 40102L ~ 5L,
      chamber == "Senate" & congress == 108L & icpsr_id == 40300L ~ 5L,
      chamber == "Senate" & congress == 109L & icpsr_id == 29373L ~ 5L,
      chamber == "Senate" & congress == 110L & icpsr_id %in% c(40707L, 29534L) ~ 5L,
      chamber == "Senate" & congress == 111L & icpsr_id == 20735L ~ 5L,
      chamber == "Senate" & congress == 112L & icpsr_id %in% c(40913L, 29936L) ~ 1L,
      TRUE ~ last_means
    )
  ) |>
  # Senate special election carry-forwards across unexpired terms (when not appointed)
  mutate(
    last_means = case_when(
      chamber == "Senate" & last_means != 5L & congress == 103L & icpsr_id %in% c(14400L, 49300L, 15502L) ~ 2L,
      chamber == "Senate" & last_means != 5L & congress %in% c(103L, 104L) & icpsr_id %in% c(15424L, 49503L) ~ 2L,
      chamber == "Senate" & last_means != 5L & congress %in% c(103L, 104L, 105L) & icpsr_id %in% c(29523L, 14871L) ~ 2L,
      chamber == "Senate" & last_means != 5L & congress %in% c(106L, 107L, 108L) & icpsr_id == 49904L ~ 2L,
      chamber == "Senate" & last_means != 5L & congress %in% c(107L, 108L, 109L) & icpsr_id == 29369L ~ 2L,
      chamber == "Senate" & last_means != 5L & congress %in% c(110L, 111L, 112L) & icpsr_id %in% c(29534L, 40707L, 40913L, 20735L, 40915L) ~ 2L,
      chamber == "Senate" & last_means != 5L & congress %in% c(111L, 112L, 113L) & icpsr_id == 40916L ~ 2L,
      chamber == "Senate" & last_means != 5L & congress %in% c(112L, 113L, 114L) & icpsr_id %in% c(41112L, 21166L, 21173L) ~ 2L,
      TRUE ~ last_means
    )
  ) |>
  select(
    congress, chamber, icpsr_id, state_icpsr, district_code, state_code,
    party_code, occupancy, last_means, bio_name, bioguide_id, born, died,
    nominate_dim1, nominate_dim2, nominate_log_likelihood,
    nominate_geo_mean_probability, nominate_number_of_votes,
    nominate_number_of_errors, nokken_poole_dim1, nokken_poole_dim2
  ) |>
  arrange(congress, chamber, state_code, district_code)

dbWriteTable(db, "voteview_members", voteview_members, append = TRUE)
message(sprintf("Loaded %d member records into 'voteview_members' (with attainment corrections).", nrow(voteview_members)))

# ----- 2. voteview_info (roll call metadata) ----------------------------------

existing_info <- if (dbExistsTable(db, "voteview_info")) {
  dbGetQuery(db, "SELECT count(*) FROM voteview_info")[[1]]
} else {
  0L
}

if (existing_info == 0L) {
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
      na = c("", "NA", "N/A"),
      show_col_types = FALSE
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
} else {
  message(sprintf("'voteview_info' already contains %d records.", existing_info))
}

# ----- 3. voteview_cast (individual votes cast) ------------------------------

existing_cast <- if (dbExistsTable(db, "voteview_cast")) {
  dbGetQuery(db, "SELECT count(*) FROM voteview_cast")[[1]]
} else {
  0L
}

if (existing_cast == 0L) {
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
      na = c("", "NA", "N/A"),
      show_col_types = FALSE
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
} else {
  message(sprintf("'voteview_cast' already contains %d records.", existing_cast))
}

cnt_members <- dbGetQuery(db, "SELECT count(*) FROM voteview_members")[[1]]
cnt_info    <- dbGetQuery(db, "SELECT count(*) FROM voteview_info")[[1]]
cnt_cast    <- dbGetQuery(db, "SELECT count(*) FROM voteview_cast")[[1]]

dbDisconnect(db)

message("Voteview ETL Complete:")
message(sprintf("  voteview_members: %d rows", cnt_members))
message(sprintf("  voteview_info:    %d rows", cnt_info))
message(sprintf("  voteview_cast:    %d rows", cnt_cast))
