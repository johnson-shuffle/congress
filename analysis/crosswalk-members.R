# ============================================================================
# Tier 1 Synthesis: Master Congressional Member Identity Crosswalk
# Synthesizes voteview_members and govtrack_info directly from congress.sqlite
# Produces unified crosswalk_members dimension table
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(DBI)
  library(RSQLite)
})

source("config.R")

db <- get_db()

# ----- 1. extract staged domain tables from sqlite --------------------------

message("Extracting voteview_members and govtrack_info from database...")

vm <- dbGetQuery(
  db,
  "SELECT 
     congress,
     chamber,
     icpsr_id,
     state_code,
     district_code AS district_number,
     party_code,
     bio_name,
     bioguide_id
   FROM voteview_members"
) |>
  as_tibble()

gi <- dbGetQuery(
  db,
  "SELECT 
     govtrack_id,
     bioguide_id,
     opensecrets_id,
     votesmart_id,
     fec_id,
     first_name,
     last_name,
     official_full_name
   FROM govtrack_info
   WHERE bioguide_id IS NOT NULL"
) |>
  as_tibble()

# ----- 2. synthesize member crosswalk ---------------------------------------

party_lookup <- c(
  "100" = "Democrat",
  "200" = "Republican",
  "328" = "Independent",
  "329" = "Independent",
  "331" = "Independent",
  "340" = "Reform",
  "370" = "Progressive"
)

crosswalk_members <- vm |>
  left_join(gi, by = "bioguide_id") |>
  mutate(
    party = coalesce(party_lookup[as.character(party_code)], as.character(party_code)),
    full_name = coalesce(official_full_name, bio_name),
    last_name = coalesce(
      last_name,
      sub(",.*$", "", bio_name)
    ),
    district_number = coalesce(as.integer(district_number), 0L)
  ) |>
  select(
    congress,
    chamber,
    icpsr_id,
    bioguide_id,
    govtrack_id,
    opensecrets_id,
    votesmart_id,
    fec_id,
    state_code,
    district_number,
    party,
    last_name,
    full_name
  ) |>
  distinct(congress, chamber, icpsr_id, district_number, .keep_all = TRUE) |>
  arrange(congress, chamber, state_code, district_number)

# ----- 3. write synthesized crosswalk to database ---------------------------

init_table(db, "crosswalk_members")
dbWriteTable(db, "crosswalk_members", crosswalk_members, append = TRUE)

dbDisconnect(db)

message(sprintf(
  "Successfully synthesized %d member records into 'crosswalk_members' table.",
  nrow(crosswalk_members)
))
