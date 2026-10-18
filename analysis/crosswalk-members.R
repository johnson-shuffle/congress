# ============================================================================
# Tier 1 Synthesis: Master Congressional Member Identity & Term Crosswalk
# Synthesizes voteview_members, govtrack_info, and govtrack_term from SQLite.
# Incorporates biographical, Senate class, and mid-Congress term date patches
# from legacy names_house.R, names_senate.R, and names_*_patch.R.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(DBI)
  library(RSQLite)
})

source("config.R")

db <- get_db()

# ----- 1. extract staged domain tables from sqlite --------------------------

message("Extracting voteview_members, govtrack_info, and govtrack_term from database...")

vm <- dbGetQuery(
  db,
  "SELECT 
     congress,
     chamber,
     icpsr_id,
     state_code,
     district_code AS district_number,
     party_code,
     occupancy,
     last_means AS means,
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
     nickname,
     last_name,
     suffix,
     official_full_name,
     gender,
     birthday
   FROM govtrack_info
   WHERE bioguide_id IS NOT NULL"
) |>
  as_tibble()

gt <- dbGetQuery(
  db,
  "SELECT
     govtrack_id,
     chamber,
     start_date,
     end_date,
     senate_class
   FROM govtrack_term"
) |>
  as_tibble()

# ----- 2. resolve Congress-specific terms from govtrack_term ----------------

# Map each Congress (103–114) to its constitutional window [1789 + 2*(c-1), 1789 + 2*c]
cong_windows <- tibble(
  congress = 103:114,
  cong_start = sprintf("%04d-01-03", 1789 + (103:114 - 1) * 2),
  cong_end   = sprintf("%04d-01-03", 1789 + (103:114) * 2)
)

gt_by_cong <- gt |>
  cross_join(cong_windows) |>
  filter(start_date < cong_end & end_date > cong_start) |>
  group_by(govtrack_id, chamber, congress) |>
  summarise(
    start_date   = min(start_date, na.rm = TRUE),
    end_date     = max(end_date, na.rm = TRUE),
    senate_class = if (all(is.na(senate_class))) 0L else as.integer(max(senate_class, na.rm = TRUE)),
    .groups = "drop"
  )

# ----- 3. synthesize member crosswalk & apply legacy names_* patches --------

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
  left_join(gt_by_cong, by = c("govtrack_id", "chamber", "congress")) |>
  mutate(
    party = coalesce(party_lookup[as.character(party_code)], as.character(party_code)),
    preferred_name = coalesce(nickname, first_name),
    full_name = coalesce(official_full_name, bio_name),
    last_name = coalesce(last_name, sub(",.*$", "", bio_name)),
    district_number = coalesce(as.integer(district_number), 0L),
    senate_class = if_else(chamber == "Senate", coalesce(as.integer(senate_class), 0L), 0L),
    gender = case_when(
      gender == "M" ~ "Male",
      gender == "F" ~ "Female",
      TRUE ~ gender
    )
  ) |>
  # Legacy manual fixes from names_house.R & names_senate.R
  mutate(
    first_name = case_when(
      icpsr_id == 14238L ~ "William",
      icpsr_id == 11036L ~ "John",
      icpsr_id == 39306L ~ "Karen",
      icpsr_id == 21563L ~ "Bruce",
      icpsr_id == 14247L ~ "Robert",
      TRUE ~ first_name
    ),
    preferred_name = case_when(
      icpsr_id == 14238L ~ "Bill",
      icpsr_id == 11036L ~ "John",
      icpsr_id == 39306L ~ "Karen",
      icpsr_id == 21563L ~ "Bruce",
      icpsr_id == 14247L ~ "Robert",
      TRUE ~ preferred_name
    ),
    last_name = case_when(
      icpsr_id == 14238L ~ "Hughes",
      icpsr_id == 11036L ~ "Myers",
      icpsr_id == 39306L ~ "Shepherd",
      icpsr_id == 21563L ~ "Westerman",
      icpsr_id == 14247L ~ "Krueger",
      TRUE ~ last_name
    ),
    suffix = case_when(
      icpsr_id %in% c(14238L, 11036L, 39306L, 21563L, 14247L) ~ NA_character_,
      TRUE ~ suffix
    ),
    gender = case_when(
      icpsr_id %in% c(39306L, 29324L) ~ "Female",
      icpsr_id %in% c(14073L, 14247L, 14511L, 14713L, 40901L, 49305L) ~ "Male",
      is.na(gender) | gender == "" ~ "Male",
      TRUE ~ gender
    ),
    # Mid-Congress & party-switcher start_date / end_date overrides (names_house.R, names_senate.R, names_*_patch.R)
    start_date = case_when(
      # House 103
      chamber == "House" & congress == 103L & icpsr_id == 29313L ~ "1993-06-17",
      chamber == "House" & congress == 103L & icpsr_id == 29352L ~ "1994-05-24",
      chamber == "House" & congress == 103L & icpsr_id == 29362L ~ "1993-12-03",
      chamber == "House" & congress == 103L & icpsr_id == 29368L ~ "1993-04-13",
      chamber == "House" & congress == 103L & icpsr_id == 29386L ~ "1993-05-04",
      chamber == "House" & congress == 103L & icpsr_id == 29393L ~ "1994-05-10",
      chamber == "House" & congress == 103L & icpsr_id == 39314L ~ "1993-05-04",
      # House 104
      chamber == "House" & congress == 104L & icpsr_id == 15600L ~ "1995-10-12",
      chamber == "House" & congress == 104L & icpsr_id == 29586L ~ "1996-03-26",
      chamber == "House" & congress == 104L & icpsr_id == 99342L ~ "1995-04-11",
      chamber == "House" & congress == 104L & icpsr_id == 29585L ~ "1995-10-12",
      chamber == "House" & congress == 104L & icpsr_id == 94679L ~ "1995-08-08",
      chamber == "House" & congress == 104L & icpsr_id == 95418L ~ "1995-10-01",
      chamber == "House" & congress == 104L & icpsr_id == 29587L ~ "1996-04-16",
      chamber == "House" & congress == 104L & icpsr_id == 95617L ~ "1995-11-10",
      chamber == "House" & congress == 104L & icpsr_id == 29588L ~ "1996-05-21",
      chamber == "House" & congress == 104L & icpsr_id == 95611L ~ "1995-06-27",
      # House 105
      chamber == "House" & congress == 105L & icpsr_id == 29788L ~ "1998-04-07",
      chamber == "House" & congress == 105L & icpsr_id == 29774L ~ "1998-03-10",
      chamber == "House" & congress == 105L & icpsr_id == 29775L ~ "1998-04-07",
      chamber == "House" & congress == 105L & icpsr_id == 29779L ~ "1998-06-23",
      chamber == "House" & congress == 105L & icpsr_id == 29772L ~ "1997-05-13",
      chamber == "House" & congress == 105L & icpsr_id == 29776L ~ "1998-02-03",
      chamber == "House" & congress == 105L & icpsr_id == 29773L ~ "1997-11-04",
      chamber == "House" & congress == 105L & icpsr_id == 25777L ~ "1998-05-19",
      chamber == "House" & congress == 105L & icpsr_id == 29771L ~ "1997-04-12",
      # House 106
      chamber == "House" & congress == 106L & icpsr_id == 94879L ~ "2000-06-27",
      chamber == "House" & congress == 106L & icpsr_id == 29942L ~ "1999-11-16",
      chamber == "House" & congress == 106L & icpsr_id == 29918L ~ "1999-06-08",
      chamber == "House" & congress == 106L & icpsr_id == 99542L ~ "1999-07-17",
      chamber == "House" & congress == 106L & icpsr_id == 99767L ~ "2000-01-24",
      # House 107
      chamber == "House" & congress == 107L & icpsr_id == 14620L ~ "2001-01-03",
      chamber == "House" & congress == 107L & icpsr_id == 20101L ~ "2001-11-29",
      chamber == "House" & congress == 107L & icpsr_id == 20106L ~ "2001-06-07",
      chamber == "House" & congress == 107L & icpsr_id == 20110L ~ "2001-10-23",
      chamber == "House" & congress == 107L & icpsr_id == 20119L ~ "2001-10-23",
      chamber == "House" & congress == 107L & icpsr_id == 20131L ~ "2002-02-27",
      chamber == "House" & congress == 107L & icpsr_id == 20134L ~ "2001-05-17",
      chamber == "House" & congress == 107L & icpsr_id == 20138L ~ "2001-12-18",
      chamber == "House" & congress == 107L & icpsr_id == 20143L ~ "2001-06-26",
      chamber == "House" & congress == 107L & icpsr_id == 89767L ~ "2002-08-01",
      # House 108
      chamber == "House" & congress == 108L & icpsr_id == 20326L ~ "2004-02-17",
      chamber == "House" & congress == 108L & icpsr_id == 20340L ~ "2004-07-21",
      chamber == "House" & congress == 108L & icpsr_id == 20349L ~ "2004-06-03",
      chamber == "House" & congress == 108L & icpsr_id == 94828L ~ "2004-01-01",
      chamber == "House" & congress == 108L & icpsr_id == 20353L ~ "2003-06-05",
      # House 114
      chamber == "House" & congress == 114L & icpsr_id %in% c(29137L, 20914L, 21147L) ~ "2015-01-06",
      # Senate 103–111
      chamber == "Senate" & congress == 103L & icpsr_id == 15424L ~ "1994-12-02",
      chamber == "Senate" & congress == 103L & icpsr_id == 49306L ~ "1993-06-14",
      chamber == "Senate" & congress == 104L & icpsr_id == 95407L ~ "1995-03-03",
      chamber == "Senate" & congress == 104L & icpsr_id == 49504L ~ "1996-06-11",
      chamber == "Senate" & congress == 106L & icpsr_id == 49904L ~ "2000-07-19",
      chamber == "Senate" & congress == 106L & icpsr_id == 49905L ~ "1999-11-04",
      chamber == "Senate" & congress == 107L & icpsr_id == 94240L ~ "2001-05-25",
      chamber == "Senate" & congress == 108L & govtrack_id == "300075" ~ "2003-01-07",
      chamber == "Senate" & congress == 111L & icpsr_id == 94910L ~ "2009-04-01",
      TRUE ~ start_date
    ),
    end_date = case_when(
      # House 103
      chamber == "House" & congress == 103L & icpsr_id == 14444L ~ "1993-01-21",
      chamber == "House" & congress == 103L & icpsr_id == 6845L  ~ "1994-03-29",
      chamber == "House" & congress == 103L & icpsr_id == 15103L ~ "1993-07-31",
      chamber == "House" & congress == 103L & icpsr_id == 15411L ~ "1993-01-22",
      chamber == "House" & congress == 103L & icpsr_id == 14225L ~ "1993-01-31",
      chamber == "House" & congress == 103L & icpsr_id == 14219L ~ "1994-01-07",
      chamber == "House" & congress == 103L & icpsr_id == 13003L ~ "1993-01-20",
      # House 104
      chamber == "House" & congress == 104L & icpsr_id == 14257L ~ "1995-10-10",
      chamber == "House" & congress == 104L & icpsr_id == 29319L ~ "1995-12-15",
      chamber == "House" & congress == 104L & icpsr_id == 29342L ~ "1995-04-10",
      chamber == "House" & congress == 104L & icpsr_id == 29347L ~ "1995-10-01",
      chamber == "House" & congress == 104L & icpsr_id == 14679L ~ "1995-08-07",
      chamber == "House" & congress == 104L & icpsr_id == 15418L ~ "1995-09-30",
      chamber == "House" & congress == 104L & icpsr_id == 15433L ~ "1996-02-18",
      chamber == "House" & congress == 104L & icpsr_id == 15617L ~ "1995-11-09",
      chamber == "House" & congress == 104L & icpsr_id == 14871L ~ "1996-02-05",
      chamber == "House" & congress == 104L & icpsr_id == 15611L ~ "1995-06-26",
      # House 106
      chamber == "House" & congress == 106L & icpsr_id == 14879L ~ "2000-06-26",
      chamber == "House" & congress == 106L & icpsr_id == 10573L ~ "1999-11-15",
      chamber == "House" & congress == 106L & icpsr_id == 14469L ~ "1999-03-01",
      chamber == "House" & congress == 106L & icpsr_id == 29542L ~ "1999-07-16",
      chamber == "House" & congress == 106L & icpsr_id == 29767L ~ "2000-01-23",
      # House 107
      chamber == "House" & congress == 107L & icpsr_id == 14620L ~ "2000-12-08",
      chamber == "House" & congress == 107L & icpsr_id == 99767L ~ "2002-07-31",
      # House 108
      chamber == "House" & congress == 108L & icpsr_id == 29917L ~ "2003-12-08",
      chamber == "House" & congress == 108L & icpsr_id == 20339L ~ "2004-06-11",
      chamber == "House" & congress == 108L & icpsr_id == 20348L ~ "2004-01-20",
      chamber == "House" & congress == 108L & icpsr_id == 14828L ~ "2003-12-31",
      chamber == "House" & congress == 108L & icpsr_id == 15093L ~ "2003-05-31",
      # House 114
      chamber == "House" & congress == 114L & icpsr_id == 29137L ~ "2015-10-29",
      chamber == "House" & congress == 114L & icpsr_id == 20914L ~ "2015-03-31",
      chamber == "House" & congress == 114L & icpsr_id == 21147L ~ "2015-02-06",
      # Senate 103–111
      chamber == "Senate" & congress == 103L & icpsr_id == 14700L ~ "1994-12-01",
      chamber == "Senate" & congress == 103L & icpsr_id == 14247L ~ "1993-06-13",
      chamber == "Senate" & congress == 104L & icpsr_id == 15407L ~ "1995-03-02",
      chamber == "Senate" & congress == 104L & icpsr_id == 10513L ~ "1996-06-10",
      chamber == "Senate" & congress == 106L & icpsr_id == 49301L ~ "2000-07-18",
      chamber == "Senate" & congress == 106L & icpsr_id == 14500L ~ "1999-10-24",
      chamber == "Senate" & congress == 107L & icpsr_id == 14240L ~ "2001-05-24",
      chamber == "Senate" & congress == 108L & govtrack_id == "300075" ~ "2005-01-03",
      chamber == "Senate" & congress == 111L & icpsr_id == 40903L ~ "2010-10-28",
      chamber == "Senate" & congress == 111L & icpsr_id == 14910L ~ "2009-03-31",
      TRUE ~ end_date
    )
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
    senate_class,
    party,
    first_name,
    preferred_name,
    last_name,
    suffix,
    full_name,
    gender,
    birthday,
    occupancy,
    means,
    start_date,
    end_date
  ) |>
  distinct(congress, chamber, icpsr_id, district_number, .keep_all = TRUE) |>
  arrange(congress, chamber, state_code, district_number)

# ----- 4. write synthesized crosswalk to database ---------------------------

execute_sql_file(db, file.path(path_sql, "tables", "crosswalk-members.sql"))
dbWriteTable(db, "crosswalk_members", crosswalk_members, append = TRUE)

dbDisconnect(db)

message(sprintf(
  "Successfully synthesized %d member records into 'crosswalk_members' table.",
  nrow(crosswalk_members)
))
