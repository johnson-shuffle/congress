# ==============================================================================
# Census & STF3D: Congressional District & State Demographics ETL
# Sources:
#   - 1990 Summary Tape File 3D (raw/census/1990_STF3D.csv)
#   - 2000 Census Demographic Profiles (raw/census/c106_DP*.csv)
#   - 109th Congress ACS Profiles (raw/census/c109_DP*.csv)
#   - 111th Congress ACS 5-Year Profiles (raw/census/c111_DP*.csv)
#   - 113th Congress ACS Profiles (raw/census/c113_DP*.csv)
# Targets: census_district and census_state in congress.sqlite
# ==============================================================================

source("config.R")
library(DBI)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# ----- database connection ----------------------------------------------------

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

states_lookup <- dbGetQuery(db, "SELECT fips, state_code FROM states")
raw_dir <- file.path(path_raw, "census")

# Identify single-member at-large states (standardized to district_number = 0)
at_large_fips <- c(2, 10, 30, 38, 46, 50, 56) # AK, DE, MT, ND, SD, VT, WY

# ==============================================================================
# 1. 103rd Congress (1990 Census STF3D, 1993-1998)
# ==============================================================================

parse_census_103 <- function(dir = raw_dir, states_df = states_lookup) {
  message("Parsing 103rd Congress (1990 STF3D)...")
  stf_file <- file.path(dir, "1990_STF3D.csv")
  if (!file.exists(stf_file)) stop("Missing 1990 STF3D file: ", stf_file)

  req_cols <- c(
    "STUSAB", "SUMLEV", "GEOCOMP", "SAC7",
    "P1_1", paste0("P13_", 27:31), paste0("P8_", 1:5), "P10_1",
    paste0("P57_", 1:7), paste0("P77_", 1:17),
    "P80A_1", "P107A_1", paste0("H30_", 1:9)
  )

  df <- read_csv(stf_file, col_select = all_of(req_cols), col_types = cols(.default = "d", STUSAB = "c")) |>
    filter(SUMLEV == 40 | (SUMLEV == 501 & GEOCOMP == 0)) |>
    left_join(states_df, by = c("STUSAB" = "state_code")) |>
    mutate(
      district = if_else(SUMLEV == 40, NA_integer_, as.integer(SAC7)),
      district = if_else(!is.na(district) & district == 0, 1L, district)
    )

  pop_tot <- df$P1_1
  pop_65 <- rowSums(df[, paste0("P13_", 27:31)])
  pop_25 <- rowSums(df[, paste0("P57_", 1:7)])
  empl_tot <- rowSums(df[, paste0("P77_", 1:17)])
  hm_tot <- rowSums(df[, paste0("H30_", 1:9)])

  df_base <- tibble(
    fips = as.integer(df$fips),
    state_code = df$STUSAB,
    district = df$district,
    pop_total = as.integer(pop_tot),
    pct_white = df$P8_1 / pop_tot * 100,
    pct_black = df$P8_2 / pop_tot * 100,
    pct_indian = df$P8_3 / pop_tot * 100,
    pct_asian_island = df$P8_4 / pop_tot * 100,
    pct_other = df$P8_5 / pop_tot * 100,
    pct_hispanic = df$P10_1 / pop_tot * 100,
    pct_pop_65 = pop_65 / pop_tot * 100,
    pct_pop_25 = pop_25 / pop_tot * 100,
    pct_less_than_9th = df$P57_1 / pop_25 * 100,
    pct_more_than_9th = df$P57_2 / pop_25 * 100,
    pct_highschool = df$P57_3 / pop_25 * 100,
    pct_somecollege = df$P57_4 / pop_25 * 100,
    pct_associate = df$P57_5 / pop_25 * 100,
    pct_bachelor = df$P57_6 / pop_25 * 100,
    pct_graduate = df$P57_7 / pop_25 * 100,
    empl_total = as.integer(empl_tot),
    pct_empl_res = (df$P77_1 + df$P77_2) / empl_tot * 100,
    pct_empl_con = df$P77_3 / empl_tot * 100,
    pct_empl_man = (df$P77_4 + df$P77_5) / empl_tot * 100,
    pct_empl_wst = df$P77_8 / empl_tot * 100,
    pct_empl_rt = df$P77_9 / empl_tot * 100,
    pct_empl_pub = df$P77_17 / empl_tot * 100,
    med_inc_house = df$P80A_1,
    med_inc_fam = df$P107A_1,
    hm_total = as.integer(hm_tot),
    pct_hm_electric = df$H30_3 / hm_tot * 100
  )

  # Expand across 1993-1998
  bind_rows(lapply(1993:1998, function(y) mutate(df_base, year = as.integer(y))))
}

# ==============================================================================
# Helper for 106, 109, 111, 113 Profile Datasets
# ==============================================================================

parse_dp_dataset <- function(prefix, years, dir = raw_dir, states_df = states_lookup, hm_override_prefix = NULL) {
  message("Parsing ", prefix, " (", min(years), "-", max(years), ")...")

  # 1. Demographics DP1
  dp1 <- read_csv(file.path(dir, str_c(prefix, "_DP1.csv")), col_types = cols(.default = "c")) |>
    filter(GEO.id != "Id" & !grepl("ZZ", GEO.id)) |>
    mutate(
      fips = as.integer(substr(GEO.id, 10, 11)),
      district = as.integer(substr(GEO.id, 12, 13))
    ) |>
    filter(fips <= 56) |>
    mutate(district = if_else(!is.na(district) & district == 0, 1L, district))

  # Variables by prefix
  if (prefix == "c106") {
    demo_cols <- c("HC01_VC01", paste0("HC01_VC", c(15:17, 29:33, 45, 56)))
    dp1_sub <- dp1 |> select(fips, district, all_of(demo_cols)) |> mutate(across(-c(fips, district), as.numeric))
    pop_tot <- dp1_sub$HC01_VC01
    pop_65 <- dp1_sub$HC01_VC15 + dp1_sub$HC01_VC16 + dp1_sub$HC01_VC17
    demo_df <- tibble(
      fips = dp1_sub$fips, district = dp1_sub$district,
      pop_total = pop_tot, white = dp1_sub$HC01_VC29, black = dp1_sub$HC01_VC30,
      indian = dp1_sub$HC01_VC31, asian = dp1_sub$HC01_VC32, island = dp1_sub$HC01_VC33,
      other = dp1_sub$HC01_VC45, hispanic = dp1_sub$HC01_VC56, pop_65 = pop_65
    )
  } else if (prefix == "c109") {
    demo_cols <- paste0("EST_VC", c("01", 34:36, 41, 49, 54, 70, 22))
    dp1_sub <- dp1 |> select(fips, district, all_of(demo_cols)) |> mutate(across(-c(fips, district), as.numeric))
    demo_df <- tibble(
      fips = dp1_sub$fips, district = dp1_sub$district,
      pop_total = dp1_sub$EST_VC01, white = dp1_sub$EST_VC34, black = dp1_sub$EST_VC35,
      indian = dp1_sub$EST_VC36, asian = dp1_sub$EST_VC41, island = dp1_sub$EST_VC49,
      other = dp1_sub$EST_VC54, hispanic = dp1_sub$EST_VC70, pop_65 = dp1_sub$EST_VC22
    )
  } else { # c111, c113
    demo_cols <- c("HD01_S001", paste0("HD01_S0", c(15:19, 78:81, 89, 94)), "HD01_S107")
    dp1_sub <- dp1 |> select(fips, district, all_of(demo_cols)) |> mutate(across(-c(fips, district), as.numeric))
    pop_65 <- rowSums(dp1_sub[, paste0("HD01_S0", 15:19)])
    demo_df <- tibble(
      fips = dp1_sub$fips, district = dp1_sub$district,
      pop_total = dp1_sub$HD01_S001, white = dp1_sub$HD01_S078, black = dp1_sub$HD01_S079,
      indian = dp1_sub$HD01_S080, asian = dp1_sub$HD01_S081, island = dp1_sub$HD01_S089,
      other = dp1_sub$HD01_S094, hispanic = dp1_sub$HD01_S107, pop_65 = pop_65
    )
  }

  # State aggregates for demo
  demo_st <- demo_df |>
    group_by(fips) |>
    summarise(across(-district, sum), .groups = "drop") |>
    mutate(district = NA_integer_)
  demo_all <- bind_rows(demo_df, demo_st)

  # 2. Education DP2
  dp2 <- read_csv(file.path(dir, str_c(prefix, "_DP2.csv")), col_types = cols(.default = "c")) |>
    filter(GEO.id != "Id" & !grepl("ZZ", GEO.id)) |>
    mutate(
      fips = as.integer(substr(GEO.id, 10, 11)),
      district = as.integer(substr(GEO.id, 12, 13))
    ) |>
    filter(fips <= 56) |>
    mutate(district = if_else(!is.na(district) & district == 0, 1L, district))

  if (prefix == "c106") {
    edu_cols <- paste0("HC01_VC", c("09", 10:16))
    dp2_sub <- dp2 |> select(fips, district, all_of(edu_cols)) |> mutate(across(-c(fips, district), as.numeric))
    edu_df <- tibble(
      fips = dp2_sub$fips, district = dp2_sub$district,
      pop_25 = dp2_sub$HC01_VC09, less_than_9th = dp2_sub$HC01_VC10, more_than_9th = dp2_sub$HC01_VC11,
      highschool = dp2_sub$HC01_VC12, somecollege = dp2_sub$HC01_VC13, associate = dp2_sub$HC01_VC14,
      bachelor = dp2_sub$HC01_VC15, graduate = dp2_sub$HC01_VC16
    )
  } else if (prefix == "c109") {
    edu_cols <- paste0("HC01_EST_VC", c("06", "07", "08", "09", 10:13))
    dp2_sub <- dp2 |> select(fips, district, all_of(edu_cols)) |> mutate(across(-c(fips, district), as.numeric))
    p25 <- dp2_sub$HC01_EST_VC06
    edu_df <- tibble(
      fips = dp2_sub$fips, district = dp2_sub$district,
      pop_25 = p25, less_than_9th = p25 * dp2_sub$HC01_EST_VC07 / 100, more_than_9th = p25 * dp2_sub$HC01_EST_VC08 / 100,
      highschool = p25 * dp2_sub$HC01_EST_VC09 / 100, somecollege = p25 * dp2_sub$HC01_EST_VC10 / 100,
      associate = p25 * dp2_sub$HC01_EST_VC11 / 100, bachelor = p25 * dp2_sub$HC01_EST_VC12 / 100,
      graduate = p25 * dp2_sub$HC01_EST_VC13 / 100
    )
  } else if (prefix == "c111") {
    edu_cols <- paste0("HC01_VC", 84:91)
    dp2_sub <- dp2 |> select(fips, district, all_of(edu_cols)) |> mutate(across(-c(fips, district), as.numeric))
    edu_df <- tibble(
      fips = dp2_sub$fips, district = dp2_sub$district,
      pop_25 = dp2_sub$HC01_VC84, less_than_9th = dp2_sub$HC01_VC85, more_than_9th = dp2_sub$HC01_VC86,
      highschool = dp2_sub$HC01_VC87, somecollege = dp2_sub$HC01_VC88, associate = dp2_sub$HC01_VC89,
      bachelor = dp2_sub$HC01_VC90, graduate = dp2_sub$HC01_VC91
    )
  } else { # c113
    edu_cols <- paste0("HC01_VC", 85:92)
    dp2_sub <- dp2 |> select(fips, district, all_of(edu_cols)) |> mutate(across(-c(fips, district), as.numeric))
    edu_df <- tibble(
      fips = dp2_sub$fips, district = dp2_sub$district,
      pop_25 = dp2_sub$HC01_VC85, less_than_9th = dp2_sub$HC01_VC86, more_than_9th = dp2_sub$HC01_VC87,
      highschool = dp2_sub$HC01_VC88, somecollege = dp2_sub$HC01_VC89, associate = dp2_sub$HC01_VC90,
      bachelor = dp2_sub$HC01_VC91, graduate = dp2_sub$HC01_VC92
    )
  }

  edu_st <- edu_df |>
    group_by(fips) |>
    summarise(across(-district, sum), .groups = "drop") |>
    mutate(district = NA_integer_)
  edu_all <- bind_rows(edu_df, edu_st)

  # 3. Employment & Incomes DP3
  dp3 <- read_csv(file.path(dir, str_c(prefix, "_DP3.csv")), col_types = cols(.default = "c")) |>
    filter(GEO.id != "Id" & !grepl("ZZ", GEO.id)) |>
    mutate(
      fips = as.integer(substr(GEO.id, 10, 11)),
      district = as.integer(substr(GEO.id, 12, 13))
    ) |>
    filter(fips <= 56) |>
    mutate(district = if_else(!is.na(district) & district == 0, 1L, district))

  if (prefix == "c106") {
    empl_cols <- c(paste0("HC01_VC", c(34:38, 46, 64, 86)))
    dp3_sub <- dp3 |> select(fips, district, all_of(empl_cols)) |> mutate(across(-c(fips, district), as.numeric))
    empl_tot <- rowSums(dp3_sub[, paste0("HC01_VC", c(34:38, 46))])
    empl_df <- tibble(
      fips = dp3_sub$fips, district = dp3_sub$district,
      empl_res = dp3_sub$HC01_VC34, empl_con = dp3_sub$HC01_VC35, empl_man = dp3_sub$HC01_VC36,
      empl_wst = dp3_sub$HC01_VC37, empl_rt = dp3_sub$HC01_VC38, empl_pub = dp3_sub$HC01_VC46,
      med_inc_house = dp3_sub$HC01_VC64, med_inc_fam = dp3_sub$HC01_VC86, empl_total = empl_tot
    )
  } else if (prefix == "c109") {
    empl_cols <- paste0("EST_VC", c(37:41, 49, 67, 91))
    dp3_sub <- dp3 |> select(fips, district, all_of(empl_cols)) |> mutate(across(-c(fips, district), as.numeric))
    empl_tot <- rowSums(dp3_sub[, paste0("EST_VC", c(37:41, 49))])
    empl_df <- tibble(
      fips = dp3_sub$fips, district = dp3_sub$district,
      empl_res = dp3_sub$EST_VC37, empl_con = dp3_sub$EST_VC38, empl_man = dp3_sub$EST_VC39,
      empl_wst = dp3_sub$EST_VC40, empl_rt = dp3_sub$EST_VC41, empl_pub = dp3_sub$EST_VC49,
      med_inc_house = dp3_sub$EST_VC67, med_inc_fam = dp3_sub$EST_VC91, empl_total = empl_tot
    )
  } else if (prefix == "c111") {
    empl_cols <- c(paste0("HC01_VC", c(50:54, 62, 85, 112)))
    dp3_sub <- dp3 |> select(fips, district, all_of(empl_cols)) |> mutate(across(-c(fips, district), as.numeric))
    empl_tot <- rowSums(dp3_sub[, paste0("HC01_VC", c(50:54, 62))])
    empl_df <- tibble(
      fips = dp3_sub$fips, district = dp3_sub$district,
      empl_res = dp3_sub$HC01_VC50, empl_con = dp3_sub$HC01_VC51, empl_man = dp3_sub$HC01_VC52,
      empl_wst = dp3_sub$HC01_VC53, empl_rt = dp3_sub$HC01_VC54, empl_pub = dp3_sub$HC01_VC62,
      med_inc_house = dp3_sub$HC01_VC85, med_inc_fam = dp3_sub$HC01_VC112, empl_total = empl_tot
    )
  } else { # c113
    empl_cols <- c(paste0("HC01_VC", c(50:54, 62, 85, 114)))
    dp3_sub <- dp3 |> select(fips, district, all_of(empl_cols)) |> mutate(across(-c(fips, district), as.numeric))
    empl_tot <- rowSums(dp3_sub[, paste0("HC01_VC", c(50:54, 62))])
    empl_df <- tibble(
      fips = dp3_sub$fips, district = dp3_sub$district,
      empl_res = dp3_sub$HC01_VC50, empl_con = dp3_sub$HC01_VC51, empl_man = dp3_sub$HC01_VC52,
      empl_wst = dp3_sub$HC01_VC53, empl_rt = dp3_sub$HC01_VC54, empl_pub = dp3_sub$HC01_VC62,
      med_inc_house = dp3_sub$HC01_VC85, med_inc_fam = dp3_sub$HC01_VC114, empl_total = empl_tot
    )
  }

  empl_st_sum <- empl_df |>
    group_by(fips) |>
    summarise(across(c(empl_res, empl_con, empl_man, empl_wst, empl_rt, empl_pub, empl_total), sum), .groups = "drop")
  empl_st_mean <- empl_df |>
    group_by(fips) |>
    summarise(across(c(med_inc_house, med_inc_fam), mean), .groups = "drop")
  empl_st <- left_join(empl_st_sum, empl_st_mean, by = "fips") |> mutate(district = NA_integer_)
  empl_all <- bind_rows(empl_df, empl_st)

  # 4. Housing Electricity DP4
  hm_prefix <- if (!is.null(hm_override_prefix)) hm_override_prefix else prefix
  dp4 <- read_csv(file.path(dir, str_c(hm_prefix, "_DP4.csv")), col_types = cols(.default = "c")) |>
    filter(GEO.id != "Id" & !grepl("ZZ", GEO.id)) |>
    mutate(
      fips = as.integer(substr(GEO.id, 10, 11)),
      district = as.integer(substr(GEO.id, 12, 13))
    ) |>
    filter(fips <= 56) |>
    mutate(district = if_else(!is.na(district) & district == 0, 1L, district))

  if (hm_prefix == "c106") {
    hm_cols <- paste0("HC01_VC", 46:54)
    dp4_sub <- dp4 |> select(fips, district, all_of(hm_cols)) |> mutate(across(-c(fips, district), as.numeric))
    hm_df <- tibble(
      fips = dp4_sub$fips, district = dp4_sub$district,
      hm_electric = dp4_sub$HC01_VC48, hm_total = rowSums(dp4_sub[, hm_cols])
    )
  } else if (hm_prefix == "c113") {
    hm_cols <- paste0("HC01_VC", 92:100)
    dp4_sub <- dp4 |> select(fips, district, all_of(hm_cols)) |> mutate(across(-c(fips, district), as.numeric))
    hm_df <- tibble(
      fips = dp4_sub$fips, district = dp4_sub$district,
      hm_electric = dp4_sub$HC01_VC94, hm_total = rowSums(dp4_sub[, hm_cols])
    )
  } else { # c111, or c109 override
    hm_cols <- paste0("HC01_VC", 90:98)
    dp4_sub <- dp4 |> select(fips, district, all_of(hm_cols)) |> mutate(across(-c(fips, district), as.numeric))
    hm_df <- tibble(
      fips = dp4_sub$fips, district = dp4_sub$district,
      hm_electric = dp4_sub$HC01_VC92, hm_total = rowSums(dp4_sub[, hm_cols])
    )
  }

  hm_st <- hm_df |>
    group_by(fips) |>
    summarise(across(-district, sum), .groups = "drop") |>
    mutate(district = NA_integer_)
  hm_all <- bind_rows(hm_df, hm_st)

  # Combine DP1, DP2, DP3, DP4
  merged <- demo_all |>
    left_join(edu_all, by = c("fips", "district")) |>
    left_join(empl_all, by = c("fips", "district")) |>
    left_join(hm_all, by = c("fips", "district")) |>
    left_join(states_df, by = "fips")

  # Calculate percentages matching legacy definitions
  p_tot <- merged$pop_total
  p_25 <- merged$pop_25
  e_tot <- merged$empl_total
  h_tot <- merged$hm_total

  res_base <- tibble(
    fips = merged$fips,
    state_code = merged$state_code,
    district = merged$district,
    pop_total = as.integer(p_tot),
    pct_white = merged$white / p_tot * 100,
    pct_black = merged$black / p_tot * 100,
    pct_indian = merged$indian / p_tot * 100,
    pct_asian_island = (merged$asian + merged$island) / p_tot * 100,
    pct_other = merged$other / p_tot * 100,
    pct_hispanic = merged$hispanic / p_tot * 100,
    pct_pop_65 = merged$pop_65 / p_tot * 100,
    pct_pop_25 = p_25 / p_tot * 100,
    pct_less_than_9th = merged$less_than_9th / p_25 * 100,
    pct_more_than_9th = merged$more_than_9th / p_25 * 100,
    pct_highschool = merged$highschool / p_25 * 100,
    pct_somecollege = merged$somecollege / p_25 * 100,
    pct_associate = merged$associate / p_25 * 100,
    pct_bachelor = merged$bachelor / p_25 * 100,
    pct_graduate = merged$graduate / p_25 * 100,
    empl_total = as.integer(e_tot),
    pct_empl_res = merged$empl_res / e_tot * 100,
    pct_empl_con = merged$empl_con / e_tot * 100,
    pct_empl_man = merged$empl_man / e_tot * 100,
    pct_empl_wst = merged$empl_wst / e_tot * 100,
    pct_empl_rt = merged$empl_rt / e_tot * 100,
    pct_empl_pub = merged$empl_pub / e_tot * 100,
    med_inc_house = merged$med_inc_house,
    med_inc_fam = merged$med_inc_fam,
    hm_total = as.integer(h_tot),
    pct_hm_electric = merged$hm_electric / h_tot * 100
  )

  # Expand across active years
  bind_rows(lapply(years, function(y) mutate(res_base, year = as.integer(y))))
}

# ==============================================================================
# Main ETL Execution
# ==============================================================================

message("Synthesizing unified Census & ACS demographic panel (1993-2015)...")

c103 <- parse_census_103()
c106 <- parse_dp_dataset("c106", years = 1999:2002)
c109 <- parse_dp_dataset("c109", years = 2003:2008, hm_override_prefix = "c111")
c111 <- parse_dp_dataset("c111", years = 2009:2012)
c113 <- parse_dp_dataset("c113", years = 2013:2015)

all_census <- bind_rows(c113, c111, c109, c106, c103)
message("Total parsed records: ", nrow(all_census))

# ----------------- Separate District & State Records --------------------------

# District records
df_district <- all_census |>
  filter(!is.na(district)) |>
  mutate(
    district_number = as.integer(if_else(fips %in% at_large_fips & district == 1, 0, district))
  ) |>
  select(-district) |>
  select(year, fips, state_code, district_number, everything()) |>
  arrange(year, state_code, district_number)

# State records
df_state <- all_census |>
  filter(is.na(district)) |>
  select(-district) |>
  select(year, fips, state_code, everything()) |>
  arrange(year, state_code)

message("District observations: ", nrow(df_district))
message("State observations: ", nrow(df_state))

# ----- execute DDL and populate database --------------------------------------

execute_sql_file(db, file.path(path_sql, "tables", "census-district.sql"))

dbAppendTable(db, "census_district", as.data.frame(df_district))
message("Inserted ", nrow(df_district), " rows into census_district.")

dbAppendTable(db, "census_state", as.data.frame(df_state))
message("Inserted ", nrow(df_state), " rows into census_state.")

# ----- verification -----------------------------------------------------------

n_dist <- dbGetQuery(db, "SELECT count(*) AS n FROM census_district")$n
n_state <- dbGetQuery(db, "SELECT count(*) AS n FROM census_state")$n

message("Verification: ", n_dist, " rows in census_district, ", n_state, " in census_state.")
stopifnot(n_dist + n_state == nrow(all_census))
stopifnot(n_dist == 10028)
stopifnot(n_state == 1173)

message("Census district and state ETL completed successfully.")
