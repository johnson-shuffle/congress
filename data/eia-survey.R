# ==============================================================================
# EIA: Electricity Generation Survey Ingestion (Forms 759, 906, 923)
# Sources: raw/eia/ f759*.xls, F906*.csv/xls, f923_*.csv
# Target: eia_survey_gen table in congress.sqlite
# ==============================================================================

source("config.R")
library(readr)
library(readxl)
library(dplyr)
library(stringr)

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

states <- dbGetQuery(db, "SELECT fips, state_code FROM states")
all_states <- states$state_code

# Execute DDL
execute_sql_file(db, file.path(path_sql, "eia-survey.sql"))

# ----- 1. Form 759: Utility Data (1990-2000) ----------------------------------

message("Ingesting Form 759 raw survey records (1990-2000)...")
plts_759_list <- list()

for (yr in 1990:2000) {
  fn <- file.path(path_raw, "eia", paste0("f759", yr, "u.xls"))
  dat <- read_excel(fn, sheet = 1)
  names(dat) <- toupper(names(dat))
  
  if (yr <= 1995) {
    con_cols <- grep("^CON[0-9]{2}", names(dat), value = TRUE)
    gen_cols <- grep("^GEN[0-9]{2}", names(dat), value = TRUE)
    dat <- dat |> mutate(across(all_of(c(con_cols, gen_cols)), as.numeric))
    dat$CONSUMPTIO <- rowSums(dat[con_cols], na.rm = TRUE)
    dat$NETGENERAT <- rowSums(dat[gen_cols], na.rm = TRUE)
  }
  
  fuel_col <- if ("FUELDESC" %in% names(dat)) "FUELDESC" else if ("FUELNM" %in% names(dat)) "FUELNM" else "FUELTYP"
  pcode_col <- if ("PCODE" %in% names(dat)) "PCODE" else "PLTCODE"
  
  df_yr <- dat |>
    transmute(
      plant_id = as.integer(.data[[pcode_col]]),
      fips = as.integer(FIPST),
      energy_source = as.character(.data[[fuel_col]]),
      fuel_quantity = as.numeric(CONSUMPTIO),
      fuel_mmbtu = NA_real_,
      net_generation_mwh = as.numeric(NETGENERAT),
      year = as.integer(yr),
      survey_form = "F759"
    ) |>
    inner_join(states, by = "fips") |>
    select(-fips) |>
    filter(net_generation_mwh >= 0)
    
  plts_759_list[[as.character(yr)]] <- df_yr
}
p759 <- bind_rows(plts_759_list)

# ----- 2. Form 906: Non-Utility Data (1990-1998) ------------------------------

message("Ingesting Form 906 non-utility raw records (1990-1998)...")
f906_98 <- read_csv(
  file.path(path_raw, "eia", "F9061998nu.csv"),
  col_types = cols(),
  show_col_types = FALSE
)

p1998 <- f906_98 |>
  filter(Year >= 1990) |>
  transmute(
    plant_id = as.integer(`Facility Code`),
    state_code = as.character(State),
    energy_source = as.character(`Fuel Code Standardized to 2002 Codes`),
    fuel_quantity = NA_real_,
    fuel_mmbtu = as.numeric(gsub(",", "", `Total Fuel Consumption (million Btu)`)),
    net_generation_mwh = as.numeric(gsub(",", "", `Net Generation (MWh)`)),
    year = as.integer(Year),
    survey_form = "F906_NU"
  ) |>
  mutate(
    fuel_mmbtu = replace(fuel_mmbtu, is.na(fuel_mmbtu), 0),
    net_generation_mwh = replace(net_generation_mwh, is.na(net_generation_mwh), 0),
    net_generation_mwh = replace(net_generation_mwh, net_generation_mwh < 0, 0)
  )

# ----- 3. Form 906: Non-Utility Data (1999-2000) ------------------------------

message("Ingesting Form 906 non-utility raw records (1999-2000)...")
p2000_list <- list()

for (j in 1999:2000) {
  dat <- read_excel(file.path(path_raw, "eia", paste0("F906", j, "nu.xls")), sheet = 1)
  names(dat) <- toupper(names(dat))
  con_cols <- grep("CONSUMP|COMSUMP", names(dat), value = TRUE)
  gen_cols <- grep("GENERAT", names(dat), value = TRUE)
  dat <- dat |> mutate(across(all_of(c(con_cols, gen_cols)), ~suppressWarnings(as.numeric(as.character(.x)))))
  dat$fuel_quantity <- rowSums(dat[con_cols], na.rm = TRUE)
  dat$net_gen_mwh <- rowSums(dat[gen_cols], na.rm = TRUE) / 1000
  dat$FUELTYPE[is.na(dat$FUELTYPE) | dat$FUELTYPE == ""] <- "0"
  
  df_j <- dat |>
    filter(net_gen_mwh >= 0) |>
    transmute(
      plant_id = as.integer(FACILITYID),
      state_code = as.character(FACILSTATE),
      energy_source = as.character(FUELTYPE),
      fuel_quantity = as.numeric(fuel_quantity),
      fuel_mmbtu = NA_real_,
      net_generation_mwh = as.numeric(net_gen_mwh),
      year = as.integer(j),
      survey_form = "F906_NU"
    )
  p2000_list[[as.character(j)]] <- df_j
}
p2000 <- bind_rows(p2000_list)

# ----- 4. Form 923: Utility & Non-Utility Data (2001-2014) --------------------

message("Ingesting Form 923 raw survey records (2001-2014)...")
p923_list <- list()

for (k in 2001:2014) {
  fn <- file.path(path_raw, "eia", paste0("f923_", k, ".csv"))
  dat <- read_csv(fn, col_types = cols(), show_col_types = FALSE)
  cnames <- names(dat)
  
  df_k <- tibble(
    plant_id = as.integer(dat[[cnames[1]]]),
    state_code = as.character(dat[[cnames[7]]]),
    energy_source = as.character(dat[[cnames[15]]]),
    fuel_quantity = NA_real_,
    fuel_mmbtu = as.numeric(gsub(",", "", dat[[cnames[95]]])),
    net_generation_mwh = as.numeric(gsub(",", "", dat[[cnames[96]]])),
    year = as.integer(k),
    survey_form = "F923"
  ) |>
    filter(!is.na(fuel_mmbtu), fuel_mmbtu >= 0, !is.na(net_generation_mwh), net_generation_mwh >= 0)
    
  p923_list[[as.character(k)]] <- df_k
}
p923 <- bind_rows(p923_list)

# ----- 5. Bind and Insert into SQLite -----------------------------------------

all_surveys <- bind_rows(p923, p2000, p1998, p759) |>
  mutate(
    state_code = if_else(is.na(state_code) | state_code == "" | !state_code %in% all_states, NA_character_, state_code)
  )

dbAppendTable(db, "eia_survey_gen", as.data.frame(all_surveys))
message("Inserted ", nrow(all_surveys), " rows into eia_survey_gen")

message("EIA raw survey ingestion completed successfully.")
