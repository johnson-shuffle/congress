# ==============================================================================
# Synthesis: House Master Annual Panel & Analytical Roll-Call View
# Synthesizes 12 SQLite domains into panel_house_annual (10,191 member-years)
# and establishes analytical view panel_house_vw (180,500 vote observations).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(DBI)
  library(RSQLite)
})

source("config.R")

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

message("Extracting base legislator crosswalk for House...")

# 1. Base House Members across 103rd–114th Congresses
# Excludes territorial non-voting delegates and late-2016 special election entrants
non_voting <- c("AS", "GU", "MP", "PR", "VI", "DC")
late_2016_special <- c(21124L, 21564L, 21565L, 21566L)

cw_hou <- dbGetQuery(
  db,
  "SELECT 
     congress,
     chamber,
     icpsr_id,
     state_code,
     district_number,
     bioguide_id,
     govtrack_id,
     opensecrets_id,
     votesmart_id,
     party,
     first_name,
     preferred_name,
     last_name,
     suffix,
     gender,
     birthday,
     occupancy,
     means,
     start_date,
     end_date
   FROM crosswalk_members
   WHERE chamber = 'House' AND icpsr_id < 99909"
) |>
  as_tibble() |>
  filter(!state_code %in% non_voting) |>
  filter(!icpsr_id %in% late_2016_special)

# Expand each Congress (103–114) into its odd year (1993, 1995...) and even year (1994, 1996...)
odd_years <- cw_hou |> mutate(year = as.integer(1993L + (congress - 103L) * 2L))
even_years <- cw_hou |> mutate(year = as.integer(1994L + (congress - 103L) * 2L))

hou_annual <- bind_rows(odd_years, even_years) |>
  filter(year != 2016) |>
  arrange(congress, year, state_code, district_number)

# Independent party overrides matching legacy
hou_annual <- hou_annual |>
  mutate(
    party = case_when(
      icpsr_id == 29147L ~ "Democrat",   # Bernie Sanders
      icpsr_id == 94240L ~ "Democrat",   # Jim Jeffords
      icpsr_id == 99767L ~ "Republican", # Virgil Goode
      TRUE ~ party
    ),
    party_d = if_else(party %in% c("Democrat", "D"), 1L, 0L),
    party_r = if_else(party %in% c("Republican", "R"), 1L, 0L),
    gender_female = if_else(gender == "Female", 1L, 0L),
    gender_male   = if_else(gender == "Male", 1L, 0L)
  )

# 2. State Lookup
message("Joining states lookup...")
states_df <- dbGetQuery(
  db,
  "SELECT 
     state_code,
     fips,
     state_name,
     census_region,
     census_division,
     adage_region,
     recs_domain,
     recs_division,
     nerc_region,
     interconnect,
     icpsr_code AS icpsr_state
   FROM states"
) |>
  as_tibble()

hou_annual <- hou_annual |>
  left_join(states_df, by = "state_code")

# Division indicator dummies
for (d in 1:9) {
  col_name <- paste0("census_division", d)
  hou_annual[[col_name]] <- if_else(hou_annual$census_division == as.character(d), 1L, 0L)
}

# 3. BEA & BLS Economic Data
message("Joining BEA income, GSP, BLS unemployment, and CPI...")
cpi_df <- dbGetQuery(db, "SELECT year, avg_cpi FROM bls_cpi") |> as_tibble()
cpi_2010 <- cpi_df$avg_cpi[cpi_df$year == 2010]
cpi_df <- cpi_df |>
  mutate(cpi_factor = cpi_2010 / avg_cpi)

# Unemployment
unemp_df <- dbGetQuery(db, "SELECT year, fips, avg_unempl AS avg_unemp FROM bls_unempl") |> as_tibble()

# BEA GSI, Pop, GSP
gsi_df <- dbGetQuery(db, "SELECT year, fips, value * 1000.0 AS gsi FROM bea_gsi") |> as_tibble()
pop_bea_df <- dbGetQuery(db, "SELECT year, fips, value AS pop_bea FROM bea_pop") |> as_tibble()
gsp_df <- dbGetQuery(db, "SELECT year, fips, value * 1e6 AS gsp FROM bea_gsp WHERE industry_id = 1 AND component_id = 200") |>
  as_tibble() |>
  distinct(year, fips, .keep_all = TRUE)

bea_all <- gsi_df |>
  inner_join(pop_bea_df, by = c("year", "fips")) |>
  left_join(gsp_df, by = c("year", "fips"))

# Carry forward 2014 values for 2015 matching legacy
bea_2015 <- bea_all |>
  filter(year == 2014) |>
  mutate(year = 2015)

bea_all <- bind_rows(bea_all, bea_2015) |>
  left_join(cpi_df |> select(year, avg_cpi), by = "year") |>
  mutate(
    rgsp = gsp / (avg_cpi / 100),
    rgsi = gsi / (avg_cpi / 100)
  ) |>
  select(year, fips, gsi, pop_bea, gsp, rgsp, rgsi)

hou_annual <- hou_annual |>
  left_join(bea_all, by = c("year", "fips")) |>
  left_join(unemp_df, by = c("year", "fips"))

# 4. Census Demographics
message("Joining Census district demographics...")
census_df <- dbGetQuery(
  db,
  "SELECT 
     year,
     fips,
     district_number,
     pop_total,
     pct_white AS white,
     pct_black AS black,
     pct_indian AS indian,
     pct_other AS other,
     pct_hispanic AS hispanic,
     pct_asian_island AS asian_island,
     pct_pop_65 AS pop_65,
     pct_pop_25 AS pop_25,
     pct_less_than_9th AS less_than_9th,
     pct_more_than_9th AS more_than_9th,
     pct_highschool AS highschool,
     pct_somecollege AS somecollege,
     pct_associate AS associate,
     pct_bachelor AS bachelor,
     pct_graduate AS graduate,
     pct_empl_res AS empl_res,
     pct_empl_con AS empl_con,
     pct_empl_man AS empl_man,
     pct_empl_wst AS empl_wst,
     pct_empl_rt AS empl_rt,
     pct_empl_pub AS empl_pub,
     empl_total,
     med_inc_house,
     med_inc_fam,
     pct_hm_electric AS hm_electric,
     hm_total
   FROM census_district"
) |>
  as_tibble()

# Deflate median income by base year CPI factor matching redistricting intervals
cpi_103 <- cpi_df$avg_cpi[cpi_df$year == 1993] / 100
cpi_106 <- cpi_df$avg_cpi[cpi_df$year == 1999] / 100
cpi_109 <- cpi_df$avg_cpi[cpi_df$year == 2006] / 100
cpi_111 <- cpi_df$avg_cpi[cpi_df$year == 2009] / 100
cpi_113 <- cpi_df$avg_cpi[cpi_df$year == 2013] / 100

census_df <- census_df |>
  mutate(
    med_inc_house = case_when(
      year %in% 1993:1998 ~ med_inc_house * cpi_103,
      year %in% 1999:2002 ~ med_inc_house * cpi_106,
      year %in% 2003:2008 ~ med_inc_house * cpi_109,
      year %in% 2009:2012 ~ med_inc_house * cpi_111,
      year %in% 2013:2015 ~ med_inc_house * cpi_113,
      TRUE ~ med_inc_house
    )
  )

hou_annual <- hou_annual |>
  left_join(census_df, by = c("year", "fips", "district_number"))

# 5. OpenSecrets / CRP Campaign Finance
message("Joining OpenSecrets contributions...")
crp_df <- dbGetQuery(
  db,
  "SELECT 
     osid,
     congress,
     agribusiness,
     energy,
     transport,
     environment,
     d_leader_pacs,
     r_leader_pacs,
     d_cand_cmte,
     r_cand_cmte,
     d_party_cmte,
     r_party_cmte,
     d_joint_cand_cmte,
     r_joint_cand_cmte,
     d_cmte,
     r_cmte
   FROM crp_contributions"
) |>
  as_tibble()

hou_annual <- hou_annual |>
  left_join(crp_df, by = c("opensecrets_id" = "osid", "congress")) |>
  mutate(
    across(
      c(agribusiness, energy, transport, environment,
        d_leader_pacs, r_leader_pacs, d_cand_cmte, r_cand_cmte,
        d_party_cmte, r_party_cmte, d_joint_cand_cmte, r_joint_cand_cmte,
        d_cmte, r_cmte),
      ~ coalesce(., 0.0)
    )
  )

# 6. EIA Energy Production, District Emissions & Real Prices
message("Joining EIA energy production, emissions, and prices...")
eia_states_df <- dbGetQuery(
  db,
  "SELECT 
     state_code,
     year,
     coal_production_btu AS cl_total,
     natgas_production_btu AS ng_total,
     petroleum_production_btu AS pa_total,
     renewable_production_btu AS ro_total,
     coal_real_price AS cl_real_price,
     electricity_real_price AS es_real_price,
     natgas_real_price AS ng_real_price,
     petroleum_real_price AS pa_real_price
   FROM eia_states"
) |>
  as_tibble()

# Carry forward 2013 values for 2014 and 2015
eia_states_14 <- eia_states_df |> filter(year == 2013) |> mutate(year = 2014)
eia_states_15 <- eia_states_df |> filter(year == 2013) |> mutate(year = 2015)
eia_states_df <- bind_rows(eia_states_df, eia_states_14, eia_states_15)

# District emissions
eia_emiss_df <- dbGetQuery(
  db,
  "SELECT 
     state_code,
     district_number,
     year,
     net_generation_mwh AS electric,
     co2_metric_tons AS co2_prod,
     co2_metric_tons AS co2_cons
   FROM eia_district_emissions"
) |>
  as_tibble()

# District emissions (has 1993-2014; carry forward 2014 for 2015)
eia_emiss_15 <- eia_emiss_df |> filter(year == 2014) |> mutate(year = 2015)
eia_emiss_df <- bind_rows(eia_emiss_df, eia_emiss_15)

hou_annual <- hou_annual |>
  left_join(eia_states_df, by = c("state_code", "year")) |>
  left_join(eia_emiss_df, by = c("state_code", "district_number", "year"))

# 7. Electoral Margins, Retirements & Turnovers
message("Joining electoral margins and retirements...")
margins_df <- dbGetQuery(
  db,
  "SELECT congress, govtrack_id, margin AS general_margin
   FROM election_margins_vw
   WHERE chamber = 'House'"
) |>
  as_tibble() |>
  distinct(congress, govtrack_id, .keep_all = TRUE)

retire_df <- dbGetQuery(
  db,
  "SELECT congress, govtrack_id, retire
   FROM retirements
   WHERE chamber = 'House'"
) |>
  as_tibble() |>
  distinct(congress, govtrack_id, .keep_all = TRUE)

hou_annual <- hou_annual |>
  left_join(margins_df, by = c("congress", "govtrack_id")) |>
  left_join(retire_df, by = c("congress", "govtrack_id")) |>
  mutate(
    retire = coalesce(retire, 0L),
    election_year = 1L
  )

# 8. LCV & DW-NOMINATE Ideology
message("Joining LCV scores and DW-NOMINATE coordinates...")
lcv_scores_df <- dbGetQuery(
  db,
  "SELECT congress, icpsr_id, lcv_score
   FROM lcv_congress_scores
   WHERE chamber = 'House'"
) |>
  as_tibble() |>
  distinct(congress, icpsr_id, .keep_all = TRUE)

nom_df <- dbGetQuery(
  db,
  "SELECT congress, icpsr_id, nominate_dim1, nominate_dim2
   FROM voteview_members
   WHERE chamber = 'House'"
) |>
  as_tibble() |>
  distinct(congress, icpsr_id, .keep_all = TRUE)

hou_annual <- hou_annual |>
  left_join(lcv_scores_df, by = c("congress", "icpsr_id")) |>
  left_join(nom_df, by = c("congress", "icpsr_id")) |>
  mutate(
    se1 = NA_real_,
    se2 = NA_real_
  )

# 9. Climate Degree Days (PRISM)
message("Joining PRISM degree days...")
prism_df <- dbGetQuery(
  db,
  "SELECT state_code, district_number, year, cool_deg_days, heat_deg_days
   FROM prism_district_annual_vw"
) |>
  as_tibble()

# 2014 & 2015 mean fill matching legacy
prism_14 <- prism_df |>
  group_by(state_code, district_number) |>
  summarise(
    cool_deg_days = mean(cool_deg_days, na.rm = TRUE),
    heat_deg_days = mean(heat_deg_days, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(year = 2014)

prism_15 <- prism_14 |> mutate(year = 2015)
prism_df <- bind_rows(prism_df, prism_14, prism_15)

hou_annual <- hou_annual |>
  left_join(prism_df, by = c("state_code", "district_number", "year"))

# 10. Model Interactions & Per-Capita Variables
message("Computing model interactions and per-capita variables...")
hou_annual <- hou_annual |>
  mutate(
    appoint = if_else(means == 5L, 1L, 0L),
    appoint = coalesce(appoint, 0L),
    r_appoint = party_r * appoint,
    d_appoint = party_d * appoint,
    r_elect = party_r * election_year,
    d_elect = party_d * election_year,
    r_retire = party_r * retire,
    d_retire = party_d * retire,
    general_margin = if_else(appoint == 1L, 0.0, general_margin),
    general_margin = coalesce(general_margin, 0.0),
    r_margin = party_r * (1L - appoint) * general_margin,
    d_margin = party_d * (1L - appoint) * general_margin,
    r_female = party_r * gender_female,
    d_female = party_d * gender_female,
    age = year - as.integer(substr(birthday, 1, 4)),
    r_age = party_r * age,
    d_age = party_d * age,
    cl_total_pc = cl_total / pop_total,
    ng_total_pc = ng_total / pop_total,
    pa_total_pc = pa_total / pop_total,
    ro_total_pc = ro_total / pop_total,
    carbon_prod_pc = co2_prod / pop_total * 1e6 * (12 / 44),
    carbon_cons_pc = co2_cons / pop_total * 1e6 * (12 / 44)
  )

# Select standardized columns
panel_house_annual <- hou_annual |>
  select(
    state_code, district_number, year, icpsr_id, congress, govtrack_id, opensecrets_id,
    fips, votesmart_id, first_name, preferred_name, last_name, suffix,
    gender_female, gender_male, birthday, party_d, party_r,
    occupancy, means, start_date, end_date, state_name, census_region,
    census_division1, census_division2, census_division3, census_division4,
    census_division5, census_division6, census_division7, census_division8, census_division9,
    icpsr_state, adage_region, recs_domain, recs_division, nerc_region, interconnect,
    gsi, pop_bea, gsp, rgsp, rgsi, avg_unemp,
    pop_total, white, black, indian, other, hispanic,
    pop_65, pop_25, less_than_9th, more_than_9th, highschool, somecollege,
    associate, bachelor, graduate,
    empl_res, empl_con, empl_man, empl_wst, empl_rt, empl_pub,
    med_inc_house, med_inc_fam, empl_total, hm_electric, hm_total, asian_island,
    agribusiness, d_cand_cmte, d_joint_cand_cmte, d_leader_pacs, d_party_cmte,
    energy, environment, r_cand_cmte, r_joint_cand_cmte, r_leader_pacs, r_party_cmte,
    transport, r_cmte, d_cmte,
    cl_total, ng_total, pa_total, ro_total, electric, co2_prod, co2_cons,
    cl_real_price, es_real_price, ng_real_price, pa_real_price,
    general_margin, retire, election_year,
    lcv_score, nominate_dim1, nominate_dim2, se1, se2,
    cool_deg_days, heat_deg_days,
    appoint, r_appoint, d_appoint, r_elect, d_elect, r_retire, d_retire,
    r_margin, d_margin, r_female, d_female, r_age, d_age, age,
    carbon_cons_pc, carbon_prod_pc, ro_total_pc, pa_total_pc, ng_total_pc, cl_total_pc
  )

# Write physical table
message("Writing panel_house_annual to database...")
execute_sql_file(db, file.path(path_sql, "tables", "panel-house-annual.sql"))
dbWriteTable(db, "panel_house_annual", panel_house_annual, append = TRUE)

message(sprintf(
  "Successfully synthesized panel_house_annual: %d rows x %d cols.",
  nrow(panel_house_annual), ncol(panel_house_annual)
))

# Initialize analytical roll call view
message("Initializing panel_house_vw analytical roll-call view...")
execute_sql_file(db, file.path(path_sql, "views", "panel-house-vw.sql"))

n_vw <- dbGetQuery(db, "SELECT count(*) AS n FROM panel_house_vw")$n
message(sprintf("Successfully initialized panel_house_vw: %d roll-call observations.", n_vw))
