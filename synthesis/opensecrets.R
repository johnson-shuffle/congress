# ==============================================================================
# Synthesis: OpenSecrets Campaign Finance Contributions by Sector & Committee
# Queries: crp_indivs, crp_pacs, crp_categories, bls_cpi, crosswalk_members
# Target: crp_contributions table in congress.sqlite
# Keyed by (osid, congress) spanning 101st–114th Congresses (1990–2016).
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

# ----- 1. extract CPI series for 2010 base year deflation ---------------------

message("Loading BLS CPI index for constant 2010 dollar deflation...")
cpi_df <- dbGetQuery(db, "SELECT year, avg_cpi FROM bls_cpi")
cpi_2010 <- cpi_df$avg_cpi[cpi_df$year == 2010]

if (length(cpi_2010) == 0 || is.na(cpi_2010)) {
  stop("Base year 2010 CPI not found in bls_cpi table.")
}

cpi_df <- cpi_df |>
  mutate(cpi_factor = cpi_2010 / avg_cpi)

# ----- 2. query aggregated PAC contributions ----------------------------------

message("Querying and aggregating PAC contributions by candidate and category...")
pacs_q <- "
  SELECT 
    p.cycle,
    CAST(substr(p.date, 1, 4) AS INTEGER) AS year,
    p.osid,
    c.sector_short,
    c.cat_group_alt,
    SUM(p.amount) AS amount
  FROM crp_pacs p
  JOIN crp_categories c ON p.cat_code = c.cat_code
  WHERE p.direct_indirect = 'D'
    AND substr(p.cat_code, 1, 2) != 'Z9'
    AND CAST(substr(p.date, 1, 4) AS INTEGER) BETWEEN (p.cycle - 1) AND p.cycle
    AND p.osid IN (SELECT DISTINCT opensecrets_id FROM crosswalk_members WHERE opensecrets_id IS NOT NULL AND opensecrets_id != '')
    AND (c.sector_short IN ('Agribusiness', 'Energy/Nat Resource', 'Transportation')
         OR c.cat_group IN ('Q03', 'Q11', 'Q16', 'Z02', 'Z04'))
  GROUP BY p.cycle, year, p.osid, c.sector_short, c.cat_group_alt
"
pacs_agg <- dbGetQuery(db, pacs_q) |>
  as_tibble() |>
  mutate(source_type = "pac")

# ----- 3. query aggregated individual contributions ---------------------------

message("Querying and aggregating Individual contributions by candidate and category...")
indivs_q <- "
  SELECT 
    i.cycle,
    CAST(substr(i.date, 1, 4) AS INTEGER) AS year,
    i.recip_id AS osid,
    c.sector_short,
    c.cat_group_alt,
    SUM(i.amount) AS amount
  FROM crp_indivs i
  JOIN crp_categories c ON i.cat_code = c.cat_code
  WHERE i.trans_type IN ('10', '11', '15', '15E', '15J', '22Y')
    AND substr(i.cat_code, 1, 2) != 'Z9'
    AND CAST(substr(i.date, 1, 4) AS INTEGER) BETWEEN (i.cycle - 1) AND i.cycle
    AND i.recip_id IN (SELECT DISTINCT opensecrets_id FROM crosswalk_members WHERE opensecrets_id IS NOT NULL AND opensecrets_id != '')
    AND (c.sector_short IN ('Agribusiness', 'Energy/Nat Resource', 'Transportation')
         OR c.cat_group IN ('Q03', 'Q11', 'Q16', 'Z02', 'Z04'))
  GROUP BY i.cycle, year, i.recip_id, c.sector_short, c.cat_group_alt
"
indivs_agg <- dbGetQuery(db, indivs_q) |>
  as_tibble() |>
  mutate(source_type = "indiv")

# ----- 4. harmonize categories and apply inflation factors --------------------

message("Harmonizing sector buckets and computing constant 2010 real dollars...")
all_contribs <- bind_rows(pacs_agg, indivs_agg) |>
  left_join(cpi_df |> select(year, cpi_factor), by = "year") |>
  mutate(
    cpi_factor = coalesce(cpi_factor, 1.0),
    amount_real = amount * cpi_factor,
    amount_nom = amount,
    congress = as.integer(cycle / 2L - 894L)
  )

# Filter out unassigned non-party category suffixes (consistent with legacy panel_house.R)
filtered_contribs <- all_contribs |>
  filter(!cat_group_alt %in% c("Q03", "Q16", "Z02", "Z04")) |>
  mutate(
    sector_bucket = case_when(
      cat_group_alt == "Q03D" ~ "d_leader_pacs",
      cat_group_alt == "Q03R" ~ "r_leader_pacs",
      cat_group_alt == "Q11"  ~ "environment",
      cat_group_alt == "Q16D" ~ "d_cand_cmte",
      cat_group_alt == "Q16R" ~ "r_cand_cmte",
      cat_group_alt == "Z02D" ~ "d_party_cmte",
      cat_group_alt == "Z02R" ~ "r_party_cmte",
      cat_group_alt == "Z04D" ~ "d_joint_cand_cmte",
      cat_group_alt == "Z04R" ~ "r_joint_cand_cmte",
      sector_short == "Agribusiness" ~ "agribusiness",
      sector_short == "Energy/Nat Resource" ~ "energy",
      sector_short == "Transportation" ~ "transport",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(sector_bucket))

# ----- 5. aggregate by candidate, congress, and sector bucket -----------------

message("Synthesizing wide contribution panel...")
by_bucket <- filtered_contribs |>
  group_by(osid, congress, cycle, sector_bucket) |>
  summarise(
    real_sum = round(sum(amount_real, na.rm = TRUE), 2),
    nom_sum = round(sum(amount_nom, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Ensure all 12 buckets are present in columns
all_buckets <- c(
  "agribusiness", "energy", "transport", "environment",
  "d_leader_pacs", "r_leader_pacs", "d_cand_cmte", "r_cand_cmte",
  "d_party_cmte", "r_party_cmte", "d_joint_cand_cmte", "r_joint_cand_cmte"
)

wide_real <- by_bucket |>
  select(osid, congress, cycle, sector_bucket, real_sum) |>
  pivot_wider(names_from = sector_bucket, values_from = real_sum, values_fill = 0.0)

for (col in all_buckets) {
  if (!col %in% names(wide_real)) wide_real[[col]] <- 0.0
}

wide_nom <- by_bucket |>
  select(osid, congress, cycle, sector_bucket, nom_sum) |>
  pivot_wider(
    names_from = sector_bucket,
    values_from = nom_sum,
    values_fill = 0.0,
    names_glue = "{sector_bucket}_nom"
  )

for (col in paste0(all_buckets, "_nom")) {
  if (!col %in% names(wide_nom)) wide_nom[[col]] <- 0.0
}

# Aggregate totals by donor source type (PAC vs Individual)
totals_by_type <- filtered_contribs |>
  group_by(osid, congress, cycle, source_type) |>
  summarise(
    real_sum = round(sum(amount_real, na.rm = TRUE), 2),
    nom_sum = round(sum(amount_nom, na.rm = TRUE), 2),
    .groups = "drop"
  )

wide_type_real <- totals_by_type |>
  select(osid, congress, cycle, source_type, real_sum) |>
  pivot_wider(names_from = source_type, values_from = real_sum, values_fill = 0.0)

if (!"pac" %in% names(wide_type_real)) wide_type_real$pac <- 0.0
if (!"indiv" %in% names(wide_type_real)) wide_type_real$indiv <- 0.0
wide_type_real <- wide_type_real |>
  rename(total_pac = pac, total_indiv = indiv)

wide_type_nom <- totals_by_type |>
  select(osid, congress, cycle, source_type, nom_sum) |>
  pivot_wider(names_from = source_type, values_from = nom_sum, values_fill = 0.0)

if (!"pac" %in% names(wide_type_nom)) wide_type_nom$pac <- 0.0
if (!"indiv" %in% names(wide_type_nom)) wide_type_nom$indiv <- 0.0
wide_type_nom <- wide_type_nom |>
  rename(total_pac_nom = pac, total_indiv_nom = indiv)

# Combine everything and compute composites
crp_contributions <- wide_real |>
  full_join(wide_nom, by = c("osid", "congress", "cycle")) |>
  left_join(wide_type_real, by = c("osid", "congress", "cycle")) |>
  left_join(wide_type_nom, by = c("osid", "congress", "cycle")) |>
  mutate(across(where(is.numeric), ~ coalesce(., 0.0))) |>
  mutate(
    d_cmte = round(d_cand_cmte + d_joint_cand_cmte, 2),
    r_cmte = round(r_cand_cmte + r_joint_cand_cmte, 2),
    total_contrib = round(total_pac + total_indiv, 2),
    d_cmte_nom = round(d_cand_cmte_nom + d_joint_cand_cmte_nom, 2),
    r_cmte_nom = round(r_cand_cmte_nom + r_joint_cand_cmte_nom, 2),
    total_contrib_nom = round(total_pac_nom + total_indiv_nom, 2)
  ) |>
  select(
    osid, congress, cycle,
    agribusiness, energy, transport, environment,
    d_leader_pacs, r_leader_pacs, d_cand_cmte, r_cand_cmte,
    d_party_cmte, r_party_cmte, d_joint_cand_cmte, r_joint_cand_cmte,
    d_cmte, r_cmte,
    total_pac, total_indiv, total_contrib,
    agribusiness_nom, energy_nom, transport_nom, environment_nom,
    d_leader_pacs_nom, r_leader_pacs_nom, d_cand_cmte_nom, r_cand_cmte_nom,
    d_party_cmte_nom, r_party_cmte_nom, d_joint_cand_cmte_nom, r_joint_cand_cmte_nom,
    d_cmte_nom, r_cmte_nom,
    total_pac_nom, total_indiv_nom, total_contrib_nom
  ) |>
  arrange(congress, osid)

# ----- 6. write to database ---------------------------------------------------

message("Writing crp_contributions table to congress.sqlite...")
execute_sql_file(db, file.path(path_sql, "tables", "crp-contributions.sql"))
dbWriteTable(db, "crp_contributions", crp_contributions, append = TRUE)

message(sprintf(
  "Successfully populated crp_contributions: %d candidate-congress records.",
  nrow(crp_contributions)
))
