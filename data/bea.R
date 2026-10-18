# ==============================================================================
# Ingestion: Bureau of Economic Analysis (BEA)
# Reads raw BEA datasets from raw/bea/ and populates SQLite tables:
#   - bea_gsp: Gross State Product (1963–2014, SIC & NAICS)
#   - bea_gsi: Gross State Personal Income (1929–2014)
#   - bea_pop: State Population Estimates (1929–2014)
# ==============================================================================

source("config.R")
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(DBI)


# ----- gross-state-product ----------------------------------------------------

naics_file <- "raw/bea/gsp_naics_all.csv"
sic_file <- "raw/bea/gsp_sic_all.csv"

if (!file.exists(naics_file) || !file.exists(sic_file)) {
  stop("Missing raw GSP files in raw/bea/")
}

# 1. NAICS (1997–2014)
naics_raw <- read_csv(
  naics_file,
  col_types = cols(.default = "c"),
  show_col_types = FALSE
) |>
  filter(!is.na(GeoFIPS) & !is.na(ComponentId) & !is.na(IndustryId)) |>
  mutate(fips = as.integer(str_sub(GeoFIPS, 1, 2))) |>
  filter(!is.na(fips) & fips >= 1 & fips <= 56)

year_cols_naics <- names(naics_raw)[grepl("^[0-9]{4}$", names(naics_raw))]

naics_long <- naics_raw |>
  pivot_longer(
    cols = all_of(year_cols_naics),
    names_to = "year_str",
    values_to = "val_str"
  ) |>
  mutate(
    geo_fips = GeoFIPS,
    geo_name = GeoName,
    region = as.integer(Region),
    component_id = as.integer(ComponentId),
    component_name = ComponentName,
    industry_id = as.integer(IndustryId),
    industry_classification = IndustryClassification,
    description = Description,
    year = as.integer(year_str),
    value = as.numeric(val_str),
    units = "Millions of current dollars"
  ) |>
  select(
    geo_fips, geo_name, region, component_id, component_name,
    industry_id, industry_classification, description,
    fips, year, value, units
  )

# 2. SIC (1963–1996; exclude 1997 overlap)
sic_raw <- read_csv(
  sic_file,
  col_types = cols(.default = "c"),
  show_col_types = FALSE
) |>
  filter(!is.na(FIPS) & !is.na(ComponentId) & !is.na(IndustryId)) |>
  mutate(fips = as.integer(FIPS)) |>
  filter(!is.na(fips) & fips >= 1 & fips <= 56)

year_cols_sic <- setdiff(
  names(sic_raw)[grepl("^gsp[0-9]{4}$", names(sic_raw))],
  "gsp1997"
)

sic_long <- sic_raw |>
  pivot_longer(
    cols = all_of(year_cols_sic),
    names_to = "year_str",
    values_to = "val_str"
  ) |>
  mutate(
    geo_fips = sprintf("%05d", fips * 1000),
    geo_name = GeoName,
    region = as.integer(Region),
    component_id = as.integer(ComponentId),
    component_name = ComponentName,
    industry_id = as.integer(IndustryId),
    industry_classification = "SIC",
    description = IndustryName,
    year = as.integer(gsub("gsp", "", year_str)),
    value = as.numeric(val_str),
    units = "Millions of current dollars"
  ) |>
  select(
    geo_fips, geo_name, region, component_id, component_name,
    industry_id, industry_classification, description,
    fips, year, value, units
  )

bea_gsp <- bind_rows(sic_long, naics_long) |>
  filter(!is.na(year) & !is.na(fips) & !is.na(industry_id) & !is.na(component_id)) |>
  distinct(year, fips, industry_id, component_id, .keep_all = TRUE)


# ----- personal-income-and-population -----------------------------------------

sa1_file <- "raw/bea/SA1_1929_2014.csv"

if (!file.exists(sa1_file)) {
  stop("Missing raw SA1 file in raw/bea/: ", sa1_file)
}

sa1_raw <- read_csv(
  sa1_file,
  col_types = cols(.default = "c"),
  show_col_types = FALSE
) |>
  filter(!is.na(GeoFIPS) & !is.na(LineCode)) |>
  mutate(fips = as.integer(str_sub(GeoFIPS, 1, 2))) |>
  filter(!is.na(fips) & fips >= 1 & fips <= 56)

year_cols_sa1 <- names(sa1_raw)[grepl("^[0-9]{4}$", names(sa1_raw))]

sa1_long <- sa1_raw |>
  pivot_longer(
    cols = all_of(year_cols_sa1),
    names_to = "year_str",
    values_to = "val_str"
  ) |>
  mutate(
    geo_fips = GeoFIPS,
    geo_name = GeoName,
    region = as.integer(Region),
    series = Table,
    line_code = as.integer(LineCode),
    industry_classification = IndustryClassification,
    description = Description,
    year = as.integer(year_str),
    value = as.numeric(val_str)
  )

# Personal income (LineCode 1)
bea_gsi <- sa1_long |>
  filter(line_code == 1) |>
  mutate(
    description = "Personal income",
    units = "thousands of dollars"
  ) |>
  select(
    geo_fips, geo_name, region, series, line_code,
    industry_classification, description, fips, year, value, units
  ) |>
  distinct(year, fips, .keep_all = TRUE)

# Population (LineCode 2)
bea_pop <- sa1_long |>
  filter(line_code == 2) |>
  mutate(
    description = "Population",
    units = "persons"
  ) |>
  select(
    geo_fips, geo_name, region, series, line_code,
    industry_classification, description, fips, year, value, units
  ) |>
  distinct(year, fips, .keep_all = TRUE)


# ----- insert-to-database -----------------------------------------------------

db <- get_db()

execute_sql_file(db, file.path(path_sql, "tables", "bea-gsp.sql"))
dbWriteTable(db, "bea_gsp", bea_gsp, append = TRUE)

execute_sql_file(db, file.path(path_sql, "tables", "bea-gsi.sql"))
dbWriteTable(db, "bea_gsi", bea_gsi, append = TRUE)

execute_sql_file(db, file.path(path_sql, "tables", "bea-pop.sql"))
dbWriteTable(db, "bea_pop", bea_pop, append = TRUE)

message("Successfully populated bea_gsp: ", dbGetQuery(db, "SELECT count(*) FROM bea_gsp")[[1]], " rows.")
message("Successfully populated bea_gsi: ", dbGetQuery(db, "SELECT count(*) FROM bea_gsi")[[1]], " rows.")
message("Successfully populated bea_pop: ", dbGetQuery(db, "SELECT count(*) FROM bea_pop")[[1]], " rows.")
