# ==============================================================================
# EIA: State Energy Production & Real Prices Panel Synthesis
# Queries: eia_seds, eia_prices, bls_cpi, states from congress.sqlite
# Target: eia_states table in congress.sqlite
# ==============================================================================

source("config.R")
library(DBI)
library(dplyr)
library(tidyr)

# ----- database connection ----------------------------------------------------

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

states <- dbGetQuery(db, "SELECT state_code FROM states")$state_code
cpi_df <- dbGetQuery(db, "SELECT year, avg_cpi FROM bls_cpi")

# ----- 1. Synthesize SEDS energy production by state and fuel ------------------

message("Querying eia_seds from database to synthesize state energy production...")
seds <- dbGetQuery(db, "
  SELECT state_code, year, msn, data
  FROM eia_seds
  WHERE year >= 1980
    AND state_code NOT IN ('X3', 'X5')
")

prod <- seds |>
  mutate(
    type = substr(msn, 5, 5),
    esource = substr(msn, 1, 2),
    sector_activity = substr(msn, 3, 4)
  ) |>
  filter(
    sector_activity %in% c("PR", "MP"),
    type == "B" | (type == "B" & esource == "RO"),
    esource %in% c("CL", "EN", "NG", "PA", "RO")
  ) |>
  group_by(state_code, year = as.integer(year), esource) |>
  summarise(total = sum(data, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = esource, values_from = total, values_fill = 0) |>
  transmute(
    state_code,
    year,
    coal_production_btu = CL,
    natgas_production_btu = NG,
    petroleum_production_btu = PA,
    renewable_production_btu = RO
  )

# ----- 2. Synthesize real energy prices adjusted for inflation ----------------

message("Querying eia_prices from database to synthesize real energy prices...")
pr_raw <- dbGetQuery(db, "
  SELECT state_code, year, msn, price
  FROM eia_prices
")

pr_long <- pr_raw |>
  mutate(
    year = as.integer(year),
    esource = substr(msn, 1, 2)
  ) |>
  group_by(state_code, year, esource) |>
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop") |>
  filter(esource %in% c("CL", "ES", "NG", "PA", "RO")) |>
  inner_join(cpi_df, by = "year") |>
  mutate(real_price = avg_price / (avg_cpi / 100)) |>
  select(state_code, year, esource, real_price) |>
  pivot_wider(names_from = esource, values_from = real_price) |>
  transmute(
    state_code,
    year,
    coal_real_price = CL,
    electricity_real_price = ES,
    natgas_real_price = NG,
    petroleum_real_price = PA
  )

# ----- 3. Merge panel into eia_states -----------------------------------------

df_states <- full_join(prod, pr_long, by = c("state_code", "year")) |>
  filter(state_code %in% states) |>
  arrange(state_code, year)

execute_sql_file(db, file.path(path_sql, "eia-states.sql"))
dbAppendTable(db, "eia_states", as.data.frame(df_states))
message("Inserted ", nrow(df_states), " rows into eia_states")

# ----- verification -----------------------------------------------------------

n_states <- dbGetQuery(db, "SELECT count(*) AS n FROM eia_states")$n
message("Verification: ", n_states, " state-year observations in eia_states")
stopifnot(n_states == 2244)
message("EIA state energy panel synthesis completed successfully.")
