# ==============================================================================
# EIA: Congressional District Generation & Emissions Synthesis
# Sources:
#   - eia_generation (plant-level net generation and emissions in congress.sqlite)
#   - eia_plants_district (spatial plant-district crosswalk in congress.sqlite)
# Target: eia_district_emissions table in congress.sqlite
# ==============================================================================

source("config.R")
library(DBI)
library(dplyr)

# ----- database connection ----------------------------------------------------

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

message("Synthesizing congressional district annual electricity generation & emissions...")

# ----- execute DDL schema -----------------------------------------------------

execute_sql_file(db, file.path(path_sql, "tables", "eia-district-emissions.sql"))

# ----- aggregate plant emissions into congressional district totals -----------

query <- "
INSERT INTO eia_district_emissions (
    year,
    congress,
    state_code,
    district_number,
    net_generation_mwh,
    fuel_mmbtu,
    co2_metric_tons,
    plant_count
)
SELECT 
    g.year,
    (((g.year - 1993) / 2) + 103) AS congress,
    d.state_code,
    d.district_number,
    ROUND(SUM(g.net_generation_mwh), 2) AS net_generation_mwh,
    ROUND(SUM(g.fuel_mmbtu), 2) AS fuel_mmbtu,
    ROUND(SUM(g.co2_metric_tons), 4) AS co2_metric_tons,
    COUNT(DISTINCT g.plant_id) AS plant_count
FROM eia_generation g
JOIN eia_plants_district d 
  ON g.plant_id = d.plant_code 
 AND (((g.year - 1993) / 2) + 103) = d.congress
WHERE g.year >= 1993 AND g.year <= 2016
GROUP BY g.year, congress, d.state_code, d.district_number
ORDER BY g.year, d.state_code, d.district_number;
"

n_inserted <- dbExecute(db, query)
message("Inserted ", n_inserted, " district-year observations into eia_district_emissions.")

# ----- verification -----------------------------------------------------------

n_rows <- dbGetQuery(db, "SELECT count(*) AS n FROM eia_district_emissions")$n
stopifnot(n_rows == 9165)

message("Verification: ", n_rows, " records successfully verified in eia_district_emissions.")
message("EIA district emissions synthesis completed successfully.")
