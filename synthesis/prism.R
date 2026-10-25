# ==============================================================================
# PRISM Climate Group: Congressional District Synthesis & Analysis
# Sources:
#   - prism_district (base monthly climate observations in congress.sqlite)
#   - noaa_degree_days (state degree days benchmark in congress.sqlite)
# Target: prism_district_annual_vw view in congress.sqlite
# ==============================================================================

source("config.R")
library(DBI)
library(dplyr)

# ----- database connection ----------------------------------------------------

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

message("Synthesizing congressional district annual climate indicators...")

# ----- execute DDL for analytical views ---------------------------------------

execute_sql_file(db, file.path(path_sql, "views", "prism-district-annual-vw.sql"))

# ----- verification and validation --------------------------------------------

n_annual <- dbGetQuery(db, "SELECT count(*) AS n FROM prism_district_annual_vw")$n
message("Verification: ", n_annual, " district-year observations in prism_district_annual_vw.")

# Check sample metrics
sample_data <- dbGetQuery(db, "
  SELECT state_code, district_number, year, total_precipitation_mm, mean_temp_f, cool_deg_days, heat_deg_days
  FROM prism_district_annual_vw
  WHERE year = 2010 AND state_code IN ('CA', 'TX', 'NY', 'FL', 'IL')
  ORDER BY state_code, district_number
  LIMIT 5
")

message("Sample 2010 district climate indicators:")
print(sample_data)

stopifnot(n_annual == 9073)
message("PRISM climate synthesis and analytical view generation completed successfully.")
