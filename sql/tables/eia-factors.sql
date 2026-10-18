-- ============================================================================
-- eia_fuel_factors: EIA fuel type carbon and greenhouse gas conversion factors
-- Sources: raw/eia/eia_fuel_type_carbon.csv, raw/eia/eia_fuel_type_carbon_old.csv
-- ============================================================================

DROP TABLE IF EXISTS eia_fuel_factors;
DROP TABLE IF EXISTS eia_fuel_factors_old;

CREATE TABLE eia_fuel_factors (
    esource TEXT PRIMARY KEY,
    description TEXT,
    physical_units TEXT,
    kg_co2_per_mmbtu REAL,
    g_ch4_per_mmbtu REAL,
    g_n2o_per_mmbtu REAL,
    notes TEXT
);

CREATE TABLE eia_fuel_factors_old (
    esource TEXT,
    esource_other TEXT,
    description TEXT,
    physical_units TEXT,
    mmbtu REAL,
    kg_co2_per_mmbtu REAL,
    g_ch4_per_mmbtu REAL,
    g_n2o_per_mmbtu REAL,
    notes TEXT
);
