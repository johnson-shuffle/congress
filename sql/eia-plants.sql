-- ============================================================================
-- eia_plants: EIA Form 860 Power Plants with GPS coordinates and utility metadata
-- Source: raw/eia/eia_plants_gps.csv
-- ============================================================================

DROP TABLE IF EXISTS eia_plants;

CREATE TABLE eia_plants (
    plant_code INTEGER PRIMARY KEY,
    utility_id INTEGER,
    plant_name TEXT NOT NULL,
    street_address TEXT,
    city TEXT,
    county TEXT,
    state_code TEXT,
    zip TEXT,
    water_source TEXT,
    nerc_region TEXT,
    primary_naics INTEGER,
    system_owner TEXT,
    regulatory_status TEXT,
    sector_name TEXT,
    sector INTEGER,
    grid_voltage_kv REAL,
    ferc_cogen_status TEXT,
    iso_rto TEXT,
    iso_rto_code TEXT,
    latitude REAL,
    longitude REAL,
    FOREIGN KEY (state_code) REFERENCES states(state_code)
);

CREATE INDEX IF NOT EXISTS idx_eia_plants_state ON eia_plants(state_code);
CREATE INDEX IF NOT EXISTS idx_eia_plants_util ON eia_plants(utility_id);
