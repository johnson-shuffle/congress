-- ============================================================================
-- carma_plants: CARMA v3.0 power plant level emissions and generation
-- Source: raw/carma/CARMAv30_PowerUser.zip (Plant.csv)
-- ============================================================================

DROP TABLE IF EXISTS carma_plants;

CREATE TABLE carma_plants (
    plant_id INTEGER PRIMARY KEY,
    plant_name TEXT NOT NULL,
    company TEXT,
    city TEXT,
    county TEXT,
    state_code TEXT,
    district_code INTEGER,
    latitude REAL,
    longitude REAL,
    carbon_2004 REAL,
    carbon_2009 REAL,
    carbon_future REAL,
    energy_2004 REAL,
    energy_2009 REAL,
    energy_future REAL,
    intensity_2004 REAL,
    intensity_2009 REAL,
    intensity_future REAL
);

CREATE INDEX IF NOT EXISTS idx_carma_plants_state ON carma_plants(state_code);
CREATE INDEX IF NOT EXISTS idx_carma_plants_dist ON carma_plants(state_code, district_code);
