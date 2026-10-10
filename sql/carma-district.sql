-- ============================================================================
-- carma_district: CARMA v3.0 congressional district power emissions & generation
-- Source: raw/carma/CARMAv30_PowerUser.zip (Region.csv)
-- ============================================================================

DROP VIEW IF EXISTS carma;
DROP TABLE IF EXISTS carma_district;

CREATE TABLE carma_district (
    fips INTEGER NOT NULL,
    state_code TEXT NOT NULL,
    district_code INTEGER NOT NULL,
    representative TEXT,
    plant_count INTEGER,
    carbon_2004 REAL,
    carbon_2009 REAL,
    carbon_future REAL,
    energy_2004 REAL,
    energy_2009 REAL,
    energy_future REAL,
    intensity_2004 REAL,
    intensity_2009 REAL,
    intensity_future REAL,
    share_fossil_2004 REAL,
    share_fossil_2009 REAL,
    share_hydro_2004 REAL,
    share_hydro_2009 REAL,
    share_nuclear_2004 REAL,
    share_nuclear_2009 REAL,
    share_renewable_2004 REAL,
    share_renewable_2009 REAL,
    PRIMARY KEY (fips, district_code),
    FOREIGN KEY (state_code) REFERENCES states(state_code)
);

CREATE INDEX IF NOT EXISTS idx_carma_district_state ON carma_district(state_code);

-- Backward-compatibility view matching legacy schema
CREATE VIEW IF NOT EXISTS carma AS
SELECT 2004 AS year, fips, district_code, 'carbon dioxide' AS variable, carbon_2004 AS value, 'tons' AS units
FROM carma_district
UNION ALL
SELECT 2009 AS year, fips, district_code, 'carbon dioxide' AS variable, carbon_2009 AS value, 'tons' AS units
FROM carma_district;
