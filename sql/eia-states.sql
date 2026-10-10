-- ============================================================================
-- eia_states: State-level annual energy production (SEDS) & real energy prices
-- Sources: raw/eia/Complete_SEDS.csv, raw/eia/eia_prices.csv
-- ============================================================================

DROP TABLE IF EXISTS eia_states;

CREATE TABLE eia_states (
    state_code TEXT NOT NULL,
    year INTEGER NOT NULL,
    coal_production_btu REAL,
    natgas_production_btu REAL,
    petroleum_production_btu REAL,
    renewable_production_btu REAL,
    coal_real_price REAL,
    electricity_real_price REAL,
    natgas_real_price REAL,
    petroleum_real_price REAL,
    PRIMARY KEY (state_code, year),
    FOREIGN KEY (state_code) REFERENCES states(state_code)
);

CREATE INDEX IF NOT EXISTS idx_eia_states_year ON eia_states(year);
