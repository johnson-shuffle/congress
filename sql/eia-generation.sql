-- ============================================================================
-- eia_generation: Multi-decade electricity generation & emissions by power plant
-- Sources: EIA Form 759/906/923 consolidated (raw/eia/eia_generation.Rda)
-- ============================================================================

DROP TABLE IF EXISTS eia_generation;

CREATE TABLE eia_generation (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    plant_id INTEGER NOT NULL,
    state_code TEXT,
    year INTEGER NOT NULL,
    energy_source TEXT NOT NULL,
    net_generation_mwh REAL,
    fuel_mmbtu REAL,
    co2_metric_tons REAL,
    FOREIGN KEY (state_code) REFERENCES states(state_code)
);

CREATE INDEX IF NOT EXISTS idx_eia_gen_plant_year ON eia_generation(plant_id, year);
CREATE INDEX IF NOT EXISTS idx_eia_gen_state_year ON eia_generation(state_code, year);
