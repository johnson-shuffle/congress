-- ============================================================================
-- eia_survey_gen: Raw electricity generation survey records (Forms 759, 906, 923)
-- Sources: raw/eia/f759*.xls, F906*.csv/xls, f923_*.csv
-- ============================================================================

DROP TABLE IF EXISTS eia_survey_gen;

CREATE TABLE eia_survey_gen (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    plant_id INTEGER NOT NULL,
    state_code TEXT,
    year INTEGER NOT NULL,
    survey_form TEXT NOT NULL,
    energy_source TEXT,
    net_generation_mwh REAL,
    fuel_quantity REAL,
    fuel_mmbtu REAL
);

CREATE INDEX IF NOT EXISTS idx_eia_survey_plant_year ON eia_survey_gen(plant_id, year);
CREATE INDEX IF NOT EXISTS idx_eia_survey_source ON eia_survey_gen(energy_source);
CREATE INDEX IF NOT EXISTS idx_eia_survey_form ON eia_survey_gen(survey_form);
