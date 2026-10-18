-- ============================================================================
-- noaa_degree_days: NOAA CPC Daily Population-Weighted Cooling and Heating
-- Degree Days for US CONUS States (1993-2016)
-- Source: https://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/daily_data/
-- Ingestion: data/noaa.R
-- ============================================================================

DROP TABLE IF EXISTS noaa_degree_days;

CREATE TABLE noaa_degree_days (
    state_code TEXT NOT NULL,
    date TEXT NOT NULL,
    year INTEGER NOT NULL,
    cdd REAL NOT NULL,
    hdd REAL NOT NULL,
    PRIMARY KEY (state_code, date),
    FOREIGN KEY (state_code) REFERENCES states(state_code)
);

CREATE INDEX IF NOT EXISTS idx_noaa_dd_state_year ON noaa_degree_days(state_code, year);
CREATE INDEX IF NOT EXISTS idx_noaa_dd_year ON noaa_degree_days(year);
