-- ============================================================================
-- noaa_degree_days: NOAA CPC Daily Population-Weighted Cooling and Heating
-- Degree Days for US CONUS States (1993-2016)
-- Source: https://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/daily_data/
-- Ingestion: data/noaa.R
-- ============================================================================

DROP VIEW IF EXISTS noaa_state_degree_days;
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

-- Analytical view aggregating annual heating and cooling degree days by state
CREATE VIEW IF NOT EXISTS noaa_state_degree_days AS
SELECT 
    state_code,
    year,
    ROUND(SUM(cdd), 1) AS cdd,
    ROUND(SUM(hdd), 1) AS hdd
FROM noaa_degree_days
GROUP BY state_code, year;
