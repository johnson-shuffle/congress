-- ============================================================================
-- census_district & census_state: Decennial & ACS Demographics (1993-2015)
-- Sources:
--   - 1990 Census Summary Tape File 3D (raw/census/1990_STF3D.csv)
--   - 2000 Census & ACS Demographic Profiles DP1-DP4 (raw/census/c*_DP*.csv)
-- Ingestion: data/census.R
-- ============================================================================

DROP TABLE IF EXISTS census_state;
DROP TABLE IF EXISTS census_district;

CREATE TABLE census_district (
    year INTEGER NOT NULL,
    fips INTEGER NOT NULL,
    state_code TEXT NOT NULL,
    district_number INTEGER NOT NULL DEFAULT 0,
    pop_total INTEGER,
    pct_white REAL,
    pct_black REAL,
    pct_indian REAL,
    pct_asian_island REAL,
    pct_other REAL,
    pct_hispanic REAL,
    pct_pop_65 REAL,
    pct_pop_25 REAL,
    pct_less_than_9th REAL,
    pct_more_than_9th REAL,
    pct_highschool REAL,
    pct_somecollege REAL,
    pct_associate REAL,
    pct_bachelor REAL,
    pct_graduate REAL,
    empl_total INTEGER,
    pct_empl_res REAL,
    pct_empl_con REAL,
    pct_empl_man REAL,
    pct_empl_wst REAL,
    pct_empl_rt REAL,
    pct_empl_pub REAL,
    med_inc_house REAL,
    med_inc_fam REAL,
    hm_total INTEGER,
    pct_hm_electric REAL,
    PRIMARY KEY (year, state_code, district_number),
    FOREIGN KEY (state_code) REFERENCES states(state_code)
);

CREATE INDEX IF NOT EXISTS idx_census_dist_state_num ON census_district(state_code, district_number);
CREATE INDEX IF NOT EXISTS idx_census_dist_year ON census_district(year);

-- State-level aggregates table
CREATE TABLE census_state (
    year INTEGER NOT NULL,
    fips INTEGER NOT NULL,
    state_code TEXT NOT NULL,
    pop_total INTEGER,
    pct_white REAL,
    pct_black REAL,
    pct_indian REAL,
    pct_asian_island REAL,
    pct_other REAL,
    pct_hispanic REAL,
    pct_pop_65 REAL,
    pct_pop_25 REAL,
    pct_less_than_9th REAL,
    pct_more_than_9th REAL,
    pct_highschool REAL,
    pct_somecollege REAL,
    pct_associate REAL,
    pct_bachelor REAL,
    pct_graduate REAL,
    empl_total INTEGER,
    pct_empl_res REAL,
    pct_empl_con REAL,
    pct_empl_man REAL,
    pct_empl_wst REAL,
    pct_empl_rt REAL,
    pct_empl_pub REAL,
    med_inc_house REAL,
    med_inc_fam REAL,
    hm_total INTEGER,
    pct_hm_electric REAL,
    PRIMARY KEY (year, state_code),
    FOREIGN KEY (state_code) REFERENCES states(state_code)
);

CREATE INDEX IF NOT EXISTS idx_census_state_year ON census_state(year);
