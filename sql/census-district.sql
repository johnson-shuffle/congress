-- ============================================================================
-- census_district & census_state: Decennial & ACS Demographics (1993-2015)
-- Sources:
--   - 1990 Census Summary Tape File 3D (raw/census/1990_STF3D.csv)
--   - 2000 Census & ACS Demographic Profiles DP1-DP4 (raw/census/c*_DP*.csv)
-- Ingestion: data/census.R
-- ============================================================================

DROP VIEW IF EXISTS census;
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

-- Compatibility view mimicking legacy census_all schema
CREATE VIEW IF NOT EXISTS census AS
SELECT 
    fips,
    district_number AS district,
    pop_total AS "pop.total",
    pct_white AS white,
    pct_black AS black,
    pct_indian AS indian,
    pct_other AS other,
    pct_hispanic AS hispanic,
    pct_pop_65 AS "pop.65",
    pct_pop_25 AS "pop.25",
    pct_less_than_9th AS "less.than.9th",
    pct_more_than_9th AS "more.than.9th",
    pct_highschool AS highschool,
    pct_somecollege AS somecollege,
    pct_associate AS associate,
    pct_bachelor AS bachelor,
    pct_graduate AS graduate,
    pct_empl_res AS "empl.res",
    pct_empl_con AS "empl.con",
    pct_empl_man AS "empl.man",
    pct_empl_wst AS "empl.wst",
    pct_empl_rt AS "empl.rt",
    pct_empl_pub AS "empl.pub",
    med_inc_house AS "med.inc.house",
    med_inc_fam AS "med.inc.fam",
    empl_total AS "empl.total",
    pct_hm_electric AS "hm.electric",
    hm_total AS "hm.total",
    year,
    pct_asian_island AS "asian.island"
FROM census_district
UNION ALL
SELECT 
    fips,
    NULL AS district,
    pop_total AS "pop.total",
    pct_white AS white,
    pct_black AS black,
    pct_indian AS indian,
    pct_other AS other,
    pct_hispanic AS hispanic,
    pct_pop_65 AS "pop.65",
    pct_pop_25 AS "pop.25",
    pct_less_than_9th AS "less.than.9th",
    pct_more_than_9th AS "more.than.9th",
    pct_highschool AS highschool,
    pct_somecollege AS somecollege,
    pct_associate AS associate,
    pct_bachelor AS bachelor,
    pct_graduate AS graduate,
    pct_empl_res AS "empl.res",
    pct_empl_con AS "empl.con",
    pct_empl_man AS "empl.man",
    pct_empl_wst AS "empl.wst",
    pct_empl_rt AS "empl.rt",
    pct_empl_pub AS "empl.pub",
    med_inc_house AS "med.inc.house",
    med_inc_fam AS "med.inc.fam",
    empl_total AS "empl.total",
    pct_hm_electric AS "hm.electric",
    hm_total AS "hm.total",
    year,
    pct_asian_island AS "asian.island"
FROM census_state;
