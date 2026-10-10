-- ============================================================================
-- cffr: Consolidated Federal Funds Report - Geographic Spending Outlays
-- Covers 1993–2010 across US States, Territories, and Congressional Districts
-- Source: raw/cffr/*CFFIND/*.csv
-- ============================================================================

DROP TABLE IF EXISTS cffr;

CREATE TABLE cffr (
    year INTEGER NOT NULL,
    fips_state INTEGER,
    fips_county INTEGER,
    fips_place INTEGER,
    state_code TEXT NOT NULL,
    county_name TEXT,
    place_name TEXT,
    population INTEGER,
    congdist TEXT,
    prog_id TEXT,
    obj_type TEXT,
    agency_code TEXT,
    amount REAL
);

CREATE INDEX IF NOT EXISTS idx_cffr_year_state ON cffr(year, state_code);
CREATE INDEX IF NOT EXISTS idx_cffr_congdist ON cffr(year, state_code, congdist);
CREATE INDEX IF NOT EXISTS idx_cffr_agency ON cffr(agency_code);
CREATE INDEX IF NOT EXISTS idx_cffr_prog ON cffr(prog_id);
CREATE INDEX IF NOT EXISTS idx_cffr_obj_type ON cffr(obj_type);
