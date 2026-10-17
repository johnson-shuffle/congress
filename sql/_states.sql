-- ============================================================================
-- states: Reference table for US States, FIPS codes, and regional definitions
-- ============================================================================

DROP TABLE IF EXISTS states;

CREATE TABLE states (
    fips INTEGER PRIMARY KEY,
    state_code TEXT NOT NULL UNIQUE,
    state_name TEXT NOT NULL UNIQUE,
    census_region INTEGER,
    census_division INTEGER,
    icpsr_code INTEGER UNIQUE,
    adage_region TEXT,
    recs_domain INTEGER,
    recs_division INTEGER,
    nerc_region TEXT,
    interconnect TEXT,
    at_large INTEGER NOT NULL DEFAULT 0
);
