-- ============================================================================
-- bea_pop: Bureau of Economic Analysis State Annual Population Estimates
-- Source: raw/bea/SA1_1929_2014.csv (LineCode 2: Population)
-- ============================================================================

DROP TABLE IF EXISTS bea_pop;

CREATE TABLE bea_pop (
    geo_fips TEXT NOT NULL,
    geo_name TEXT NOT NULL,
    region INTEGER,
    series TEXT,
    line_code INTEGER NOT NULL,
    industry_classification TEXT,
    description TEXT,
    fips INTEGER NOT NULL,
    year INTEGER NOT NULL,
    value REAL,
    units TEXT,
    PRIMARY KEY (year, fips),
    FOREIGN KEY (fips) REFERENCES states(fips)
);

CREATE INDEX IF NOT EXISTS idx_bea_pop_geo ON bea_pop(fips, year);
