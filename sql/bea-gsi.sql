-- ============================================================================
-- bea_gsi: Bureau of Economic Analysis State Personal Income (GSI)
-- Source: raw/bea/SA1_1929_2014.csv (LineCode 1: Personal income)
-- ============================================================================

DROP TABLE IF EXISTS bea_gsi;

CREATE TABLE bea_gsi (
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

CREATE INDEX IF NOT EXISTS idx_bea_gsi_geo ON bea_gsi(fips, year);
