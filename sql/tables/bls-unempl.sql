-- ============================================================================
-- bls_unempl: Bureau of Labor Statistics state annual unemployment rates
-- Source: raw/bls/unemp.txt (LA State Unemployment Series LASST)
-- ============================================================================

DROP TABLE IF EXISTS bls_unempl;

CREATE TABLE bls_unempl (
    year INTEGER NOT NULL,
    fips INTEGER NOT NULL,
    avg_unempl REAL,
    PRIMARY KEY (year, fips),
    FOREIGN KEY (fips) REFERENCES states(fips)
);

CREATE INDEX IF NOT EXISTS idx_bls_unempl_fips ON bls_unempl(fips);
