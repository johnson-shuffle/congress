-- ============================================================================
-- bls_cpi: Bureau of Labor Statistics Consumer Price Index (CPI) annual averages
-- Source: raw/bls/cpi.txt (CU Time Series Summaries)
-- ============================================================================

DROP TABLE IF EXISTS bls_cpi;

CREATE TABLE bls_cpi (
    year INTEGER PRIMARY KEY,
    avg_cpi REAL NOT NULL
);
