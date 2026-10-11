-- ============================================================================
-- fec_presidential_margins: State presidential election vote shares & margins (1992–2012)
-- Source: raw/fec/margins_presidents.csv
-- ============================================================================

DROP TABLE IF EXISTS fec_presidential_margins;

CREATE TABLE fec_presidential_margins (
    year INTEGER NOT NULL,
    state_code TEXT NOT NULL,
    dem_pct REAL NOT NULL,
    rep_pct REAL NOT NULL,
    margin_pct REAL NOT NULL,
    margin REAL NOT NULL,
    PRIMARY KEY (year, state_code)
);

CREATE INDEX IF NOT EXISTS idx_fec_pres_state ON fec_presidential_margins(state_code);
