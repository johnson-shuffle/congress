-- ============================================================================
-- fec_elections: Official Federal Election Commission candidate results (2004–2014)
-- Source: raw/fec/results04.xls through results14.xls
-- ============================================================================

DROP TABLE IF EXISTS fec_elections;

CREATE TABLE fec_elections (
    cycle INTEGER NOT NULL,
    chamber TEXT NOT NULL,
    state_code TEXT NOT NULL,
    district_number INTEGER NOT NULL DEFAULT 0,
    is_unexpired INTEGER NOT NULL DEFAULT 0,
    fec_id TEXT,
    incumbent_status TEXT,
    candidate_first_name TEXT,
    candidate_last_name TEXT,
    candidate_name TEXT,
    party TEXT,
    primary_votes INTEGER,
    primary_pct REAL,
    runoff_votes INTEGER,
    runoff_pct REAL,
    general_votes INTEGER,
    general_pct REAL,
    ge_runoff_votes INTEGER,
    ge_runoff_pct REAL,
    winner_indicator TEXT
);

CREATE INDEX IF NOT EXISTS idx_fec_elec_cycle ON fec_elections(cycle, chamber, state_code, district_number);
CREATE INDEX IF NOT EXISTS idx_fec_elec_fec_id ON fec_elections(fec_id);
CREATE INDEX IF NOT EXISTS idx_fec_elec_cand ON fec_elections(candidate_last_name, state_code);
