-- ============================================================================
-- fec_margins_2014: 2014 General election victory margins (114th Congress)
-- Source: raw/fec/results14.xls
-- ============================================================================

DROP TABLE IF EXISTS fec_margins_2014;

CREATE TABLE fec_margins_2014 (
    congress INTEGER NOT NULL DEFAULT 114,
    chamber TEXT NOT NULL,
    state_code TEXT NOT NULL,
    district_number INTEGER NOT NULL DEFAULT 0,
    govtrack_id TEXT,
    icpsr_id INTEGER,
    fec_id TEXT,
    winner_name TEXT,
    winner_pct REAL,
    runner_up_pct REAL,
    margin_pct REAL,
    margin REAL,
    PRIMARY KEY (congress, chamber, state_code, district_number, govtrack_id)
);

CREATE INDEX IF NOT EXISTS idx_fec_m14_govtrack ON fec_margins_2014(govtrack_id);
CREATE INDEX IF NOT EXISTS idx_fec_m14_icpsr ON fec_margins_2014(icpsr_id);
CREATE INDEX IF NOT EXISTS idx_fec_m14_fec ON fec_margins_2014(fec_id);
