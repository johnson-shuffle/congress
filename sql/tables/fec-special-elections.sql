-- ============================================================================
-- fec_special_elections: Congressional special election results (103rd–114th Congress)
-- Source: raw/fec/special_elections_house.csv, special_elections_senate.csv
-- ============================================================================

DROP TABLE IF EXISTS fec_special_elections;

CREATE TABLE fec_special_elections (
    congress INTEGER NOT NULL,
    chamber TEXT NOT NULL,
    state_code TEXT NOT NULL,
    district_number INTEGER NOT NULL DEFAULT 0,
    govtrack_id TEXT NOT NULL,
    icpsr_id INTEGER,
    candidate_name TEXT,
    occupancy INTEGER,
    means INTEGER,
    first_place_pct REAL NOT NULL,
    second_place_pct REAL NOT NULL,
    margin_pct REAL NOT NULL,
    margin REAL NOT NULL,
    PRIMARY KEY (congress, chamber, state_code, district_number, govtrack_id)
);

CREATE INDEX IF NOT EXISTS idx_fec_spec_govtrack ON fec_special_elections(govtrack_id);
CREATE INDEX IF NOT EXISTS idx_fec_spec_icpsr ON fec_special_elections(icpsr_id);
