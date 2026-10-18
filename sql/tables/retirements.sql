-- ============================================================================
-- retirements: Congressional member retirements and election year status
-- Covers House and Senate (103rd–114th Congress, 1993–2016)
-- Sources: raw/retirements.csv, crosswalk_members, govtrack_term
-- ============================================================================

DROP TABLE IF EXISTS retirements;

CREATE TABLE retirements (
    congress INTEGER NOT NULL,
    chamber TEXT NOT NULL,
    icpsr_id INTEGER NOT NULL,
    district_number INTEGER NOT NULL DEFAULT 0,
    govtrack_id TEXT,
    state_code TEXT NOT NULL,
    year INTEGER NOT NULL,
    senate_class INTEGER NOT NULL DEFAULT 0,
    election_year INTEGER NOT NULL DEFAULT 0,
    retire INTEGER NOT NULL DEFAULT 0,
    PRIMARY KEY (congress, chamber, icpsr_id, district_number)
);

CREATE INDEX IF NOT EXISTS idx_retirements_member ON retirements(icpsr_id);
CREATE INDEX IF NOT EXISTS idx_retirements_govtrack ON retirements(govtrack_id);
CREATE INDEX IF NOT EXISTS idx_retirements_congress ON retirements(congress, chamber);
CREATE INDEX IF NOT EXISTS idx_retirements_retire ON retirements(retire);
