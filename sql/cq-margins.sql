-- ============================================================================
-- cq_margins: Member electoral margins of victory (103rd–114th Congress)
-- Joined with crosswalk_members identifiers
-- ============================================================================

DROP TABLE IF EXISTS cq_margins;

CREATE TABLE cq_margins (
    congress INTEGER NOT NULL,
    cycle INTEGER NOT NULL,
    chamber TEXT NOT NULL,
    state_code TEXT NOT NULL,
    district_number INTEGER NOT NULL DEFAULT 0,
    senate_class INTEGER NOT NULL DEFAULT 0,
    govtrack_id TEXT,
    icpsr_id INTEGER,
    bioguide_id TEXT,
    winner_name TEXT,
    winner_status TEXT,
    winner_pct REAL,
    runner_up_pct REAL,
    margin REAL NOT NULL,
    PRIMARY KEY (congress, chamber, state_code, district_number, senate_class)
);

CREATE INDEX IF NOT EXISTS idx_cq_margins_govtrack ON cq_margins(govtrack_id);
CREATE INDEX IF NOT EXISTS idx_cq_margins_icpsr ON cq_margins(icpsr_id);
CREATE INDEX IF NOT EXISTS idx_cq_margins_bioguide ON cq_margins(bioguide_id);
