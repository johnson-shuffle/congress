-- ============================================================================
-- govtrack_term: Historical and current congressional terms served
-- ============================================================================

DROP TABLE IF EXISTS govtrack_term;

CREATE TABLE govtrack_term (
    govtrack_id TEXT NOT NULL,
    chamber TEXT NOT NULL,
    start_date TEXT,
    end_date TEXT,
    state_code TEXT NOT NULL,
    district_number INTEGER,
    party TEXT,
    senate_class INTEGER,
    state_rank TEXT,
    caucus TEXT,
    PRIMARY KEY (govtrack_id, start_date)
);

CREATE INDEX IF NOT EXISTS idx_govtrack_term_member ON govtrack_term(govtrack_id);
CREATE INDEX IF NOT EXISTS idx_govtrack_term_geo ON govtrack_term(state_code, district_number);
