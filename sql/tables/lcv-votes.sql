-- ============================================================================
-- lcv_votes: League of Conservation Voters (LCV) roll call vote positions
-- Selected environmental scorecard roll calls (1971–2015, 92nd–114th Congress)
-- Source: raw/lcv/lcv-votes.csv
-- ============================================================================

DROP TABLE IF EXISTS lcv_votes;

CREATE TABLE lcv_votes (
    congress INTEGER NOT NULL,
    year INTEGER NOT NULL,
    chamber TEXT NOT NULL,
    rollnumber INTEGER NOT NULL,
    rollcall TEXT,
    lcv_type TEXT,
    lcv_dscr TEXT,
    lcv_vote INTEGER,
    lcv_note TEXT,
    not_in_lcv INTEGER NOT NULL DEFAULT 0,
    double_count INTEGER NOT NULL DEFAULT 0,
    PRIMARY KEY (year, chamber, rollnumber)
);

CREATE INDEX IF NOT EXISTS idx_lcv_votes_congress ON lcv_votes(congress, chamber);
CREATE INDEX IF NOT EXISTS idx_lcv_votes_rollcall ON lcv_votes(rollcall);
CREATE INDEX IF NOT EXISTS idx_lcv_votes_year ON lcv_votes(year);
