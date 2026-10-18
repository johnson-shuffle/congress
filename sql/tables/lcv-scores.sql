-- ============================================================================
-- lcv_scores: League of Conservation Voters (LCV) annual member voting scores
-- Covers House and Senate (103rd–114th Congress, 1993–2015)
-- Sources: raw/lcv/lcv-scores-annual.csv, raw/lcv/scorecards/*.pdf
-- ============================================================================

DROP TABLE IF EXISTS lcv_scores;

CREATE TABLE lcv_scores (
    icpsr_id INTEGER NOT NULL,
    icpsrid INTEGER GENERATED ALWAYS AS (icpsr_id) VIRTUAL,
    congress INTEGER NOT NULL,
    year INTEGER NOT NULL,
    chamber TEXT NOT NULL,
    eligible_votes INTEGER,
    lcv_score REAL,
    PRIMARY KEY (icpsr_id, congress, year, chamber)
);

CREATE INDEX IF NOT EXISTS idx_lcv_scores_member ON lcv_scores(icpsr_id);
CREATE INDEX IF NOT EXISTS idx_lcv_scores_congress ON lcv_scores(congress, chamber);
CREATE INDEX IF NOT EXISTS idx_lcv_scores_year ON lcv_scores(year, chamber);

-- ============================================================================
-- lcv_congress_scores: LCV biennial scores aggregated across Congress sessions
-- ============================================================================

DROP TABLE IF EXISTS lcv_congress_scores;

CREATE TABLE lcv_congress_scores (
    icpsr_id INTEGER NOT NULL,
    icpsrid INTEGER GENERATED ALWAYS AS (icpsr_id) VIRTUAL,
    congress INTEGER NOT NULL,
    chamber TEXT NOT NULL,
    eligible_votes INTEGER,
    lcv_score REAL,
    PRIMARY KEY (icpsr_id, congress, chamber)
);

CREATE INDEX IF NOT EXISTS idx_lcv_congress_scores_member ON lcv_congress_scores(icpsr_id);
CREATE INDEX IF NOT EXISTS idx_lcv_congress_scores_congress ON lcv_congress_scores(congress, chamber);
