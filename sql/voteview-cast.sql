-- ============================================================================
-- voteview_cast: Individual member roll call cast votes and probabilities
-- Covers House and Senate (103rd–114th Congress)
-- Source: raw/voteview/{H,S}{congress}_votes.csv
-- ============================================================================

DROP VIEW IF EXISTS voteview_votes;
DROP TABLE IF EXISTS voteview_cast;

CREATE TABLE voteview_cast (
    congress INTEGER NOT NULL,
    chamber TEXT NOT NULL,
    rollnumber INTEGER NOT NULL,
    icpsr_id INTEGER NOT NULL,
    icpsrid INTEGER GENERATED ALWAYS AS (icpsr_id) VIRTUAL,
    cast_code INTEGER NOT NULL,
    prob REAL,
    PRIMARY KEY (congress, chamber, rollnumber, icpsr_id)
);

CREATE INDEX IF NOT EXISTS idx_vv_cast_member ON voteview_cast(icpsr_id);
CREATE INDEX IF NOT EXISTS idx_vv_cast_roll ON voteview_cast(congress, chamber, rollnumber);
CREATE INDEX IF NOT EXISTS idx_vv_cast_cong_chamber ON voteview_cast(congress, chamber);

CREATE VIEW IF NOT EXISTS voteview_votes AS 
SELECT * FROM voteview_cast;
