-- ============================================================================
-- voteview_info: Voteview roll call descriptions, dates, results, and parameters
-- Covers House and Senate (103rd–114th Congress)
-- Source: raw/voteview/{H,S}{congress}_rollcalls.csv
-- ============================================================================

DROP TABLE IF EXISTS voteview_info;

CREATE TABLE voteview_info (
    congress INTEGER NOT NULL,
    chamber TEXT NOT NULL,
    rollnumber INTEGER NOT NULL,
    date TEXT,
    session INTEGER,
    clerk_rollnumber INTEGER,
    majority_requirement TEXT,
    yea_count INTEGER,
    nay_count INTEGER,
    nominate_mid_1 REAL,
    nominate_mid_2 REAL,
    nominate_spread_1 REAL,
    nominate_spread_2 REAL,
    nominate_log_likelihood REAL,
    bill_number TEXT,
    vote_result TEXT,
    vote_desc TEXT,
    vote_question TEXT,
    dtl_desc TEXT,
    PRIMARY KEY (congress, chamber, rollnumber)
);

CREATE INDEX IF NOT EXISTS idx_vv_info_clerk ON voteview_info(congress, chamber, clerk_rollnumber);
CREATE INDEX IF NOT EXISTS idx_vv_info_date ON voteview_info(date);
