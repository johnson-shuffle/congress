-- ============================================================================
-- congress_sessions: Convening, adjournment, and recess dates for House & Senate
-- Covers 1st to 119th Congress (1789–present)
-- Source: House History (history.house.gov) and Senate Reference (senate.gov)
-- ============================================================================

DROP VIEW IF EXISTS congress;
DROP TABLE IF EXISTS congress_sessions;

CREATE TABLE congress_sessions (
    congress INTEGER NOT NULL,
    session INTEGER NOT NULL,
    chamber TEXT NOT NULL,
    session_start TEXT NOT NULL,
    session_end TEXT,
    calendar_days INTEGER,
    legislative_days INTEGER,
    recesses TEXT,
    start_date TEXT,
    end_date TEXT,
    PRIMARY KEY (congress, session, chamber, session_start)
);

CREATE INDEX IF NOT EXISTS idx_congress_sessions_cong ON congress_sessions(congress, chamber);

CREATE VIEW IF NOT EXISTS congress AS SELECT * FROM congress_sessions;
