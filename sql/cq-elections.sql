-- ============================================================================
-- cq_elections: Congressional Quarterly federal election contest returns
-- Covers House, Senate, Presidential, and Gubernatorial races (1988–2016)
-- ============================================================================

DROP TABLE IF EXISTS cq_elections;

CREATE TABLE cq_elections (
    cycle INTEGER NOT NULL,
    office TEXT NOT NULL,
    state_code TEXT NOT NULL,
    district_number INTEGER NOT NULL DEFAULT 0,
    election_type TEXT NOT NULL DEFAULT 'General',
    rep_candidate TEXT,
    rep_status TEXT,
    rep_votes INTEGER,
    rep_pct REAL,
    dem_candidate TEXT,
    dem_status TEXT,
    dem_votes INTEGER,
    dem_pct REAL,
    third_party TEXT,
    third_candidate TEXT,
    third_status TEXT,
    third_votes INTEGER,
    third_pct REAL,
    other_votes INTEGER,
    plurality_party TEXT,
    plurality_votes INTEGER,
    PRIMARY KEY (cycle, office, state_code, district_number, election_type)
);

CREATE INDEX IF NOT EXISTS idx_cq_elections_geo ON cq_elections(state_code, district_number);
CREATE INDEX IF NOT EXISTS idx_cq_elections_office ON cq_elections(office, cycle);
