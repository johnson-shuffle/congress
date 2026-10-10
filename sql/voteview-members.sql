-- ============================================================================
-- voteview_members: Voteview roster and DW-NOMINATE ideological scores
-- ============================================================================

DROP TABLE IF EXISTS voteview_members;

CREATE TABLE voteview_members (
    congress INTEGER NOT NULL,
    chamber TEXT NOT NULL,
    icpsr_id INTEGER NOT NULL,
    state_icpsr INTEGER,
    district_code INTEGER NOT NULL DEFAULT 0,
    state_code TEXT NOT NULL,
    party_code INTEGER,
    occupancy INTEGER,
    last_means INTEGER,
    bio_name TEXT,
    bioguide_id TEXT,
    born INTEGER,
    died REAL,
    nominate_dim1 REAL,
    nominate_dim2 REAL,
    nominate_log_likelihood REAL,
    nominate_geo_mean_probability REAL,
    nominate_number_of_votes INTEGER,
    nominate_number_of_errors INTEGER,
    nokken_poole_dim1 REAL,
    nokken_poole_dim2 REAL,
    PRIMARY KEY (congress, chamber, icpsr_id, district_code)
);

CREATE INDEX IF NOT EXISTS idx_voteview_members_icpsr ON voteview_members(icpsr_id);
CREATE INDEX IF NOT EXISTS idx_voteview_members_bioguide ON voteview_members(bioguide_id);
CREATE INDEX IF NOT EXISTS idx_voteview_members_geo ON voteview_members(state_code, district_code);
