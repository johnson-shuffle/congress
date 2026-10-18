-- ============================================================================
-- crosswalk_members: Master identity & term crosswalk for US Congress members
-- Links GovTrack, ICPSR (Voteview), Bioguide, OpenSecrets (CRP), VoteSmart,
-- and FEC IDs alongside biographical and term attainment metadata (103rd–114th)
-- ============================================================================

DROP TABLE IF EXISTS crosswalk_members;

CREATE TABLE crosswalk_members (
    congress INTEGER NOT NULL,
    chamber TEXT NOT NULL,
    icpsr_id INTEGER NOT NULL,
    bioguide_id TEXT,
    govtrack_id TEXT,
    opensecrets_id TEXT,
    votesmart_id TEXT,
    fec_id TEXT,
    state_code TEXT NOT NULL,
    district_number INTEGER NOT NULL DEFAULT 0,
    senate_class INTEGER NOT NULL DEFAULT 0,
    party TEXT,
    first_name TEXT,
    preferred_name TEXT,
    last_name TEXT,
    suffix TEXT,
    full_name TEXT,
    gender TEXT,
    birthday TEXT,
    occupancy INTEGER,
    means INTEGER,
    start_date TEXT,
    end_date TEXT,
    PRIMARY KEY (congress, chamber, icpsr_id, district_number)
);

CREATE INDEX IF NOT EXISTS idx_cw_members_icpsr ON crosswalk_members(icpsr_id);
CREATE INDEX IF NOT EXISTS idx_cw_members_bioguide ON crosswalk_members(bioguide_id);
CREATE INDEX IF NOT EXISTS idx_cw_members_govtrack ON crosswalk_members(govtrack_id);
CREATE INDEX IF NOT EXISTS idx_cw_members_opensecrets ON crosswalk_members(opensecrets_id);
CREATE INDEX IF NOT EXISTS idx_cw_members_fec ON crosswalk_members(fec_id);
CREATE INDEX IF NOT EXISTS idx_cw_members_geography ON crosswalk_members(state_code, district_number);
