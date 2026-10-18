-- ============================================================================
-- govtrack_info: Comprehensive legislator bio and external cross-reference IDs
-- ============================================================================

DROP TABLE IF EXISTS govtrack_info;

CREATE TABLE govtrack_info (
    govtrack_id TEXT PRIMARY KEY,
    bioguide_id TEXT,
    thomas_id TEXT,
    lis_id TEXT,
    opensecrets_id TEXT,
    votesmart_id TEXT,
    fec_id TEXT,
    cspan_id TEXT,
    wikipedia_id TEXT,
    house_history_id TEXT,
    ballotpedia_id TEXT,
    maplight_id TEXT,
    icpsr_id TEXT,
    wikidata_id TEXT,
    google_entity_id TEXT,
    first_name TEXT,
    last_name TEXT,
    official_full_name TEXT,
    middle_name TEXT,
    nickname TEXT,
    suffix TEXT,
    gender TEXT,
    birthday TEXT,
    religion TEXT
);

CREATE INDEX IF NOT EXISTS idx_govtrack_info_bioguide ON govtrack_info(bioguide_id);
CREATE INDEX IF NOT EXISTS idx_govtrack_info_icpsr ON govtrack_info(icpsr_id);
CREATE INDEX IF NOT EXISTS idx_govtrack_info_opensecrets ON govtrack_info(opensecrets_id);
