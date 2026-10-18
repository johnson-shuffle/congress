-- ============================================================================
-- crp_pacs: OpenSecrets / CRP PAC Contributions to Candidates
-- Source: raw/opensecrets/CampaignFin*.zip (pacs*.txt, 1990–2016)
-- ============================================================================

DROP TABLE IF EXISTS crp_pacs;

CREATE TABLE crp_pacs (
    cycle INTEGER NOT NULL,
    transaction_id TEXT,
    cmte_id TEXT NOT NULL,
    osid TEXT,
    amount REAL NOT NULL,
    date TEXT,
    cat_code TEXT,
    trans_type TEXT,
    direct_indirect TEXT,
    fec_candidate_id TEXT
);

CREATE INDEX IF NOT EXISTS idx_crp_pacs_cycle ON crp_pacs(cycle);
CREATE INDEX IF NOT EXISTS idx_crp_pacs_osid ON crp_pacs(osid);
CREATE INDEX IF NOT EXISTS idx_crp_pacs_cmte ON crp_pacs(cmte_id);
CREATE INDEX IF NOT EXISTS idx_crp_pacs_code ON crp_pacs(cat_code);
CREATE INDEX IF NOT EXISTS idx_crp_pacs_fec ON crp_pacs(fec_candidate_id);
