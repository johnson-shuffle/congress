-- ============================================================================
-- crp_indivs: OpenSecrets / CRP Individual Contributions
-- Source: raw/opensecrets/CampaignFin*.zip (indivs*.txt, 1990–2016)
-- ============================================================================

DROP TABLE IF EXISTS crp_indivs;

CREATE TABLE crp_indivs (
    cycle INTEGER NOT NULL,
    transaction_id TEXT,
    contrib_id TEXT,
    contrib_name TEXT,
    recip_id TEXT,
    org_name TEXT,
    parent_org TEXT,
    cat_code TEXT,
    date TEXT,
    amount REAL,
    street TEXT,
    city TEXT,
    state_code TEXT,
    zip_code TEXT,
    recip_code TEXT,
    trans_type TEXT,
    cmte_id TEXT,
    other_id TEXT,
    gender TEXT,
    microfilm TEXT,
    occupation TEXT,
    employer TEXT,
    source TEXT
);

CREATE INDEX IF NOT EXISTS idx_crp_indivs_cycle ON crp_indivs(cycle);
CREATE INDEX IF NOT EXISTS idx_crp_indivs_recip ON crp_indivs(recip_id);
CREATE INDEX IF NOT EXISTS idx_crp_indivs_cat ON crp_indivs(cat_code);
CREATE INDEX IF NOT EXISTS idx_crp_indivs_cmte ON crp_indivs(cmte_id);
