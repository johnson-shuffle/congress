-- ============================================================================
-- crp_pac_other: OpenSecrets / CRP PAC-to-PAC and Other Committee Transactions
-- Source: raw/opensecrets/CampaignFin*.zip (pac_other*.txt, 1990–2016)
-- ============================================================================

DROP TABLE IF EXISTS crp_pac_other;

CREATE TABLE crp_pac_other (
    cycle INTEGER NOT NULL,
    transaction_id TEXT,
    filer_id TEXT,
    donor_cmte TEXT,
    contrib_lend_trans TEXT,
    city TEXT,
    state_code TEXT,
    zip_code TEXT,
    fec_occ_emp TEXT,
    prim_code TEXT,
    date TEXT,
    amount REAL,
    recip_id TEXT,
    party TEXT,
    other_id TEXT,
    recip_code TEXT,
    recip_prim_code TEXT,
    amend TEXT,
    report TEXT,
    prim_gen TEXT,
    microfilm TEXT,
    trans_type TEXT,
    cat_code TEXT,
    source TEXT
);

CREATE INDEX IF NOT EXISTS idx_crp_pac_other_cycle ON crp_pac_other(cycle);
CREATE INDEX IF NOT EXISTS idx_crp_pac_other_filer ON crp_pac_other(filer_id);
CREATE INDEX IF NOT EXISTS idx_crp_pac_other_recip ON crp_pac_other(recip_id);
CREATE INDEX IF NOT EXISTS idx_crp_pac_other_cat ON crp_pac_other(cat_code);
