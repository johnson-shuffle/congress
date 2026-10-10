-- ============================================================================
-- crp_committees: OpenSecrets / CRP Political Action Committees (PACs)
-- Source: raw/opensecrets/CampaignFin*.zip (cmtes*.txt, 1990–2016)
-- ============================================================================

DROP TABLE IF EXISTS crp_committees;

CREATE TABLE crp_committees (
    cycle INTEGER NOT NULL,
    cmte_id TEXT NOT NULL,
    pac_name TEXT,
    affiliate TEXT,
    parent_org TEXT,
    recip_id TEXT,
    recip_code TEXT,
    fec_candidate_id TEXT,
    party TEXT,
    prim_code TEXT,
    source TEXT,
    sensitive TEXT,
    foreign_connected INTEGER,
    active INTEGER,
    PRIMARY KEY (cycle, cmte_id)
);

CREATE INDEX IF NOT EXISTS idx_crp_cmtes_id ON crp_committees(cmte_id);
CREATE INDEX IF NOT EXISTS idx_crp_cmtes_prim ON crp_committees(prim_code);
