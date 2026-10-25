-- ============================================================================
-- crp_contributions: Candidate campaign contributions by sector and committee
-- Source: crp_indivs, crp_pacs, crp_categories, bls_cpi
-- Keyed by OpenSecrets candidate ID (osid) and Congress (101–114).
-- Real amounts are deflated to 2010 base year dollars using bls_cpi.
-- ============================================================================

DROP TABLE IF EXISTS crp_contributions;

CREATE TABLE crp_contributions (
    osid TEXT NOT NULL,
    congress INTEGER NOT NULL,
    cycle INTEGER NOT NULL,
    -- Real 2010 dollars (primary master panel variables)
    agribusiness REAL NOT NULL DEFAULT 0.0,
    energy REAL NOT NULL DEFAULT 0.0,
    transport REAL NOT NULL DEFAULT 0.0,
    environment REAL NOT NULL DEFAULT 0.0,
    d_leader_pacs REAL NOT NULL DEFAULT 0.0,
    r_leader_pacs REAL NOT NULL DEFAULT 0.0,
    d_cand_cmte REAL NOT NULL DEFAULT 0.0,
    r_cand_cmte REAL NOT NULL DEFAULT 0.0,
    d_party_cmte REAL NOT NULL DEFAULT 0.0,
    r_party_cmte REAL NOT NULL DEFAULT 0.0,
    d_joint_cand_cmte REAL NOT NULL DEFAULT 0.0,
    r_joint_cand_cmte REAL NOT NULL DEFAULT 0.0,
    d_cmte REAL NOT NULL DEFAULT 0.0,
    r_cmte REAL NOT NULL DEFAULT 0.0,
    total_pac REAL NOT NULL DEFAULT 0.0,
    total_indiv REAL NOT NULL DEFAULT 0.0,
    total_contrib REAL NOT NULL DEFAULT 0.0,
    -- Nominal dollars
    agribusiness_nom REAL NOT NULL DEFAULT 0.0,
    energy_nom REAL NOT NULL DEFAULT 0.0,
    transport_nom REAL NOT NULL DEFAULT 0.0,
    environment_nom REAL NOT NULL DEFAULT 0.0,
    d_leader_pacs_nom REAL NOT NULL DEFAULT 0.0,
    r_leader_pacs_nom REAL NOT NULL DEFAULT 0.0,
    d_cand_cmte_nom REAL NOT NULL DEFAULT 0.0,
    r_cand_cmte_nom REAL NOT NULL DEFAULT 0.0,
    d_party_cmte_nom REAL NOT NULL DEFAULT 0.0,
    r_party_cmte_nom REAL NOT NULL DEFAULT 0.0,
    d_joint_cand_cmte_nom REAL NOT NULL DEFAULT 0.0,
    r_joint_cand_cmte_nom REAL NOT NULL DEFAULT 0.0,
    d_cmte_nom REAL NOT NULL DEFAULT 0.0,
    r_cmte_nom REAL NOT NULL DEFAULT 0.0,
    total_pac_nom REAL NOT NULL DEFAULT 0.0,
    total_indiv_nom REAL NOT NULL DEFAULT 0.0,
    total_contrib_nom REAL NOT NULL DEFAULT 0.0,
    PRIMARY KEY (osid, congress)
);

CREATE INDEX IF NOT EXISTS idx_crp_contrib_osid ON crp_contributions(osid);
CREATE INDEX IF NOT EXISTS idx_crp_contrib_congress ON crp_contributions(congress);
CREATE INDEX IF NOT EXISTS idx_crp_contrib_cycle ON crp_contributions(cycle);
