-- ============================================================================
-- crp_candidates: OpenSecrets / CRP Federal Candidates & Office Status
-- Source: raw/opensecrets/CampaignFin*.zip (cands*.txt, 1990–2016)
-- ============================================================================

DROP TABLE IF EXISTS crp_candidates;

CREATE TABLE crp_candidates (
    cycle INTEGER NOT NULL,
    fec_candidate_id TEXT NOT NULL,
    osid TEXT,
    candidate_name TEXT,
    party TEXT,
    distid_run TEXT,
    distid_cur TEXT,
    cand_cur TEXT,
    cand_cyc TEXT,
    incumbent_code TEXT,
    recip_code TEXT,
    no_pacs TEXT,
    PRIMARY KEY (cycle, fec_candidate_id)
);

CREATE INDEX IF NOT EXISTS idx_crp_cands_osid ON crp_candidates(osid);
CREATE INDEX IF NOT EXISTS idx_crp_cands_cycle ON crp_candidates(cycle);
CREATE INDEX IF NOT EXISTS idx_crp_cands_dist ON crp_candidates(distid_run);
