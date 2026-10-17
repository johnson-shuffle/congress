-- ============================================================================
-- election_margins: Comprehensive electoral victory margins (103rd–114th Congress)
-- Synthesizes crosswalk_members with cq_margins and fec_special_elections
-- ============================================================================

DROP VIEW IF EXISTS election_margins;

CREATE VIEW election_margins AS
SELECT
    cw.congress,
    CASE 
        WHEN cw.chamber = 'House' THEN (1990 + 2 * (cw.congress - 102))
        ELSE COALESCE(cq.cycle, (1990 + 2 * (cw.congress - 102)))
    END AS cycle,
    cw.chamber,
    cw.state_code,
    cw.district_number,
    COALESCE(NULLIF(cq.senate_class, 0), cw.senate_class, 0) AS senate_class,
    cw.govtrack_id,
    cw.icpsr_id,
    cw.bioguide_id,
    COALESCE(cq.winner_name, sp.candidate_name, cw.full_name) AS winner_name,
    COALESCE(sp.margin_pct, cq.margin) AS margin_pct,
    COALESCE(sp.margin, cq.margin / 100.0) AS margin,
    CASE WHEN sp.govtrack_id IS NOT NULL THEN 1 ELSE 0 END AS is_special_election
FROM crosswalk_members cw
LEFT JOIN cq_margins cq
    ON cw.congress = cq.congress
   AND cw.chamber = cq.chamber
   AND cw.govtrack_id = cq.govtrack_id
LEFT JOIN fec_special_elections sp
    ON cw.congress = sp.congress
   AND cw.chamber = sp.chamber
   AND cw.govtrack_id = sp.govtrack_id;
