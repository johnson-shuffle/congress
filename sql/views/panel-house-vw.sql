-- ============================================================================
-- panel_house_vw: Analytical roll-call panel for House environmental votes
-- Joins panel_house_annual with lcv_votes, voteview_cast, and voteview_info.
-- Matches legacy panel_house.Rda "panel" roll-call level matrix (180,500 rows).
-- ============================================================================

DROP VIEW IF EXISTS panel_house_vw;

CREATE VIEW panel_house_vw AS
SELECT
    p.*,
    lcv.rollcall,
    vc.cast_code AS vote_raw,
    CASE
        WHEN vc.cast_code IN (1, 2, 3) THEN 1
        WHEN vc.cast_code IN (4, 5, 6) THEN 0
        ELSE 9
    END AS vote,
    lcv.lcv_vote AS lcv_pos,
    CASE
        WHEN lcv.lcv_vote = 1 AND vc.cast_code IN (1, 2, 3) THEN 1
        WHEN lcv.lcv_vote = 0 AND vc.cast_code IN (4, 5, 6) THEN 1
        WHEN lcv.lcv_vote = 0 AND vc.cast_code IN (1, 2, 3) THEN 0
        WHEN lcv.lcv_vote = 1 AND vc.cast_code IN (4, 5, 6) THEN 0
        ELSE NULL
    END AS vote_lcv,
    vi.date AS vote_date,
    vi.bill_number,
    vi.vote_result,
    vi.vote_desc,
    lcv.lcv_dscr AS lcv_description
FROM lcv_votes lcv
JOIN voteview_cast vc
    ON lcv.congress = vc.congress
   AND lcv.chamber = vc.chamber
   AND lcv.rollnumber = vc.rollnumber
LEFT JOIN voteview_info vi
    ON lcv.congress = vi.congress
   AND lcv.chamber = vi.chamber
   AND lcv.rollnumber = vi.rollnumber
JOIN panel_house_annual p
    ON vc.icpsr_id = p.icpsr_id
   AND vc.congress = p.congress
   AND COALESCE(CAST(substr(vi.date, 1, 4) AS INTEGER), lcv.year) = p.year
WHERE lcv.chamber = 'House'
  AND vc.icpsr_id < 99909
  AND vc.cast_code != 0;
