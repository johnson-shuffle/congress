-- ============================================================================
-- noaa_state_degree_days_vw: Annual State-Level Heating and Cooling Degree Days
-- Sources: noaa_degree_days in congress.sqlite
-- ============================================================================

DROP VIEW IF EXISTS noaa_state_degree_days_vw;

CREATE VIEW noaa_state_degree_days_vw AS
SELECT 
    state_code,
    year,
    ROUND(SUM(cdd), 1) AS cdd,
    ROUND(SUM(hdd), 1) AS hdd
FROM noaa_degree_days
GROUP BY state_code, year;
