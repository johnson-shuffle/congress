-- ============================================================================
-- cffr_agency: Consolidated Federal Funds Report - Federal Agencies
-- Source: raw/cffr/*agen.txt (1993–2010)
-- ============================================================================

DROP TABLE IF EXISTS cffr_agency;

CREATE TABLE cffr_agency (
    year INTEGER NOT NULL,
    agency_code TEXT NOT NULL,
    agency_name TEXT NOT NULL,
    PRIMARY KEY (year, agency_code)
);

CREATE INDEX IF NOT EXISTS idx_cffr_agency_code ON cffr_agency(agency_code);
