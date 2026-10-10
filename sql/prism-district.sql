-- ============================================================================
-- prism_district: PRISM Monthly Congressional District Weather & Climate (1993-2013)
-- Source: PRISM Climate Group 4km gridded rasters & UCLA CD boundary shapefiles
-- Ingestion: data/prism.R
-- ============================================================================

DROP TABLE IF EXISTS prism_district;

CREATE TABLE prism_district (
    year INTEGER NOT NULL,
    month INTEGER NOT NULL,
    congress INTEGER NOT NULL,
    fips INTEGER NOT NULL,
    state_code TEXT NOT NULL,
    district_number INTEGER NOT NULL DEFAULT 0,
    ppt REAL,
    tmin REAL,
    tmax REAL,
    tmean REAL,
    PRIMARY KEY (year, month, state_code, district_number),
    FOREIGN KEY (state_code) REFERENCES states(state_code)
);

CREATE INDEX IF NOT EXISTS idx_prism_dist_cong ON prism_district(congress, state_code, district_number);
CREATE INDEX IF NOT EXISTS idx_prism_dist_year ON prism_district(year, month);
