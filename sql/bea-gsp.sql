-- ============================================================================
-- bea_gsp: Bureau of Economic Analysis Gross Domestic Product by State (GSP)
-- Source: raw/bea/gsp_sic_all.csv (1963–1996) and raw/bea/gsp_naics_all.csv (1997–2014)
-- ============================================================================

DROP TABLE IF EXISTS bea_gsp;

CREATE TABLE bea_gsp (
    geo_fips TEXT NOT NULL,
    geo_name TEXT NOT NULL,
    region INTEGER,
    component_id INTEGER NOT NULL,
    component_name TEXT,
    industry_id INTEGER NOT NULL,
    industry_classification TEXT,
    description TEXT,
    fips INTEGER NOT NULL,
    year INTEGER NOT NULL,
    value REAL,
    units TEXT,
    PRIMARY KEY (year, fips, industry_id, component_id),
    FOREIGN KEY (fips) REFERENCES states(fips)
);

CREATE INDEX IF NOT EXISTS idx_bea_gsp_geo ON bea_gsp(fips, year);
CREATE INDEX IF NOT EXISTS idx_bea_gsp_ind ON bea_gsp(industry_id, component_id);
