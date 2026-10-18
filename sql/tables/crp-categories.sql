-- ============================================================================
-- crp_categories: Center for Responsive Politics (CRP) Industry & Category Codes
-- Source: raw/opensecrets/codes_industries.csv
-- ============================================================================

DROP TABLE IF EXISTS crp_categories;

CREATE TABLE crp_categories (
    cat_code TEXT PRIMARY KEY,
    cat_name TEXT NOT NULL,
    cat_group TEXT NOT NULL,
    industry TEXT NOT NULL,
    sector_short TEXT NOT NULL,
    sector_long TEXT NOT NULL,
    cat_group_alt TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_crp_cat_group ON crp_categories(cat_group);
CREATE INDEX IF NOT EXISTS idx_crp_cat_sector ON crp_categories(sector_short);
CREATE INDEX IF NOT EXISTS idx_crp_cat_industry ON crp_categories(industry);
