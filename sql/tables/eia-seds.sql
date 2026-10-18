-- ============================================================================
-- eia_seds: Raw State Energy Data System (SEDS) production and raw energy prices
-- Sources: raw/eia/Complete_SEDS.csv, raw/eia/eia_prices.csv
-- ============================================================================

DROP TABLE IF EXISTS eia_seds;
DROP TABLE IF EXISTS eia_prices;

CREATE TABLE eia_seds (
    data_status INTEGER,
    msn TEXT NOT NULL,
    state_code TEXT NOT NULL,
    year INTEGER NOT NULL,
    data REAL
);

CREATE INDEX IF NOT EXISTS idx_eia_seds_state_year ON eia_seds(state_code, year);
CREATE INDEX IF NOT EXISTS idx_eia_seds_msn ON eia_seds(msn);

CREATE TABLE eia_prices (
    state_code TEXT NOT NULL,
    msn TEXT NOT NULL,
    year INTEGER NOT NULL,
    price REAL
);

CREATE INDEX IF NOT EXISTS idx_eia_prices_state_year ON eia_prices(state_code, year);
CREATE INDEX IF NOT EXISTS idx_eia_prices_msn ON eia_prices(msn);
