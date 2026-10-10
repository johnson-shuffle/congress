-- ============================================================================
-- cffr_program: Consolidated Federal Funds Report - Federal Programs
-- Source: raw/cffr/*prog.txt (1993–2010)
-- ============================================================================

DROP TABLE IF EXISTS cffr_program;

CREATE TABLE cffr_program (
    year INTEGER NOT NULL,
    prog_id TEXT NOT NULL,
    prog_name TEXT NOT NULL,
    PRIMARY KEY (year, prog_id)
);

CREATE INDEX IF NOT EXISTS idx_cffr_program_id ON cffr_program(prog_id);
