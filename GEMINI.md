# Congress Data Pipeline & SQLite Modernization

## Project Objective
Transform legacy congressional research data workflows into a modular, production-ready R ETL pipeline backed by a local SQLite database (`congress.sqlite`).

The repository builds a consolidated data store uniting federal spending, electoral, legislative, energy, and environmental data for congressional districts and states (103rd–114th Congress).

---

## Architecture & Principles

### 1. Database Target
- **Engine:** SQLite (`congress.sqlite`), managed via `DBI` and `RSQLite`.
- **Location:** Ignored by git; created and updated locally via `config.R` (`get_db()`).
- **Foreign Keys:** Enabled via `PRAGMA foreign_keys = ON;`.

### 2. Repository Layout & Pipeline Roles
- **`raw/`**: Strictly genuine upstream raw source files in lowercase domain folders (e.g., `raw/cq/`, `raw/govtrack/`, `raw/eia/`). **100% ignored by git. Never store intermediate or compiled `.Rda` / `.RData` files in `raw/`.**
- **`data/`**: Ingestion ETL scripts that parse genuine raw sources into SQLite database base tables (e.g., `data/states.R`, `data/eia-plants.R`, `data/eia-seds.R`).
- **`analysis/`**: Synthesis and modeling scripts that query database tables to construct crosswalks, master panels, and run models (e.g., `analysis/crosswalk-members.R`, `analysis/eia-states.R`, `analysis/eia-generation.R`).
- **`sql/`**: Modular SQLite DDL files in strict `kebab-case` (e.g., `sql/states.sql`, `sql/eia-plants.sql`, `sql/eia-states.sql`).
- **`docs/`**: Technical documentation, data dictionaries, and architectural specifications.

### 3. Code Style & Naming Conventions
- **Files & Folders:** Strict `kebab-case` for all scripts, folders, and SQL DDL files (e.g., `crosswalk-members.sql`, `eia-plants.R`, `eia-states.R`).
- **Database & Columns:** Strict `snake_case` (e.g., `state_code`, `plant_code`, `net_generation_mwh`). Dot notation (`.`) from legacy files must be converted to underscores (`_`).
- **Modern R Standards:**
  - Native pipe `|>` instead of `%>%` or `%<>%`.
  - Modern `tidyverse` (`dplyr`, `tidyr`, `readr`, `readxl`) over deprecated `plyr`/`reshape2`.
  - Parameterized database operations using `DBI` and central helpers in `config.R`.

### 4. Project Management (Trello)
- Board configuration is located at `.agents/config.json`.
- The CLI tool is executable at `~/.gemini/config/skills/trello/scripts/trello_cli.py`:
  - List cards: `~/.gemini/config/skills/trello/scripts/trello_cli.py list`
  - Move card: `~/.gemini/config/skills/trello/scripts/trello_cli.py move --card-id <ID> --target-list "<LIST>"`
  - Add comment: `~/.gemini/config/skills/trello/scripts/trello_cli.py comment --card-id <ID> --text "<MSG>"`

---

## Current Database State (`congress.sqlite`)

The following 22 tables and views are populated, verified, and active in `congress.sqlite`:

| Table / View | Rows | Role / Pipeline Script | Source Files |
| :--- | :--- | :--- | :--- |
| `states` | 51 | `data/states.R` | `raw/states/us_state_info.csv` |
| `govtrack_info` | 12,770 | `data/govtrack.R` | `raw/legislators/legislators-historical.yaml`, `current.yaml` |
| `govtrack_term` | 45,535 | `data/govtrack.R` | `raw/legislators/legislators-historical.yaml`, `current.yaml` |
| `voteview_members`| 6,567 | `data/voteview.R` | `raw/voteview/H*_members.csv`, `S*_members.csv` |
| `voteview_info` | 23,724 | `data/voteview.R` | `raw/voteview/H*_rollcalls.csv`, `S*_rollcalls.csv` |
| `voteview_cast` | 7,674,580 | `data/voteview.R` | `raw/voteview/H*_votes.csv`, `S*_votes.csv` |
| `voteview_rollcalls` (view) | 23,724 | `data/voteview.R` | Compatibility view on `voteview_info` |
| `voteview_votes` (view) | 7,674,580 | `data/voteview.R` | Compatibility view on `voteview_cast` |
| `crosswalk_members`| 6,567 | `analysis/crosswalk-members.R` | SQLite join (`govtrack_info` + `voteview_members`) |
| `cq_elections` | 6,945 | `data/cq.R` | `raw/cq/` election exports |
| `cq_margins` | 5,596 | `analysis/cq-margins.R` | SQLite query (`cq_elections`) |
| `bls_unempl` | 1,989 | `data/bls.R` | `raw/bls/unemp.txt` |
| `bls_cpi` | 103 | `data/bls.R` | `raw/bls/cpi.txt` |
| `bea_gsp` | 797,130 | `data/bea.R` | `raw/bea/gsp/` |
| `bea_gsi` | 4,386 | `data/bea.R` | `raw/bea/gsi/` |
| `bea_pop` | 4,386 | `data/bea.R` | `raw/bea/gsi/` |
| `congress_sessions`| 574 | `data/congress.R` | `raw/congress/house_sessions.html`, `senate_sessions.html` |
| `congress` (view) | 574 | `data/congress.R` | Compatibility view on `congress_sessions` |
| `carma_district` | 435 | `data/carma.R` | `raw/carma/CARMAv30_PowerUser.zip` (`Region.csv`) |
| `carma` (view) | 870 | `data/carma.R` | Compatibility view on `carma_district` |
| `carma_plants` | 8,690 | `data/carma.R` | `raw/carma/CARMAv30_PowerUser.zip` (`Plant.csv`) |
| `eia_plants` | 7,289 | `data/eia-plants.R` | `raw/eia/eia_plants_gps.csv` |
| `eia_fuel_factors` | 40 | `data/eia-factors.R` | `raw/eia/eia_fuel_type_carbon.csv` |
| `eia_fuel_factors_old`| 16 | `data/eia-factors.R` | `raw/eia/eia_fuel_type_carbon_old.csv` |
| `eia_seds` | 1,524,961 | `data/eia-seds.R` | `raw/eia/Complete_SEDS.csv` |
| `eia_prices` | 250,518 | `data/eia-seds.R` | `raw/eia/eia_prices.csv` |
| `eia_survey_gen` | 218,949 | `data/eia-survey.R` | `raw/eia/f759*u.xls`, `F906*.csv/xls`, `f923_*.csv` |
| `eia_states` | 2,244 | `analysis/eia-states.R` | SQLite query (`eia_seds` + `eia_prices` + `bls_cpi`) |
| `eia_generation` | 217,971 | `analysis/eia-generation.R`| SQLite query (`eia_survey_gen` + `eia_fuel_factors`) |
| `lcv_votes` | 1,296 | `data/lcv-votes.R` | `raw/lcv/lcv-votes.csv` |
| `lcv_scores` | 12,430 | `analysis/lcv-scores.R` | SQLite query (`lcv_votes` + `voteview_info` + `voteview_cast`) |
| `lcv_congress_scores` | 6,560 | `analysis/lcv-scores.R` | SQLite query (`lcv_votes` + `voteview_info` + `voteview_cast`) |
| `retirements` | 6,567 | `analysis/retirements.R` | SQLite query (`crosswalk_members` + `raw/retirements.csv`) |
| `cffr_agency` | 9,979 | `data/cffr.R` | `raw/cffr/*agen.txt` |
| `cffr_program`| 39,191 | `data/cffr.R` | `raw/cffr/*prog.txt` |
| `cffr` | 7,556,231 | `data/cffr.R` | `raw/cffr/*CFFIND/*.csv` |
| `crp_categories` | 443 | `data/opensecrets.R` | `raw/opensecrets/codes_industries.csv` |
| `crp_candidates` | 70,454 | `data/opensecrets.R` | `raw/opensecrets/CampaignFin*.zip` (`cands*.txt`) |
| `crp_committees` | 157,353 | `data/opensecrets.R` | `raw/opensecrets/CampaignFin*.zip` (`cmtes*.txt`) |
| `crp_pacs` | 3,880,045 | `data/opensecrets.R` | `raw/opensecrets/CampaignFin*.zip` (`pacs*.txt`) |
| `eia_plants_district` | 87,096 | `analysis/eia-plants-district.R` | SQLite query (`eia_plants` + `raw/cd-maps/`) |
| `eia_district_emissions` | 9,165 | `analysis/eia-district-emissions.R` | SQLite query (`eia_generation` + `eia_plants_district`) |
| `noaa_degree_days` | 420,768 | `data/noaa.R` | `raw/noaa/` / NOAA CPC FTP |
| `noaa_state_degree_days` (view) | 1,152 | `data/noaa.R` | Analytical view on `noaa_degree_days` |
| `prism_district` | 109,632 | `data/prism.R` | `raw/prism/` (PRISM Climate Group) |
| `prism_district_annual` (view) | 9,073 | `analysis/prism.R` | Analytical view on `prism_district` |
| `census_district` | 10,028 | `data/census.R` | `raw/census/1990_STF3D.csv`, `c*_DP*.csv` |
| `census_state` | 1,173 | `data/census.R` | `raw/census/1990_STF3D.csv`, `c*_DP*.csv` |
| `census` (view) | 11,201 | `data/census.R` | Compatibility view on `census_district` + `census_state` |

---

## Active Task & Next Step

### Completed (`Done` on Trello):
- **Card:** `Migrate: Census & STF3D District Demographics` (ID: `6aca85d4cf073c440abc738c`)
  - Migrated genuine raw sources (`1990_STF3D.csv`, `c106_DP*.csv` through `c113_DP*.csv`) into `raw/census/`.
  - Built unified modern ETL pipeline `data/census.R` and schema `sql/census-district.sql`.
  - Ingested `census_district` (10,028 rows) and `census_state` (1,173 rows) with compatibility view `census` (11,201 rows) spanning 1993–2015.
- **Card:** `4. Curate Spatial & Climate Feature Extraction (Tier 2)` (ID: `6ac9ecb9426eafc98af9d131`)
  - Modernized EIA spatial plant matching across decennial redistricting cycles into `eia_plants_district` (87,096 rows).
  - Synthesized congressional district annual generation and CO2 emissions in `eia_district_emissions` (9,165 rows).
  - Refactored `data/prism.R` with modern `sf` / `terra` zonal raster extraction and self-contained validation harness.
  - Ingested NOAA CPC population-weighted heating and cooling degree days in `noaa_degree_days` (420,768 rows) and view `noaa_state_degree_days` (1,152 rows).
  - Authored comprehensive specification `docs/spatial-climate-specification.md` documenting thermal unit conversions, carbon emissions factors, and BLS CPI deflators.
- **Card:** `ETL: OpenSecrets Campaign Finance Contributions` (ID: `6aca85cd3797128d570b4647`)
  - Converted legacy `data/opensecrets.sh` into modular R/DBI pipeline `data/opensecrets.R` and schemas `sql/crp-*.sql`.
  - Ingested `crp_categories` (443 rows), `crp_candidates` (70,454 rows), `crp_committees` (157,353 rows), and `crp_pacs` (3,880,045 rows) spanning 1990–2016.
- **Card:** `ETL: CFFR Federal Funds Spending` (ID: `6aca85cd6f735187ceed644f`)
  - Converted legacy `data/cffr.sh` into modular R/DBI pipeline `data/cffr.R` and schemas `sql/cffr*.sql`.
  - Ingested `cffr_agency` (9,979 rows), `cffr_program` (39,191 rows), and `cffr` (7,556,231 rows) spanning 1993–2010.
- **Card:** `ETL: LCV Environmental Voting Scores` (ID: `6aca85cc3b75b0c78807a989`)
  - Ingestion: `data/lcv-votes.R` populates `lcv_votes` from raw curated `raw/lcv/lcv-votes.csv`.
  - Synthesis: `analysis/lcv-scores.R` computes `lcv_scores` (annual) and `lcv_congress_scores` (biennial) from SQLite base tables.
- **Card:** `ETL: Voteview Roll Calls & Votes Cast` (ID: `6aca85ceba0bac5c54a419b0`)
  - Extended `data/voteview.R` to ingest `voteview_info` (23,724 roll calls) and `voteview_cast` (7,674,580 votes).
- **Card:** `ETL: Congressional Casualties & Turnover` (ID: `6aca85cc228939089382baa9`)
  - Staged `raw/retirements.csv` (334 curated records, 103rd–114th Congress) with documentation in `data/retirements.txt`.
  - Synthesis: `analysis/retirements.R` synthesizes `retirements` (6,567 terms) with `election_year` and `retire` flags.

### Next Queue Candidates (`To Do` on Trello):
- `5. Refactor Master Panel Assembly (Tier 3)` (ID: `6ac9ecbae38c0d1f0e0b71f7`)
- `6. Showcase Core Bayesian Stan Models (Tier 4)` (ID: `6ac9ecbb2fea4242fd23ce31`)
