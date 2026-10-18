# Congress Data Pipeline & SQLite Modernization

## Project Objective
Transform legacy congressional research data workflows (originally archived in `~/Projects/congress-legacy`) into a modular, production-ready R ETL pipeline backed by a local SQLite database (`congress.sqlite`).

The repository builds a consolidated data store uniting federal spending, electoral, legislative, energy, and environmental data for congressional districts and states (103rd–114th Congress). Its primary analytical objective is to replace the ad-hoc `.Rda`-based master panel assembly scripts (`~/Projects/congress-legacy/Data/panel_house.R` and `panel_senate.R`) and feed the core Bayesian Stan estimation models directly from reproducible SQLite tables and analytical views.

---

## Architecture & Principles

### 1. Database Target
- **Engine:** SQLite (`congress.sqlite`), managed via `DBI` and `RSQLite`.
- **Location:** Ignored by git; created and updated locally via `config.R` (`get_db()`).
- **Foreign Keys:** Enabled via `PRAGMA foreign_keys = ON;`.

### 2. Repository Layout & Pipeline Roles
- **`raw/`**: Strictly genuine upstream raw source files in lowercase domain folders (e.g., `raw/cq/`, `raw/govtrack/`, `raw/eia/`). **100% ignored by git. Never store intermediate or compiled `.Rda` / `.RData` files in `raw/`.**
- **`data/`**: Ingestion ETL scripts that parse genuine raw sources into SQLite database base tables (e.g., `data/states.R`, `data/eia-plants.R`, `data/eia-seds.R`). Scripts in `data/` must not depend on other database tables, with the sole exception of the bootstrap `states` lookup table (`sql/states.sql` / `data/states.R`), which must be run first.
- **`analysis/`**: Synthesis and modeling scripts that query database tables to construct crosswalks, master panels, and run models (e.g., `analysis/crosswalk-members.R`, `analysis/fec-margins.R`, `analysis/eia-states.R`, `analysis/eia-generation.R`).
- **`sql/`**: Modular SQLite DDL files in strict `kebab-case`:
  - **`sql/states.sql`**: Foundational bootstrap schema at the root of `sql/`, initialized first before any other tables.
  - **`sql/tables/`**: Physical table schemas (e.g., `sql/tables/eia-plants.sql`, `sql/tables/eia-states.sql`).
  - **`sql/views/`**: Analytical SQLite view definitions in dedicated `*-vw.sql` files (e.g., `sql/views/election-margins-vw.sql`) with the `_vw` suffix on the view name (e.g., `election_margins_vw`).
- **`docs/`**: Technical documentation, data dictionaries, and architectural specifications.

### 3. Code Style & Naming Conventions
- **Files & Folders:** Strict `kebab-case` for all scripts, folders, and SQL DDL files (e.g., `crosswalk-members.sql`, `election-margins-vw.sql`, `eia-plants.R`, `eia-states.R`).
- **Database & Columns:** Strict `snake_case` (e.g., `state_code`, `plant_code`, `net_generation_mwh`), with `_vw` suffix for analytical views (e.g., `election_margins_vw`). Dot notation (`.`) from legacy files must be converted to underscores (`_`).
- **Modern R Standards:**
  - Native pipe `|>` instead of `%>%` or `%<>%`.
  - Modern `tidyverse` (`dplyr`, `tidyr`, `readr`, `readxl`) over deprecated `plyr`/`reshape2`.
  - Parameterized database operations using `DBI` and central helpers in `config.R` (standardized on `execute_sql_file(db, file.path(path_sql, "tables", "<schema>.sql"))` and `execute_sql_file(db, file.path(path_sql, "views", "<view>-vw.sql"))` for DDL execution).

### 4. Project Management (Trello)
- Board configuration is located at `.agents/config.json`.
- The CLI tool is executable at `~/.gemini/config/skills/trello/scripts/trello_cli.py`:
  - List cards: `~/.gemini/config/skills/trello/scripts/trello_cli.py list`
  - Move card: `~/.gemini/config/skills/trello/scripts/trello_cli.py move --card-id <ID> --target-list "<LIST>"`
  - Add comment: `~/.gemini/config/skills/trello/scripts/trello_cli.py comment --card-id <ID> --text "<MSG>"`

---

## Current Database State (`congress.sqlite`)

The following tables and views are populated, verified, and active in `congress.sqlite`:

### Tables

| Asset | Rows | Role / Pipeline Script | Source Files |
| :--- | :--- | :--- | :--- |
| `states` | 51 | `data/states.R` (`sql/states.sql`) | `raw/states/us-state-info.csv` |
| `govtrack_info` | 12,770 | `data/govtrack.R` (`sql/tables/govtrack-info.sql`) | `raw/govtrack/legislators-historical.yaml`, `legislators-current.yaml` |
| `govtrack_term` | 45,535 | `data/govtrack.R` (`sql/tables/govtrack-term.sql`) | `raw/govtrack/legislators-historical.yaml`, `legislators-current.yaml` |
| `voteview_members`| 6,567 | `data/voteview.R` (`sql/tables/voteview-members.sql`) | `raw/voteview/H*_members.csv`, `S*_members.csv` |
| `voteview_info` | 23,724 | `data/voteview.R` (`sql/tables/voteview-info.sql`) | `raw/voteview/H*_rollcalls.csv`, `S*_rollcalls.csv` |
| `voteview_cast` | 7,674,580 | `data/voteview.R` (`sql/tables/voteview-cast.sql`) | `raw/voteview/H*_votes.csv`, `S*_votes.csv` |
| `crosswalk_members`| 6,567 | `analysis/crosswalk-members.R` (`sql/tables/crosswalk-members.sql`) | SQLite join (`govtrack_info` + `govtrack_term` + `voteview_members`) |
| `cq_elections` | 6,945 | `data/cq.R` (`sql/tables/cq-elections.sql`) | `raw/cq/` election exports |
| `cq_margins` | 5,596 | `analysis/cq-margins.R` (`sql/tables/cq-margins.sql`) | SQLite query (`cq_elections`) |
| `bls_unempl` | 1,989 | `data/bls.R` (`sql/tables/bls-unempl.sql`) | `raw/bls/unemp.txt` |
| `bls_cpi` | 103 | `data/bls.R` (`sql/tables/bls-cpi.sql`) | `raw/bls/cpi.txt` |
| `bea_gsp` | 797,130 | `data/bea.R` (`sql/tables/bea-gsp.sql`) | `raw/bea/gsp_naics_all.csv`, `gsp_sic_all.csv` |
| `bea_gsi` | 4,386 | `data/bea.R` (`sql/tables/bea-gsi.sql`) | `raw/bea/SA1_1929_2014.csv` |
| `bea_pop` | 4,386 | `data/bea.R` (`sql/tables/bea-pop.sql`) | `raw/bea/SA1_1929_2014.csv` |
| `congress_sessions`| 574 | `data/congress.R` (`sql/tables/congress-sessions.sql`) | `raw/congress/house_sessions.html`, `senate_sessions.html` |
| `carma_district` | 435 | `data/carma.R` (`sql/tables/carma-district.sql`) | `raw/carma/CARMAv30_PowerUser.zip` (`Region.csv`) |
| `carma_plants` | 8,690 | `data/carma.R` (`sql/tables/carma-plants.sql`) | `raw/carma/CARMAv30_PowerUser.zip` (`Plant.csv`) |
| `eia_plants` | 7,289 | `data/eia-plants.R` (`sql/tables/eia-plants.sql`) | `raw/eia/eia_plants_gps.csv` |
| `eia_fuel_factors` | 40 | `data/eia-factors.R` (`sql/tables/eia-factors.sql`) | `raw/eia/eia_fuel_type_carbon.csv` |
| `eia_fuel_factors_old`| 16 | `data/eia-factors.R` (`sql/tables/eia-factors.sql`) | `raw/eia/eia_fuel_type_carbon_old.csv` |
| `eia_seds` | 1,524,961 | `data/eia-seds.R` (`sql/tables/eia-seds.sql`) | `raw/eia/Complete_SEDS.csv` |
| `eia_prices` | 250,518 | `data/eia-seds.R` (`sql/tables/eia-seds.sql`) | `raw/eia/eia_prices.csv` |
| `eia_survey_gen` | 218,949 | `data/eia-survey.R` (`sql/tables/eia-survey.sql`) | `raw/eia/f759*u.xls`, `F906*.csv/xls`, `f923_*.csv` |
| `eia_states` | 2,244 | `analysis/eia-states.R` (`sql/tables/eia-states.sql`) | SQLite query (`eia_seds` + `eia_prices` + `bls_cpi`) |
| `eia_generation` | 217,971 | `analysis/eia-generation.R` (`sql/tables/eia-generation.sql`) | SQLite query (`eia_survey_gen` + `eia_fuel_factors`) |
| `lcv_votes` | 1,296 | `data/lcv-votes.R` (`sql/tables/lcv-votes.sql`) | `raw/lcv/lcv-votes.csv` |
| `lcv_scores` | 12,430 | `analysis/lcv-scores.R` (`sql/tables/lcv-scores.sql`) | SQLite query (`lcv_votes` + `voteview_info` + `voteview_cast`) |
| `lcv_congress_scores` | 6,560 | `analysis/lcv-scores.R` (`sql/tables/lcv-scores.sql`) | SQLite query (`lcv_votes` + `voteview_info` + `voteview_cast`) |
| `retirements` | 6,567 | `analysis/retirements.R` (`sql/tables/retirements.sql`) | SQLite query (`crosswalk_members` + `raw/retirements.csv`) |
| `cffr_agency` | 9,979 | `data/cffr.R` (`sql/tables/cffr-agency.sql`) | `raw/cffr/*agen.txt` |
| `cffr_program`| 39,191 | `data/cffr.R` (`sql/tables/cffr-program.sql`) | `raw/cffr/*prog.txt` |
| `cffr` | 7,556,231 | `data/cffr.R` (`sql/tables/cffr.sql`) | `raw/cffr/*CFFIND/*.csv` |
| `crp_categories` | 443 | `data/opensecrets.R` (`sql/tables/crp-categories.sql`) | `raw/opensecrets/codes_industries.csv` |
| `crp_candidates` | 70,454 | `data/opensecrets.R` (`sql/tables/crp-candidates.sql`) | `raw/opensecrets/CampaignFin*.zip` (`cands*.txt`) |
| `crp_committees` | 157,353 | `data/opensecrets.R` (`sql/tables/crp-committees.sql`) | `raw/opensecrets/CampaignFin*.zip` (`cmtes*.txt`) |
| `crp_pacs` | 3,880,045 | `data/opensecrets.R` (`sql/tables/crp-pacs.sql`) | `raw/opensecrets/CampaignFin*.zip` (`pacs*.txt`) |
| `crp_pac_other` | 1,113,501 | `data/opensecrets.R` (`sql/tables/crp-pac-other.sql`) | `raw/opensecrets/CampaignFin*.zip` (`pac_other*.txt`) |
| `crp_indivs` | 43,441,148 | `data/opensecrets.R` (`sql/tables/crp-indivs.sql`) | `raw/opensecrets/CampaignFin*.zip` (`indivs*.txt`) |
| `eia_plants_district` | 87,096 | `analysis/eia-plants-district.R` (`sql/tables/eia-plants-district.sql`) | SQLite query (`eia_plants` + `raw/cd-maps/districts*.zip`) |
| `eia_district_emissions` | 9,165 | `analysis/eia-district-emissions.R` (`sql/tables/eia-district-emissions.sql`) | SQLite query (`eia_generation` + `eia_plants_district`) |
| `noaa_degree_days` | 420,768 | `data/noaa.R` (`sql/tables/noaa-degree-days.sql`) | `raw/noaa/*/*.txt` (NOAA CPC FTP) |
| `prism_district` | 109,632 | `data/prism.R` (`sql/tables/prism-district.sql`) | `raw/prism/` + `raw/cd-maps/districts*.zip` |
| `census_district` | 10,028 | `data/census.R` (`sql/tables/census-district.sql`) | `raw/census/1990_STF3D.csv`, `c*_DP*.csv` |
| `census_state` | 1,173 | `data/census.R` (`sql/tables/census-district.sql`) | `raw/census/1990_STF3D.csv`, `c*_DP*.csv` |
| `fec_special_elections` | 121 | `data/fec.R` (`sql/tables/fec-special-elections.sql`) | `raw/fec/special_elections_house.csv`, `special_elections_senate.csv` |
| `fec_presidential_margins` | 306 | `data/fec.R` (`sql/tables/fec-presidential-margins.sql`) | `raw/fec/margins_presidents.csv` |
| `fec_elections` | 16,110 | `data/fec.R` (`sql/tables/fec-elections.sql`) | `raw/fec/results04.xls` through `results14.xls` |
| `fec_margins_2014` | 474 | `analysis/fec-margins.R` (`sql/tables/fec-margins-2014.sql`) | SQLite query (`fec_elections` + `crosswalk_members`) |

### Views

| Asset | Rows | Role / Pipeline Script | Source Files |
| :--- | :--- | :--- | :--- |
| `noaa_state_degree_days_vw` | 1,152 | `data/noaa.R` (`sql/views/noaa-state-degree-days-vw.sql`) | Analytical view on `noaa_degree_days` |
| `prism_district_annual_vw` | 9,073 | `analysis/prism.R` (`sql/views/prism-district-annual-vw.sql`) | Analytical view on `prism_district` |
| `election_margins_vw` | 6,567 | `analysis/fec-margins.R` (`sql/views/election-margins-vw.sql`) | Analytical view on `crosswalk_members` + `cq_margins` + `fec_special_elections` |

---

## Master Panel Source Coverage (`panel_house.R` & `panel_senate.R`)

All upstream data domains required by legacy `~/Projects/congress-legacy/Data/panel_house.R` and `panel_senate.R` are mapped below (non-panel `Migrate:` cards on Trello are audited in `docs/non-panel-data-sources.md`):

| Panel Section | Legacy Input (`.Rda` / `.csv`) | Status in `congress.sqlite` |
| :--- | :--- | :--- |
| **1. Roll Call Votes** | `rolls_house_103_114_long.Rda`, `rolls_senate_103_114_long.Rda` | ✅ `voteview_cast`, `voteview_info` |
| **2. LCV Roll Calls** | `lcv_votes.Rda`, `GovTrack_lcv_votes.Rda` | ✅ `lcv_votes` |
| **3. Legislators & States** | `names_house.Rda`, `names_senate.Rda`, `us_state_info.csv` | ✅ `crosswalk_members`, `states` |
| **4. BEA Income / GSP** | `bea_income.Rda` | ✅ `bea_gsi`, `bea_gsp`, `bea_pop` |
| **5. BLS CPI & Unempl** | `bls_cpi.Rda`, `bls_unempl.Rda` | ✅ `bls_cpi`, `bls_unempl` |
| **6. Census Demographics** | `census_all.Rda` | ✅ `census_district`, `census_state` |
| **7. CRP Contributions** | `crp_contrib_totals.Rda` (`rbind(indivs, pacs.group)`) | ✅ `crp_categories`, `crp_candidates`, `crp_committees`, `crp_pacs`, `crp_pac_other`, `crp_indivs` |
| **8. EIA Energy, Prices, CO2**| `eia_energy.Rda`, `eia_prices.Rda`, `eia_co2_district.Rda`, `eia_co2_state.Rda` | ✅ `eia_states`, `eia_seds`, `eia_prices`, `eia_district_emissions` |
| **9. Election Margins** | `margins_house.Rda`, `margins_senate.Rda`, `special_elections_*.csv`, `margins_2014.Rda` | ✅ `cq_margins`, `fec_special_elections`, `fec_margins_2014`, `election_margins_vw` |
| **10. Retirements** | `RollCall_retire.Rda` | ✅ `retirements` |
| **11. LCV & DW-NOMINATE** | `lcv_scores_*.Rda`, `HL01113D21_BSSE.DAT` | ✅ `lcv_congress_scores`, `voteview_members` |
| **12. Climate Degree Days** | `prism_deg_days.Rda` (House), `noaa_deg_days.Rda` (Senate) | ✅ `prism_district_annual_vw`, `noaa_state_degree_days_vw` |

---

## Active Task & Next Step

### Completed (`Done` on Trello):
- **Card:** `ETL: FEC Federal Elections & Margins` (ID: `6acad29abcbe99e6325b0f72`)
  - Staged genuine raw sources (`results04.xls` through `results14.xls`, `special_elections_*.csv`, `margins_presidents.csv`) into `raw/fec/`.
  - Built raw ETL pipeline `data/fec.R`, synthesis script `analysis/fec-margins.R`, and schemas `sql/tables/fec-*.sql` + `sql/views/election-margins-vw.sql`.
  - Ingested `fec_special_elections` (121 rows), `fec_presidential_margins` (306 rows), and `fec_elections` (16,110 candidate records) via `data/fec.R`.
  - Synthesized `fec_margins_2014` (474 rows) and master analytical view `election_margins_vw` (6,567 rows) via `analysis/fec-margins.R`.
- **Card:** `Migrate: Census & STF3D District Demographics` (ID: `6aca85d4cf073c440abc738c`)
  - Migrated genuine raw sources (`1990_STF3D.csv`, `c106_DP*.csv` through `c113_DP*.csv`) into `raw/census/`.
  - Built unified modern ETL pipeline `data/census.R` and schema `sql/tables/census-district.sql`.
  - Ingested `census_district` (10,028 rows) and `census_state` (1,173 rows) spanning 1993–2015.
- **Card:** `4. Curate Spatial & Climate Feature Extraction (Tier 2)` (ID: `6ac9ecb9426eafc98af9d131`)
  - Modernized EIA spatial plant matching across decennial redistricting cycles into `eia_plants_district` (87,096 rows).
  - Synthesized congressional district annual generation and CO2 emissions in `eia_district_emissions` (9,165 rows).
  - Refactored `data/prism.R` with modern `sf` / `terra` zonal raster extraction and self-contained validation harness.
  - Ingested NOAA CPC population-weighted heating and cooling degree days in `noaa_degree_days` (420,768 rows) and view `noaa_state_degree_days_vw` (1,152 rows).
  - Authored comprehensive specification `docs/spatial-climate-specification.md` documenting thermal unit conversions, carbon emissions factors, and BLS CPI deflators.
- **Card:** `ETL: OpenSecrets Campaign Finance Contributions` (ID: `6aca85cd3797128d570b4647`)
  - Converted legacy `data/opensecrets.sh` into modular R/DBI pipeline `data/opensecrets.R` and schemas `sql/tables/crp-*.sql`.
  - Ingested `crp_categories` (443 rows), `crp_candidates` (70,454 rows), `crp_committees` (157,353 rows), `crp_pacs` (3,880,045 rows), `crp_pac_other` (1,113,501 rows), and `crp_indivs` (43,441,148 rows) spanning 1990–2016.
- **Card:** `ETL: CFFR Federal Funds Spending` (ID: `6aca85cd6f735187ceed644f`)
  - Converted legacy `data/cffr.sh` into modular R/DBI pipeline `data/cffr.R` and schemas `sql/tables/cffr*.sql`.
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
- **Immediate Next Step:** Build `analysis/opensecrets.R` (and `sql/tables/crp-contributions.sql` / view) to synthesize candidate-level `crp_indivs` + `crp_pacs` contribution totals (replacing legacy `crp_contrib_totals.R`).
- `5. Refactor Master Panel Assembly (Tier 3)` (ID: `6ac9ecbae38c0d1f0e0b71f7`)
- `6. Showcase Core Bayesian Stan Models (Tier 4)` (ID: `6ac9ecbb2fea4242fd23ce31`)
