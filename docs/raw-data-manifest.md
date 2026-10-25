# Upstream Raw Data Manifest & Replication Inventory

## 1. Executive Summary & Pipeline Architecture

This document provides a comprehensive inventory and replication manifest for the 27 upstream raw data feeds feeding the modernized congressional research pipeline (`congress.sqlite`). It establishes provenance, canonical upstream endpoints, ingestion schemas, licensing conditions, and step-by-step pipeline execution instructions.

### 1.1 Data Pipeline Philosophy

```
+-----------------------------------------------------------------------------------+
| 27 Upstream Raw Source Feeds (raw/*) [Git-Ignored, ~5.1 GB total footprint]       |
+-----------------------------------------------------------------------------------+
                                          |
                                          v  (Deterministic Ingestion ETL via DBI)
+-----------------------------------------------------------------------------------+
| Modular SQLite Base Tables (data/*.R -> sql/tables/*.sql)                         |
|   - 45+ base physical tables in congress.sqlite                                   |
|   - Strict types, snake_case column names, primary keys, and foreign keys         |
+-----------------------------------------------------------------------------------+
                                          |
                                          v  (Relational Joins & Derived Aggregations)
+-----------------------------------------------------------------------------------+
| Secondary Synthesis & Master Panels (synthesis/*.R -> sql/tables/*.sql)          |
|   - crosswalk_members, cq_margins, fec_margins_2014, crp_contributions            |
|   - eia_states, eia_district_emissions, lcv_scores, retirements                   |
|   - panel_house_annual (10,189 rows x 127 cols), panel_senate_annual (2,360 rows) |
+-----------------------------------------------------------------------------------+
                                          |
                                          v  (Analytical Roll-Call Views)
+-----------------------------------------------------------------------------------+
| Analytical Views (sql/views/*-vw.sql)                                             |
|   - panel_house_vw (180,418 vote rows)                                           |
|   - panel_senate_vw (28,868 vote rows)                                           |
+-----------------------------------------------------------------------------------+
                                          |
                                          v  (Econometric Estimation & MCMC)
+-----------------------------------------------------------------------------------+
| Bayesian Stan Models (analysis/stan/*.stan & analysis/model-*.R)                  |
|   - Hierarchical 2PL IRT Structural Ideal Point Model                             |
|   - Campaign Contribution Vote-Level Inducements Random-Effects Model            |
+-----------------------------------------------------------------------------------+
```

---

## 2. Master Manifest Summary Table (All 27 Source Feeds)

| Feed # | Feed Name | Primary Domain | Upstream Provider / Source | Raw Staging Path (`raw/`) | SQLite Target Table(s) | ETL Script | Temporal Range | License / Redistribution |
| :---: | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **01** | State Reference Metadata | Reference | U.S. Census / OMB | `raw/states/us-state-info.csv` | `states` | `data/states.R` | Static (50 states + DC) | Public Domain |
| **02** | Official Legislators Database | Roster | @unitedstates Project | `raw/govtrack/legislators-*.yaml` | `govtrack_info`, `govtrack_term` | `data/govtrack.R` | 1789–2025 (1st–119th) | CC0 1.0 (Public Domain) |
| **03** | Voteview Member Rosters & NOMINATE | Ideology | Voteview / Lewis et al. | `raw/voteview/H*_members.csv`, `S*_members.csv` | `voteview_members` | `data/voteview.R` | 1993–2016 (103rd–114th) | Open Access (Academic) |
| **04** | Voteview Roll Call Descriptions | Voting | Voteview / Lewis et al. | `raw/voteview/H*_rollcalls.csv`, `S*_rollcalls.csv` | `voteview_info` | `data/voteview.R` | 1993–2016 (103rd–114th) | Open Access (Academic) |
| **05** | Voteview Member Cast Votes | Voting | Voteview / Lewis et al. | `raw/voteview/H*_votes.csv`, `S*_votes.csv` | `voteview_cast` | `data/voteview.R` | 1993–2016 (103rd–114th) | Open Access (Academic) |
| **06** | CQ Elections Collection | Elections | CQ Press / SAGE Publishing | `raw/cq/*.csv` | `cq_elections` | `data/cq.R` | 1988–2016 | Commercial / Proprietary |
| **07** | FEC Federal Election Results | Elections | Federal Election Commission | `raw/fec/results*.xls`, `special_*.csv`, `margins_*.csv` | `fec_elections`, `fec_special_elections`, `fec_presidential_margins` | `data/fec.R` | 1992–2014 (58th–64th) | Public Domain (U.S. Gov) |
| **08** | EIA Form 860 Power Plants | Energy | Energy Information Admin. | `raw/eia/eia_plants_gps.csv` | `eia_plants` | `data/eia-plants.R` | 1990–2014 | Public Domain (U.S. Gov) |
| **09** | EIA / EPA Carbon Fuel Factors | Energy | EPA (40 CFR 98) / EIA | `raw/eia/eia_fuel_type_carbon*.csv` | `eia_fuel_factors`, `eia_fuel_factors_old` | `data/eia-factors.R` | Static Reference | Public Domain (U.S. Gov) |
| **10** | EIA Monthly Generation Surveys | Energy | EIA (Forms 759, 906, 920, 923) | `raw/eia/f759*u.xls`, `F906*.csv/xls`, `f923_*.csv` | `eia_survey_gen` | `data/eia-survey.R` | 1990–2014 | Public Domain (U.S. Gov) |
| **11** | EIA State Energy Data System | Energy | EIA (SEDS) | `raw/eia/Complete_SEDS.csv` | `eia_seds` | `data/eia-seds.R` | 1960–2014 | Public Domain (U.S. Gov) |
| **12** | EIA State Energy Prices | Energy | EIA (SEDS Prices) | `raw/eia/eia_prices.csv` | `eia_prices` | `data/eia-seds.R` | 1970–2014 | Public Domain (U.S. Gov) |
| **13** | CARMA Carbon Emissions v3.0 | Emissions | Center for Global Development | `raw/carma/CARMAv30_PowerUser.zip` | `carma_district`, `carma_plants` | `data/carma.R` | 2000, 2004, 2009, 2014 | Creative Commons (CGD) |
| **14** | UCLA Congressional District GIS | Geography | Lewis et al. / UCLA | `raw/cd-maps/districts1*.zip` | Spatial base for `eia_plants_district`, `prism_district` | `data/cd-maps.R` | 103rd–114th Congress | Open Academic Use |
| **15** | NOAA Degree Days | Climate | NOAA Climate Prediction Center | `raw/noaa/*/*.txt` | `noaa_degree_days`, `noaa_state_degree_days_vw` | `data/noaa.R` | 1993–2016 | Public Domain (U.S. Gov) |
| **16** | PRISM Gridded Climate Rasters | Climate | PRISM Climate Group (OSU) | `raw/prism/{ppt,tmin,tmax,tmean}/*.bil` | `prism_district`, `prism_district_annual_vw` | `data/prism.R` | 1993–2015 | Academic / Educational |
| **17** | OpenSecrets Campaign Finance | Finance | Center for Responsive Politics | `raw/opensecrets/CampaignFin*.zip`, `codes_industries.csv` | `crp_categories`, `crp_candidates`, `crp_committees`, `crp_pacs`, `crp_pac_other`, `crp_indivs` | `data/opensecrets.R` | 1990–2016 (Cycles) | CC BY-NC-SA 4.0 |
| **18** | LCV National Environmental Scorecards | Ideology | League of Conservation Voters | `raw/lcv/lcv-votes.csv` | `lcv_votes`, `lcv_scores`, `lcv_congress_scores` | `data/lcv-votes.R` | 1993–2016 (103rd–114th) | Open Access (Public Scorecard) |
| **19** | Roll Call Casualties & Retirements | Roster | RollCall.com Archive | `raw/retirements.csv` | `retirements` | `synthesis/retirements.R` | 1993–2016 (103rd–114th) | Curated Research Archive |
| **20** | BLS Local Area Unemployment | Macro | Bureau of Labor Statistics | `raw/bls/unemp.txt` | `bls_unempl` | `data/bls.R` | 1976–2014 | Public Domain (U.S. Gov) |
| **21** | BLS Consumer Price Index (CPI-U) | Macro | Bureau of Labor Statistics | `raw/bls/cpi.txt` | `bls_cpi` | `data/bls.R` | 1913–2015 | Public Domain (U.S. Gov) |
| **22** | BEA Gross State Product (GSP) | Macro | Bureau of Economic Analysis | `raw/bea/gsp_naics_all.csv`, `gsp_sic_all.csv` | `bea_gsp` | `data/bea.R` | 1963–2014 (SIC & NAICS) | Public Domain (U.S. Gov) |
| **23** | BEA State Personal Income & Pop | Macro | Bureau of Economic Analysis | `raw/bea/SA1_1929_2014.csv` | `bea_gsi`, `bea_pop` | `data/bea.R` | 1929–2014 | Public Domain (U.S. Gov) |
| **24** | Consolidated Federal Funds (CFFR) | Spending | U.S. Census Bureau (Historical) | `raw/cffr/*CFFIND/*.csv`, `*agen.txt`, `*prog.txt` | `cffr_agency`, `cffr_program`, `cffr` | `data/cffr.R` | 1993–2010 | Public Domain (U.S. Gov) |
| **25** | Congressional Calendar Sessions | Calendar | House & Senate Historians | `raw/congress/house_sessions.html`, `senate_sessions.html` | `congress_sessions` | `data/congress.R` | 1789–Present (1st–119th) | Public Domain (U.S. Gov) |
| **26** | Census 1990 Summary Tape File 3D | Demographics | U.S. Census Bureau | `raw/census/1990_STF3D.csv` | `census_district`, `census_state` | `data/census.R` | 1990 Decennial (103rd Cong) | Public Domain (U.S. Gov) |
| **27** | Census 2000 & ACS CD Profiles | Demographics | U.S. Census Bureau | `raw/census/c106_DP*.csv` through `c113_DP*.csv` | `census_district`, `census_state` | `data/census.R` | 2000–2014 (106th–113th) | Public Domain (U.S. Gov) |

---

## 3. Detailed Data Feed Profiles

### 3.1 Domain A: Congressional, Electoral & Biographical Feeds

#### Feed 01: State Reference Metadata
- **Canonical Upstream URL:** U.S. Census Bureau Federal Information Processing Standards (FIPS) Publication 5-2.
- **Provider:** U.S. Census Bureau / Office of Management and Budget (OMB).
- **Staged File:** `raw/states/us-state-info.csv` (~4 KB, 51 rows).
- **Target Schema:** `states` (`sql/states.sql`), populated via `data/states.R`.
- **Primary Keys:** `fips` (INTEGER), `state_code` (TEXT).
- **Core Columns:** `fips`, `state_code`, `state_name`, `census_region`, `census_division`, `latitude`, `longitude`.
- **License / Access:** Public Domain (U.S. Federal Government).
- **Acquisition Protocol:** Static reference file; bootstrap dependency for all spatial and state-level joins in `congress.sqlite`.

#### Feed 02: Official Congressional Legislators Database
- **Canonical Upstream URL:** `https://github.com/unitedstates/congress-legislators`
  - Historical: `https://theunitedstates.io/congress-legislators/legislators-historical.yaml`
  - Current: `https://theunitedstates.io/congress-legislators/legislators-current.yaml`
- **Provider:** @unitedstates Project (Civic Tech Open Data Collaborative).
- **Staged Files:** `raw/govtrack/legislators-historical.yaml` (~8.5 MB), `raw/govtrack/legislators-current.yaml` (~1.1 MB).
- **Target Schema:** `govtrack_info` (`sql/tables/govtrack-info.sql`) and `govtrack_term` (`sql/tables/govtrack-term.sql`), populated via `data/govtrack.R`.
- **Primary Keys:** `govtrack_info.govtrack_id`; `govtrack_term.(govtrack_id, term_order)`.
- **Cross-Identifier Crosswalk:** Bioguide ID, Thomas ID, GovTrack ID, OpenSecrets (CRP) ID, VoteSmart ID, ICPSR ID, FEC IDs.
- **License / Access:** CC0 1.0 Universal (Public Domain Dedication).
- **Acquisition Protocol:** Direct HTTPS download via `curl` or automated git pull.

#### Feed 03: Voteview Congressional Member Rosters & DW-NOMINATE
- **Canonical Upstream URL:** `https://voteview.com/data`
  - Endpoints: `https://voteview.com/static/data/out/members/H{congress}_members.csv`, `S{congress}_members.csv`
- **Provider:** Voteview.com (Jeffrey B. Lewis, Keith T. Poole, Howard Rosenthal, Royce Carroll, Chris Hare, J. Steven Romer).
- **Staged Files:** `raw/voteview/H*_members.csv`, `raw/voteview/S*_members.csv` (103rd–114th Congress, ~2.5 MB total).
- **Target Schema:** `voteview_members` (`sql/tables/voteview-members.sql`), populated via `data/voteview.R`.
- **Primary Keys:** `(congress, icpsr, chamber)`.
- **Core Columns:** `icpsr`, `bioname`, `party_code`, `state_abbrev`, `district_code`, `nominate_dim1`, `nominate_dim2`, `nominate_log_likelihood`, `occupancy`, `last_means`.
- **License / Access:** Open Access for academic research. Citation required.

#### Feed 04: Voteview Roll Call Descriptions & Metadata
- **Canonical Upstream URL:** `https://voteview.com/static/data/out/rollcalls/H{congress}_rollcalls.csv`, `S{congress}_rollcalls.csv`
- **Provider:** Voteview.com.
- **Staged Files:** `raw/voteview/H*_rollcalls.csv`, `raw/voteview/S*_rollcalls.csv` (103rd–114th Congress, ~18 MB total).
- **Target Schema:** `voteview_info` (`sql/tables/voteview-info.sql`), populated via `data/voteview.R`.
- **Primary Keys:** `(congress, chamber, rollnumber)`.
- **Core Columns:** `congress`, `chamber`, `rollnumber`, `date`, `session`, `clerk_rollnumber`, `bill_number`, `vote_result`, `vote_desc`, `vote_question`.
- **License / Access:** Open Access for academic research.

#### Feed 05: Voteview Member Cast Votes Records
- **Canonical Upstream URL:** `https://voteview.com/static/data/out/votes/H{congress}_votes.csv`, `S{congress}_votes.csv`
- **Provider:** Voteview.com.
- **Staged Files:** `raw/voteview/H*_votes.csv`, `raw/voteview/S*_votes.csv` (103rd–114th Congress, ~182 MB total, 7.67 million vote records).
- **Target Schema:** `voteview_cast` (`sql/tables/voteview-cast.sql`), populated via `data/voteview.R`.
- **Primary Keys:** `(congress, chamber, rollnumber, icpsr)`.
- **Core Columns:** `congress`, `chamber`, `rollnumber`, `icpsr`, `cast_code` (1=Yea, 6=Nay, 7/8/9=Absent/Abstain).
- **License / Access:** Open Access for academic research.

#### Feed 06: CQ Press Voting and Elections Collection
- **Canonical Upstream URL:** `http://library.cqpress.com/elections/`
- **Provider:** Congressional Quarterly Inc. / SAGE Publishing.
- **Staged Files:** `raw/cq/*.csv` (73 CSV export files, ~2.9 MB total, covering House, Senate, Gubernatorial, and Presidential election returns 1988–2016).
- **Target Schema:** `cq_elections` (`sql/tables/cq-elections.sql`), populated via `data/cq.R`; feeds `cq_margins` (`sql/tables/cq-margins.sql`) via `synthesis/cq-margins.R`.
- **Primary Keys:** `(race_year, office, state, district, candidate_name)`.
- **License / Access:** Proprietary commercial subscription. Re-distribution of raw CQ exports is restricted by SAGE license terms. **Derived aggregated analytical indicators (vote totals, margins of victory, candidate names) in SQLite master panels are permissible under academic fair use.**

#### Feed 07: FEC Federal Election Results & Margins
- **Canonical Upstream URL:** `https://www.fec.gov/introduction-campaign-finance/election-and-voting-information/`
  - Federal Elections Reports: `results04.xls` through `results14.xls`
  - Special elections and presidential county margins: official state certifications.
- **Provider:** Federal Election Commission (FEC).
- **Staged Files:** `raw/fec/results*.xls` (2004–2014), `raw/fec/special_elections_house.csv`, `raw/fec/special_elections_senate.csv`, `raw/fec/margins_presidents.csv` (~6.5 MB total).
- **Target Schema:** `fec_elections`, `fec_special_elections`, `fec_presidential_margins` (`sql/tables/fec-*.sql`), populated via `data/fec.R`.
- **License / Access:** Public Domain (U.S. Federal Government).

---

### 3.2 Domain B: Environmental, Energy & Emissions Feeds

#### Feed 08: EIA Form 860 Power Plant Location & Generator Attributes
- **Canonical Upstream URL:** `https://www.eia.gov/electricity/data/eia860/`
- **Provider:** U.S. Energy Information Administration (EIA).
- **Staged File:** `raw/eia/eia_plants_gps.csv` (~1.4 MB, 7,289 power plants).
- **Target Schema:** `eia_plants` (`sql/tables/eia-plants.sql`), populated via `data/eia-plants.R`.
- **Primary Keys:** `plant_code` (INTEGER EIA Facility ID).
- **Core Columns:** `plant_code`, `plant_name`, `utility_id`, `utility_name`, `state_code`, `county_name`, `latitude`, `longitude`, `primary_fuel`, `nameplate_mw`.
- **Spatial Linkage:** Point coordinates matched to UCLA shapefiles via `synthesis/eia-plants-district.R` into `eia_plants_district`.
- **License / Access:** Public Domain (U.S. Federal Government).

#### Feed 09: EIA / EPA Fuel Heat and Carbon Dioxide Emissions Factors
- **Canonical Upstream URL:** 
  - EPA Mandatory Greenhouse Gas Reporting Rule: 40 CFR Part 98, Subpart C, Table C-1 and C-2 (`https://www.ecfr.gov/current/title-40/chapter-I/subchapter-C/part-98/subpart-C`).
  - EIA Electric Power Monthly Technical Notes: `https://www.eia.gov/environment/emissions/co2_vol_mass.php`.
- **Provider:** U.S. Environmental Protection Agency (EPA) / U.S. Energy Information Administration (EIA).
- **Staged Files:** `raw/eia/eia_fuel_type_carbon.csv` (40 modern fuel codes), `raw/eia/eia_fuel_type_carbon_old.csv` (16 legacy survey codes).
- **Target Schema:** `eia_fuel_factors`, `eia_fuel_factors_old` (`sql/tables/eia-factors.sql`), populated via `data/eia-factors.R`.
- **Primary Keys:** `fuel_code` (TEXT).
- **Core Columns:** `fuel_code`, `fuel_desc`, `carbon_kg_per_mmbtu`, `heat_content_factor`, `physical_unit`.
- **License / Access:** Public Domain (U.S. Federal Government).

#### Feed 10: EIA Monthly Electricity Generation Surveys (Forms 759, 906, 920, 923)
- **Canonical Upstream URL:**
  - Form EIA-759 (1990–2000): Monthly Power Plant Report (`https://www.eia.gov/electricity/data/eia923/`)
  - Form EIA-906 / EIA-920 (2001–2007): Nonutility and Utility Power Plant Database
  - Form EIA-923 (2008–2014): Power Plant Operations Report
- **Provider:** U.S. Energy Information Administration (EIA).
- **Staged Files:** `raw/eia/f759*u.xls` (1990–2000), `raw/eia/F906*.csv/xls` (2001–2007), `raw/eia/f923_*.csv` (2008–2014) (~120 MB total).
- **Target Schema:** `eia_survey_gen` (`sql/tables/eia-survey.sql`), populated via `data/eia-survey.R`; feeds `eia_generation` (`synthesis/eia-generation.R`) and `eia_district_emissions` (`synthesis/eia-district-emissions.R`).
- **Primary Keys:** `(survey_year, plant_code, fuel_code)`.
- **Core Columns:** `plant_code`, `survey_year`, `fuel_code`, `net_generation_mwh`, `fuel_consumed_qty`, `fuel_consumed_mmbtu`.
- **License / Access:** Public Domain (U.S. Federal Government).

#### Feed 11: EIA State Energy Data System (SEDS) Complete Energy Indicators
- **Canonical Upstream URL:** `https://www.eia.gov/state/seds/seds-data-complete.php`
  - Direct file: `Complete_SEDS.csv`
- **Provider:** U.S. Energy Information Administration (EIA).
- **Staged File:** `raw/eia/Complete_SEDS.csv` (~31 MB, 1.52 million rows).
- **Target Schema:** `eia_seds` (`sql/tables/eia-seds.sql`), populated via `data/eia-seds.R`.
- **Primary Keys:** `(msn, state_code, year)`.
- **Core Columns:** `msn` (EIA 5-character series mnemonic), `state_code`, `year`, `data_value`.
- **Key Series Used:** Total primary energy consumption (`TETCB`), coal production (`CLPRB`), electric power generation (`ESTCB`), net interstate electricity imports/exports (`ELISP`, `ELNISP`).
- **License / Access:** Public Domain (U.S. Federal Government).

#### Feed 12: EIA State Energy Prices and Expenditures
- **Canonical Upstream URL:** `https://www.eia.gov/state/seds/seds-data-fuel.php?sid=US#DataFiles`
  - Direct file: `raw/eia/eia_prices.csv`
- **Provider:** U.S. Energy Information Administration (SEDS Price and Expenditure Accounts).
- **Staged File:** `raw/eia/eia_prices.csv` (~1.1 MB, 250,518 rows).
- **Target Schema:** `eia_prices` (`sql/tables/eia-seds.sql`), populated via `data/eia-seds.R`.
- **Primary Keys:** `(msn, state_code, year)`.
- **Core Columns:** `msn`, `state_code`, `year`, `price_nominal_dollars`.
- **License / Access:** Public Domain (U.S. Federal Government).

#### Feed 13: CARMA (Carbon Monitoring for Action) Database v3.0
- **Canonical Upstream URL:** `https://www.cgdev.org/page/carma-carbon-monitoring-action`
  - Archive: `raw/carma/CARMAv30_PowerUser.zip`
- **Provider:** Center for Global Development (David Wheeler, Kevin Ummel).
- **Staged Files:** `CARMAv30_PowerUser.zip` containing `Region.csv` (district-level emissions) and `Plant.csv` (facility emissions) (~6.8 MB compressed).
- **Target Schema:** `carma_district` and `carma_plants` (`sql/tables/carma-*.sql`), populated via `data/carma.R`.
- **Primary Keys:** `carma_district.(fips, district, year)`; `carma_plants.plant_id`.
- **Temporal Coverage:** 2000, 2004, 2009, and 2014 projections.
- **License / Access:** Creative Commons Attribution NonCommercial (Center for Global Development).

---

### 3.3 Domain C: Climate & Geospatial GIS Feeds

#### Feed 14: UCLA Congressional District Boundary Shapefiles
- **Canonical Upstream URL:** `http://cdmaps.polisci.ucla.edu/`
- **Provider:** Jeffrey B. Lewis, Brandon DeVine, Lincoln Pitcher, and Kenneth C. Martis (UCLA).
- **Staged Files:** `raw/cd-maps/districts103.zip` through `districts113.zip` (~118 MB total).
- **Target Usage:** Spatial polygon geometries unpacked and ingested via `data/cd-maps.R`. Used for GIS point-in-polygon assignment in `synthesis/eia-plants-district.R` and raster zonal extraction in `data/prism.R`.
- **Spatial Reference System:** EPSG:4269 (NAD83 geographic coordinates) / EPSG:5070 (NAD83 CONUS Albers).
- **License / Access:** Open Access for academic research with required citation.

#### Feed 15: NOAA Climate Prediction Center Population-Weighted Degree Days
- **Canonical Upstream URL:** `https://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/daily_data/`
- **Provider:** National Oceanic and Atmospheric Administration (NOAA) Climate Prediction Center.
- **Staged Files:** `raw/noaa/{year}/*.txt` (1993–2016 daily degree days by state, ~2.1 MB total).
- **Target Schema:** `noaa_degree_days` (`sql/tables/noaa-degree-days.sql`) and `noaa_state_degree_days_vw` (`sql/views/noaa-state-degree-days-vw.sql`), populated via `data/noaa.R`.
- **Primary Keys:** `(state_code, date)`.
- **Core Columns:** `state_code`, `date`, `cdd` (Cooling Degree Days, base 65°F), `hdd` (Heating Degree Days, base 65°F).
- **License / Access:** Public Domain (U.S. Federal Government).

#### Feed 16: PRISM Gridded Monthly Meteorological Rasters
- **Canonical Upstream URL:** `https://prism.oregonstate.edu/`
  - FTP / Web Portal: Historical monthly 4km gridded BIL rasters for precipitation (`ppt`), minimum temperature (`tmin`), maximum temperature (`tmax`), and mean temperature (`tmean`).
- **Provider:** PRISM Climate Group, Northwest Renewable Energy and Ecological Center, Oregon State University.
- **Staged Files:** `raw/prism/{ppt,tmin,tmax,tmean}/PRISM_*_stable_4kmM3_*.bil` (~338 MB total).
- **Target Schema:** `prism_district` (`sql/tables/prism-district.sql`) and `prism_district_annual_vw` (`sql/views/prism-district-annual-vw.sql`), populated via `data/prism.R` using `sf` / `terra`.
- **Primary Keys:** `(fips, district, year, month)`.
- **Core Columns:** `ppt_mean_mm`, `tmin_mean_c`, `tmax_mean_c`, `tmean_mean_c`, `cdd_month`, `hdd_month`.
- **License / Access:** Available free for academic and non-commercial research; commercial redistribution restricted.

---

### 3.4 Domain D: Campaign Finance, Scorecards & Turnover Feeds

#### Feed 17: OpenSecrets / Center for Responsive Politics Bulk Campaign Finance
- **Canonical Upstream URL:** `https://www.opensecrets.org/bulk-data`
- **Provider:** Center for Responsive Politics (OpenSecrets.org).
- **Staged Files:** `raw/opensecrets/CampaignFin{cycle}.zip` (1990–2016 election cycles, ~2.4 GB total compressed) and `raw/opensecrets/codes_industries.csv` (industry codebook).
  - Unpacks into `cands*.txt`, `cmtes*.txt`, `pacs*.txt`, `pac_other*.txt`, `indivs*.txt`.
- **Target Schema:** 
  - `crp_categories` (443 rows)
  - `crp_candidates` (70,454 rows)
  - `crp_committees` (157,353 rows)
  - `crp_pacs` (3.88 million PAC-to-candidate contributions)
  - `crp_pac_other` (1.11 million PAC-to-PAC transfers)
  - `crp_indivs` (43.44 million itemized individual contributions)
  - Schemas defined in `sql/tables/crp-*.sql`, populated via `data/opensecrets.R`.
  - Synthesized into candidate-level totals in `crp_contributions` via `synthesis/opensecrets.R`.
- **Primary Keys:**
  - `crp_candidates.cid`
  - `crp_committees.cmte_id`
  - `crp_pacs.(cycle, cmte_id, recip_id, date, amount)`
  - `crp_indivs.(cycle, contrib_id)`
- **License / Access:** Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0). Raw data cannot be sold or redistributed commercially; research usage with attribution is permitted.

#### Feed 18: League of Conservation Voters (LCV) National Environmental Scorecards
- **Canonical Upstream URL:** `https://scorecard.lcv.org/`
- **Provider:** League of Conservation Voters (LCV).
- **Staged File:** `raw/lcv/lcv-votes.csv` (~26 MB, 1,296 curated environmental roll calls spanning 1993–2016).
- **Target Schema:** `lcv_votes` (`sql/tables/lcv-votes.sql`), populated via `data/lcv-votes.R`.
- **Primary Keys:** `(congress, chamber, vote_number)`.
- **Synthesis:** Joined against `voteview_info` and `voteview_cast` in `synthesis/lcv-scores.R` to compute annual `lcv_scores` (12,430 member-years) and biennial `lcv_congress_scores` (6,560 member-terms).
- **License / Access:** Public scorecard data; open academic research use.

#### Feed 19: Roll Call Congressional Casualties & Retirements Archive
- **Canonical Upstream URL:** `http://www.rollcall.com/politics/casualtylists/` (archived via Internet Archive Wayback Machine).
- **Provider:** Roll Call (CQ Roll Call / Economist Group).
- **Staged File:** `raw/retirements.csv` (~12 KB, 334 records covering 103rd–114th Congress).
- **Target Schema:** `retirements` (`sql/tables/retirements.sql`), populated via `synthesis/retirements.R` with documentation in `data/retirements.txt`.
- **Primary Keys:** `(icpsr, congress)`.
- **Core Columns:** `icpsr`, `congress`, `chamber`, `state_code`, `district_code`, `election_year`, `retire` (1=Retiring/Defeated/Resigned, 0=Seeking Re-election).
- **License / Access:** Curated historical research dataset compiled from public news reporting.

---

### 3.5 Domain E: Macroeconomic & Federal Outlays Feeds

#### Feed 20: BLS Local Area Unemployment Statistics (LAUS)
- **Canonical Upstream URL:** `https://download.bls.gov/pub/time.series/la/`
  - Direct file: `unemp.txt` (`la.data.64.State`)
- **Provider:** Bureau of Labor Statistics (BLS), U.S. Department of Labor.
- **Staged File:** `raw/bls/unemp.txt` (~19 MB, state annual unemployment rates 1976–2014).
- **Target Schema:** `bls_unempl` (`sql/tables/bls-unempl.sql`), populated via `data/bls.R`.
- **Primary Keys:** `(fips, year)`.
- **Core Columns:** `fips`, `state_code`, `year`, `unemp_rate` (percentage).
- **License / Access:** Public Domain (U.S. Federal Government).

#### Feed 21: BLS Consumer Price Index for All Urban Consumers (CPI-U)
- **Canonical Upstream URL:** `https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems`
- **Provider:** Bureau of Labor Statistics (BLS), U.S. Department of Labor.
- **Staged File:** `raw/bls/cpi.txt` (~25 KB, annual headline CPI 1913–2015).
- **Target Schema:** `bls_cpi` (`sql/tables/bls-cpi.sql`), populated via `data/bls.R`.
- **Primary Keys:** `year`.
- **Core Columns:** `year`, `cpi` (1982–1984=100 base), `cpi_ratio_2014` (normalized deflator where 2014=1.0).
- **License / Access:** Public Domain (U.S. Federal Government).

#### Feed 22: BEA Gross Domestic Product by State (Gross State Product / GSP)
- **Canonical Upstream URL:** `https://apps.bea.gov/regional/downloadzip.cfm`
  - Direct files: `gsp_naics_all.csv` (1997–2014 NAICS series) and `gsp_sic_all.csv` (1963–1997 SIC series).
- **Provider:** Bureau of Economic Analysis (BEA), U.S. Department of Commerce.
- **Staged Files:** `raw/bea/gsp_naics_all.csv` (~5.2 MB) and `raw/bea/gsp_sic_all.csv` (~3.8 MB).
- **Target Schema:** `bea_gsp` (`sql/tables/bea-gsp.sql`), populated via `data/bea.R`.
- **Primary Keys:** `(fips, industry_code, year)`.
- **Core Columns:** `fips`, `industry_code`, `component`, `year`, `gsp_millions`.
- **License / Access:** Public Domain (U.S. Federal Government).

#### Feed 23: BEA State Personal Income & Population Estimates (Table SA1)
- **Canonical Upstream URL:** `https://apps.bea.gov/regional/downloadzip.cfm`
  - Direct file: `SA1_1929_2014.csv`
- **Provider:** Bureau of Economic Analysis (BEA), U.S. Department of Commerce.
- **Staged File:** `raw/bea/SA1_1929_2014.csv` (~520 KB).
- **Target Schema:** `bea_gsi` (Gross State Personal Income) and `bea_pop` (State Population Estimates) (`sql/tables/bea-*.sql`), populated via `data/bea.R`.
- **Primary Keys:** `(fips, year)`.
- **Core Columns:** `fips`, `state_code`, `year`, `personal_income_thousands`, `population`.
- **License / Access:** Public Domain (U.S. Federal Government).

#### Feed 24: Census Bureau Consolidated Federal Funds Report (CFFR)
- **Canonical Upstream URL:** Historical Census CFFR Archive (1993–2010), National Archives and Records Administration (NARA) Record Group 29 (`https://www.census.gov/programs-surveys/cffr.html`).
- **Provider:** Governments Division, U.S. Census Bureau.
- **Staged Files:** `raw/cffr/{year}CFFIND/{year}cffind.csv`, `raw/cffr/*agen.txt`, `raw/cffr/*prog.txt` (~1.7 GB total, 7.55 million outlay records).
- **Target Schema:** `cffr_agency`, `cffr_program`, and `cffr` (`sql/tables/cffr*.sql`), populated via `data/cffr.R`.
- **Primary Keys:** `cffr.(year, state_code, county_code, program_code, object_type)`.
- **Core Columns:** `fips`, `state_code`, `county_code`, `agency_code`, `program_code`, `object_type`, `amount_dollars`.
- **License / Access:** Public Domain (U.S. Federal Government).

---

### 3.6 Domain F: Demographics & Legislative Calendar Feeds

#### Feed 25: Congressional Calendar Legislative Sessions
- **Canonical Upstream URL:**
  - House: `https://history.house.gov/Institution/Session-Dates/`
  - Senate: `https://www.senate.gov/legislative/DatesofSessionsofCongress.htm`
- **Provider:** Office of the Historian, U.S. House of Representatives and U.S. Senate Historical Office.
- **Staged Files:** `raw/congress/house_sessions.html`, `raw/congress/senate_sessions.html` (~352 KB total).
- **Target Schema:** `congress_sessions` (`sql/tables/congress-sessions.sql`), populated via `data/congress.R`.
- **Primary Keys:** `(congress, chamber, session)`.
- **Core Columns:** `congress`, `chamber`, `session`, `begin_date`, `adjourn_date`.
- **License / Access:** Public Domain (U.S. Federal Government).

#### Feed 26: U.S. Census Bureau 1990 Summary Tape File 3D (STF 3D)
- **Canonical Upstream URL:** U.S. Census Bureau 1990 Decennial Census of Population and Housing, Summary Tape File 3D (Congressional District extract).
- **Provider:** U.S. Census Bureau.
- **Staged File:** `raw/census/1990_STF3D.csv` (~3.2 MB, 103rd Congress demographics).
- **Target Schema:** `census_district` and `census_state` (`sql/tables/census-district.sql`), populated via `data/census.R`.
- **Primary Keys:** `(fips, district, year)`.
- **Core Variables:** Total population, median household income, poverty rate, educational attainment, urban/rural share, racial/ethnic composition.
- **License / Access:** Public Domain (U.S. Federal Government).

#### Feed 27: U.S. Census Bureau 2000 Census & ACS Congressional District Demographic Profiles
- **Canonical Upstream URL:** `https://data.census.gov/`
  - Census 2000 Demographic Profiles: `c106_DP1.csv` through `c106_DP4.csv`
  - American Community Survey (ACS) 1-Year / 5-Year CD Profiles: `c109_DP*.csv` (109th), `c111_DP*.csv` (111th), `c113_DP*.csv` (113th).
- **Provider:** U.S. Census Bureau.
- **Staged Files:** `raw/census/c*_DP*.csv` (106th–113th Congress, ~133 MB total).
- **Target Schema:** `census_district` and `census_state` (`sql/tables/census-district.sql`), populated via `data/census.R`.
- **Primary Keys:** `(fips, district, year)`.
- **Core Columns:** Standardized harmonized demographic indicators spanning 1993–2015.
- **License / Access:** Public Domain (U.S. Federal Government).

---

## 4. Cross-Reference to Auxiliary & Backlog Data Feeds

In addition to the 27 core panel feeds documented above, 9 auxiliary data sources from earlier research stages or standalone manuscript figures are cataloged in [`docs/non-panel-data-sources.md`](non-panel-data-sources.md):

1. **EPA Clean Power Plan State Goal Visualizer:** Generator-level baseline used for Figure 1 and Appendix B in the Carbon manuscript.
2. **Charles Stewart III Congressional Committee Assignments:** Committee rosters used for qualitative validation of extreme legislators in Section 5.3 of the Ideology manuscript.
3. **Project Vote Smart API & Scorecards:** Directory of interest groups motivating the scorecards distribution.
4. **EPA ADAGE CGE Projections:** Macroeconomic forecasts for Waxman-Markey (H.R. 2454) and Lieberman-Warner (S. 2191), replaced by the unified Bayesian campaign inducements model.
5. **ACCF-NAM NEMS Impact Studies:** Industry impact studies used in earlier working drafts.
6. **Heritage Foundation Global Insight Studies:** Conservative think-tank district projections used in earlier working drafts.
7. **USAspending.gov Contracts & Grants:** Early prospectus exploration (2013) on discretionary executive spending.
8. **USGS Cement Clinker CO2 Emissions:** 2012 exploration prior to focusing carbon accounts on the electric power sector.
9. **FERC Form 714 Electricity Demand & Lambda:** 2014 exploration superseded by EIA SEDS net interstate electricity trade flows.

---

## 5. End-to-End Replication Instructions

### 5.1 Model Replication (Using Existing Database & Master Panels)
To replicate the empirical findings and Bayesian MCMC models directly from `congress.sqlite`:
1. Restore the pinned R environment via `renv`:
   ```R
   renv::restore()
   ```
2. Execute the Bayesian Stan models directly against the SQLite database:
   ```bash
   # Run Hierarchical 2PL IRT model on House roll calls
   Rscript analysis/model-hierarchical.R --house --iter 2000 --chains 4

   # Run Campaign Inducements model on Waxman-Markey (H.R. 2454)
   Rscript analysis/model-inducements.R --house --congress 111 --bill "hr2454"
   ```

### 5.2 Full Pipeline Rebuild (Starting from Raw Sources & SQLite Bootstrap)
To build `congress.sqlite` entirely from scratch:
1. Stage the genuine upstream files in `raw/` according to the paths in Section 2.
2. Initialize and bootstrap the database:
   ```R
   source("data/states.R")
   ```
3. Run ingestion ETL scripts in `data/*.R` to populate physical base tables.
4. Run synthesis scripts in `synthesis/*.R` to construct member crosswalks, derived features, and panels:
   ```R
   source("synthesis/crosswalk-members.R")
   source("synthesis/cq-margins.R")
   source("synthesis/fec-margins.R")
   source("synthesis/opensecrets.R")
   source("synthesis/eia-plants-district.R")
   source("synthesis/eia-states.R")
   source("synthesis/eia-generation.R")
   source("synthesis/eia-district-emissions.R")
   source("synthesis/lcv-scores.R")
   source("synthesis/retirements.R")
   source("synthesis/panel-house.R")
   source("synthesis/panel-senate.R")
   ```
5. Verify master panel integrity and concordance:
   ```R
   source("synthesis/panel-house.R")
   source("synthesis/panel-senate.R")
   ```
6. Execute the Bayesian Stan estimation models in `analysis/`.
