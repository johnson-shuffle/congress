# Non-Panel Legacy Data Sources (`congress-legacy` Audit)

This document catalogs the nine auxiliary data sources from `~/Projects/congress-legacy` tracked in the **Architecture & Backlog** list on Trello (`Migrate:*` cards and `Reassess raw/legislators files`). None of these sources are required by the master panel assembly scripts (`Data/panel_house.R` and `Data/panel_senate.R`) or the core Bayesian Stan models; instead, they were used for standalone manuscript figures, earlier working drafts, or exploratory prospectus analyses.

---

## Summary Matrix

| Backlog Card | Legacy Scripts (`Data/` & `Analysis/`) | Appears in `.tex`? | Manuscript / Version Context |
| :--- | :--- | :--- | :--- |
| **Migrate: EPA ADAGE Impacts & Clean Power Plan** (CPP component) | `Data/EPA/scripts/epa_clean_targets.R`<br>`Analysis/Clean Power Plan/scripts/clean_targets.R`<br>`Analysis/Figures/graphs_clean_maps.R` | **Yes (Final)** | `TeX/carbon/v_current/congress_carbon.tex` (Section 2, Figure 1, Appendix B, Table B.1) |
| **Migrate: Stewart Congressional Committee Assignments** | `Data/Stewart/scripts/committees.R`<br>`Analysis/Figures/graphs_ranks_extreme.R` | **Yes (Final)** | `TeX/ideology/v_current/congress_ideology.tex` (Section 5.3 qualitative model fit discussion) |
| **Migrate: VoteSmart Interest Group Ratings** | `Data/VoteSmart/scripts/votesmart_names.R`<br>`Data/VoteSmart/scripts/interest_groups.R` | **Yes (Final & `v_0715`)** | Cited in `congress_ideology.tex` (Section 3 data sources); motivating group count in `v_0715` intro |
| **Migrate: EPA ADAGE Impacts & Clean Power Plan** (ADAGE component) | `Data/EPA/scripts/adage_wm_impacts.R`, `adage_lw_impacts.R`, `adage_lm_impacts.R`<br>`Analysis/Waxman-Lieberman/scripts/*` | **Yes (`v_0415`–`v_0416` drafts)** | `TeX/ideology/v_0415/congress_carbon_v_0415.tex` (Section 5, Tables `waxman_impacts.tex`, `waxman_pr.tex`, etc.) |
| **Migrate: Heritage & ACCF-NAM Policy Impact Studies** | `Data/Heritage/scripts/heritage_*_impacts.R`<br>`Data/ACCF-NAM/scripts/accf-nam_*.R`<br>`Analysis/Waxman-Lieberman/scripts/*` | **Yes (`v_0415`–`v_0416` drafts)** | `TeX/ideology/v_0415/congress_carbon_v_0415.tex` (Section 5, Tables `waxman_impacts.tex`, `warner_impacts.tex`, etc.) |
| **Migrate: USAspending District Contracts & Grants** | `Data/USAspending/scripts/USAspending_*.R`<br>`Analysis/Spending/scripts/USAspending_*_coeff_var.R`<br>`Analysis/Figures/graphs_spending.R` | **Early Prospectus (`v_cphil`)** | Motivated Section 3 (*"Side payments"*) in `TeX/ideology/v_cphil/congress_vCPhil.tex` |
| **Migrate: USGS Cement Clinker CO2 Emissions** | `Data/USGS/usgs_co2.do` | **No** | 2012–2013 Stata exploration prior to narrowing carbon scope to the electric power sector |
| **Migrate: SUSB County Business Patterns Employment** | `Data/SUSB/scripts/susb_empl.R`<br>`Data/USGS/usgs_co2.do` | **No** | State NAICS 3273 employment weights used by `usgs_co2.do` to apportion regional clinker production |
| **Migrate: FERC Form 714 Electricity Demand & Lambda** | `Data/FERC/scripts/ferc.R`, `ferc_retry.R` | **No** | Nov 2014 exploration matching EIA plants to FERC balancing authority demand; superseded by EIA SEDS |
| **Reassess `raw/legislators` files** | `Data/Legislators/scripts/casualties_*.R`, `legislator_degrees.R`, `us_senate_classes.csv` | **No (Superseded)** | Superseded by `retirements` (`RollCall`) and `govtrack_term` / `crosswalk_members` |

---

## 1. Used in Final Manuscript Figures, Appendix, or Text (Outside the Master Panels)

### EPA Clean Power Plan Targets (`epa_clean_targets.R` & `clean_targets.R`)
- **Trello Card:** `Migrate: EPA ADAGE Impacts & Clean Power Plan` (ID: `6aca85d81b2a6c2d487a1db6`)
- **Legacy Workflow:** Combines 2012 generator-level baseline data from the EPA Clean Power Plan State Goal Visualizer with EIA-860 plant coordinates (`eia_plants_gps.csv`) and congressional district shapefiles (`Lewis:2013us`). Extends the state-level Clean Power Plan carbon intensity ($\lambda_d^f, \lambda_d^n, \lambda_d^g$) and mass-reduction goal formulas down to the congressional district level, feeding `Analysis/Figures/graphs_clean_maps.R`.
- **Manuscript Usage:** Featured in the final Carbon manuscript (`TeX/carbon/v_current/congress_carbon.tex` and `v_dissertation/congress_carbon.tex`):
  - **Main Text (Section 2, Figure 1):** Generates subfigures `rate_cd.pdf`, `rate_st.pdf`, `mass_cd.pdf`, and `mass_st.pdf` illustrating district- and state-level production-based Clean Power Plan reduction targets alongside net interstate electricity trade.
  - **Appendix B (`\section{The Clean Power Plan}`):** Details the exact generator-level mathematical formulation and parameters (`Table B.1 / tab:epa.param`) used to compute district-level Clean Power Plan goals.

### Stewart Congressional Committee Assignments (`committees.R`)
- **Trello Card:** `Migrate: Stewart Congressional Committee Assignments` (ID: `6aca85d673e62fc342650f69`)
- **Legacy Workflow:** Parses Charles Stewart III's 103rd–113th Congress committee rosters (`raw_files/house_assignments_103-113.txt`, `senate_assignments_103-113.xls`) into indicator matrices (`committees.RData`). It is never merged into `panel_house.Rda` or `panel_senate.Rda`; instead, it is loaded at the end of `Analysis/Figures/graphs_ranks_extreme.R` (lines 108–119) to check which legislators at the far-left and far-right tails of the posterior ideal-point distributions served on the House Committee on Energy and Commerce (`cmte.128`) or the Senate Committee on Environment and Public Works (`cmte.332`).
- **Manuscript Usage:** Discussed in Section 5.3 (*"Extreme Legislators"*) of the final Ideology manuscript (`TeX/ideology/v_current/congress_ideology.tex`, line 744) as a qualitative validation of model fit, highlighting that key members of both environmental committees (Waxman, Pallone, Cardin, Metzenbaum, Obama, Whitehouse, and McConnell) populate the extreme ranks in `Figure 4` (`rank_left_house.pdf`, `rank_right_house.pdf`) and `Figure 5` (`rank_left_senate.pdf`, `rank_right_senate.pdf`).

### Project Vote Smart (`votesmart_names.R` & `interest_groups.R`)
- **Trello Card:** `Migrate: VoteSmart Interest Group Ratings` (ID: `6aca85d9c3f65b5e25242d10`)
- **Legacy Workflow:**
  1. `votesmart_names.R` queried the Project Vote Smart API for legislator biographical records (`birthdate`, `gender`, `religion`, `education`) and cross-checked missing VoteSmart IDs (`vsid`) in `ids_house.R` and `ids_senate.R`. In the modernized SQLite pipeline, `vsid`, `gender`, and `birthdate` are already populated directly from the YAML `govtrack_info` table into `crosswalk_members`.
  2. `interest_groups.R` scraped the directory of national special interest groups and issue categories publishing congressional scorecards on `votesmart.org`.
- **Manuscript Usage:**
  - Cited in Section 3 (*"Data"*) of `TeX/ideology/v_current/congress_ideology.tex` (line 259) alongside Voteview and GovTrack for linking legislator biographical attributes.
  - `interest_groups.R` supplied a motivating statistic in the introduction of the July 2015 working draft (`TeX/ideology/v_0715/congress_ideology_v0715.tex`, lines 80–82): *"Project Vote Smart currently lists 657 special interest groups, ranging over 63 issues, who have published ratings at the national level at some point in the last twenty years."*

---

## 2. Used in Earlier Working Drafts (`v_0415`–`v_0416`), Replaced by the Bayesian Inducements Model

### EPA ADAGE, Heritage Foundation, and ACCF-NAM Policy Impact Studies
- **Trello Cards:**
  - `Migrate: EPA ADAGE Impacts & Clean Power Plan` (ID: `6aca85d81b2a6c2d487a1db6`)
  - `Migrate: Heritage & ACCF-NAM Policy Impact Studies` (ID: `6aca85dbaab074a069c8f093`)
- **Legacy Workflow:**
  - `Data/EPA/scripts/adage_wm_impacts.R`, `adage_lw_impacts.R`, and `adage_lm_impacts.R` parsed EPA Applied Dynamic Analysis of the Global Economy (ADAGE) CGE regional projections for the Waxman-Markey (H.R. 2454), Lieberman-Warner (S. 2191), and Lieberman-McCain (S. 280) climate bills.
  - `Data/ACCF-NAM/scripts/accf-nam_waxman.R` and `accf-nam_lieberman.R` parsed state-level 2020/2030 NEMS projections of job, income, GSP, and energy price losses from studies commissioned by the American Council for Capital Formation and National Association of Manufacturers.
  - `Data/Heritage/scripts/heritage_waxman_impacts.R` and `heritage_boxer_impacts.R` parsed congressional district-level Global Insight macroeconomic impact estimates published by the Heritage Foundation (`Campbell:2009ux`, `Campbell:2009vi`).
  - `Analysis/Waxman-Lieberman/scripts/waxman_data.R`, `lieberman_data.R`, and `wax_lieb_analysis.R` merged these three sets of projected policy impacts to estimate frequentist probit and bivariate-probit models of roll call voting on those specific climate bills (generating `Tables/waxman_impacts.tex`, `warner_impacts.tex`, `waxman_pr.tex`, `waxman_bipr.tex`, `mccain_pr.tex`, `mccain_bipr.tex`, `warner_pr.tex`, and `warner_bipr.tex` via `Analysis/Tables/tables_waxman*.R` and `tables_warner_impacts.R`).
- **Manuscript Usage:**
  - Featured prominently in Section 5 (*"Actual votes"*, lines 467–539) of the April 2015 draft (`TeX/ideology/v_0415/congress_carbon_v_0415.tex`), comparing how competing forecasts from the EPA, ACCF/NAM, and Heritage Foundation correlated with legislator votes on H.R. 2454, S. 280, and S. 2191.
  - In subsequent drafts (`v_1115`, `v_0416`, `v_dissertation`, and `v_current`), the frequentist probit section was replaced by the unified Bayesian hierarchical/inducements analysis of Waxman-Markey campaign contributions (`Analysis/NewStan/scripts/stan_induce_house.R`), and the ADAGE/Heritage/ACCF-NAM regressions were retired from the main text.

---

## 3. Early Exploratory / Prospectus Work (`v_cphil` / 2012–2014) Omitted from Final Papers

### USAspending Federal Contracts & Grants (`Data/USAspending/` & `Analysis/Spending/`)
- **Trello Card:** `Migrate: USAspending District Contracts & Grants` (ID: `6aca85d5c6f02fb7ef3efdd5`)
- **Legacy Workflow:** Parsed prime contract and grant transactions from `USAspending.gov` (2000–2012) alongside the Consolidated Federal Funds Report (`CFFR`, already ingested in `congress.sqlite`). Used in `Analysis/Spending/scripts/USAspending_district_coeff_var.R`, `USAspending_state_coeff_var.R`, `spending_state.R`, and `Analysis/Figures/graphs_spending.R` to compute coefficients of variation across federal agencies and identify high-variance discretionary outlays.
- **Manuscript Usage:** Motivated Section 3 (*"Side payments"*, lines 66–73) of the 2013 C.Phil prospectus (`TeX/ideology/v_cphil/congress_vCPhil.tex`), which originally proposed modeling executive-branch "vote buying" via discretionary federal grants and contracts (following Berry, Burden, and Howell 2010). This arm of the prospectus was set aside when the dissertation split into the two full papers on environmental ideology (`congress_ideology.tex`) and carbon emissions embodied in interstate electricity trade (`congress_carbon.tex`).

### USGS Cement Clinker CO2 Emissions & SUSB County Business Patterns (`Data/USGS/` & `Data/SUSB/`)
- **Trello Cards:**
  - `Migrate: USGS Cement Clinker CO2 Emissions` (ID: `6aca85db5e9d2ba2351f832b`)
  - `Migrate: SUSB County Business Patterns Employment` (ID: `6aca85d7678a612a48c95f07`)
- **Legacy Workflow:** Originated in the earliest 2012–2013 Stata workflow (`Data/USGS/usgs_co2.do`). Because USGS Minerals Yearbook tables report clinker production across multi-state districts (e.g., Northern/Southern Texas or combined multi-state groups), `usgs_co2.do` used Statistics of U.S. Businesses (`SUSB`) state employment shares in NAICS 3273 (*Cement and Concrete Product Manufacturing*, parsed in `Data/SUSB/scripts/susb_empl.R`) to apportion regional clinker production and international cement trade to individual states and compute process CO2 emissions (`0.52 * clinker_adj`).
- **Manuscript Usage:** Never appeared in `.tex`. Once the Carbon paper focused strictly on fossil-fuel combustion and the electric power sector (following Aldy 2005 and EIA SEDS/Form 906/923), non-energy industrial process emissions from cement clinker were dropped.

### FERC Form 714 Electricity Demand & System Lambda (`Data/FERC/`)
- **Trello Card:** `Migrate: FERC Form 714 Electricity Demand & Lambda` (ID: `6aca85d9e71570a606927e2b`)
- **Legacy Workflow:** A November 2014 exploratory script (`Data/FERC/scripts/ferc.R` and `ferc_retry.R`) that attempted to link EIA-860/923 generators and EIA-861 control areas to FERC Form 714 balancing authority hourly demand and system lambda filings (`responding_utilties_edit.csv`, `receiving_utilties_edit.csv`).
- **Manuscript Usage:** Never referenced in `Analysis/` or `.tex`. State-level net interstate electricity flows were instead measured directly via EIA SEDS (`ELISP` / `ELNISP`) and paired with district-level generation from EIA Form 906/923.

### Miscellaneous `raw/legislators/` Files (`casualties_*.R`, `legislator_degrees.R`, `us_senate_classes.csv`)
- **Trello Card:** `Reassess raw/legislators files (casualties, degrees, senate classes)` (ID: `6aca8218f52d805ebb7b9a6d`)
- **Legacy Workflow & Disposition:**
  - `casualties_house.R` and `casualties_senate.R` were early turnover scripts superseded by `Data/RollCall/scripts/rollcall_casualty.R` (now in SQLite `retirements`).
  - `legislator_degrees.R` was a 15-line scratch script extracting degree substrings from `votesmart_names$candidate.education` and was never used in any model or table.
  - `us_senate_classes.csv` mapped Senate seats to Class I/II/III election cycles, which is now natively stored in `govtrack_term.senate_class` and `crosswalk_members.senate_class`.
