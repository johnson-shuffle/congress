# Congressional Member Crosswalk & Entity Resolution Specification (Tier 1)

## Overview

The `crosswalk_members` table serves as the primary entity-resolution backbone for the congressional research database (`congress.sqlite`). It unifies multiple disparate external identifiers across the 103rd through 114th Congresses (1993–2017) to allow relational joining across:

1. **Voteview / DW-NOMINATE** (`icpsr_id`): Roll call votes, spatial ideology dimensions.
2. **Bioguide** (`bioguide_id`): Official US Congressional biographical directory identifier.
3. **GovTrack** (`govtrack_id`): Legislative tracking and term history.
4. **OpenSecrets / CRP** (`opensecrets_id`): Campaign contributions and PAC funding (`osid`).
5. **Project Vote Smart** (`votesmart_id`): Interest group ratings and demographic profiles (`vsid`).
6. **Federal Election Commission** (`fec_id`): Candidate committee filings.

---

## Table Schema (`crosswalk_members`)

Defined in [`sql/tables/crosswalk-members.sql`](../sql/tables/crosswalk-members.sql):

```sql
CREATE TABLE crosswalk_members (
    congress INTEGER NOT NULL,
    chamber TEXT NOT NULL,
    icpsr_id INTEGER NOT NULL,
    bioguide_id TEXT,
    govtrack_id TEXT,
    opensecrets_id TEXT,
    votesmart_id TEXT,
    fec_id TEXT,
    state_code TEXT NOT NULL,
    district_number INTEGER NOT NULL DEFAULT 0,
    senate_class INTEGER NOT NULL DEFAULT 0,
    party TEXT,
    first_name TEXT,
    preferred_name TEXT,
    last_name TEXT,
    suffix TEXT,
    full_name TEXT,
    gender TEXT,
    birthday TEXT,
    occupancy INTEGER,
    means INTEGER,
    start_date TEXT,
    end_date TEXT,
    PRIMARY KEY (congress, chamber, icpsr_id, district_number)
);
```

### Column Dictionary

| Column | Type | Nullable | Description |
| :--- | :--- | :--- | :--- |
| `congress` | `INTEGER` | No | Congressional session number (103–114). |
| `chamber` | `TEXT` | No | Congressional chamber (`House` or `Senate`). |
| `icpsr_id` | `INTEGER` | No | ICPSR unique member ID used by Voteview / DW-NOMINATE. |
| `bioguide_id` | `TEXT` | Yes | Alphanumeric Congressional Bioguide identifier (e.g., `M000303`). |
| `govtrack_id` | `TEXT` | Yes | GovTrack numeric identifier (e.g., `300071`). |
| `opensecrets_id` | `TEXT` | Yes | Center for Responsive Politics candidate ID (e.g., `N00006424`). |
| `votesmart_id` | `TEXT` | Yes | Project Vote Smart candidate ID. |
| `fec_id` | `TEXT` | Yes | Principal candidate FEC campaign ID (e.g., `S6AZ00019`). |
| `state_code` | `TEXT` | No | Two-letter state postal abbreviation. |
| `district_number`| `INTEGER` | No | District number for House members (0 for at-large states and Senate). |
| `senate_class` | `INTEGER` | No | Senate class (`1`, `2`, `3` for Senate; `0` for House). |
| `party` | `TEXT` | Yes | Political party during this congressional session. |
| `first_name` | `TEXT` | Yes | Legal first name. |
| `preferred_name` | `TEXT` | Yes | Preferred name or nickname (fallback to `first_name`). |
| `last_name` | `TEXT` | Yes | Member surname. |
| `suffix` | `TEXT` | Yes | Name suffix (`Jr.`, `III`, etc.). |
| `full_name` | `TEXT` | Yes | Full normalized member name. |
| `gender` | `TEXT` | Yes | Member gender (`Male` or `Female`). |
| `birthday` | `TEXT` | Yes | Member date of birth (`YYYY-MM-DD`). |
| `occupancy` | `INTEGER` | Yes | Seat occupancy order within the Congress (`0` = sole, `1` = first, `2` = second). |
| `means` | `INTEGER` | Yes | Means of attaining office (`1` = general election, `2` = special election, `5` = appointed). |
| `start_date` | `TEXT` | Yes | Term start date (`YYYY-MM-DD`), patched for mid-Congress transitions. |
| `end_date` | `TEXT` | Yes | Term end date (`YYYY-MM-DD`), patched for mid-Congress transitions. |

---

## Edge Case Handling

### 1. Redistricting Cycles
- The 103rd–114th Congresses span three decennial redistricting cycles:
  - **103rd–107th Congress (1993–2003):** 1990 census district lines (plus mid-decade court modifications in GA, NC, TX, VA, FL).
  - **108th–112th Congress (2003–2013):** 2000 census district lines (plus 2004 Texas and 2006 Georgia re-redistricting).
  - **113th–114th Congress (2013–2017):** 2010 census district lines.
- Because the crosswalk records entries at the `(congress, member)` grain, a member whose district number changes across cycles (or mid-decade) maintains persistent identifier linkages (`icpsr_id`, `bioguide_id`, `opensecrets_id`) across distinct congressional sessions.

### 2. Chamber Transitions
- When a House representative is elected to the Senate (e.g., Sherrod Brown, Bernie Sanders, Maria Cantwell):
  - Persistent IDs (`icpsr_id`, `bioguide_id`, `opensecrets_id`) remain identical.
  - The `chamber` and `district_number` delineate the service context: House records retain the historical district; Senate records reflect `chamber = 'Senate'` and `district_number = 0`.

### 3. Party Switches
- Persistent identifiers resolve the individual regardless of party realignment (e.g., Michael Forbes switching R to D in the 106th; Virgil Goode switching D to I to R; Arlen Specter switching R to D in the 111th; Jim Jeffords switching R to I in the 107th).
- The `party` column records the specific party affiliation held during that Congress.

### 4. Special Elections & Mid-Session Vacancies
- Members seated via special election (e.g., Marcia Fudge filling the vacancy in OH-11 in November 2008 during the 110th Congress) have geographic information resolved from GovTrack term records (`geo_lookup`) to guarantee non-null state codes and district numbers.

### 5. Exclusion of Presidential Codes
- Voteview roll call files encode presidential positions under reserved ICPSR codes (`99909` for Clinton, `99910` for Bush, `99911` for Obama).
- These are explicitly filtered (`icpsr_id < 99909`) from `crosswalk_members` to ensure only seated legislators populate the table.

---

## ETL & Synthesis Pipeline
- **Raw Ingestion Scripts (`data/`):**
  - [`data/govtrack.R`](../data/govtrack.R): Ingests `@unitedstates` YAML rosters into `govtrack_info` and `govtrack_term`.
  - [`data/voteview.R`](../data/voteview.R): Ingests Voteview CSVs into `voteview_members`.
- **Synthesis Script (`synthesis/`):**
  - [`synthesis/crosswalk-members.R`](../synthesis/crosswalk-members.R): Joins `voteview_members` and `govtrack_info` directly from SQLite to construct the unified `crosswalk_members` table.
- **DDL Definitions (`sql/tables/`):**
  - [`sql/tables/govtrack-info.sql`](../sql/tables/govtrack-info.sql)
  - [`sql/tables/govtrack-term.sql`](../sql/tables/govtrack-term.sql)
  - [`sql/tables/voteview-members.sql`](../sql/tables/voteview-members.sql)
  - [`sql/tables/crosswalk-members.sql`](../sql/tables/crosswalk-members.sql)
- **Raw Input Sources (`raw/`):**
  - `raw/govtrack/legislators-current.yaml`
  - `raw/govtrack/legislators-historical.yaml`
  - `raw/voteview/` (`H{congress}_members.csv`, `S{congress}_members.csv`)
