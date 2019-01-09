------- database ---------------------------------------------------------

CREATE DATABASE congress;


------- postgis -----------------------------------------------------------

CREATE EXTENSION postgis;


------- states ------------------------------------------------------------

DROP TABLE IF EXISTS states;
CREATE TABLE states (
    fips INTEGER PRIMARY KEY,
    state_code CHAR(2),
    state_name VARCHAR,
    census_region INTEGER,
    census_division INTEGER,
    icpsr_code INTEGER,
    adage_region VARCHAR,
    recs_domain INTEGER,
    recs_division INTEGER,
    nerc_region VARCHAR(4),
    interconnect VARCHAR,
    at_large BOOLEAN);


------- bureau of economic analysis ---------------------------------------

-- gross state product
DROP TABLE IF EXISTS bea_gsp;
CREATE TABLE bea_gsp (
    geofips CHAR(5),
    geoname VARCHAR,
    region INTEGER,
    componentid INTEGER,
    componentname VARCHAR,
    industryid INTEGER,
    industryclassification VARCHAR,
    description VARCHAR,
    fips INTEGER REFERENCES states(fips),
    year INTEGER,
    value REAL,
    units VARCHAR,
    PRIMARY KEY (year, fips, industryid, componentid));

-- gross state income
DROP TABLE IF EXISTS bea_gsi;
CREATE TABLE bea_gsi (
    geofips CHAR(5),
    geoname VARCHAR,
    region INTEGER,
    series CHAR(3),
    linecode INTEGER,
    industryclassification VARCHAR,
    fips INTEGER REFERENCES states(fips),
    year INTEGER,
    value bigint,
    units VARCHAR,
    PRIMARY KEY (year, fips));

-- population
DROP TABLE IF EXISTS bea_pop;
CREATE TABLE bea_pop (
    geofips CHAR(5),
    geoname VARCHAR,
    region INTEGER,
    series CHAR(3),
    linecode INTEGER,
    industryclassification VARCHAR,
    fips INTEGER REFERENCES states(fips),
    year INTEGER,
    value bigint,
    units VARCHAR,
    PRIMARY KEY (year, fips));


-------- bureau of labor statistics --------------------------------------

-- unemployment
DROP TABLE IF EXISTS bls_unempl;
CREATE TABLE bls_unempl (
    year INTEGER,
    fips INTEGER REFERENCES states(fips),
    avg_unempl REAL,
    PRIMARY KEY (year, fips));

-- inflation
DROP TABLE IF EXISTS bls_cpi;
CREATE TABLE bls_cpi (
    year INTEGER PRIMARY KEY,
    avg_cpi REAL);


------- voteview ---------------------------------------------------------

-- rollcall information
DROP TABLE IF EXISTS voteview_info;
CREATE TABLE voteview_info (
    congress INTEGER,
    chamber VARCHAR,
    rollnumber INTEGER,
    date DATE,
    session INTEGER,
    clerk_rollnumber INTEGER,
    mid_1 REAL,
    mid_2 REAL,
    spread_1 REAL,
    spread_2 REAL,
    log_likelihood REAL,
    bill_number VARCHAR,
    vote_result VARCHAR,
    vote_desc VARCHAR,
    vote_question VARCHAR,
    dtl_desc VARCHAR,
    PRIMARY KEY (congress, chamber, rollnumber));

-- member information
DROP TABLE IF EXISTS voteview_memb;
CREATE TABLE voteview_memb (
    congress INTEGER,
    chamber VARCHAR,
    icpsr INTEGER,
    icpsr_code INTEGER REFERENCES states(icpsr_code),
    district_code INTEGER,
    state_code CHAR(2) REFERENCES states(state_code),
    party_code INTEGER,
    occupancy INTEGER,
    last_means INTEGER,
    bioname VARCHAR,
    bioguide_id VARCHAR,
    born INTEGER,
    died INTEGER,
    dim1 REAL,
    dim2 REAL,
    log_likelihood REAL,
    geo_mean_probability REAL,
    number_of_votes INTEGER,
    number_of_errors INTEGER,
    conditional BOOLEAN,
    PRIMARY KEY (congress, chamber, icpsr));

-- votes cast
DROP TABLE IF EXISTS voteview_cast;
CREATE TABLE voteview_cast (
    congress INTEGER,
    chamber VARCHAR,
    rollnumber INTEGER,
    icpsr INTEGER,
    cast_code INTEGER,
    PRIMARY KEY (congress, chamber, rollnumber, icpsr));


------- carma -------------------------------------------------------------

DROP TABLE IF EXISTS carma;
CREATE TABLE carma (
    year INTEGER,
    fips INTEGER REFERENCES states(fips),
    district_code INTEGER,
    variable CHAR(14),
    value bigint,
    units CHAR(4),
    PRIMARY KEY (year, fips, district_code));


------- casualties --------------------------------------------------------

------- congress ----------------------------------------------------------

DROP TABLE IF EXISTS congress;
CREATE TABLE congress (
    congress INTEGER,
    session INTEGER,
    session_start DATE,
    session_end DATE,
    calendar_days INTEGER,
    legislative_days INTEGER,
    recesses VARCHAR,
    chamber VARCHAR,
    start_date DATE,
    end_date DATE,
    PRIMARY KEY (congress, session, chamber, session_start));
