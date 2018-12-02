#!/bin/bash

# ----- locations ---------------------------------------------------------

# database
db=congress


# ----- states ------------------------------------------------------------

echo -e "DROP TABLE IF EXISTS states;
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
        at_large BOOLEAN);" | psql -d $db


# ----- bureau of economic analysis ---------------------------------------

# gross state product
echo -e "DROP TABLE IF EXISTS bea_gsp;
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
        PRIMARY KEY (year, fips, industryid, componentid));" | psql -d $db

# gross state income
echo -e "DROP TABLE IF EXISTS bea_gsi;
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
        PRIMARY KEY (year, fips));" | psql -d $db

# population
echo -e "DROP TABLE IF EXISTS bea_pop;
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
        PRIMARY KEY (year, fips));" | psql -d $db


# ----- bureau of labor statistics ----------------------------------------

# unemployment
echo -e "DROP TABLE IF EXISTS bls_unempl;
    CREATE TABLE bls_unempl (
        year INTEGER,
        fips INTEGER REFERENCES states(fips),
        avg_unempl REAL,
        PRIMARY KEY (year, fips));" | psql -d $db

# inflation
echo -e "DROP TABLE IF EXISTS bls_cpi;
    CREATE TABLE bls_cpi (
        year INTEGER PRIMARY KEY,
        avg_cpi REAL);" | psql -d $db


# ----- voteview ----------------------------------------------------------

# rollcall information
echo -e "DROP TABLE IF EXISTS voteview_info;
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
        PRIMARY KEY (congress, chamber, rollnumber));" | psql -d $db

# member information
echo -e "DROP TABLE IF EXISTS voteview_memb;
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
        PRIMARY KEY (congress, chamber, icpsr));" | psql -d $db

# votes cast
echo -e "DROP TABLE IF EXISTS voteview_cast;
    CREATE TABLE voteview_cast (
        congress INTEGER,
        chamber VARCHAR,
        rollnumber INTEGER,
        icpsr INTEGER,
        cast_code INTEGER,
        PRIMARY KEY (congress, chamber, rollnumber, icpsr));" | psql -d $db


# ----- carma -------------------------------------------------------------

echo -e "DROP TABLE IF EXISTS carma;
    CREATE TABLE carma (
        year INTEGER,
        fips INTEGER REFERENCES states(fips),
        district_code INTEGER,
        variable CHAR(14),
        value bigint,
        units CHAR(4),
        PRIMARY KEY (year, fips, district_code));" | psql -d $db


# ----- casualties  -------------------------------------------------------
