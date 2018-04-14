# ----- Preample ----------------------------------------------------------


# ----- Download and Unzip ------------------------------------------------

td <- tempdir()

pag <- 'https://www.bea.gov/regional/zip/'

# naics
download.file(str_c(pag, 'gsp/gsp_naics_all.zip'), str_c(td, '/naics.zip'))
unzip(str_c(td, '/naics.zip'), file = 'gsp_naics_all.csv', exdir = td)
dat1 <- read_csv(str_c(td, '/gsp_naics_all.csv'))

# sic
download.file(str_c(pag, 'gsp/gsp_sic_all.zip'), str_c(td, '/sic.zip'))
unzip(str_c(td, '/sic.zip'), file = 'gsp_sic_all.csv', exdir = td)
dat2 <- read_csv(str_c(td, '/gsp_sic_all.csv'))

# gsi/population
download.file(str_c(pag, '/spi.zip'), str_c(td, '/spi.zip'))
unzip(str_c(td, '/spi.zip'), file = 'SA1_1929_2016.csv', exdir = td)
dat3 <- read_csv(str_c(td, '/SA1_1929_2016.csv'))

unlink(td)


# ----- NAICS 1997 to 2016 ------------------------------------------------

# add fips (na's correspond to notes)
naics <- dat1 %>%
  mutate(
    fips = as.numeric(str_sub(GeoFIPS, 1, 2))
    ) %>%
  filter(!is.na(fips))

# reshape to wide and convert year
naics <- gather(naics, year, value, `1997`:`2016`) %>%
  mutate_at(vars(value, year), as.numeric)

# add fips (na's correspond to notes)
sic <- dat2 %>%
  mutate(
    fips = as.numeric(str_sub(GeoFIPS, 1, 2))
  ) %>%
  filter(!is.na(fips)) %>%
  select(-`1997`)


# ----- SIC 1963 to 1996 --------------------------------------------------

# reshape to wide and convert year
sic <- gather(sic, year, value, `1963`:`1996`) %>%
  mutate_at(vars(value, year), as.numeric)

# add fips (na's correspond to notes)
gsi <- dat3 %>%
  mutate(
    fips = as.numeric(str_sub(GeoFIPS, 1, 2))
  ) %>%
  filter(!is.na(fips))


# ----- GSI and Population -----------------------------------------------

# reshape to wide and convert value & year 
gsi <- gather(gsi, year, value, `1929`:`2016`) %>%
  mutate_at(vars(value, year), as.numeric)

# split into gsi and population tables
bea_gsi <- filter(gsi, LineCode == 1)
bea_pop <- filter(gsi, LineCode == 2)


# ----- Tidy Up ----------------------------------------------------------
 
# bind gsp together
bea_gsp <- rbind(sic, naics)

# names
names(bea_gsi) %<>% gsub(' ', '_', .) %>% tolower()
names(bea_gsp) %<>% gsub(' ', '_', .) %>% tolower()
names(bea_pop) %<>% gsub(' ', '_', .) %>% tolower()

# add units
bea_gsi %<>% mutate(
  units = str_extract(description, "\\([^()]+\\)"),
  units = str_replace(units, '\\(', ''),
  units = str_replace(units, '\\)', ''),
  description = 'Personal income'
)
bea_pop %<>% mutate(
  units = str_extract(description, "\\([^()]+\\)"),
  units = str_replace(units, '\\(', ''),
  units = str_replace(units, '\\)', ''),
  description = 'Population'
)
bea_gsp %<>% mutate(
  units = 'millions of current dollars'
)


# ----- Add to Database --------------------------------------------------

copy_to(db, bea_gsp, temporary = F, overwrite = T)
copy_to(db, bea_gsi, temporary = F, overwrite = T)
copy_to(db, bea_pop, temporary = F, overwrite = T)
