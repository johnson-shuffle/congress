# ----- preample ----------------------------------------------------------


# ----- download and unzip ------------------------------------------------

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


# ----- naics 1997 to 2016 ------------------------------------------------

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


# ----- sic 1963 to 1996 --------------------------------------------------

# reshape to wide and convert year
sic <- gather(sic, year, value, `1963`:`1996`) %>%
  mutate_at(vars(value, year), as.numeric)

# add fips (na's correspond to notes)
gsi <- dat3 %>%
  mutate(
    fips = as.numeric(str_sub(GeoFIPS, 1, 2))
  ) %>%
  filter(!is.na(fips))


# ----- gsi and population -----------------------------------------------

# reshape to wide and convert value & year 
gsi <- gather(gsi, year, value, `1929`:`2016`) %>%
  mutate_at(vars(value, year), as.numeric)

# split into gsi and population tables
bea_gsi <- filter(gsi, LineCode == 1)
bea_pop <- filter(gsi, LineCode == 2)


# ----- tidy up ----------------------------------------------------------
 
# bind gsp together
bea_gsp <- rbind(sic, naics)

# names
names(bea_gsi) %<>% str_replace(' ', '_') %>% tolower()
names(bea_pop) %<>% str_replace(' ', '_') %>% tolower()
names(bea_gsp) %<>% str_replace(' ', '_') %>% tolower()

# renames
bea_gsi %<>% rename(series = table)
bea_pop %<>% rename(series = table)

# drop national and regional totals
bea_gsi %<>% filter(fips != 0 & fips <= 56)
bea_pop %<>% filter(fips != 0 & fips <= 56)
bea_gsp %<>% filter(fips != 0 & fips <= 56)

# add units
bea_gsi %<>% 
  mutate(
    units = str_extract(description, "\\([^()]+\\)"),
    units = str_replace_all(units, '\\(|\\)', ''),
    description = "Personal income"
  )

bea_pop %<>% 
  mutate(
    units = str_extract(description, "\\([^()]+\\)"),
    units = str_replace_all(units, '\\(|\\)', ''),
    description = "Population"
  )

bea_gsp %<>% 
  mutate(
    units = 'millions of current dollars'
  )


# ----- add to database --------------------------------------------------

dbWriteTable(db, "bea_gsp", bea_gsp, append = T, row.names = F)
dbWriteTable(db, "bea_gsi", bea_gsi, append = T, row.names = F)
dbWriteTable(db, "bea_pop", bea_pop, append = T, row.names = F)
