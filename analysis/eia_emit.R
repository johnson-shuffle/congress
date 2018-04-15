# ----- Preample ----------------------------------------------------------


# ----- Fuel Consumption Data ---------------------------------------------

# non utility data 1989 - 1998
con1 <- tbl(db, 'eia_f867') %>%
  select(
    year,
    facility_code,
    fuel_code_standardized_to_2002_codes,
    `fuel_consumption_for_electric_power_generation(mmbtu)`
    ) %>%
  group_by(year, facility_code, fuel_code_standardized_to_2002_codes) %>%
  summarise(
    con = sum(`fuel_consumption_for_electric_power_generation(mmbtu)`)
    ) %>%
  ungroup() %>%
  collect()
names(con1) <- c('year', 'plant_id', 'esource', 'con')

# non utility data 1999 and 2000
con2 <- tbl(db, 'eia_f906n') %>%
  select(
    year,
    facilityid,
    fueltype,
    con
    ) %>%
  group_by(year, facilityid, fueltype) %>%
  filter(!is.na(fueltype)) %>%
  group_by(year, facilityid, fueltype) %>%
  summarise(
    con = sum(con)
    ) %>%
  ungroup() %>%
  collect()
names(con2) <- c('year', 'plant_id', 'esource', 'con')

# utility data 1970 to 2000
con3 <- tbl(db, 'eia_f906') %>%
  select(
    year,
    pcode,
    fueltyp,
    con
    ) %>%
  group_by(year, pcode, fueltyp) %>%
  filter(!is.na(fueltyp)) %>%
  group_by(year, pcode, fueltyp) %>%
  summarise(
    con = sum(con)
    ) %>%
  ungroup() %>%
  collect()
names(con3) <- c('year', 'plant_id', 'esource', 'con')

# utility data 2001 to 2007
con4 <- tbl(db, 'eia_f923a') %>%
  select(
    year, 
    plant_id, 
    reported_fuel_type_code, 
    elec_fuel_consumption_mmbtus
    ) %>%
  filter(!is.na(reported_fuel_type_code)) %>%
  group_by(year, plant_id, reported_fuel_type_code) %>%
  summarise(
    con = sum(elec_fuel_consumption_mmbtus)
    ) %>%
  ungroup() %>% 
  collect()
names(con4) <- c('year', 'plant_id', 'esource', 'con')

# utility data 2008 to 2016
con5 <- tbl(db, 'eia_f923b') %>%
  select(
    year, 
    plant_id,
    reported_fuel_type_code, 
    elec_fuel_consumption_mmbtus
    ) %>%
  filter(!is.na(reported_fuel_type_code)) %>%
  group_by(year, plant_id, reported_fuel_type_code) %>%
  summarise(
    con = sum(elec_fuel_consumption_mmbtus)
    ) %>%
  ungroup() %>%
  collect()
names(con5) <- c('year', 'plant_id', 'esource', 'con')


# ----- Calculate Emissions -----------------------------------------------

eia_factors <- tbl(db, 'eia_factors') %>% collect()

e1 <- left_join(con1, eia_factors) %>%
  mutate(co2 = con * kg_co2_per_mmbtu) %>%
  group_by(year, plant_id, esource) %>%
  summarise(co2 = sum(co2, na.rm = T))

e2 <- left_join(con2, eia_factors) %>%
  mutate(co2 = con * kg_co2_per_unit) %>%
  group_by(year, plant_id, esource) %>%
  summarise(co2 = sum(co2, na.rm = T))

e3 <- left_join(con3, eia_factors) %>%
  mutate(co2 = con * kg_co2_per_unit) %>%
  group_by(year, plant_id, esource) %>%
  summarise(co2 = sum(co2, na.rm = T))

e4 <- con4 %>% left_join(eia_factors) %>%
  mutate(co2 = con * kg_co2_per_mmbtu) %>%
  group_by(year, plant_id, esource) %>%
  summarise(co2 = sum(co2, na.rm = T))

e5 <- con5 %>% left_join(eia_factors) %>%
  mutate(co2 = con * kg_co2_per_mmbtu) %>%
  group_by(year, plant_id, esource) %>%
  summarise(co2 = sum(co2, na.rm = T))

# bind
co2 <- rbind(e1, e2, e3, e4, e5)


# ----- Add Congress ------------------------------------------------------

congress <- tibble(
  year = 1970:2016,
  congress = c(91, map(92:114, rep, times = 2) %>% unlist())
)
co2 %<>% left_join(congress)


# ----- Add Spatial Information -------------------------------------------

eia_gps <- tbl(db, 'eia_gps') %>% collect()
names(eia_gps)[1] <- 'plant_id'

dat <- left_join(co2, eia_gps)

x1 <- dat %>%
  filter(!is.na(fips)) %>%
  group_by(year, fips) %>% 
  summarise(co2 = sum(co2) / 1E9) %>% 
  ungroup() %>%
  left_join(collect(tbl(db, 'states')), by = 'fips')

y1 <- tbl(db, 'eia_sect') %>%
  filter(variable == 'Total') %>%
  filter(sector == 'electric power') %>%
  collect()

y2 <- tbl(db, 'eia_emit') %>%
  filter(energy_source == 'All Sources') %>%
  filter(producer_type == 'Electric Utility') %>%
  filter(type == 'CO2') %>%
  collect()

z1 <- left_join(x1, y1, by = c('year' = 'year', 'statedscr' = 'statecode')) %>%
  select(year, statecode, co2, value) %>%
  mutate(r = co2 / value) %>%
  arrange(desc(r), year)

p <- ggplot(aes(factor(year), r), data = filter(z1, !statecode %in% c('ID', 'VT'))) +
  #geom_text(aes(label = statecode)) +
  geom_boxplot()
p
