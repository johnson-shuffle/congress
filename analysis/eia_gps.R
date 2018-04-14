# ----- Preample ----------------------------------------------------------

library(rgdal)
library(rgeos)


# ----- Load the Data -----------------------------------------------------

cdm <- readOGR(db_gis, 'cd103')

f860 <- tbl(db, sql(
  "SELECT DISTINCT 
   plant_code, street_address, city, zip, state, latitude, longitude, year
   FROM eia_f860"
  )) %>%
  filter(!is.na(longitude)) %>%
  filter(state != 'DC') %>%
  collect()

f860_2015 <- filter(f860, year == 2015)$plant_code
f860_2014 <- filter(f860, year == 2014)$plant_code  
f860_2013 <- filter(f860, year == 2013)$plant_code
f860_2012 <- filter(f860, year == 2012)$plant_code

pcs <- setdiff(f860_2015, f860_2014)
pcs <- union(pcs, f860_2013)
pcs <- union(pcs, f860_2012)

f860_sp <- f860 %>%
  filter(plant_code %in% pcs) %>%
  group_by(plant_code) %>%
  filter(year == max(year)) %>%
  select(-year) %>%
  left_join(collect(tbl(db, 'states')), by = c('state' = 'statecode')) %>%
  ungroup()

# project to match district maps
coordinates(f860_sp) <- c('longitude', 'latitude')
proj4string(f860_sp) <- proj4string(cdm)


# ----- Loop Through Maps and Match with Plants ---------------------------

dat_list <- list()
for (i in 103:114) {
  
  # district map
  tmp1 <- readOGR(db_gis, str_c('cd', i))
  
  # spatial merge
  tmp2 <- over(f860_sp, tmp1)
  tmp2 <- f860_sp@data %>% mutate(district_code = tmp2$DISTRICT)
  tmp2$congress <- i
  
  # keep longitude and latitude
  tmp2$longitude <- f860_sp@coords[, 'longitude']
  tmp2$latitude  <- f860_sp@coords[, 'latitude']
  
  # populate lists
  dat_list[[i]] <- tmp2
  
  rm(tmp1, tmp2, i)
  
}

dat <- do.call(rbind, dat_list)
mis <- filter(dat, is.na(district_code))
dat <- filter(dat, !is.na(district_code))


# ----- Guess Missing Districts via Minimum Distance ----------------------

# project to utm-15n (compromise for calculating distances)
f860_sp %<>% spTransform('+init=epsg:26915')

pb <- progress_estimated(length(1:nrow(mis)))
for (n in 1:nrow(mis)) {
  
  # plant coordinates
  tmp1 <- f860_sp[f860_sp$plant_code == mis$plant_code[n], ]
  
  # district map (subsetted using plant's state)
  tmp2 <- readOGR(db_gis, str_c('cd', mis$congress[n]), verbose = F)
  tmp2 <- tmp2[tmp2$STATENAME == mis$statedscr[n], ]
  
  # reproject to utm-15n
  tmp2 %<>% spTransform('+init=epsg:26915')
  
  # distances to each district
  tmp3 <- gDistance(tmp2, tmp1, byid = T)
  
  # assign based on minimum
  mis$district_code[n] <- tmp2$DISTRICT[which(tmp3 == min(tmp3))]
  
  rm(tmp1, tmp2, tmp3, n) ; pb$tick()$print()
  
}

dat <- rbind(dat, mis)


# ----- Tidy Up -----------------------------------------------------------

# change classes
eia_gps <- dat %>% mutate_at(vars(fips, district_code, congress), as.numeric)


# ----- Add to Database ----------------------------------------------------

copy_to(db, eia_gps, temporary = F, overwrite = T)
