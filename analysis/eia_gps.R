# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
load_tidy()
library(rgdal)
library(rgeos)

db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

# ------------------------------------------------------------------------------
# load the data
# ------------------------------------------------------------------------------
load(str_c('~/GoogleDrive/Projects/congress/cd_maps.Rda'))

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

coordinates(f860_sp) <- c('longitude', 'latitude')
proj4string(f860_sp) <- proj4string(cd_maps[[1]])

# ------------------------------------------------------------------------------
# loop through plants using relevant district maps
# ------------------------------------------------------------------------------
dat_list <- list()
for (i in seq_along(cd_maps)) {
  
  # district map
  tmp1 <- cd_maps[[i]]
  
  # spatial merge
  tmp2 <- over(f860_sp, tmp1)
  tmp2 <- f860_sp@data %>% mutate(district_code = tmp2$DISTRICT)
  tmp2$congress <- c(103:114)[i]
  
  # keep longitude and latitude
  tmp2$longitude <- f860_sp@coords[, 'longitude']
  tmp2$latitude <- f860_sp@coords[, 'latitude']
  
  # populate lists
  dat_list[[i]] <- tmp2
  
  rm(tmp1, tmp2, i)
  
}

dat <- do.call(rbind, dat_list)
mis <- filter(dat, is.na(district_code))
dat <- filter(dat, !is.na(district_code))

# ------------------------------------------------------------------------------
# guess missing districts using minimum distance
# ------------------------------------------------------------------------------
for (n in 1:nrow(mis)) {
  
  # plant coordinates
  tmp1 <- f860_sp[f860_sp$plant_code == mis$plant_code[n], ]
  
  # district map (subsetted using plant's state)
  tmp2 <- cd_maps[[which(103:114 == mis$congress[n])]]
  tmp2 <- tmp2[tmp2$STATENAME == mis$statedscr[n], ]
  
  # distances to each district
  tmp3 <- gDistance(tmp2, tmp1, byid = T)
  
  # assign based on minimum
  mis$district_code[n] <- tmp2$DISTRICT[which(tmp3 == min(tmp3))]
  
  rm(tmp1, tmp2, tmp3, n)
  
}

dat <- rbind(dat, mis)

# ------------------------------------------------------------------------------
# tidy up
# ------------------------------------------------------------------------------

# change classes
eia_gps <- dat %>% mutate_at(vars(fips, district_code, congress), as.numeric)

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, eia_gps, temporary = F, overwrite = T)
