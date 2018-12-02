# ----- Preample ----------------------------------------------------------

library(rgdal)


# ----- Old GET Request ---------------------------------------------------

#htm <- GET(
#  'http://prism.oregonstate.edu/fetchData.php',
#  query = list(
#    type = 'all_asc',
#    kind = 'recent',
#    elem = type,
#    range = 'monthly',
#    temporal = year
#  ),
#  write_disk(str_c(td, '/data.zip'), overwrite = T)
#  )


# ----- Weather Data ------------------------------------------------------

# helper function
prism_fun <- function(y, pb = NULL, td = NULL) {
  
  stopifnot(!is.null(td))
  
  if (!is.null(pb)) pb$tick()$print()
  
  # daily .bil files
  zip <- list.files(file.path(td, y))
  fln <- map(zip, ~unzip(file.path(td, y, .x), exdir = td))
  bil <- list.files(td, pattern = '.bil$')
  sdf <- map(file.path(td, bil), readGDAL, silent = T)
  
  # remove the daily files
  map(fln, file.remove)
  
  # extract data points from spatial data frames
  xxx <- sdf[[1]]
  yyy <- map(sdf[2:length(sdf)], ~.x@data)
  
  # cbind all data to first file sdf
  xxx@data <- cbind(xxx@data, do.call(cbind, yyy))
  xlabs <- str_sub(str_extract_all(zip, '\\d{8}'), 5, 8)
  names(xxx@data) <- str_c('d_', xlabs)

  # district boundaries
  congress <- map(103:114, rep, times = 2) %>% unlist()
  names(congress) <- 1993:2016
  cd <- readOGR('congress_gis.sqlite', str_c('cd', congress[as.character(y)]))
  cd <- spTransform(cd, proj4string(xxx))
  
  # overlay mean of mean onto districts
  out <- over(cd, xxx, fn = mean, na.rm = T) ; gc()
  out$year <- y
  
  # attach district information
  out <- cbind(xxx@data[c('ID', 'DISTRICT')], out)
  
  return(out)
  
}

pb <- progress_estimated(length(1993:2016))
tmean <- map(1993:2016, prism_fun, pb = pb, td = '~/Downloads/prism/tmean')


# ----- Tidy Up -----------------------------------------------------------

cd_tmean <- do.call(plyr::rbind.fill, tmean)

# fips
cd_tmean$fips <- str_sub(z$ID, 1, 3) %>% as.numeric()
cd_tmean %<>% select(-ID)

# names
names(cd_tmean) %<>% tolower()

# gather
cd_tmean %<>% gather(date, tmean, -year, -district, -fips)
cd_tmean %<>% mutate(
  date = str_split(date, '_', simplify = T)[, 2],
  date = ymd(str_c(year, date))
  ) %>%
  select(-year)

# drop feb 29 (propagated through every year)
cd_tmean %<>% filter(!is.na(date))

# degree days
ddl <- (9 / 5) * cd_temp$tmean + 32 - 65 >= 0
ddv <- abs(round((9 / 5) * cd_temp$tmean + 32 - 65))
dd_cd <- cd_temp %>%
  mutate(
    cdd = if_else(ddl, ddv, 0),
    hdd = if_else(!ddl, ddv, 0)
  )


# ----- Add to Database ---------------------------------------------------

dd_cd %<>% mutate_if(is.Date, as.character)
copy_to(db, dd_cd, temporary = F, overwrite = T)
