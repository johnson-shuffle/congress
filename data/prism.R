# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
load_tidy()
library(sp)

db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

# ------------------------------------------------------------------------------
# prism weather data
# ------------------------------------------------------------------------------
prism_fun <- function(year, type = 'NULL', pb = NULL) {
  
  stopifnot(!is.null(type))
  
  if (!is.null(pb)) pb$tick()$print()
  
  # get request
  htm <- GET(
    'http://prism.oregonstate.edu/fetchData.php',
    query = list(
      type = 'all_asc',
      kind = 'recent',
      elem = type,
      range = 'monthly',
      temporal = year
    ),
    write_disk(str_c(td, '/data.zip'), overwrite = T)
    )
  
  # locate monthly .asc files
  tmp <- unzip(str_c(td, '/data.zip'), list = T)
  fln <- tmp$Name[str_detect(tmp$Name, '_\\d{4}_asc\\.asc$')]
  
  # unzip and convert to spatial objects
  asc <- unzip(str_c(td, '/data.zip'), files = fln, exdir = td)
  map(asc, read.asciigrid)
  
}

td <- tempdir()

# precipitation
pb <- progress_estimated(length(1993:2016))
ppt <- map(1993:2016, prism_fun, type = 'ppt', pb = pb)
save(ppt, file = '~/GoogleDrive/Projects/congress/prism_ppt.Rda')
rm(ppt) ; gc()
file.remove(str_c(td, dir(path = td, pattern = '\\.asc'), sep = '/'))

# min temperature
pb <- progress_estimated(length(1993:2016))
tmin <- map(1993:2016, prism_fun, type = 'tmin', pb = pb)
save(tmin, file = '~/GoogleDrive/Projects/congress/prism_tmin.Rda')
rm(tmin) ; gc()
file.remove(str_c(td, dir(path = td, pattern = '\\.asc'), sep = '/'))

# max temperature
pb <- progress_estimated(length(1993:2016))
tmax <- map(1993:2016, prism_fun, type = 'tmax', pb = pb)
save(tmax, file = '~/GoogleDrive/Projects/congress/prism_tmax.Rda')
rm(tmax) ; gc()
file.remove(str_c(td, dir(path = td, pattern = '\\.asc'), sep = '/'))

# mean temperature
pb <- progress_estimated(length(1993:2016))
tmean <- map(1993:2016, prism_fun, type = 'tmean', pb = pb)
save(tmean, file = '~/Desktop/prism_tmean.Rda')
rm(tmean) ; gc()
file.remove(str_c(td, dir(path = td, pattern = '\\.asc'), sep = '/'))
