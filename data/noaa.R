# ----- Preample ----------------------------------------------------------


# ----- Cooling and Heating FTP -------------------------------------------

ftp <- 'ftp://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/daily_data/'
cooling <- '/StatesCONUS.Cooling.txt'
heating <- '/StatesCONUS.Heating.txt'

# years
y <- RCurl::getURL(ftp, ftp.use.epsv = F, dirlistonly = T)
y %<>% str_split('\n') %>% unlist()
y %<>% str_extract('\\d{4}')
y <- y[!is.na(y)]

# degree day helper function
dd_fun <- function(year, file = NULL) {
  suppressMessages(
    dat <- read_delim(str_c(ftp, year, file), delim = '|', skip = 3)
  )
  dat %<>% gather(date, deg_days, -Region)
  dat %<>% rename(statecode = Region)
  dat %<>% mutate(date = ymd(date))
}

# cooling/heating degree days (all years)
cdd <- map_dfr(y, dd_fun, file = cooling)
hdd <- map_dfr(y, dd_fun, file = heating)

# combine
dd_st <- full_join(cdd, hdd, by = c('statecode', 'date'))
names(dd_st)[3:4] <- c('cdd', 'hdd')


# ----- Add to Database ---------------------------------------------------

dd_st %<>% mutate_if(is.Date, as.character)
copy_to(db, dd_st, temporary = F, overwrite = T)
