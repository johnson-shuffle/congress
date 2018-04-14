# ----- Preample ----------------------------------------------------------

td <- tempdir()


# ----- State Information -------------------------------------------------

states <- tbl(db, sql("SELECT statecode, statedscr FROM states"))
states <- collect(states)
  

# ----- EIA State Electricity Profiles (disposition) 1990 to 2015 ---------

disp_fun <- function(state, pb = NULL, dir = NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  state <- as.character(state)
  
  # file extension
  ext <- str_c(state[2], '/xls/', state[1], '.xlsx') %>% tolower()
  ext <- gsub(' ', '', ext)
  
  # download
  pag <- 'https://www.eia.gov/electricity/state/'
  download.file(str_c(pag, ext), dest = str_c(dir, '/tmp.xlsx'))
  
  # read
  dat <- read_excel(
    str_c(dir, '/tmp.xlsx'),
    sheet = '10. Source-Disposition',
    skip = 3
    )
  names(dat)[1] <- 'category'
  
  # breaks
  b <- c(
    grep("Electric utilities", dat$category),
    grep("Combined heat and power, commercial", dat$category),
    grep("Total net generation", dat$category),
    grep("Total international imports", dat$category),
    grep("Total international exports", dat$category),
    grep("Net interstate trade", dat$category)
  )
  
  # net generation, international imports/exports, and net interstate trade
  dat <- dat[c(b[1]:(b[1] + 2), b[2]:(b[2] + 1), b[3:6]), ]
  dat$category <- gsub('\\.\\.', '', dat$category) %>% tolower() %>% str_trim()
  
  # convert to numeric
  dat %<>% mutate_at(vars(2:ncol(dat)), as.numeric)
  dat %<>% mutate_at(vars(2:ncol(dat)), function(x) replace(x, is.na(x), 0))
  
  # reshape
  dat <- gather(dat, year, value, -1)
  dat$year <- str_extract(dat$year, '\\d{4}') %>% as.numeric()
  dat$statecode <- state[2]
  dat$units <- 'megawatthours'
  return(dat)

}

disp <- map(1:51, function(x) disp_fun(states[x, ], dir = td))
eia_disp <- do.call(rbind, disp)


# ----- EIA State Emissions Profiles by Sector 1980 to 2014 ---------------

sect_fun <- function(state, pb = NULL, dir = NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  state <- as.character(state)
  
  # file extension
  ext <- str_c(state[2], '.xlsx') %>% tolower()
  ext <- gsub(' ', '%20', ext)
  
  # download
  pag <- 'https://www.eia.gov/environment/emissions/state/excel/'
  download.file(str_c(pag, ext), dest = str_c(dir, '/tmp.xlsx'))
  
  # read
  dat <- read_excel(str_c(dir, '/tmp.xlsx'), skip = 3)
  names(dat)[2] <- 'variable'
  
  # breaks
  b <- grep('Coal', dat$variable)
  
  # gather pieces
  dat <- map(b, function(x) dat[x:(x + 3), 2:ncol(dat)])
  dat <- do.call(rbind, dat)
  dat$sector <- c(
    rep('residential', 4),
    rep('commericial', 4),
    rep('industrial', 4),
    rep('transportation', 4),
    rep('electric power', 4)
  )
  
  # reshape
  dat <- gather(dat, year, value, -variable, -sector)
  dat$year <- as.numeric(dat$year)
  dat$statecode <- state[2]
  dat$units <- 'million metric tons of carbon dioxide'
  return(dat)
  
}

sect <- map(1:51, function(x) sect_fun(states[x, ], dir = td))
eia_sect <- do.call(rbind, sect)


# ----- EIA State Emissions from Electricity 1990 to 2015 -----------------

download.file(
  'https://www.eia.gov/electricity/data/state/emission_annual.xls',
  destfile = str_c(td, '/tmp.xls')
)
dat <- read_excel(str_c(td, '/tmp.xls'))

# reshape
eia_emit <- gather(dat, type, value, -1:-4)

# names
names(eia_emit) %<>% gsub(' ', '_', .) %>% tolower()

# units
eia_emit %<>% mutate(
  units = str_extract(type, '\\([^()]+\\)'),
  units = str_replace(units, '\\(', ''),
  units = str_replace(units, '\\)', ''),
  type  = str_sub(type, 1, 3)
)


# ----- Add to Database ---------------------------------------------------

copy_to(db, eia_disp, temporary = F, overwrite = T)
copy_to(db, eia_sect, temporary = F, overwrite = T)
copy_to(db, eia_emit, temporary = F, overwrite = T)
