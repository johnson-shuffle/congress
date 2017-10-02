# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
load_tidy()

db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

td <- tempdir()

# ------------------------------------------------------------------------------
# eia form 767 data
#   https://www.eia.gov/electricity/data/eia767/
# ------------------------------------------------------------------------------
f767_fun <- function(year, pb = NULL, dir = NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  # file extension
  ext <- str_c('f767_', year, '.zip')
  
  # download
  pag <- 'https://www.eia.gov/electricity/data/eia767/zip/'
  download.file(str_c(pag, ext), dest = str_c(dir, '/tmp.zip'))
  
  # filename
  fln <- unzip(str_c(dir, '/tmp.zip'), list = T) %>% arrange(desc(Length))
  fl1 <- str_detect(fln$Name, fixed('boiler', ignore_case =T))
  fl2 <- str_detect(fln$Name, fixed('fuel', ignore_case =T))
  fln <- fln$Name[as.logical(fl1 * fl2)]
  
  # unzip
  unzip(str_c(dir, '/tmp.zip'), fln, exdir = dir)
  
  # read
  dat <- read_xls(str_c(dir, fln, sep = '/'))
  return(dat)
}

f767_list <- map(1985:2005, f767_fun, dir = td)

# ------------------------------------------------------------------------------
# eia form 906 data
#   https://www.eia.gov/electricity/data/eia923/eia906u.html
# ------------------------------------------------------------------------------
f906_fun <- function(year, pb = NULL, dir = NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  # file extension
  ext <- str_c(year, 'u.xls')
  
  # download
  pag <- 'https://www.eia.gov/electricity/data/eia923/xls/utility/'
  download.file(str_c(pag, 'f759', ext), dest = str_c(dir, '/tmp.xls'))
  
  # read
  dat <- read_xls(str_c(dir, '/tmp.xls'))
  names(dat) %<>% gsub(' ', '_', .) %>% tolower()
  
  # totals
  if (year <= 1995) {
    dat$con <- apply(dat[str_detect(names(dat), 'con\\d{2}')], 1, sum)
    dat$gen <- apply(dat[str_detect(names(dat), 'gen\\d{2}')], 1, sum)
    dat$stk <- apply(dat[str_detect(names(dat), 'stk\\d{2}')], 1, sum)
  } else {
    dat %<>% rename(con = consumptio)
    dat %<>% rename(gen = netgenerat)
    dat %<>% rename(stk = stocks)
  }
  
  return(dat)
  
}

f906_list <- map(1970:2000, f906_fun, dir = td)

# ------------------------------------------------------------------------------
# eia form 867/906 data (non-utility)
#   https://www.eia.gov/electricity/data/eia923/eia906u.html
# ------------------------------------------------------------------------------
nonu_fun <- function(year, pb = NULL, dir = NULL) {
  
  stopifnot(year == 1989 | (year >= 1999 & year <= 2000))
  
  if (!is.null(pb)) pb$tick()$print()
  
  # extension
  ext <- str_c('f906nonutil', year, '.zip')
  
  # download
  pag <- 'https://www.eia.gov/electricity/data/eia923/xls/'
  download.file(str_c(pag, ext), dest = str_c(dir, '/tmp.zip'))
  
  # filename
  fln <- unzip(str_c(dir, '/tmp.zip'), list = T) %>% arrange(desc(Length))
  fln <- fln$Name[1]
  
  # unzip
  unzip(str_c(dir, '/tmp.zip'), fln, exdir = dir)
  
  # read
  dat <- read_excel(str_c(dir, fln, sep = '/'))
  
  # totals
  if (year != 1989) {
    dat$con <- apply(dat[str_detect(names(dat), 'CONSUMP$')], 1, sum)
    dat$gen <- apply(dat[str_detect(names(dat), 'GENERAT$')], 1, sum)
  }
  
  return(dat)
  
}

f906n_list <- map(1999:2000, nonu_fun, dir = td)

# ------------------------------------------------------------------------------
# eia form 920/923 data
#   https://www.eia.gov/electricity/data/eia923/
# ------------------------------------------------------------------------------
f923_fun <- function(year, pb = NULL, dir = NULL) {
  
  stopifnot(year >= 2001, year <= 2016)
  
  if (!is.null(pb)) pb$tick()$print()
  
  # extension
  if (year <=2007) {
    ext <- str_c('f906920_', year, '.zip')
  } else {
    ext <- if_else(year < 2016, '.zip', 'er.zip')
    ext <- str_c('f923_', year, ext)
  }
    
  # download
  pag <- 'https://www.eia.gov/electricity/data/eia923/xls/'
  download.file(str_c(pag, ext), dest = str_c(dir, '/tmp.zip'))
  
  # filename
  fln <- unzip(str_c(dir, '/tmp.zip'), list = T) %>% arrange(desc(Length))
  fln <- fln$Name[1]
  
  # unzip
  unzip(str_c(dir, '/tmp.zip'), fln, exdir = dir)
  
  # find top left
  tpl <- read_excel(str_c(dir, fln, sep = '/'), col_names = F, n_max = 10)
  tpl %<>% mutate_all(tolower)
  row <- which(tpl == 'plant id', arr.ind = T)[1, 'row'] %>% as.numeric()
  col <- which(tpl == 'plant id', arr.ind = T)[1, 'col'] %>% as.numeric()
  
  # read
  dat <- read_excel(str_c(dir, fln, sep = '/'), skip = (row - 1))
  dat <- dat[, col:ncol(dat)]
  names(dat) %<>% tolower()
  return(dat)
  
}

# eia form 906/920/923 data
f923a_list <- map(2001:2007, f923_fun, dir = td)

# eia form 923 data
f923b_list <- map(2008:2016, f923_fun, dir = td)

# ------------------------------------------------------------------------------
# tidy up
# ------------------------------------------------------------------------------

# f767 - add year to 1985-2000 and bind
f767_list[1:16] <- map(1:16, ~mutate(f767_list[[.x]], YEAR = c(1985:2005)[.x]))
eia_f767 <- do.call(plyr::rbind.fill, f767_list)

# f867
eia_f867 <- nonu_fun(1989, dir = td)

# f906 - bind and manual fixes to utility code
eia_f906 <- do.call(plyr::rbind.fill, f906_list)
eia_f906 %<>%
  mutate_at(vars(fipst, year, utilcode), as.numeric) %>%
  mutate(
    ucode = if_else(year >= 70 & year <= 84, utilcode, ucode),
    ucode = if_else(year >= 91 & year <= 95, utilcode, ucode),
    utilcode = ifelse(ucode == utilcode, NA, utilcode),
    year  = if_else(year == 0, 2000, year),
    year  = if_else(year < 100, year + 1900, year)
  )

# f906nu - bind
eia_f906n <- do.call(rbind, f906n_list)

# f923a - bind
eia_f923a <- do.call(plyr::rbind.fill, f923a_list)

# f923b - manual fix to names and bind
f923b_list_n <- map(f923b_list, ~setNames(.x, names(f923b_list[[1]])))
eia_f923b <- do.call(rbind, f923b_list_n)

# names
names(eia_f767) %<>% gsub(' ', '_', .) %>% tolower()
names(eia_f867) %<>% gsub(' ', '_', .) %>% tolower()
names(eia_f906) %<>% gsub(' ', '_', .) %>% tolower()
names(eia_f906n) %<>% gsub(' ', '_', .) %>% tolower()
names(eia_f923a) %<>% gsub(' ', '_', .) %>% tolower()
names(eia_f923b) %<>% gsub(' ', '_', .) %>% tolower()

# change classes
eia_f906$pcode %<>% as.numeric()
i1 <- match('quantity_jan', names(eia_f923b))
i2 <- match('netgen_dec', names(eia_f923b))
eia_f923b %<>% mutate_at(i1:i2, as.numeric)

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, eia_f767, temporary = F, overwrite = T)
copy_to(db, eia_f867, temporary = F, overwrite = T)
copy_to(db, eia_f906, temporary = F, overwrite = T)
copy_to(db, eia_f906n, temporary = F, overwrite = T)
copy_to(db, eia_f923a, temporary = F, overwrite = T)
copy_to(db, eia_f923b, temporary = F, overwrite = T)
