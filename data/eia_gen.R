# ----- preample ----------------------------------------------------------

td <- tempdir()


# ----- eia form 767 ------------------------------------------------------

f767_fun <- function(year, pb = NULL, dir = NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  # file extension
  ext <- str_c('f767_', year, '.zip')
  
  # download
  pag <- 'https://www.eia.gov/electricity/data/eia767/zip/'
  download.file(str_c(pag, ext), destfile = str_c(dir, '/tmp.zip'))
  
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


# ----- eia form 906 ------------------------------------------------------

f906_fun <- function(year, pb = NULL, dir = NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  # file extension
  ext <- str_c(year, 'u.xls')
  
  # download
  pag <- 'https://www.eia.gov/electricity/data/eia923/archive/xls/utility/'
  download.file(str_c(pag, 'f759', ext), destfile = str_c(dir, '/tmp.xls'))
  
  # read
  dat <- read_xls(str_c(dir, '/tmp.xls'))
  names(dat) %<>% gsub(' ', '_', .) %>% tolower()
  
  return(dat)
  
}

f906_list <- map(1970:2000, f906_fun, dir = td)

# patch
f906_patch_fun <- function(dat) {
  
  # fix year
  dat %<>%
    mutate(
      year  = if_else(year == 0, 2000, year),
      year  = if_else(year < 100, year + 1900, year)
    ) %>%
    mutate_at(vars(fipst, year, utilcode), as.numeric)
  
  yr <- dat$year[1]
  
  # totals
  if (yr <= 1995) {
    
    dat %>%
      mutate(
        consumptio = rowSums(select(., starts_with("con"))),
        netgenerat = rowSums(select(., starts_with("gen"))),
        stocks = rowSums(select(., starts_with("stk"))),
      ) 
    dat <- dat[!str_detect(names(dat), "[a-z]{3}\\d{2}")]
    
  }
  
  # utility codes
  if ( (yr >= 1970 & yr <= 1984) | (yr >= 1991 & yr <= 1995) ) {
    
    dat$ucode <- dat$utilcode
    dat$utilcode <- NA
    
  }
  
  return(dat)
  
}

f906_list %<>% map(f906_patch_fun)


# ----- eia form 867/906 (non-utility) ------------------------------------

nonu_fun <- function(year, pb = NULL, dir = NULL) {
  
  stopifnot(year == 1989 | (year >= 1999 & year <= 2000))
  
  if (!is.null(pb)) pb$tick()$print()
  
  # extension
  ext <- str_c('f906nonutil', year, '.zip')
  
  # download
  pag <- 'https://www.eia.gov/electricity/data/eia923/archive/xls/'
  download.file(str_c(pag, ext), destfile = str_c(dir, '/tmp.zip'))
  
  # filename
  fln <- unzip(str_c(dir, '/tmp.zip'), list = T) %>% arrange(desc(Length))
  fln <- fln$Name[1]
  
  # unzip
  unzip(str_c(dir, '/tmp.zip'), fln, exdir = dir)
  
  # read
  dat <- read_excel(str_c(dir, fln, sep = '/'))
  
  return(dat)
  
}

f906n_list <- map(c(1989, 1999:2000), nonu_fun, dir = td)

# patch
nonu_patch_fun <- function(dat) {
  
  yr <- dat$YEAR[1]
  
  if (yr != 1989) {
    
    dat %<>%
      mutate(
        consumptio = rowSums(select(., ends_with("CONSUMP"))),
        netgenerat = rowSums(select(., ends_with("GENERAT")))
      )
    
  } else {
    
    dat %<>%
      rename(consumptio = con, netgenerat = gen)
  
  }
  
  dat[!str_detect(names(dat), "CONSUMP$|GENERAT$")]
  
  return(dat)
  
}
# ----- eia form 920/923 --------------------------------------------------

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
  download.file(str_c(pag, ext), destfile = str_c(dir, '/tmp.zip'))
  
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


# ----- tidy up -----------------------------------------------------------

# f767 - add year to 1985-2000 and bind
f767_list[1:16] <- map(1:16, ~mutate(f767_list[[.x]], YEAR = c(1985:2005)[.x]))
eia_f767 <- do.call(plyr::rbind.fill, f767_list)

# f867
eia_f867 <- nonu_fun(1989, dir = td)

# f906nu - bind
eia_f906n <- do.call(rbind, f906n_list)

# f923a - bind
eia_f923a <- do.call(plyr::rbind.fill, f923a_list)

# f923b - manual fix to names and bind
f923b_list_n <- map(f923b_list, ~setNames(.x, names(f923b_list[[1]])))
eia_f923b <- do.call(rbind, f923b_list_n)

# names
names(eia_f767)  %<>% str_replace_all(' ', '_') %>% tolower()
names(eia_f867)  %<>% str_replace_all(' ', '_') %>% tolower()
names(eia_f906)  %<>% str_replace_all(' ', '_') %>% tolower()
names(eia_f906n) %<>% str_replace_all(' ', '_') %>% tolower()
names(eia_f923a) %<>% str_replace_all(' ', '_') %>% tolower()
names(eia_f923b) %<>% str_replace_all(' ', '_') %>% tolower()

# change classes
eia_f906$pcode %<>% as.numeric()
i1 <- match('quantity_jan', names(eia_f923b))
i2 <- match('netgen_dec', names(eia_f923b))
eia_f923b %<>% mutate_at(i1:i2, as.numeric)


# ----- add to database ---------------------------------------------------

copy_to(db, eia_f767,  temporary = F, overwrite = T)
copy_to(db, eia_f867,  temporary = F, overwrite = T)
copy_to(db, eia_f906,  temporary = F, overwrite = T)
copy_to(db, eia_f906n, temporary = F, overwrite = T)
copy_to(db, eia_f923a, temporary = F, overwrite = T)
copy_to(db, eia_f923b, temporary = F, overwrite = T)
