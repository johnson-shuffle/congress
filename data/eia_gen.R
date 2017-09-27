# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
load_tidy()

db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

td <- tempdir()

# ------------------------------------------------------------------------------
# eia form 759 data
#   https://www.eia.gov/electricity/data/eia923/eia906u.html
# ------------------------------------------------------------------------------
f759_fun <- function(year, pb = NULL, dir = NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  # file extension
  ext <- if_else(year >= 1996, 'mu.xls', 'u.xls')
  ext <- str_c(year, ext)
  
  # download
  pag <- 'https://www.eia.gov/electricity/data/eia923/xls/utility/'
  download.file(str_c(pag, 'f759', ext), dest = str_c(dir, '/tmp.xls'))
  
  # read
  dat <- read_xls(str_c(dir, '/tmp.xls'))
  names(dat) %<>% gsub(' ', '_', .) %>% tolower()
  return(dat)
}

f759_list <- map(1970:2000, f759_fun, dir = td)

# ------------------------------------------------------------------------------
# eia form 906/923 data
#   https://www.eia.gov/electricity/data/eia923/
# ------------------------------------------------------------------------------
f906923_fun <- function(year, pb = NULL, dir = NULL) {
  
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

# eia form 906 data
f906_list <- map(2001:2007, f906923_fun, dir = td)

# eia form 923 data
f923_list <- map(2008:2016, f906923_fun, dir = td)

# ------------------------------------------------------------------------------
# tidy up
# ------------------------------------------------------------------------------

# f759 - bind and manual fixes to utility code
eia_f759 <- do.call(plyr::rbind.fill, f759_list)
eia_f759 %<>%
  mutate_at(vars(fipst, year, utilcode), as.numeric) %>%
  mutate(
    ucode = if_else(year >= 70 & year <= 84, utilcode, ucode),
    ucode = if_else(year >= 91 & year <= 95, utilcode, ucode),
    utilcode = ifelse(ucode == utilcode, NA, utilcode),
    year  = if_else(year == 0, 2000, year),
    year  = if_else(year < 100, year + 1900, year)
  )

# f906 - bind
eia_f906 <- do.call(plyr::rbind.fill, f906_list)

# f923 - bind and manual fix to names
f923_list_n <- map(f923_list, function(x) setNames(x, names(f923_list[[1]])))
eia_f923 <- do.call(rbind, f923_list_n)

# names
names(eia_f759) %<>% gsub(' ', '_', .) %>% tolower()
names(eia_f906) %<>% gsub(' ', '_', .) %>% tolower()
names(eia_f923) %<>% gsub(' ', '_', .) %>% tolower()

# change classes
i1 <- match('quantity_jan', names(eia_f923))
i2 <- match('netgen_dec', names(eia_f923))
eia_f923 <- mutate_at(eia_f923, i1:i2, as.numeric)

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, eia_f759, temporary = F, overwrite = T)
copy_to(db, eia_f906, temporary = F, overwrite = T)
copy_to(db, eia_f923, temporary = F, overwrite = T)
