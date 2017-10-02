# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
load_tidy()

db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

td <- tempdir()

# ------------------------------------------------------------------------------
# eia form 860 data
#   https://www.eia.gov/electricity/data/eia860/
# ------------------------------------------------------------------------------
f860_fun <- function(year, pb = NULL, dir = NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  # file extension
  ext <- str_c('eia860', year, '.zip')
  
  # download
  pag <- 'http://www.eia.gov/electricity/data/eia860/xls/'
  download.file(str_c(pag, ext), dest = str_c(dir, '/tmp.zip'))
  
  # filename
  fln <- unzip(str_c(dir, '/tmp.zip'), list = T)
  fln <- fln$Name[grep('plant', fln$Name, ignore.case = T)]
  
  # unzip
  unzip(str_c(dir, '/tmp.zip'), fln, exdir = dir)
  
  # read
  dat <- read_excel(str_c(dir, fln, sep = '/'), skip = 1)
  dat %<>% mutate(year = year)
  return(dat)

}
  
f860_list <- map(2012:2015, f860_fun, dir = td)
eia_f860 <- do.call(rbind, f860_list[2:4])
eia_f860 <- plyr::rbind.fill(eia_f860, f860_list[[1]])

# ------------------------------------------------------------------------------
# tidy up
# ------------------------------------------------------------------------------

# names
names(eia_f860) %<>% gsub(' ', '_', .) %>% tolower()

# change classes
eia_f860 %<>% mutate_if(is.integer, as.numeric)

# additions after 2012
ads <- names(f860_list[[2]])[!names(f860_list[[2]]) %in% names(f860_list[[1]])]

# test uniqueness
dat <- eia_f860[!names(eia_f860) %in% c('year', ads)] %>% distinct()
dup <- eia_f860$plant_code[duplicated(eia_f860$plant_code)]

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, eia_f860, temporary = F, overwrite = T)
