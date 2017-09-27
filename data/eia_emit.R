# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
load_tidy()

db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

td <- tempdir()

# ------------------------------------------------------------------------------
# estimated emissions by state 1990 to 2015
# ------------------------------------------------------------------------------
download.file(
  'https://www.eia.gov/electricity/data/state/emission_annual.xls',
  destfile = str_c(td, '/tmp.xls')
)
dat <- read_excel(str_c(td, '/tmp.xls'))

# ------------------------------------------------------------------------------
# tidy up
# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, eia_emit, temporary = F, overwrite = T)
