# ------------------------------------------------------------------------------
# preample
# ------------------------------------------------------------------------------
load_tidy()

db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

#-------------------------------------------------------------------------------
# unemployment
#-------------------------------------------------------------------------------
dat <- read_delim(
  'http://download.bls.gov/pub/time.series/la/la.data.1.CurrentS',
  delim = '\t',
  col_types = cols(value = col_number())
  )

# create fips, LASST = seasonal/state, measure code 03 = unemployment rate
bls_unempl <- dat %>%
  filter(str_sub(series_id, 1, 5) == "LASST") %>%
  filter(str_sub(series_id, 20, 20) == "3") %>%
  mutate(fips = as.numeric(str_sub(series_id, 6, 7)))

# average unemployment
bls_unempl <- bls_unempl %>%
  group_by(fips, year) %>%
  summarise(avg_unemp = mean(value)) %>%
  mutate(year = as.numeric(year)) %>%
  ungroup()

#-------------------------------------------------------------------------------
# inflation data
#-------------------------------------------------------------------------------
dat <- read_delim(
  'http://download.bls.gov/pub/time.series/cu/cu.data.2.Summaries',
  delim = '\t',
  col_types = cols(value = col_number())
  )

# average inflation
bls_cpi <- dat %>%
  select(year, value, period) %>%
  group_by(year) %>%
  summarise(avg_cpi = mean(value)) %>%
  mutate(year = as.numeric(year)) %>%
  ungroup()
  
# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
copy_to(db, bls_unempl, temporary = F, overwrite = T)
copy_to(db, bls_cpi, temporary = F, overwrite = T)
