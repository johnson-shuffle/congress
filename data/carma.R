# ------------------------------------------------------------------------------
# create list of districts
# ------------------------------------------------------------------------------
db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

# inputs
in1 <- tbl(db, 'voteview_memb') %>% collect()
in2 <- tbl(db, 'states') %>% collect()

# carma data is for 2004 & 2009 (check for differences in districts)
dis_108 <- in1 %>%
  filter(congress == 108 & chamber == 'House') %>%
  select(congress, state_abbrev, district_code) %>%
  distinct() %>%
  select(-congress)
dis_111 <- in1 %>%
  filter(congress == 111 & chamber == 'House') %>%
  select(congress, state_abbrev, district_code) %>%
  distinct() %>%
  select(-congress)
all.equal(dis_108, dis_111)

# add fips, pad fips and district_code
dis  <- dis_108 %>%
  left_join(states, by = c('state_abbrev' = 'statecode')) %>%
  mutate(
    fips = str_pad(fips, 2, 'left', '0'),
    district_code = str_pad(district_code, 2, 'left', '0')
    )

# single district states
singles <- c("AK", "DE", "MT", "ND", "SD", "VT", "WY")
dis <- dis %>% 
  mutate(
    district_code = replace(district_code, state_abbrev %in% singles, '00')
  )

#-------------------------------------------------------------------------------
# district emissions from carma.org
#-------------------------------------------------------------------------------
carma_fun <- function(row, data = NULL, pb = NULL) {
  
  stopifnot(!is.null(data))
  
  if (!is.null(pb)) pb$tick()$print()
  
  # extract carma table for given fips-district pair
  pag <- 'http://carma.org/region/detail/9999'
  src <- read_html(str_c(pag, data$fips[row], data$district_code[row]))
  dat <- html_table(src)
  dat <- dat[[2]]
  names(dat)[1] <- 'year'

  # add district and drop ':' from year
  dat %>%
    mutate(
      fips = data$fips[row],
      district_code = data$district_code[row],
      year = gsub(':', '', year)
    )
  
}
pb <- progress_estimated(nrow(dis))
carma_lst <- map(1:nrow(dis), carma_fun, data = dis, pb = pb)

# ------------------------------------------------------------------------------
# manual edits
# ------------------------------------------------------------------------------

# bind togther
carma <- do.call(rbind, carma_lst)

# change classes
carma <- carma %>%
  filter(year != 'Future') %>%
  mutate_at(c(2:4), function(x) gsub(',', '', x)) %>%
  mutate_at(c(1, 9, 10), as.integer) %>%
  mutate_if(is.character, as.numeric)

# reshape
carma <- gather(carma, variable, value, 2:8)

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

copy_to(db, carma, temporary = F, overwrite = T)
