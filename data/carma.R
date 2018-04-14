# ----- Preample ----------------------------------------------------------


# ----- Create List of Districts ------------------------------------------

# carma data is for 2004 & 2009 (check for differences in districts)
dis_108 <- tbl(db, sql(
  "SELECT state_abbrev, district_code FROM (
   SELECT DISTINCT congress, state_abbrev, district_code FROM voteview_memb
   WHERE congress = 108 AND chamber = 'House')"
  ))
dis_111 <- tbl(db, sql(
  "SELECT state_abbrev, district_code FROM (
   SELECT DISTINCT congress, state_abbrev, district_code FROM voteview_memb
   WHERE congress = 111 AND chamber = 'House')"
))
all.equal(collect(dis_108), collect(dis_111))

# add fips
dis <- tbl(db, sql(
  "SELECT state_abbrev, district_code, fips FROM (
   SELECT DISTINCT congress, state_abbrev, district_code FROM voteview_memb
   WHERE congress = 108 AND chamber = 'House') AS TBL_L
   LEFT JOIN states ON TBL_L.state_abbrev = states.statecode"
))

# pad fips & district_code and correct for single district states
singles <- c("AK", "DE", "MT", "ND", "SD", "VT", "WY")
dis <- collect(dis) %>%
  mutate(
    fips = str_pad(fips, 2, 'left', '0'),
    district_code = str_pad(district_code, 2, 'left', '0'),
    district_code = replace(district_code, state_abbrev %in% singles, '00')
  )


# ----- District Emissions ------------------------------------------------

carma_fun <- function(row, data = NULL, pb = NULL) {
  
  stopifnot(!is.null(data))
  
  if (!is.null(pb)) pb$tick()$print()
  
  # extract carma table for given fips-district pair
  pag <- 'http://carma.org/region/detail/9999'
  htm <- read_html(str_c(pag, data$fips[row], data$district_code[row]))
  dat <- html_table(htm)
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
carma_list <- map(1:nrow(dis), carma_fun, data = dis, pb = pb)


# ----- Tidy Up -----------------------------------------------------------

# bind togther
carma <- do.call(rbind, carma_list)

# change classes
carma <- carma %>%
  filter(year != 'Future') %>%
  mutate_at(c(2:4), function(x) gsub(',', '', x)) %>%
  mutate_at(c(1, 9, 10), as.numeric) %>%
  mutate_if(is.character, as.numeric)

# reshape
carma <- gather(carma, variable, value, 2:8)

# units
carma$variable <- 'carbon dioxide'
carma$units <- 'tons'


# ----- Add to Database --------------------------------------------------

copy_to(db, carma, temporary = F, overwrite = T)
