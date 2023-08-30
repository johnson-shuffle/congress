
# ----- create list of districts ------------------------------------------

# carma data is for 2004 & 2009
dis_108 <- dbGetQuery(db, "
  SELECT state_code, district_code
  FROM (
    SELECT DISTINCT congress, state_code, district_code
    FROM voteview_memb
    WHERE congress = 108 AND chamber = 'House'
  ) AS temp
  ")

dis_111 <- dbGetQuery(db, "
  SELECT state_code, district_code
  FROM (
    SELECT DISTINCT congress, state_code, district_code
    FROM voteview_memb
    WHERE congress = 108 AND chamber = 'House'
  ) AS temp
  ")

# check for differences in districts
all.equal(dis_108, dis_111)
rm(dis_108, dis_111)

# get districts with fips
dis <- dbGetQuery(db, "
  SELECT fips, temp.state_code, district_code, at_large
  FROM (
    SELECT DISTINCT congress, state_code, district_code
    FROM voteview_memb
    WHERE congress = 108 AND chamber = 'House'
  ) AS temp
  LEFT JOIN states ON temp.state_code = states.state_code
  ")

# pad fips / district_code, correct for single district states
dis %<>%
  mutate(
    fips = str_pad(fips, 2, 'left', '0'),
    district_code = str_pad(district_code, 2, 'left', '0'),
    district_code = if_else(at_large, '00', district_code)
  ) %>%
  select(-at_large)


# ----- district emissions ------------------------------------------------

# helper function
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


# ----- tidy up -----------------------------------------------------------

# bind togther
carma <- do.call(rbind, carma_list)

# change classes
carma %<>%
  filter(year != 'Future') %>%
  mutate_at(
    c(2:4),
    str_remove_all
  ) %>%
  mutate_at(
    c(1, 9, 10),
    as.integer
  ) %>%
  mutate_if(is.character, as.integer)

# reshape
carma <- gather(carma, variable, value, 2:8)

# units
carma %<>%
  mutate(
    variable = 'carbon dioxide',
    units = 'tons'
  )

# add by district
carma %<>%
  group_by(year, fips, district_code, variable, units) %>%
  summarise(
    value = sum(value)
  )

# ----- add to database --------------------------------------------------

dbWriteTable(db, 'carma', carma, append = T, row.names = F)
