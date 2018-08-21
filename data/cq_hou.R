# ----- Preample ----------------------------------------------------------

# helper function for reading cq csv files
cq_fun <- function(file, office = NULL) {
  dat <- read_csv(file, skip = 2) %>% 
    filter(Office == office)
  if (office == 'Senate') {
    dat$raceYear <- as.numeric(str_extract(file, '\\d{4}'))
  }
  return(dat)
}


# ----- Import ------------------------------------------------------------   

# raw data
fls <- list.files(path = './raw', pattern = 'hou', full.names = T)
dat <- map_dfr(fls, cq_fun, office = 'House')
dat %<>% rename(ThirdVotesMajorPercent = ThirdVotesTotalPercent)

# blank lines, runoffs, and special elections
dat_dupes <- duplicates(dat, c('State', 'Area', 'raceYear'))
dat %<>%
  mutate(Type = 'General') %>%
  unite(id, State, AreaNumber, raceYear) %>%
  filter(id != 'Louisiana_1_2014' | RepVotes != 'N/A') %>%  # blank line
  filter(id != 'Louisiana_2_2014' | RepVotes != 'N/A') %>%  # blank line
  filter(id != 'Louisiana_3_2014' | RepVotes != 'N/A') %>%  # blank line
  filter(id != 'Louisiana_4_2014' | RepVotes != 'N/A') %>%  # blank line
  within({
    Type[id == 'Louisiana_2_2006' & DemVotes == '35,153'] <- 'Runoff'
    Type[id == 'Louisiana_3_2012' & RepVotes == '58,820'] <- 'Runoff'
    Type[id == 'Louisiana_3_2016' & RepVotes == '77,671'] <- 'Runoff'
    Type[id == 'Louisiana_4_2016' & RepVotes == '87,370'] <- 'Runoff'
    Type[id == 'Louisiana_5_2014' & RepVotes == '134,616'] <- 'Runoff'
    Type[id == 'Louisiana_6_2014' & RepVotes == '139,209'] <- 'Runoff'
    Type[id == 'Texas_23_2006' & RepVotes == '32,217'] <- 'Runoff'
  }) %>%
  separate(id, into = c('State', 'AreaNumber', 'raceYear'), sep = '_')
rm(dat_dupes)

# missing elections?
# 1996: LA election: 1-3 (unopposed), 4, 6
# 2006: is ok
# 2014: LA election: 2
# 2016: MT election: At-Large
table(dat$Type, dat$raceYear)


# ----- Extra Candidates --------------------------------------------------

ext <- dat[grepl('The other vote was:', dat$RaceNotes), ]
ext$RaceNotes <- str_extract(ext$RaceNotes, '(The other vote was:)[:print:]+')
ext$RaceNotes %<>% str_replace('The other vote was:', '')
ext <- cbind(
  select(ext, State, Area, raceYear, Type),
  as_tibble(str_split(ext$RaceNotes, ';', simplify = T))
  )
ext %<>% gather(key, value, -1:-4)

# extract patterns: party, name, votes
p1 <- '([A-z]+\\-?[A-z]+, [A-z]+(\\s[A-Z]\\.)?)|([A-z]+\\-?[A-z]+)'
p2 <- '(?<=\\().*?(?=\\))'
p3 <- '(\\d+,?\\d+)|\\d{1}'
ext %<>%
  mutate(
    name  = str_extract(value, p1),
    party = str_extract(value, p2),
    status = 'Challenger',
    votes = str_extract(value, p3)
  ) %>%
  select(-key, -value)


# ----- Reshape to Long ---------------------------------------------------

dat_long <- tibble(
  statename = NA,
  district = NA,
  cycle = NA,
  type = NA,
  name = NA,
  party = NA,
  status = NA,
  votes = NA
)

# identifying columns
cols1 <- c('State', 'Area', 'raceYear', 'Type')
cols2 <- c('Candidate', 'Status', 'Votes')

# rows
rows1 <- 1:nrow(dat)
rows2 <- (nrow(dat) + 1):(2 * nrow(dat))
rows3 <- (2 * nrow(dat) + 1):(3 * nrow(dat))

# republicans
dat_long[rows1, c(1:5, 7:8)] <- dat[rows1, c(cols1, str_c('Rep', cols2))]
dat_long$party[rows1] <- 'Rep'

# democrats
dat_long[rows2, c(1:5, 7:8)] <- dat[rows1, c(cols1, str_c('Dem', cols2))]
dat_long$party[rows2] <- 'Dem'

# third party
dat_long[rows3, c(1:5, 7:8)] <- dat[rows1, c(cols1, str_c('Third', cols2))]
dat_long$party[rows3] <- 'Third'

# recode statenames to statecodes using state data
states <- tbl(db, 'states') %>% collect()
codes  <- states$statecode ; names(codes) <- states$statename
dat_long$statename %<>% str_replace_all(codes)
dat_long$statename %<>% str_replace_all('West VA', 'WV')
dat_long %<>% rename(statecode = statename)

# recode at large districts to one
dat_long$district %<>% str_replace('District ', '')
dat_long$district[dat_long$district == 'At Large'] <- '1'
dat_long$district %<>% as.numeric()


# ----- Fix Miscodes ------------------------------------------------------

# convert votes to numeric/store unopposed as infinity
dat_long$votes %<>% str_replace(',', '')
dat_long$votes %<>% str_replace('Unopposed', 'Inf')
dat_long$votes %<>% as.numeric()

# find N/A row entries
dat_nas <- dat_long %>%
  group_by(statecode, district, cycle, type) %>%
  mutate(
    votes = as.numeric(votes),
    votes = replace(votes, is.na(votes), 0)
  ) %>%
  summarise(
    votes = sum(votes)
  ) %>%
  filter(votes == 0) %>%
  unite(id, statecode, district, cycle) %>%
  use_series('id')

# fix N/A row entries (plus one completely miscoded name)
dat_long %<>%
  unite(id, statecode, district, cycle) %>%
  mutate(
    votes = replace(votes, id %in% dat_nas, Inf)
  ) %>%
  within({
    name[id == 'FL_19_2004'] <- 'Wexler, Robert'
    status[id == 'FL_19_2004'] <- 'Incumbent'
    name[id == 'FL_15_2012'] <- 'Ross, Dennis A.'
    status[id == 'FL_15_2012'] <- 'Incumbent'
    name[id == 'FL_24_2012'] <- 'Wilson, Frederica S.'
    status[id == 'FL_24_2012'] <- 'Incumbent'
    name[id == 'FL_24_2016'] <- 'Wilson, Frederica S.'
    status[id == 'FL_24_2016'] <- 'Incumbent'
    name[id == 'OK_1_2016'] <- ' Bridenstine, Jim'
    status[id == 'OK_1_2016'] <- 'Incumbent'
  }) %>%
  separate(id, into = c('statecode', 'district', 'cycle'), sep = '_')

# drop problematic names
dat_long <- dat_long[!str_detect(dat_long$name, 'All Others'), ]
dat_long <- dat_long[!str_detect(dat_long$name, 'N/A'), ]
dat_long <- dat_long[!str_detect(dat_long$name, 'NULL'), ]
dat_long <- dat_long[!str_detect(dat_long$name, 'Scatter(ing)|(ed)'), ]
dat_long <- dat_long[!str_detect(dat_long$name, 'Write-In'), ]
dat_long <- dat_long[!str_detect(dat_long$name, 'Miscellaneous'), ]

# convert using latin1
dat_long$name %<>% str_conv('latin1')

# two incumbents
dat_incb <- dat_long %>%
  group_by(statecode, district, cycle, type) %>%
  count(status) %>%
  filter(status == 'Incumbent' & n == 2)

# two incumbents: all but one are result of redistricting
dat_long %<>%
  unite(id, statecode, district, cycle, party) %>%
  within({
    status[id == 'CA_12_2004_Rep'] <- 'Challenger'
  }) %>%
  separate(id, into = c('statecode', 'district', 'cycle', 'party'), sep = '_')
rm(dat_incb)  


# ----- Legislator osid's -------------------------------------------------

# opensecrets data
cands <- tbl(db_crp, 'cands') %>% collect()
cands %<>%
  mutate(
    name = str_trim(firstlast),
    name = str_replace(name, '\\([^()]*\\)$', ''),
    statecode = str_sub(distid_run, 1, 2),
    district = str_sub(distid_run, 3, 4) %>% as.numeric()
  ) %>%
  filter(!is.na(district)) %>%
  mutate_if(is.character, str_trim) %>%
  select(cycle, name, osid, statecode, district) %>%
  distinct()

# helper function
osid_fun <- function(nm, st, dt, cy, cut = 0.2) {
  
  # initial filter
  tmp <- filter(cands, statecode == st & district == dt & cycle == cy)
  
  # rearrange name
  nms <- str_split(nm, ',', n = 2, simplify = T)
  nms %<>% str_trim()
  fnm <- nms[2]
  lnm <- nms[1]
  rnm <- str_c(fnm, lnm, sep = ' ')
  
  # first criterion: last name (remove special characters and initials)
  lnmc <- str_replace_all(lnm, ',|-|\\.', ' ')
  lnmc %<>% str_replace_all('^[A-Z]\\s+', ' ')
  lnmc %<>% str_replace_all('\\s+[A-Z]\\s+', ' ')
  lnmc %<>% str_trim()
  lnmc %<>% str_replace_all('\\s+', '|')
  cond_lnm <- T %in% grepl(lnmc, tmp$name, ignore.case = T)
  if (cond_lnm) {
    tmp <- tmp[grep(lnmc, tmp$name, ignore.case = T), ]
  }
  
  # second criterion: rearranged name (remove special characters and initials)
  rnmc <- str_replace_all(rnm, ',|-|\\.', ' ')
  rnmc %<>% str_replace_all('^[A-Z]\\s+', ' ')
  rnmc %<>% str_replace_all('\\s+[A-Z]\\s+', ' ')
  rnmc %<>% str_trim()
  rnmc %<>% str_replace_all('\\s+', '|')
  cond_rnm <- T %in% grepl(rnmc, tmp$name, ignore.case = T)
  if (cond_rnm & !cond_lnm) {
    tmp <- tmp[grep(rnmc, tmp$name, ignore.case = T), ]
  }
  
  # third criterion: distance
  d1 <- stringdist::stringdist(rnm, tmp$name, method = 'jw')
  d2 <- NA
  if (cond) {
    d2 <- length(tmp$name)
  } else {
    d1 <- ifelse(min(d1) > cut, NA, min(d1))
    d2 <- 0
  }
  
  # return value
  return(list(
    osid = tmp$osid[which.min(d1)],
    match = tmp$name[which.min(d1)],
    last_name = cond_lnm,
    distance = min(d1),
    matches = d2
  ))

}
osid_fun <- safely(osid_fun)

# new variables
dat_long <- dat_long %>%
  mutate(
    osid = NA,
    match_name = NA,
    last_name = F,
    distance = NA,
    matches = NA
  )

# loop the helper function
pb <- progress_estimated(nrow(dat_long))
for (i in 1:nrow(dat_long)) {
  tmp <- osid_fun(
    dat_long$name[i],
    dat_long$statecode[i],
    dat_long$district[i],
    dat_long$cycle[i]
  )
  dat_long <- dat_long %>%
    within({
      osid[i] <- ifelse(is.null(tmp$result), NA, tmp$result$osid)
      match_name[i] <- ifelse(is.null(tmp$result), NA, tmp$result$match)
      last_name[i] <- ifelse(is.null(tmp$result), NA, tmp$result$last_name)
      distance[i] <- ifelse(is.null(tmp$result), NA, tmp$result$distance)
      matches[i] <- ifelse(is.null(tmp$result), NA, tmp$result$matches)
    })
  pb$tick()$print() ; rm(tmp)
}


# ----- Manual Fixes ------------------------------------------------------

# fix one: no last name match but high vote getters with reasonable distance
mf1 <- filter(dat_long, !last_name & (distance < 0.25 | votes > 1E4)) %>%
  distinct(statecode, district, name, match_name)

mf1_probs <- dat_long %>%
  filter(name %in% mf1$name[c(2, 10, 15, 21, 29, 30, 34, 43, 49, 52, 53, 57)]) %>%
  group_by(name, statecode, district) %>%
  summarise_at(vars(cycle, votes), str_c, collapse = ',') %>%
  distinct()

mf1_nas <- c(
  'Bruner, Natalie M.',
  'Depalma, Jennifer A.',
  'Hayward, William R.',
  'Henderson, Elaine M.',
  'Markgraf, Rosemarie',
  'McCourt, John C.',
  'Miller, Jeffrey A.',
  'Sawyer, John W',
  'Stockwell, Christopher John',
  'Thompson, Drew'
)

dat_long %<>%
  mutate(
    name = ifelse(name == 'Belardino, Mario D.', 'Boustany, Charles W. Jr.', name),
    osid = ifelse(name == 'Boustany, Charles W. Jr.', 'N00026595', osid),
    osid = ifelse(name %in% mf1_nas, NA, osid)
    
  )

# fix two: no last name match and everyone else
mf2 <- filter(dat_long, !last_name & !(distance < 0.25 | votes > 1E4)) %>%
  distinct(statecode, district, name, match_name)

mf2_nas <- mf2$name

dat_long %<>%
  mutate(
    osid = ifelse(name %in% mf2_nas, NA, osid)
  )

# fix three: last name match but large distance
mf3 <- filter(dat_long, last_name & distance > 0.25) %>%
  distinct(statecode, district, name, match_name, distance) %>%
  arrange(desc(distance))

# fix four
mf4 <- dat_long %>%
  distinct(name, osid) %>%
  count(name) %>%
  filter(n > 1) %>%
  use_series('name')

mf4_probs <- dat_long %>%
  filter(name %in% mf4) %>%
  arrange(name, osid, distance)

#   within({
#     name[statecode == 'LA' & cycle == 2006 & name == 'Belardino, Mario D.'] <-
#       'Boustany, Charles W. Jr.'
#     osid[statecode == 'LA' &cycle == 2006 & name == 'Belardino, Mario D.'] <-
#       'N00026595'
#     osid[statecode == 'PA' &cycle == 2008 & name == "O'Donnell, Steve"] <-
#       'N00029337'
#   })

# fix two: matches with large distances
mf2 <- filter(dat_long, distance > 0.2) %>%
  group_by(statecode, district, name, match_name, osid, distance) %>%
  summarise_at(vars(votes, matches), mean) %>%
  distinct() %>%
  arrange(desc(distance))

dat_long %<>%
  within({
    osid[statecode == 'SC' & district ==  4 & name == 'Sumerel, Jeff'] <-
      NA
    osid[statecode == 'VA' & district ==  8 & name == 'Moran, James P. Jr.'] <-
      'N00002083'
    osid[statecode == 'MD' & district ==  2 & name == 'Ehrlich, Robert L. Jr.'] <-
      'N00001925'
    osid[statecode == 'MA' & district ==  6 & name == 'Stockwell, Christopher John'] <-
      'N00001925'
    osid[statecode == 'FL' & district == 23 & name == 'Woods, Al'] <-
      'N00002864'
    osid[statecode == 'VA' & district == 11 & name == 'Davis, Thomas M. III'] <-
      'N00002045'
    osid[statecode == 'SC' & district ==  1 & name == 'Sanford, Marshall Clement Jr.'] <-
      'N00002424'
    osid[statecode == 'GA' & district ==  1 & name == 'Carter, Earl Leroy'] <-
      'N00035861'
    osid[statecode == 'OK' & district ==  2 & name == 'Boren, David Daniel'] <-
      'N00026481'
    osid[statecode == 'NJ' & district == 13 & name == 'Hester, Richard S. Sr.'] <-
      'N00000710'
    osid[statecode == 'GA' & district ==  3 & name == 'Collins, Michael Allen'] <-
      'N00002556'
    osid[statecode == 'FL' & district ==  5 & name == 'Russell, John'] <-
      'N00026818'
    osid[statecode == 'IL' & district == 18 & name == 'Vance, Don'] <-
      NA
    osid[statecode == 'AK' & district ==  1 & name == 'Duncan, Jim'] <-
      'N00008022'
    osid[statecode == 'TN' & district ==  5 & name == 'Kovach, Thomas F.'] <-
      'N00009517'
    osid[statecode == 'IN' & district ==  7 & name == 'Pease, Edward A.'] <-
      'N00003910'
    osid[statecode == 'GA' & district == 13 & name == 'Crane, Mike'] <-
      'N00031893'
    osid[statecode == 'IL' & district == 11 & name == 'Weller, Gerald C.'] <-
      'N00004745'
    osid[statecode == 'AZ' & district ==  2 & name == 'Barron, Ed'] <-
      'N00006416'
    osid[statecode == 'WA' & district ==  4 & name == 'Hastings, Richard'] <-
      'N00009157'
    osid[statecode == 'OH' & district == 12 & name == 'Hart, Robert M.'] <-
      'N00035631'
    osid[statecode == 'NH' & district ==  1 & name == 'Preston, Robert T.'] <-
      'N00000094'
    osid[statecode == 'VA' & district ==  3 & name == 'Scott, Robert C.'] <-
      'N00002147'
    osid[statecode == 'FL' & district == 12 & name == 'Hagenmaier, Robert D.'] <-
      'N0002700'
    osid[statecode == 'FL' & district == 22 & name == 'McLain, John'] <-
      'N00025756'
    osid[statecode == 'PA' & district == 10 & name == 'Hackett, Chris'] <-
      'N00029334'
    osid[statecode == 'NY' & district == 27 & name == 'Reynolds, Thomas M.'] <-
      'N00001295'
    osid[statecode == 'CT' & district ==  5 & name == 'Maloney, James H.'] <-
      'N00000637'
    osid[statecode == 'CA' & district == 50 & name == 'Divine, Bob'] <-
      'N00006980'
  })