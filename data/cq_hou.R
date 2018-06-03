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


# ----- House -------------------------------------------------------------

fls <- list.files(path = './raw', pattern = 'hou', full.names = T)
hou <- map_dfr(fls, cq_fun, office = 'House')
hou %<>% rename(ThirdVotesMajorPercent = ThirdVotesTotalPercent)

# blank lines, runoffs, and special elections
hou_dupes <- duplicates(hou, c('State', 'Area', 'raceYear'))
hou %<>%
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

# missing elections?
# 1996: LA election: 1-3 (unopposed), 4, 6
# 2006: is ok
# 2014: LA election: 2
# 2016: MT election: At-Large
table(hou$Type, hou$raceYear)


# ----- Extra Candidates --------------------------------------------------

ext <- hou[grepl('The other vote was:', hou$RaceNotes), ]
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

hou_long <- tibble(
  State = NA,
  Area = NA,
  raceYear = NA,
  type = NA,
  name = NA,
  party = NA,
  status = NA,
  votes = NA
)

# republicans (and election type)
hou_long[1:5656, c(1:5, 7:8)] <- hou[1:5656, c(2, 6, 4, 25, 9:10, 8)]
hou_long$party[1:5656] <- 'Rep'

# democrats
hou_long[5657:11312, c(1:5, 7:8)] <- hou[1:5656, c(2, 6, 4, 25, 12:13, 11)]
hou_long$party[5657:11312] <- 'Dem'

# third party
hou_long[11313:16968, c(1:5, 7:8)] <- hou[1:5656, c(2, 6, 4, 25, 16:17, 15)]
hou_long$party[11313:16968] <- 'Third'


# ----- Fix Miscodes ------------------------------------------------------

# convert votes to numeric / store unopposed as infinity
hou_long$votes %<>% str_replace(',', '')
hou_long$votes %<>% str_replace('Unopposed', 'Inf')
hou_long$votes %<>% as.numeric()

# find N/A row entries
hou_nas <- hou_long %>%
  group_by(State, Area, raceYear, type) %>%
  mutate(
    votes = as.numeric(votes),
    votes = replace(votes, is.na(votes), 0)
  ) %>%
  summarise(
    votes = sum(votes)
  ) %>%
  filter(votes == 0) %>%
  unite(id, State, Area, raceYear) %>%
  use_series('id')

# fix N/A row entries
hou_long %<>%
  unite(id, State, Area, raceYear) %>%
  mutate(
    votes = replace(votes, id %in% hou_nas, Inf)
  ) %>%
  within({
    name[id == 'Florida_District 19_2004'] <- 'Wexler, Robert'
    status[id == 'Florida_District 19_2004'] <- 'Incumbent'
    name[id == 'Florida_District 15_2012'] <- 'Ross, Dennis A.'
    status[id == 'Florida_District 15_2012'] <- 'Incumbent'
    name[id == 'Florida_District 24_2012'] <- 'Wilson, Frederica S.'
    status[id == 'Florida_District 24_2012'] <- 'Incumbent'
    name[id == 'Florida_District 24_2016'] <- 'Wilson, Frederica S.'
    status[id == 'Florida_District 24_2016'] <- 'Incumbent'
    name[id == 'Oklahoma_District 1_2016'] <- ' Bridenstine, Jim'
    status[id == 'Oklahoma_District 1_2016'] <- 'Incumbent'
  }) %>%
  separate(id, into = c('State', 'Area', 'raceYear'), sep = '_')

# drop remaining N/A's
hou_long %<>% filter(!name %in%  c('N/A', 'Write-In'))

# two incumbents
hou_incb <- hou_long %>%
  group_by(State, Area, raceYear, type) %>%
  count(status) %>%
  filter(status == 'Incumbent' & n == 2)

# two incumbents: all but one are result of redistricting
hou_long <- hou_long %>%
  unite(id, State, Area, raceYear, party) %>%
  within({
    status[id == 'California_District 12_2004_Rep'] <- 'Challenger'
  }) %>%
  separate(id, into = c('State', 'Area', 'raceYear', 'party'), sep = '_')
  

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
  mutate_if(is.character, str_trim) %>%
  select(cycle, name, osid, statecode, district) %>%
  distinct()

# recode statenames to statecodes using state data
states <- tbl(db, 'states') %>% collect()
codes  <- states$statecode ; names(codes) <- states$statename
hou_long$State %<>% str_replace_all(codes)
hou_long$State %<>% str_replace_all('West VA', 'WV')

# recode at large districts to one
hou_long$Area %<>% str_replace('District ', '')
hou_long$Area[hou_long$Area == 'At Large'] <- '1'
hou_long$Area %<>% as.numeric()

# convert using latin1
hou_long$name %<>% str_conv('latin1')

# helper function
osid_fun <- function(nm, st, dt, cy, cut = 0.2) {
  
  # initial filter
  tmp <- filter(cands, statecode == st & district == dt & cycle == cy)
  
  # rearrange name
  nm %<>% str_split(',', n = 2, simplify = T)
  nm %<>% str_trim()
  nm <- str_c(nm[2], nm[1], sep = ' ')
  
  # first criterion: name
  nm2 <- nm
  nm2 %<>% str_replace_all(',|-|\\.', ' ')
  nm2 %<>% str_replace_all('\\s+', '|')
  cond <- T %in% grepl(nm2, tmp$name, ignore.case = T)
  if (cond) {
    tmp <- tmp[grep(nm2, tmp$name, ignore.case = T), ]
  }
  
  # second criterion: distance
  d1 <- stringdist::stringdist(nm, tmp$name, method = 'jw')
  d2 <- NA
  if (cond) {
    d2 <- length(tmp$name)
  } else {
    d1 <- ifelse(min(d1) > cut, NA, min(d1))
    d2 <- 0
  }
  
  # return value
  if (is_empty(d1) | is.na(d1[1])) {
    return(list(
      osid = NA,
      match = NA,
      distance = NA,
      matches = 0
    ))
  } else {
    return(list(
      osid = tmp$osid[which.min(d1)],
      match = tmp$name[which.min(d1)],
      distance = min(d1),
      matches = d2
    ))
  }

}
osid_fun <- safely(osid_fun)

# new variables
hou_long <- hou_long %>%
  mutate(
    osid = NA,
    match_name = NA,
    distance = NA
  )

# loop the helper function
for (i in 1:nrow(hou_long)) {
  tmp <- osid_fun(
    hou_long$name[i],
    hou_long$State[i],
    hou_long$Area[i],
    hou_long$raceYear[i]
  )
  hou_long <- hou_long %>%
    within({
      osid[i] <- ifelse(is.null(tmp$result), NA, tmp$result$osid)
      match_name[i] <- ifelse(is.null(tmp$result), NA, tmp$result$match)
      distance[i] <- ifelse(is.null(tmp$result), NA, tmp$result$distance)
    })
  print(i) ; rm(tmp)
}

y <- filter(hou_long, is.na(osid)) %>% 
  arrange(desc(votes))
z <- filter(hou_long, distance > 0.2 & matches == 0) %>%
  distinct(name, match_name, osid, distance) %>%
  arrange(desc(distance))
