# ----- Preample ----------------------------------------------------------


# ----- Casualties 93 to 113 ----------------------------------------------

# helper function
casualty_fun <- function(congress) {
  
  suffix <- 'th'
  if (str_sub(congress, -1, -1) == 1 & congress < 110) suffix <- 'st'
  if (str_sub(congress, -1, -1) == 2 & congress < 110) suffix <- 'nd'
  if (str_sub(congress, -1, -1) == 3 & congress < 110) suffix <- 'rd'
  
  pag <- str_c(
    'http://web.archive.org/web/2012/',
    'http://www.rollcall.com/politics/casualtylists/',
    congress, suffix,
    'casualtylist.html'
    )
  
  # legislators
  nds <- read_html(pag) %>% html_nodes('li')
  legis <- map_chr(nds, xml_text)
  
  # find correct nodes
  if (congress == 94) {
    x <- c(105:191)
  } else if (congress == 95) {
    x <- c(105:209)
  } else {
    x <- grep('term', legis)
  }
  legis <- legis[x]
  
  # misc fixes
  legis <- legis[legis != 'n/a']
  if (congress == 108) {
    legis[53] <- legis[52]
    legis[52] <- 'Larry Combest (R-Texas), 59, 10 terms'
  }
  if (!congress %in% 94:95) {
    legis %<>% str_split(',', simplify = T)
  }
  
  # create tibble
  legis %<>% as_tibble() ; names(legis)[1] <- 'V1'
  if (congress <= 111) {
    legis %<>%
      mutate(
        name  = str_replace(V1, '\\([^()]*\\)', ''),
        value = str_extract(V1, '(?<=\\()[^()]*(?=\\))'),
        party = str_split(value, '-', simplify = T)[, 1],
        state = str_split(value, '-', simplify = T)[, 2]
      ) %>%
      select(name, party, state)
  } else {
    legis %<>%
      rename(name = V1, value = V2) %>%
      mutate(
        party = str_split(value, '-', simplify = T)[, 1],
        state = str_split(value, '-', simplify = T)[, 2]
      ) %>%
      select(name, party, state)
  }
  legis %<>% mutate_if(is.character, str_trim)
  
  # add congress
  legis %<>% mutate(congress = congress)
  
  # reasons for leaving office and distribution
  pat <- 'Appointed|Elected|Defeated|Expel|Vacat|Running|Retir|Resign|Died'
  nds <- read_html(pag) %>% html_nodes('h3')
  dis <- map_chr(nds, xml_text)
  dis <- dis[grep(pat, dis)]
  reasons <- str_replace(dis, '\\([^()]*\\)', '')
  reasons %<>% str_replace('-|—|\\u0097', '')
  reasons %<>% str_trim()
  counts <- str_extract(dis, '(?<=\\()[^()]*(?=\\))')
  counts %<>% str_split(';', simplify = T)
  counts %<>% as_tibble()
  counts %<>% mutate_all(str_trim)
  counts %<>% mutate_all(~str_extract(.x, '^\\d+'))
  counts %<>% mutate_all(as.numeric)
  counts %<>% mutate_all(~replace(.x, is.na(.x), 0))
  
  # misc adjustments
  if (congress == 94) {
    counts$V2[7] <- 4
  } else if (congress == 95) {
    
  } else if (congress == 101) {
    counts$V1[2] <- 1
  } else if (congress == 105) {
    counts$V1[7] <- 3
    counts$V2[8] <- 2
  } else if (congress == 106) {
    counts$V2[3] <- 6
    counts$V2[7] <- 0
  } else if (congress == 107) {
    counts$V1[8] <- 2
    counts$V2[5] <- 0
  } else if (congress == 109) {
    counts$V1[9] <- 4
    counts$V2[9] <- 0
  }
  counts %<>% by_row(sum, .collate = 'cols', .to = 'V3')
  
  # output
  chamber <- map2(counts$V1, counts$V2, ~c(rep('House', .x), rep('Senate', .y)))
  reason  <- map2(reasons, counts$V3, ~rep(.x, .y))
  legis$chamber <- unlist(chamber)
  legis$reason  <- unlist(reason)
  
  return(legis)

}
casualty_fun_s <- safely(casualty_fun)

# apply the helper function
dat_113 <- map(93:113, casualty_fun_s) ; map(dat_113, ~.x$error)
dat_113 %<>% map_dfr(~.x$result)


# ----- Casualties 114 ----------------------------------------------------

pag <- 'https://www.opensecrets.org/'
pag %<>% str_c('members-of-congress/outgoing-members-list?cycle=2016')

# reasons for leaving office and distribution
nds <- read_html(pag) %>% html_nodes('h2')
dis <- c(10, 6, 8, 2, 2, 3, 1, 29)
reason <- map2(map_chr(nds, xml_text)[1:8], dis, ~rep(.x, .y))

# legislators
nds1 <- read_html(pag) %>% html_nodes("a[href^='/members-of-congress/summary']")

# states & districts
nds2 <- read_html(pag) %>% html_nodes('i')

# data
dat_114 <- tibble(
  congress = 114,
  value1 = map_chr(nds1, xml_text),
  value2 = map_chr(nds1, xml_attrs),
  value3 = map_chr(nds2, xml_text),
  party = str_extract(value1, '(?<=\\()[^()]*(?=\\))'),
  name = str_replace(value1, '\\([^()]*\\)', ''),
  osid = str_extract(value2, 'N\\d{8}'),
  state = str_split(value3, ',', simplify = T)[, 1],
  reason = unlist(reason),
  chamber = str_split(value3, ',', simplify = T)[, 2],
  ) %>%
  mutate_if(is.character, str_trim) %>%
  select(name, osid, party, state, congress, chamber, reason)


# ----- Tidy Up -----------------------------------------------------------

# bind
dat <- plyr::rbind.fill(dat_113, dat_114)

# remove periods
dat$name %<>% str_replace_all('\\.', '')
dat$name %<>% str_replace_all('\\*', '')

# latin characters
dat$name %<>% str_replace_all(c('á' = 'a', 'ñ' = 'n'))

# blank states
filter(dat, state == '')
dat %<>%
  within({
    name[name == 'Russ Carnahan  D-Mo' & congress == 112] <- 'Russ Carnahan'
    party[name == 'Russ Carnahan' & congress == 112] <- 'D'
    state[name == 'Russ Carnahan' & congress == 112] <- 'Mo'
    party[name == 'Mark Pryor' & congress == 113] <- 'D'
    state[name == 'Mark Pryor' & congress == 113] <- 'Arkansas'
    party[name == 'Dave Camp' & congress == 113] <- 'R'
    state[name == 'Dave Camp' & congress == 113] <- 'Mich.'
  })

# standardize states
states <- tbl(db, 'states') %>% collect()
probs <- filter(dat, !state %in% states$statedscr)$state
probs %<>% unique()
probs %<>% sort()
probs <- c(probs, states$statedscr)
replacements <- c(
  'AS', 'AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL', 'GA', 'GU',
  'IN', 'KS', 'KY', 'LA', 'MA', 'MD', 'MI', 'MN', 'MS', 'MO', 'MO', 'MT',
  'NC', 'ND', 'NH', 'NH', 'NJ', 'NJ', 'NM', 'NY', 'NE', 'NV', 'OK', 'OR',
  'PR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'VI', 'VA', 'VT', 'WV', 'WA',
  'WA', 'WI', 'WY',
  states$statecode
)  
replacements <- tibble(state = probs, statecode = replacements)
dat %<>% left_join(replacements, by = 'state')
dat %<>% select(-state)


# ----- Legislator osid's -------------------------------------------------

# opensecrets data
cands <- tbl(os, 'cands') %>% collect()
cands %<>%
  mutate(
    name = str_trim(firstlast),
    name = str_replace(name, '\\([^()]*\\)$', ''),
    statecode = str_sub(distid_run, 1, 2)
  ) %>%
  mutate_if(is.character, str_trim) %>%
  select(cycle, name, osid, statecode) %>%
  distinct()

# restrict attention to missing osid's between 101 and 114
mis <- c(1:nrow(dat))[is.na(dat$osid) & dat$congress >= 101]

# helper function
osid_fun <- function(nm, st, cg) {
  
  # cycle / congress
  cycle <- seq(1990, 2016, by = 2)
  names(cycle) <- 101:114
  x <- cycle[[as.character(cg)]]
  
  # first criterion: lastname
  ln <- str_split(nm, ' ')[[1]]
  ln <- ln[!ln %in% c('Jr', "II", 'III', '')]
  ln <- ln[length(ln)]
  tmp <- filter(cands, statecode == st & cycle >= (x - 2) & cycle <= (x + 2))
  tmp <- tmp[str_detect(tmp$name, ln), ]
  
  # second criterion: distance
  d1 <- stringdist::stringdist(nm, tmp$name, method = 'jw')
  d2 <- unique(tmp$name)
  
  # return value
  if (is_empty(d1)) {
    return(list(match = NA, matches = 0))
  } else {
    return(list(match = tmp$osid[which.min(d1)], matches = length(d2)))
  }
  
}

# loop the helper function
mat <- rep(NA, length(mis))
for (i in mis) {
  tmp <- osid_fun(dat$name[i], dat$statecode[i], dat$congress[i])
  dat$osid[i] <- tmp$match
  mat[i] <- tmp$matches
  rm(tmp)
}
mat <- mat[!is.na(mat)]

# still missing?
filter(dat, is.na(osid) & congress >= 101 & congress <= 113)


# ----- Add to Database ---------------------------------------------------

casualty <- dat
copy_to(db, casualty, temporary = F, overwrite = T)
