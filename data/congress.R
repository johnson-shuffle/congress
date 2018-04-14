# ----- Preample ---------------------------------------------------------


# ----- House ------------------------------------------------------------

# page indices
in1 <- c(1, seq(10, 110, by = 10))
in2 <- in1 + 9 ; in2[1] <- 9 ; in2[12] <- 'Current'

# loop
hou_list <- list()
pb <- progress_estimated(length(in1))
for (i in seq_along(in1)) {
  
  pb$tick()$print()
  
  pag <- 'http://history.house.gov/Institution/Session-Dates/'
  htm <- read_html(str_c(pag, in1[i], '-', in2[i], '/'))
  hou_list[[i]] <- html_table(htm)[[1]]
  
}

# normalize names and bind together
lab <- c(
  'congress',
  'session',
  'session_start',
  'session_end',
  'calendar_days',
  'legislative_days',
  'recesses'
)
hou_list <- map(hou_list, ~setNames(.x, lab))
hou <- do.call(rbind, hou_list)

# label each congress with number only
hou %<>%
  mutate(
    congress = str_replace(congress, '\\([^()]+\\)', ''),
    congress = str_extract(congress, '\\d+')
  )

# fill in missing congress & session numbers
for (n in 2:nrow(hou)) {
  if (is.na(hou$congress[n])) {hou$congress[n] <- hou$congress[n - 1]}
  if (is.na(hou$session[n])) {hou$session[n] <- hou$session[n - 1] + 1}
}

# parse dates, create start and end dates for entire congress
mon <- month.abb ; names(mon) <- month.name
hou %<>%
  mutate(
    chamber = 'House',
    session_start = str_replace(session_start, '\\.', ''),
    session_start = str_replace_all(session_start, mon),
    session_start = parse_date(session_start, '%b %d, %Y'),
    session_end = str_replace(session_end, '\\.', ''),
    session_end = str_replace_all(session_end, mon),
    session_end = parse_date(session_end, '%b %d, %Y')
  ) %>%
  group_by(congress) %>%
  mutate(
    start_date = min(session_start),
    end_date = max(session_end)
  ) %>%
  ungroup()


# ----- Senate -----------------------------------------------------------

# fetch table
pag <- 'https://www.senate.gov/reference/Sessions/sessionDates.htm'
htm <- read_html(pag)
sen <- html_table(htm, fill = T)[[1]]
sen <- sen[, 1:4] %>%
  setNames(c('congress', 'session', 'session_start', 'session_end')) %>%
  filter(session != '')

# fill in missing congress number
for (n in 2:nrow(sen)) {
  if (is.na(sen$congress[n])) {sen$congress[n] <- sen$congress[n - 1]}
}

# account for footnotes
dat <- str_split(sen$session_end, ',', simplify = T)
dat[, 2] <- str_sub(dat[, 2], 2, 5)

# parse dates, create start and end dates for entire congress
sen %<>%
  mutate(
    chamber = 'Senate',
    session_start = parse_date(session_start, '%b %d, %Y'),
    session_end = str_c(dat[, 1], dat[, 2], sep = ', '),
    session_end = parse_date(session_end, '%b %d, %Y')
  ) %>%
  group_by(congress) %>%
  mutate(
    start_date = min(session_start),
    end_date = max(session_end)
  ) %>%
  ungroup()


# ----- Tidy Up ----------------------------------------------------------

# bind together
congress <- plyr::rbind.fill(hou, sen)

# change classes
congress %<>% mutate_at(c(1, 5:6), as.numeric)
congress %<>% mutate_if(is.Date, as.character)

# sort
congress %<>% arrange(chamber, congress, session)


# ----- Add to Database --------------------------------------------------

copy_to(db, congress, temporary = F, overwrite = T)
