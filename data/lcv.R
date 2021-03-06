# ----- Preample ----------------------------------------------------------


# ----- LCV Scorecard Votes -----------------------------------------------

# lcv scorecards
htm <- read_html('http://scorecard.lcv.org/scorecard')
votes_nodes <- html_nodes(htm, "a[href^='roll-call-vote']")
class_nodes <- html_nodes(htm, "span[class='voteIssuesFULL']")

# create data frame
lcv_score <- tibble(href = map_chr(votes_nodes, ~xml_attrs(.x)[['href']])) %>%
  mutate(
    rollnumber = map_chr(href, ~str_split(.x, '-')[[1]][4]) %>% as.numeric(),
    href = str_c('http://scorecard.lcv.org/', href),
    year = map_chr(href, ~str_split(.x, '-')[[1]][3]),
    year = map_chr(year, ~str_split(.x, '/')[[1]][2]) %>% as.numeric(),
    lcv_type = map_chr(class_nodes, ~xml_text(.x)[[1]]),
    lcv_type = str_sub(lcv_type, 1, nchar(lcv_type) - 1) %>% str_trim(),
    lcv_dscr = map_chr(votes_nodes, ~xml_text(.x)[[1]])
  )

# lcv positions
lcv_position <- function(href, pb = NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  htm <- read_html(href)
  nds <- html_nodes(htm, "div[id='roll-call-vote-pro-env-choice']")
  pro <- xml_text(nds) %>% gsub('\n', '', .) %>% gsub('\t', '', .)
  
  if_else(pro == 'No', 0, 1)
  
}

pb <- progress_estimated(nrow(lcv))
lcv_score$lcv_vote <- map_chr(lcv_score$href, lcv_position, pb = pb) # very slow
lcv_score$lcv_vote <- as.numeric(lcv_score$lcv_vote)

# add note, indicator for not in LCV, and indicator for double count
lcv_score <- lcv_score %>%
  filter(year != 1970) %>%
  mutate(
    lcv_note = "scorecard",
    not_in_lcv = 0,
    double_count = 0
  )

# find cutoff and add chamber
x <- which(grep(2016, lcv_score$year) > lag(grep(2016, lcv_score$year)) + 1)
x <- grep(2016, lcv_score$year)[x]
lcv_score$chamber <- c(
  rep("Senate", x - 1), 
  rep("House", nrow(lcv_score) - x + 1)
  )

# double counts
doubles <- grepl("2x Score", lcv_score$lcv_dscr)
lcv_score %<>% mutate(double_count = replace(double_count, doubles, 1))


# ----- LCV Recent Votes --------------------------------------------------

htm <- read_html('http://scorecard.lcv.org/recent-votes')
votes_nodes <- html_nodes(htm, "a[href^='roll-call-vote']")
class_nodes <- html_nodes(htm, "span[class='voteIssues']")

# create data frame
lcv_recent <- tibble(href = map_chr(votes_nodes, ~xml_attrs(.x)[['href']])) %>%
  mutate(
    rollnumber = map_chr(href, ~str_split(.x, '-')[[1]][4]) %>% as.numeric(),
    href = str_c('http://scorecard.lcv.org/', href),
    year = map_chr(href, ~str_split(.x, '-')[[1]][3]),
    year = map_chr(year, ~str_split(.x, '/')[[1]][2]) %>% as.numeric(),
    lcv_type = map_chr(class_nodes, ~xml_text(.x)[[1]]),
    lcv_dscr = map_chr(votes_nodes, ~xml_text(.x)[[1]])
  )

# lcv positions
pb <- progress_estimated(nrow(lcv_recent))
lcv_recent$lcv_vote <- map_chr(lcv_recent$href, lcv_position, pb = pb)
lcv_recent$lcv_vote <- as.numeric(lcv_recent$lcv_vote)

# add note, indicator for not in LCV, and indicator for double count
lcv_recent <- lcv_recent %>%
  mutate(
    lcv_note = "scorecard",
    not_in_lcv = 0,
    double_count = 0
  )

# find cutoff and add chamber
lcv_recent$chamber <- c(rep("Senate", 12), rep("House", nrow(lcv_recent) - 12))

# double counts
doubles <- grepl("2x Score", lcv_recent$lcv_dscr)
lcv_recent %<>% mutate(double_count = replace(double_count, doubles, 1))


# ----- Tidy Up -----------------------------------------------------------

# bind together
lcv <- rbind(lcv_recent, lcv_score)

# drop double counts (duplicate entries)
doubles <- str_c(
  lcv$year[lcv$double_count == 1],
  lcv$rollnumber[lcv$double_count == 1],
  lcv$chamber[lcv$double_count == 1]
  )
lcv <- lcv %>%
  mutate(id = str_c(year, rollnumber, chamber)) %>%
  filter(!id %in% doubles | double_count == 1) %>%
  select(-id)

# add congress
congress <- tibble(
  year = 1971:2017,
  congress = c(map(92:114, rep, times = 2) %>% unlist(), 115)
  )
lcv <- left_join(lcv, congress)

# change classes
lcv <- mutate_if(is.integer, as.numeric)


# ----- Add to Database ---------------------------------------------------

copy_to(db, lcv, temporary = F, overwrite = T)
