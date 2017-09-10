#-------------------------------------------------------------------------------
# lcv scorecard votes
#-------------------------------------------------------------------------------

# lcv scorecards
src <- read_html('http://scorecard.lcv.org/scorecard')
votes_nodes <- html_nodes(src, "a[href^='roll-call-vote']")
class_nodes <- html_nodes(src, "span[class='voteIssuesFULL']")

# create data frame
votes <- tibble(href = map_chr(votes_nodes, ~xml_attrs(.x)[['href']])) %>%
  mutate(
    rollnumber = map_chr(href, ~str_split(.x, '-')[[1]][4]) %>% as.numeric(),
    href = str_c('http://scorecard.lcv.org/', href),
    year = map_chr(href, ~str_split(.x, '-')[[1]][3]),
    year = map_chr(year, ~str_split(.x, '/')[[1]][2]) %>% as.numeric(),
    lcv_type = map_chr(class_nodes, ~xml_text(.x)[[1]]),
    lcv_type = str_split(lcv_type, 1, nchar(lcv_type) - 1) %>% trim()
    lcv_dscr = map_chr(votes_nodes, ~xml_text(.x)[[1]])
  )

# lcv positions
lcv_position <- function(href, pb = NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  src <- read_html(href)
  nds <- html_nodes(src, "div[id='roll-call-vote-pro-env-choice']")
  pro <- xml_text(nds) %>% gsub('\n', '', .) %>% gsub('\t', '', .)
  
  if_else(pro == 'No', 0, 1)
  
}
pb <- progress_estimated(nrow(votes))
votes$lcv_vote <- map_chr(votes$href, lcv_position, pb = pb) # very slow!!!
votes$lcv_vote <- as.numeric(votes$lcv_vote)

# find cutoff for senate votes
grep(2016, votes$year) ; cutoff <- 547

# Extraction function
get.vote <- function(x) {
  
  rc <- votes[x] %>% 
    xml_attrs %>% 
    extract2(1) %>%
    strsplit("-") %>%
    extract2(1) %>%
    extract(4)
  
  yr <- votes[x] %>% 
    xml_attrs %>% 
    extract2(1) %>%
    strsplit("/") %>%
    extract2(1) %>%
    extract(2) %>%
    substr(1, 4)
  
  ds <- votes[x] %>% 
    xml_text %>%
    trim
  
  ct <- class[x] %>%
    xml_text %>%
    strsplit(",") %>%
    extract2(1) %>%
    extract(1) %>%
    trim
  
  link <- votes[x] %>% 
    xml_attrs %>% 
    extract2(1)
  
  pro <- paste0("http://scorecard.lcv.org/", link) %>%
    html_session %>%
    html_nodes("div[id='roll-call-vote-pro-env-choice']") %>%
    xml_text %>%
    gsub("\n", "", .) %>%
    gsub("\t", "", .)
  if (pro == "Yes") { pv <- 1 } else { pv <- 0 }
  
  c(rc, yr, ct, ds, pv)
  
} # End function

# Extract votes
lcv <- NULL
for (i in 1:length(votes)) {
  lcv <- rbind(lcv, get.vote(i))
  print(i)
} 

# Add indicator for not in LCV, indicator for double count, and note
lcv <- lcv %>%
  data.frame %>%
  setNames(c("rc.no", "year", "lcv.type", "lcv.dscr", "lcv.pos")) %>%
  subset(year != 1970) %>%
  mutate(
    lcv.note = rep("scorecard", n()),
    not.in.lcv = rep(0, n()),
    double.count = rep(0, n())
  )

# Find break between chambers
grep(2014, lcv$year)
b <- 505

# Add chamber
lcv$type <- c(
  rep("sen", b - 1),
  rep("hou", nrow(lcv) - b + 1)
  )

# Double counts
lcv <- within(lcv, {
  double.count[rc.no == 337 & year == 1995 & type == "hou"] <- 1
  double.count[rc.no == 145 & year == 2003 & type == "hou"] <- 1
  double.count[rc.no == 477 & year == 2009 & type == "hou"] <- 1
  double.count[rc.no == 71  & year == 2002 & type == "sen"] <- 1
  double.count[rc.no == 456 & year == 2003 & type == "sen"] <- 1
  double.count[rc.no == 184 & year == 2010 & type == "sen"] <- 1
  })

#-------------------------------------------------------------------------------
# Not in LCV dataset
#-------------------------------------------------------------------------------

lcv.add.hou <- data.frame(
  rc.no = c(575, 230, 231),
  year = c(1993, 2011, 2011),
  lcv.type = c("Other", rep("Climate Change", 2)),
  lcv.dscr = rep("", 3),
  lcv.pos = c(0, 0, 0),
  lcv.note = rep("manual-add", 3),
  not.in.lcv = rep(1, 3),
  double.count = rep(0, 3),
  type = rep("hou", 3)
  )

lcv.add.sen <- data.frame(
  rc.no = c(395, 148, 151, 141, 117, 126, 141, 142, 51, 52, 53),
  year = c(1993, 2005, 2005, 2008, 2009, 2009, 2009, 2009, 2011, 2011, 2011),
  lcv.type = c("Other", rep("Climate Change", 10)),
  lcv.dscr = rep("", 11),
  lcv.pos = c(0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0),
  lcv.note = rep("manual-add", 11),
  not.in.lcv = rep(1, 11),
  double.count = rep(0, 11),
  type = rep("sen", 11)
  )

# Bind together
lcv <- rbind(lcv, lcv.add.hou, lcv.add.sen)

#-------------------------------------------------------------------------------
# LCV recent votes
#-------------------------------------------------------------------------------
url <- "http://scorecard.lcv.org/recent-votes"
ses <- html_session(url)
votes <- ses %>% html_nodes("a[href^='roll-call-vote']")
class <- ses %>% html_nodes("span[class='voteIssues']")

# Extract recent votes
lcv.recent <- NULL
for (i in 1:length(votes)) {
  lcv.recent <- rbind(lcv.recent, get.vote(i))
  print(i)
} 

# Add indicator for not in LCV, indicator for double count, and note
lcv.recent <- lcv.recent %>%
  data.frame %>%
  setNames(c("rc.no", "year", "lcv.type", "lcv.dscr", "lcv.pos")) %>%
  mutate(
    lcv.note = rep("recent", n()),
    not.in.lcv = rep(0, n()),
    double.count = rep(0, n())
  )

# Add chamber
lcv.recent$type <- c(
  rep("sen", 36 - 1),
  rep("hou", nrow(lcv.recent) - 36 + 1)
)

# Bind together
lcv <- rbind(lcv, lcv.recent)

#-------------------------------------------------------------------------------
# Manual edits and save
#-------------------------------------------------------------------------------

# Drop sponsorships and double counts
lcv <- lcv %>%
  subset(rc.no != 0) %>%
  subset(!grepl("2x Score", lcv.dscr))

# Add rollcall variable
lcv <- lcv %>%
  mutate(
    rollcall = paste("rc", year, rc.no, sep = ".")
  )
  
# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

copy_to(db, lcv, temporary = F)

#-------------------------------------------------------------------------------
# Check against older version
#-------------------------------------------------------------------------------

# House
hou.new <- lcv %>%
  subset(type == "hou" & year >= 1993 & year <= 2012) %>%
  mutate(
    rc.no = as.numeric(rc.no)
    ) %>%
  arrange(year, rc.no)

rep.old <- read.csv(paste0(dlcv, "old/lcv_votes_dscr_house.csv")) %>%
  arrange(year, rollcall)

table(hou.new$rc.no == rep.old$rollcall)

table(hou.new$lcv.pos == rep.old$vote_lcv)

# Senate
sen.new <- lcv %>%
  subset(type == "sen" & year >= 1993 & year <= 2012) %>%
  mutate(
    rc.no = as.numeric(rc.no)
  ) %>%
  arrange(year, rc.no)

sen.old <- read.csv(paste0(dlcv, "old/lcv_votes_dscr_senate.csv")) %>%
  arrange(year, rollcall)

table(sen.new$rc.no == sen.old$rollcall)

table(sen.new$lcv.pos == sen.old$vote_lcv)

# Two miscodes:
#   1997 - Roll call 42 shoud have been a no position
#   1999 - Roll call 268 should have been 266
