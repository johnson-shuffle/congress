# ----- Preample ----------------------------------------------------------

# helper function for reading cq csv files
cq_fun <- function(file, office = NULL) {
  dat <- read_csv(file, skip = 2) %>% filter(Office == office)
  if (office == "Senate") {
    dat$raceYear <- as.numeric(str_extract(file, "\\d{4}"))
  }
  return(dat)
}


# ----- House -------------------------------------------------------------

fls <- list.files(path = "./raw", pattern = "hou", full.names = T)
raw <- map_dfr(fls, cq_fun, office = "House")

# blank lines, runoffs, and special elections
hou_dupes <- duplicates(raw, c("State", "Area", "raceYear"))
hou %<>%
  mutate(Type = "General") %>%
  unite(id, State, AreaNumber, raceYear) %>%
  filter(id != "Louisiana_1_2014" | RepVotes != "N/A") %>%  # blank line
  filter(id != "Louisiana_2_2014" | RepVotes != "N/A") %>%  # blank line
  filter(id != "Louisiana_3_2014" | RepVotes != "N/A") %>%  # blank line
  filter(id != "Louisiana_4_2014" | RepVotes != "N/A") %>%  # blank line
  within({
    Type[id == "Florida_19_2014" & RepVotes == "66,922"] <- "Special"
    Type[id == "Louisiana_2_2006" & DemVotes == "35,153"] <- "Runoff"
    Type[id == "Louisiana_3_2012" & RepVotes == "58,820"] <- "Runoff"
    Type[id == "Louisiana_5_2014" & RepVotes == "134,616"] <- "Runoff"
    Type[id == "Louisiana_6_2014" & RepVotes == "139,209"] <- "Runoff"
    Type[id == "Texas_23_2006" & RepVotes == "32,217"] <- "Runoff"
  }) %>%
  separate(id, into = c("State", "AreaNumber", "raceYear"), sep = "_")

# extra candidates
ext <- hou[grepl("The other vote was:", hou$RaceNotes), ]
ext$RaceNotes <- str_extract(ext$RaceNotes, "(The other vote was:)[:print:]+")
ext$RaceNotes %<>% str_replace("The other vote was:", "")
ext <- cbind(
  select(ext, State, Area, raceYear),
  as_tibble(str_split(ext$RaceNotes, ";", simplify = T))
)
ext %<>% gather(key, value, -1:-3)
ext %<>% filter(!is.na(value))
ext <- ext[grepl("The other vote", ext$value), ]

p1 <- "(?<=\\().*?(?=\\))"
n1 <- "([A-z]+\\-[A-z]+, [A-z]+\\s[A-Z]\\.)"
n2 <- "([A-z]+\\-[A-z]+, [A-z]+)"
n3 <- "([A-z]+\\-[A-z]+)"
p2 <- str_c(n1, n2, n3, sep = "|")
p3 <- "(\\d+)|(\\d+,\\d+)"

# missing elections (435 each cycle)
#   1996: LA districts 1, 2, 3, 4, 6
#   2006: LA districts 2 (has two lines)
#   2014: LA districts 2
table(filter(hou, Type == "General")$raceYear)

# convert votes to numeric
hou %<>% mutate_at(vars(contains("Votes")), str_replace, ",", "")
hou %<>% mutate_at(vars(contains("Votes")), as.numeric)

# number of votes cast
hou$AllVotes <- rowSums(hou[str_subset(names(hou), "[^y]Votes$")], na.rm = T)
hou$AllPercent  <- rowSums(hou[str_subset(names(hou), "Percent$")], na.rm = T)

# fix uncontested elections
hou_zero <- filter(hou, AllPercent < 100)
hou %<>%
  unite(id, State, Area, raceYear) %>%
  within({
    DemVotesMajorPercent[id == "Arkansas_District 4_2004"] <- 100
    RepVotesMajorPercent[id == "Florida_District 7_2004"] <- 100
    RepVotesMajorPercent[id == "Florida_District 25_2004"] <- 100
    RepVotesMajorPercent[id == "Florida_District 24_2004"] <- 100
    DemVotesMajorPercent[id == "Florida_District 23_2004"] <- 100
  }) %>%
  within({
    DemCandidate[id == "Florida_District 19_2004"] <- "Wexler, Robert"
    DemStatus[id == "Florida_District 19_2004"] <- "Incumbent"
    DemVotesMajorPercent[id == "Florida_District 19_2004"] <- 100
    RepCandidate[id == "Florida_District 15_2012"] <- "Ross, Dennis A."
    RepStatus[id == "Florida_District 15_2012"] <- "Incumbent"
    RepVotesMajorPercent[id == "Florida_District 15_2012"] <- 100
    DemCandidate[id == "Florida_District 24_2012"] <- "Wilson, Frederica S."
    DemStatus[id == "Florida_District 24_2012"] <- "Incumbent"
    DemVotesMajorPercent[id == "Florida_District 24_2012"] <- 100
  }) %>%
  separate(id, into = c("State", "Area", "raceYear"), sep = "_")

# two incumbents?? (all but one are result of redistricting)
hou_incb <- as.numeric(hou$RepStatus == "Incumbent")
hou_incb <- hou_incb + as.numeric(hou$DemStatus == "Incumbent")
hou_incb <- hou_incb + as.numeric(hou$ThirdStatus == "Incumbent")
hou_incb <- hou[hou_incb >= 2, ]
hou %<>%
  unite(id, State, Area, raceYear) %>%
  within({
    RepStatus[id == "California_District 12_2004"] <- "Challenger"
  }) %>%
  separate(id, into = c("State", "Area", "raceYear"), sep = "_")

# bind by candidate
hou %<>% rename(ThirdVotesMajorPercent = ThirdVotesTotalPercent)
cand_attr <- c("Candidate", "Status", "VotesMajorPercent")
D <- hou[c("raceYear", "State", "Area", "AllVotes", str_c("Dem", cand_attr))]
R <- hou[c("raceYear", "State", "Area", "AllVotes", str_c("Rep", cand_attr))]
X <- hou[c("raceYear", "State", "Area", "AllVotes", str_c("Third", cand_attr))]
names(D)[5:7] <- cand_attr
names(R)[5:7] <- cand_attr
names(X)[5:7] <- cand_attr

# rank by percent
dat_hou <- rbind(D, R, X) %>%
  group_by(raceYear, State, Area) %>%
  mutate(Rank = rank(desc(VotesMajorPercent), ties.method = "first")) %>%
  ungroup()

# join by 1st and 2nd
dat_hou <- left_join(
  select(filter(dat_hou, Rank == 1), -Rank),
  select(filter(dat_hou, Rank == 2), -Rank, -AllVotes),
  by = c("raceYear", "State", "Area"),
  suffix = c("_1", "_2")
)


# ----- Senate ------------------------------------------------------------
fls <- list.files(path = "./raw", pattern = "sen", full.names = T)
sen <- map_dfr(fls, cq_fun, office = "Senate")

# duplicates
sen_dupes <- duplicates(sen, c("Area", "raceYear"))

# fix runoffs and special elections
sen %<>%
  unite(id, Area, raceYear) %>%
  filter(id != "Georgia_1992" | RepVotes != "1,073,282") %>%      # 1st round
  filter(id != "Georgia_2008" | RepVotes != "1,867,097") %>%      # 1st round
  separate(id, into = c("State", "raceYear"), sep = "_")

# convert votes to numeric
sen %<>% mutate_at(vars(contains("Votes")), str_replace, ",", "")
sen %<>% mutate_at(vars(contains("Votes")), as.numeric)

# number of votes cast
sen$AllVotes <- rowSums(sen[str_subset(names(sen), "[^y]Votes$")], na.rm = T)

# bind by candidate
sen %<>% rename(ThirdVotesMajorPercent = ThirdVotesTotalPercent)
cand_attr <- c("Candidate", "Status", "VotesMajorPercent")
D <- sen[c("raceYear", "State", "AllVotes", str_c("Dem", cand_attr))]
R <- sen[c("raceYear", "State", "AllVotes", str_c("Rep", cand_attr))]
X <- sen[c("raceYear", "State", "AllVotes", str_c("Third", cand_attr))]
names(D)[4:6] <- cand_attr
names(R)[4:6] <- cand_attr
names(X)[4:6] <- cand_attr

# rank by percent
dat_sen <- rbind(D, R, X) %>%
  group_by(raceYear, State) %>%
  mutate(Rank = rank(desc(VotesMajorPercent), ties.method = "first"))

# join by 1st and 2nd
dat_sen <- left_join(
  select(filter(dat_sen, Rank == 1), -Rank),
  select(filter(dat_sen, Rank == 2), -Rank, -AllVotes),
  by = c("raceYear", "State"),
  suffix = c("_one", "_two")
)


# ----- President ---------------------------------------------------------

cq.prez <- NULL
for (i in seq(1988, 2012, by = 4)) {
  dat <- read.csv(paste0(d$cq, "raw_files/prez_", i, ".csv"), skip = 2)
  dat <- dat %>% subset(Office == "President")
  cq.prez <- rbind(cq.prez, dat) ; rm(dat)
}

cq.prez <- cq.prez %>%
  merge(states[c("statedscr", "statecode", "fips")],
        by.x = "Area", by.y = "statedscr")

# Governors
cq.gov <- NULL
for (i in seq(1988, 2012, by = 4)) {

  dat <- read.csv(paste0(d$cq, "raw_files/gov_", i, ".csv"), skip = 2)

  dat <- dat %>% subset(Office == "Governor")

  cq.gov <- rbind(cq.gov, dat) ; rm(dat)

}

cq.gov <- cq.gov %>%
  merge(states[c("statedscr", "statecode", "fips")],
        by.x = "Area", by.y = "statedscr")

#-------------------------------------------------------------------------------
# Save general results
#-------------------------------------------------------------------------------
save(list = paste("cq", c("hou", "sen", "prez", "gov"), sep = "."),
     file = paste0(d$cq, "cq_elections.Rda"))

#-------------------------------------------------------------------------------
# Add GovTrack id"s
#-------------------------------------------------------------------------------

# House
load(paste0(d$legis, "names_house.Rda"))

hou <- hou %>%
  subset(occupancy <= 1 & means == 1) %>%
  mutate(
    cycle = factor(congress, labels = seq(1992, 2014, by = 2)),
    cycle = as.character(cycle)
  ) %>%
  select(congress, cycle, statecode, district, govtrackid)

margins.hou <- hou %>%
  merge(dat.hou, by = c("cycle", "statecode", "district"), all.x = T) %>%
  mutate(
    margin = replace(margin, is.na(margin), 1)
  )

dat.hou[!paste(dat.hou$firstname, dat.hou$lastname, dat.hou$cycle) %in%
        paste(margins.hou$firstname, margins.hou$lastname, margins.hou$cycle), ]

# Senate
load(paste0(d$legis, "names_senate.Rda"))

sen <- sen %>%
  subset(occupancy <= 1 & means == 1) %>%
  mutate(
    cycle = factor(congress, labels = seq(1992, 2014, by = 2)),
    cycle = as.character(cycle)
  ) %>%
  select(congress, cycle, statecode, class, govtrackid)

margins.sen <- sen %>%
  merge(dat.sen, by = c("cycle", "statecode", "class"), all.x = T) %>%
  subset(!is.na(margin))

dat.sen[!paste(dat.sen$firstname, dat.sen$lastname, dat.sen$cycle) %in%
        paste(margins.sen$firstname, margins.sen$lastname, margins.sen$cycle), ]

#-------------------------------------------------------------------------------
# Save
#-------------------------------------------------------------------------------
save(margins.hou, file = paste0(d$cq, "margins_house.Rda"))

save(margins.sen, file = paste0(d$cq, "margins_senate.Rda"))
