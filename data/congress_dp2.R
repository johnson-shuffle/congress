source('data/congress_meta.R')
census_dir <- '~/GoogleDrive/Projects/congress/raw/census/'
meta <- list.files(census_dir)[grepl('metadata', list.files(census_dir))]

files <- meta[grepl('DP2|DP02', meta) & grepl('metadata', meta)]
csv <- map(str_c(census_dir, files), congress_meta)

acs109_05 <- csv[[1]]
acs109_06 <- csv[[2]]
acs110_07 <- csv[[3]]
acs110_08 <- csv[[4]]
acs111_09 <- csv[[5]]
acs111_10 <- csv[[6]]
acs112_11 <- csv[[7]]
acs113_12 <- csv[[8]]
acs113_13 <- csv[[9]]
acs114_14 <- csv[[10]]
acs114_15 <- csv[[11]]
cen110 <- csv[[12]]
cen106 <- csv[[13]]

# check tables for equality
x <- acs111_10 %>%
  filter(str_sub(field, 1, 4) == 'HC01') %>%
  rename(field111_10 = field)
y <- acs111_09 %>%
  filter(str_sub(field, 1, 4) == 'HC01' & units == 'estimate') %>%
  rename(field111_09 = field)
z <- full_join(x, y)
z <- z[c(1, 10, 2:9)]


# fix 110th table - switch category & universe
x <- cen110$universe[101:192]
y <- cen110$category[101:192]
cen110$universe[101:192] <- y
cen110$category[101:192] <- x
cen110$id <- apply(select(cen110, -field, -id), 1, str_c, collapse = ' - ')
rm(x, y)

# fix 109th table - column shifts
acs109[55:114, 5:7] <- acs109[55:114, 4:6]
acs109[115:128, 5:6] <- acs109[115:128, c(3, 5)]
acs109[161:174, 4:7] <- acs109[161:174, 3:6]
acs109[181:198, 6:7] <- acs109[181:198, 5:6]
acs109[199:206, 4:5] <- acs109[199:206, 3:4]

# fix 109th table - recode universe
acs109$universe[115:174] <- 'total population'
acs109$universe[175:206] <- 'total households'

# fix 109th table - recode category
acs109$category[55:128] <- 'race'
acs109$category[59] <- ''
acs109$category[60] <- ''
acs109$category[129:160] <- 'hispanic or latino and race'
acs109$category[175:206] <- 'households by type'

# fix 109th table - recode group1
acs109$group1[161:174] <- 'in households'
acs109$group1[181:192] <- 'family households (families)'
acs109$group1[193:198] <- 'nonfamily households'

acs109$id <- apply(select(acs109, -field, -id), 1, str_c, collapse = ' - ')

# fix 106th table - switch category & universe
x <- cen106$universe[101:192]
y <- cen106$category[101:192]
cen106$universe[101:192] <- y
cen106$category[101:192] <- x
cen106$id <- apply(select(cen106, -field, -id), 1, str_c, collapse = ' - ')
rm(x, y)

# start with 113th
cw1 <- cen113 %>%
  filter(str_sub(field, 1, 4) == 'HD01') %>%
  rename(field113 = field)

# ------------------------------------------------------------------------------
# compare with 111th
# ------------------------------------------------------------------------------
tmp <- cen111 %>%
  filter(str_sub(field, 1, 4) == 'HD01') %>%
  rename(field111 = field)

cw2 <- full_join(cw1, tmp)
cw2 <- cw2[c(1, 10, 2:9)]

mis <- filter(cw2, is.na(field113))

# ------------------------------------------------------------------------------
# compare with 110th
# ------------------------------------------------------------------------------
tmp <- cen110 %>%
  filter(str_sub(field, 1, 4) == 'HC01') %>%
  rename(field110 = field)

cw3 <- full_join(cw2, tmp)
cw3 <- cw3[c(1:2, 11, 3:10)]

mis <- filter(cw3, is.na(field113) & is.na(field111))

# manual matches
mat <- tribble(
  ~field113, ~ field110,
  #---|----
  'HD01_S001_DP1', 'HC01_VC01_DP1',
  'HD01_S026_DP1', 'HC01_VC03_DP1',
  'HD01_S051_DP1', 'HC01_VC04_DP1',
  'HD01_S047_DP1', 'HC01_VC20_DP1',
  'HD01_S072_DP1', 'HC01_VC21_DP1',
  'HD01_S050_DP1', 'HC01_VC25_DP1',
  'HD01_S075_DP1', 'HC01_VC26_DP1',
  'HD01_S107_DP1', 'HC01_VC56_DP1',
  'HD01_S108_DP1', 'HC01_VC57_DP1',
  'HD01_S109_DP1', 'HC01_VC58_DP1',
  'HD01_S110_DP1', 'HC01_VC59_DP1',
  'HD01_S111_DP1', 'HC01_VC60_DP1',
  'HD01_S153_DP1', 'HC01_VC81_DP1',
  'HD01_S154_DP1', 'HC01_VC82_DP1',
  'HD01_S183_DP1', 'HC01_VC103_DP1',
  'HD01_S186_DP1', 'HC01_VC104_DP1'
)
  
# recode and drop
cw3$field110[cw3$field113 %in% mat$field113] <- mat$field110
cw3 <- filter(cw3, !field110 %in% mat$field110 | !is.na(field113))

# ------------------------------------------------------------------------------
# compare with 109th
# ------------------------------------------------------------------------------
tmp <- acs109 %>%
  filter(str_sub(field, 1, 3) == 'EST') %>%
  rename(field109 = field)

cw4 <- full_join(cw3, tmp)
cw4 <- cw4[c(1:3, 12, 4:11)]

mis <- filter(cw4, is.na(field113))

# manual matches (fill in field 110)
cw4$field109[cw4$field110 == 'HC01_VC87_DP1'] <- 'EST_VC105_DP1'

# manual matches (fill in field 113)
mat <- tribble(
  ~field113, ~field109,
  #---|---
  'HD01_S001_DP1', 'EST_VC01_DP1',  # total population
  'HD01_S026_DP1', 'EST_VC03_DP1',  # male population
  'HD01_S051_DP1', 'EST_VC04_DP1',  # female population
  'HD01_S047_DP1', 'EST_VC24_DP1',  # male 18 and over
  'HD01_S072_DP1', 'EST_VC25_DP1',  # female 18 and over
  'HD01_S050_DP1', 'EST_VC27_DP1',  # male 65 and over
  'HD01_S075_DP1', 'EST_VC28_DP1',  # female 65 and over
  'HD01_S099_DP1', 'EST_VC56_DP1',  # two races: white & black
  'HD01_S096_DP1', 'EST_VC57_DP1',  # two races: white & indian/native
  'HD01_S097_DP1', 'EST_VC58_DP1',  # two races: white & asian
  'HD01_S107_DP1', 'EST_VC70_DP1',  # hispanic/latino
  'HD01_S108_DP1', 'EST_VC71_DP1',  # hispanic/latino - mexican
  'HD01_S109_DP1', 'EST_VC72_DP1',  # hispanic/latino - puerto rican
  'HD01_S110_DP1', 'EST_VC73_DP1',  # hispanic/latino - cuban
  'HD01_S111_DP1', 'EST_VC74_DP1',  # hispanic/latino - other
  'HD01_S153_DP1', 'EST_VC97_DP1',  # married-couple
  'HD01_S154_DP1', 'EST_VC98_DP1',  # married-couple w/ children under 18
  'HD01_S165_DP1', 'EST_VC106_DP1',  # households with people under 18 years
  'HD01_S166_DP1', 'EST_VC107_DP1'   # households with people over 65 years
)

# recode and drops
cw4$field109[cw4$field113 %in% mat$field113] <- mat$field109
cw4 <- cw4 %>%
  filter(field109 != 'EST_VC105_DP1' | !is.na(field113) | !is.na(field110)) %>%
  filter(!field109 %in% mat$field109 | !is.na(field113))

# ------------------------------------------------------------------------------
# compare with 106th
# ------------------------------------------------------------------------------
all.equal(cen110, cen106)

cw5 <- cw4
cw5$field106 <- cw5$field110
cw5 <- cw5[c(1:4, 13, 5:12)]
