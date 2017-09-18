source('data/congress_meta.R')
census_dir <- '~/GoogleDrive/Projects/congress/raw/census/'
meta <- list.files(census_dir)[grepl('metadata', list.files(census_dir))]

# ------------------------------------------------------------------------------
# acs 2005
# ------------------------------------------------------------------------------
files <- meta[grepl('ACS_05', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs05 <- do.call(rbind, map(files, congress_meta))
acs05 <- acs05 %>%
  filter(units == 'estimate') %>%
  mutate(field = gsub('EST_', '', field))

# column shifts
acs05[58:80, 4:7] <- acs05[58:80, c(3, 5:7)]
acs05[97:103, 5:7] <- acs05[97:103, 4:6]
acs05[88:103, 4] <- acs05[88:103, 3]
acs05[254:277, 4] <- acs05[254:277, 3]
acs05[346:379, 5] <- acs05[346:379, 3]
acs05[380:409, 4:5] <- acs05[380:409, c(3, 5)]
acs05[410:468, 4:6] <- acs05[410:468, c(3, 5:6)]

# recode universe
acs05$universe[58:80] <- 'total population'
acs05$universe[88:103] <- 'total households'
acs05$universe[254:277] <- 'civilian employed population 16 years and over'
acs05$universe[346:379] <- 'total housing units'
acs05$universe[380:409] <- 'occupied housing units'
acs05$universe[410:451] <- 'owner-occupied units'
acs05$universe[452:468] <- 'renter-occupied units'

# redefine id
acs05$id <- apply(select(acs05, -field, -id), 1, str_c, collapse = ' - ')

# ------------------------------------------------------------------------------
# acs 2006
# ------------------------------------------------------------------------------
files <- meta[grepl('ACS_06', meta) & grepl('metadata', meta)]
acs06 <- do.call(rbind, map(str_c(census_dir, files), congress_meta))
acs06 <- acs06 %>%
  filter(units == 'estimate') %>%
  mutate(field = gsub('EST_', '', field))

# column shifts
acs06[17:23, 4] <- acs06[17:23, 3]
acs06[38, 5] <- acs06[38, 6]
acs06[174:197, 4] <- acs06[174:197, 3]
acs06[266:299, 5] <- acs06[266:299, 3]
acs06[300:329, 4:5] <- acs06[300:329, c(3, 5)]
acs06[330:388, 4:6] <- acs06[330:388, c(3, 5:6)]

# recode universe
acs06$universe[17:23] <- 'relationship'
acs06$universe[174:197] <- 'civilian employed population 16 years and over'
acs06$universe[266:299] <- 'total housing units'
acs06$universe[300:329] <- 'occupied housing units'
acs06$universe[330:371] <- 'owner-occupied units'
acs06$universe[372:388] <- 'renter-occupied units'

# recode category3
acs06$category3[38] <- ''

# redefine id
acs06$id <- apply(select(acs06, -field, -id), 1, str_c, collapse = ' - ')

# ------------------------------------------------------------------------------
# acs 2005/2006 - 109th congress
# ------------------------------------------------------------------------------
cen109 <- full_join(
  rename(acs06, field06 = field),
  rename(acs05, field05 = field)
)
cen109 <- cen109[c(1, 10, 2:9)]
mis <- filter(cen109, is.na(field06))

# matches
mat109 <- tribble(
  ~field06, ~field05,
  #---|---
  'VC48_DP2', 'VC41_DP2',  # grandparents living w/ grandchildren
  'VC49_DP2', 'VC42_DP2',  # grandparents living w/ grandchildren (resp)
  'VC51_DP2', 'VC44_DP2',  # grandparents living w/ grandchildren (< 1 yr)
  'VC52_DP2', 'VC45_DP2',  # grandparents living w/ grandchildren (1 to 2 yr)
  'VC53_DP2', 'VC46_DP2',  # grandparents living w/ grandchildren (3 to 4 yr)
  'VC54_DP2', 'VC47_DP2'  # grandparents living w/ grandchildren (> 5 yr)
)

# filter and drop
cen109$field05[cen109$field06 %in% mat109$field06] <- mat109$field05
cen109 <- filter(cen109, !field05 %in% mat109$field05 | !is.na(field06))

# ------------------------------------------------------------------------------
# acs 2007
# ------------------------------------------------------------------------------
files <- meta[grepl('ACS_07', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs07 <- do.call(rbind, map(files, congress_meta))
acs07 <- acs07 %>%
  filter(units == 'estimate') %>%
  mutate(field = gsub('HC01_EST_', '', field))

# column shifts
acs07[17:23, 3] <- acs07[17:23, 4]
acs07[268:305, 5] <- acs07[268:305, 3]

# recode universe
acs07$universe[268:305] <- 'total housing units'

# recode category1
acs07$category1[17:23] <- 'household population'
acs07$category1[c(351, 352, 361)] <- 'mortgage status'
acs07$category1[c(353:360, 362:367)] <- 'selected monthly owner costs'

# recode category2
acs07$category2[c(268, 278, 288, 299)] <- ''
acs07$category2[369:375] <- 'housing units with a mortgage'
acs07$category2[376:384] <- 'housing units without a mortgage'

# redefine id
acs07$id <- apply(select(acs07, -field, -id), 1, str_c, collapse = ' - ')

# ------------------------------------------------------------------------------
# acs 2008
# ------------------------------------------------------------------------------
files <- meta[grepl('ACS_08', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs08 <- do.call(rbind, map(files, congress_meta))
acs08 <- acs08 %>%
  filter(units == 'estimate') %>%
  mutate(field = gsub('HC01_EST_', '', field))

# column shifts
acs08[17:23, 3] <- acs08[17:23, 4]
acs08[269:306, 5] <- acs08[269:306, 3]
acs08[355:386, 5:6] <- acs08[355:386, c(3, 5)]

# recode universe
acs08$universe[269:306] <- 'total housing units'
acs08$universe[355:386] <- 'owner-occupied units'
acs08$universe[387:404] <- 'renter-occupied units'

# recode category1
acs08$category1[17:23] <- 'household population'

# recode category2
acs08$category2[c(269, 279, 289, 300)] <- ''
acs08$category2[371:377] <- 'housing units with a mortgage'
acs08$category2[378:386] <- 'housing units without a mortgage'

# redefine id
acs08$id <- apply(select(acs08, -field, -id), 1, str_c, collapse = ' - ')

# ------------------------------------------------------------------------------
# acs 2007/2008 - 110th congress
# ------------------------------------------------------------------------------
cen110 <- full_join(
  rename(acs08, field08 = field),
  rename(acs07, field07 = field)
)
cen110 <- cen110[c(1, 10, 2:9)]
mis <- filter(cen110, is.na(field08))

# matches
mat110 <- tribble(
  ~field08, ~field07,
  #---|---
  'VC56_DP2', 'VC56_DP2',  # female grandparents responsible for grandchildren
  'VC57_DP2', 'VC57_DP2',  # married grandparents responsible for grandchildren
  'VC86_DP2', 'VC86_DP2',  # pop 65 or older
  'VC87_DP2', 'VC86_DP2',  # pop 65 or older with disability
  'VC40_DP4', 'VC40_DP4',  # median rooms
  'VC149_DP4', 'VC146_DP4'  # no cash rent
)

# filter and drop
cen110$field07[cen110$field08 %in% mat110$field08] <- mat110$field07
cen110 <- filter(cen110, !field07 %in% mat110$field07 | !is.na(field08))

# ------------------------------------------------------------------------------
# acs 2009
# ------------------------------------------------------------------------------
files <- meta[grepl('ACS_09', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs09 <- do.call(rbind, map(files, congress_meta))
acs09 <- acs09 %>%
  filter(units == 'estimate') %>%
  mutate(field = gsub('HC01_EST_', '', field))

# column shifts
acs09[17:23, 3] <- acs09[17:23, 4]
acs09[38, 5] <- acs09[38, 6]
acs09[276:313, 5] <- acs09[276:313, 3]
acs09[362:393, 5:6] <- acs09[362:393, c(3, 5)]

# recode universe
acs09$universe[276:313] <- 'total housing units'
acs09$universe[362:393] <- 'owner-occupied units'
acs09$universe[394:411] <- 'renter-occupied units'

# recode category1
acs09$category1[17:23] <- 'household population'

# recode category2
acs09$category2[38] <- ''
acs09$category2[c(276, 286, 296, 307)] <- ''
acs09$category2[378:384] <- 'housing units with a mortgage'
acs09$category2[385:393] <- 'housing units without a mortgage'

# recode category3
acs09$category3[38] <- ''

# redefine id
acs09$id <- apply(select(acs09, -field, -id), 1, str_c, collapse = ' - ')

# ------------------------------------------------------------------------------
# acs 2010 + census 2010
# ------------------------------------------------------------------------------
files <- meta[grepl('ACS_10', meta) & grepl('metadata', meta)]
files <- append('DEC_10_SF2_SF2DP1_metadata.csv', files)
files <- str_c(census_dir, files)
acs10 <- do.call(rbind, map(files, congress_meta))
acs10 <- acs10 %>%
  filter(units == 'estimate') %>%
  mutate(field = gsub('HD01_', '', field)) %>%
  mutate(field = gsub('HC01_', '', field))

# metadata is terrible! check against 2009
str_extract(acs09$field, 'DP\\d') %>% table()
str_extract(acs10$field, 'DP\\d') %>% table()

# replace metadata in DP2 and DP4 using 2009
acs10[122:270, -1] <- acs09[1:149, -1]
acs10[408:548, -1] <- acs09[271:411, -1]

# DP3 changes between 2009 and 2010
# 1. 2009 has a "farming, fishing, and forestry occupations" category
# 2. 2010 has additional information regarding insurance for those 18 to 64
acs10[271:299, -1] <- acs09[150:178, -1]
acs10[300:371, -1] <- acs09[180:251, -1]
acs10[389:407, -1] <- acs09[252:270, -1]

# get additional changes from acs 2011
acs11 <- congress_meta(
  '~/GoogleDrive/Projects/congress/raw/census/ACS_11_1YR_DP03_metadata.csv'
  ) %>%
  filter(units == 'estimate') %>%
  mutate(field = gsub('HC01_', '', field))
acs11[103:118, 5:8] <- acs11[103:118, c(3, 5:7)]
acs11[102:118, 3] <- 'civilian noninstitutionalized population 18 to 64 years'
acs11$category2[102:118] <- str_replace(acs11$category2[102:118], ':', '')
acs11$category3[102:118] <- str_replace(acs11$category3[102:118], ':', '')

# redefine id
acs11$id <- apply(select(acs11, -field, -id), 1, str_c, collapse = ' - ')

# DP3 use acs patched portion of 2011 for acs 2010
acs10[372:388, -1] <- acs11[102:118, -1]

# ------------------------------------------------------------------------------
# acs 2009/2010 - 111th congress
# ------------------------------------------------------------------------------
cen111 <- full_join(
  rename(acs10, field10 = field),
  rename(acs09, field09 = field)
)
cen111 <- cen111[c(1, 10, 2:9)]
mis <- filter(cen111, is.na(field10))

# matches
mat110 <- tribble(
  ~field08, ~field07,
  #---|---
  'VC56_DP2', 'VC56_DP2',  # female grandparents responsible for grandchildren
  'VC57_DP2', 'VC57_DP2',  # married grandparents responsible for grandchildren
  'VC86_DP2', 'VC86_DP2',  # pop 65 or older
  'VC87_DP2', 'VC86_DP2',  # pop 65 or older with disability
  'VC40_DP4', 'VC40_DP4',  # median rooms
  'VC149_DP4', 'VC146_DP4'  # no cash rent
)

# filter and drop
cen110$field07[cen110$field08 %in% mat110$field08] <- mat110$field07
cen110 <- filter(cen110, !field07 %in% mat110$field07 | !is.na(field08))

# ------------------------------------------------------------------------------
# acs 2011
# ------------------------------------------------------------------------------
files <- meta[grepl('ACS_11', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs11 <- do.call(rbind, map(files, congress_meta))
acs11 <- acs11 %>%
  filter(units == 'estimate') %>%
  mutate(field = gsub('HC01_', '', field))

# metadata is terrible! check against 2010
str_extract(acs10$field, 'DP\\d') %>% table()
str_extract(acs11$field, 'DP\\d') %>% table()

# replace metadata in DP2, DP3, and DP4 using 2010
acs11[, -1] <- acs10[122:548, -1]

# redefine id
acs11$id <- apply(select(acs11, -field, -id), 1, str_c, collapse = ' - ')

# ------------------------------------------------------------------------------
# acs 2011 - 112th congress
# ------------------------------------------------------------------------------
cen112 <- acs11

# ------------------------------------------------------------------------------
# acs 2012
# ------------------------------------------------------------------------------
files <- meta[grepl('ACS_12', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs12 <- do.call(rbind, map(files, congress_meta))
acs12 <- acs12 %>%
  filter(units == 'estimate') %>%
  mutate(field = gsub('HC01_', '', field))

# metadata is terrible! check against 2010
str_extract(acs10$field, 'DP\\d') %>% table()
str_extract(acs12$field, 'DP\\d') %>% table()

# replace metadata in DP2, DP3, and DP4 using 2010
acs12[, -1] <- acs10[122:548, -1]

# redefine id
acs12$id <- apply(select(acs12, -field, -id), 1, str_c, collapse = ' - ')

# ------------------------------------------------------------------------------
# acs 2013 + census 2010
# ------------------------------------------------------------------------------
files <- meta[grepl('ACS_13', meta) & grepl('metadata', meta)]
files <- append('DEC_10_113_113DP1_metadata.csv', files)
files <- str_c(census_dir, files)
acs13 <- do.call(rbind, map(files, congress_meta))
acs13 <- acs13 %>%
  filter(units == 'estimate') %>%
  mutate(field = gsub('HD01_', '', field)) %>%
  mutate(field = gsub('HC01_', '', field))

# check against 2012
str_extract(acs12$field, 'DP\\d') %>% table()
str_extract(acs13$field, 'DP\\d') %>% table()

# replace metadata in DP2, DP3, and DP4 using 2012
# 1. 2013 has a "computer and internet use" category
acs13[187:335, -1] <- acs12[1:149, -1]
acs13[339:616, -1] <- acs12[150:427, -1]

# redefine id
acs13$id <- apply(select(acs13, -field, -id), 1, str_c, collapse = ' - ')

# ------------------------------------------------------------------------------
# acs 2012/2013 + census 2010 - 113th congress
# ------------------------------------------------------------------------------
cen111 <- full_join(
  rename(acs10, field10 = field),
  rename(acs09, field09 = field)
)
cen111 <- cen111[c(1, 10, 2:9)]
mis <- filter(cen111, is.na(field10))

# matches
mat110 <- tribble(
  ~field08, ~field07,
  #---|---
  'VC56_DP2', 'VC56_DP2',  # female grandparents responsible for grandchildren
  'VC57_DP2', 'VC57_DP2',  # married grandparents responsible for grandchildren
  'VC86_DP2', 'VC86_DP2',  # pop 65 or older
  'VC87_DP2', 'VC86_DP2',  # pop 65 or older with disability
  'VC40_DP4', 'VC40_DP4',  # median rooms
  'VC149_DP4', 'VC146_DP4'  # no cash rent
)

# filter and drop
cen110$field07[cen110$field08 %in% mat110$field08] <- mat110$field07
cen110 <- filter(cen110, !field07 %in% mat110$field07 | !is.na(field08))