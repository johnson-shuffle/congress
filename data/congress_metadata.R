census_dir <- '~/GoogleDrive/Projects/congress/raw/census/'
meta <- list.files(census_dir)[grepl('metadata', list.files(census_dir))]

# ------------------------------------------------------------------------------
# 103rd - census 1990 - summary tape file 3 (icpsr series 06012)
# ------------------------------------------------------------------------------
dat <- read_csv(
  str_c(census_dir, 'DEC_90_STF3D_metadata.csv'),
  col_names = c("table", 'field', 'desc', 'segment'),
  skip = 1,
  trim_ws = F
)

# universe
tbl_uni <- filter(dat, grepl('Universe', desc)) %>%
  mutate(universe = toupper(gsub('Universe:', '', desc)))

# category
tbl_cat <- filter(dat, is.na(field) & toupper(desc) == desc) %>%
  group_by(table) %>%
  mutate(r = rank(table, ties.method = 'first')) %>%
  select(-segment)
tbl_cat <- spread(tbl_cat, r, desc) %>%
  mutate(
    `2` = replace(`2`, is.na(`2`), ''),
    category = str_c(`1`, `2`))

# groups
tbl_grp <- dat
tbl_grp$group1 <- NA
tbl_grp$group2 <- NA
tbl_grp$group3 <- NA
for (n in 2:nrow(tbl_grp)) {
  
  if (is.na(tbl_grp$field[n]) & str_sub(tbl_grp$desc[n], 1, 2) != '  ') {
    tbl_grp$group1[n] <- tbl_grp$desc[n]
  } else {
    tbl_grp$group1[n] <- tbl_grp$group1[n - 1]
    tbl_grp$group2[n] <- NA
    tbl_grp$group3[n] <- NA
  }
    
  if (is.na(tbl_grp$field[n]) & str_sub(tbl_grp$desc[n], 1, 2) == '  ') {
    tbl_grp$group2[n] <- tbl_grp$desc[n]
  } else if (!is.na(tbl_grp$field[n])) {
    tbl_grp$group2[n] <- tbl_grp$group2[n - 1]
  } 
    
  if (is.na(tbl_grp$field[n]) & str_sub(tbl_grp$desc[n], 1, 4) == '    ') {
    tbl_grp$group2[n] <- tbl_grp$group2[n - 1]
    tbl_grp$group3[n] <- tbl_grp$desc[n]
  } else if (!is.na(tbl_grp$field[n])) {
    tbl_grp$group3[n] <- tbl_grp$group3[n - 1]
  }

}

tbl_grp <- tbl_grp %>%
  filter(!is.na(field)) %>%
  mutate(
    group1 = replace(group1, group1 %in% c(tbl_uni$desc, tbl_cat$`1`), NA)
  ) %>%
  mutate_at(5:7, function(x) toupper(gsub(':', '', x)))

# merge back together
cen103 <- select(tbl_grp, -segment) %>%
  left_join(select(tbl_uni, table, universe)) %>%
  left_join(select(tbl_cat, table, category))
cen103 <- cen103[c(1:3, 7:8, 4:6)]

rm(dat, tbl_cat, tbl_grp, tbl_uni)

# ------------------------------------------------------------------------------
# census extraction function
# ------------------------------------------------------------------------------
meta_get <- function(file, s = 3, m = 2) {
  
  # identify table number
  tbl <- str_extract(file, 'DP\\d+')
  tbl <- gsub('0', '', tbl)
  
  # read data
  dat <- read_csv(file, skip = s, col_names = c('field', 'desc')) %>%
    mutate(
      field = str_c(field, tbl, sep = '_')
      )
  dat$desc <- gsub('White;', 'White and', dat$desc)
  
  # parse out descriptives
  txt <- str_split(dat$desc, ';', simplify = T)
  txt <- cbind(txt[, 1], str_split(txt[, m], ' - ', simplify = T))
  txt <- cbind(dat$field, txt)
  
  # convert to data frame
  out <- as_data_frame(txt)
  
  # remove year reference for inflation, footnotes, and acronyms
  out <- out %>%
    mutate_all(~str_replace(.x, '\\(IN\\s\\d{4}\\sINFLATION', '(IN INFLATION')) %>%
    mutate_all(~str_replace(.x, '\\[\\d+\\]', '')) %>%
    mutate_all(~str_replace(.x, '\\([A-Z]+\\)', ''))
  
  # bind to fake tibble to get columns
  fak <- tibble(V1 = NA, V2 = NA, V3 = NA, V4 = NA, V5 = NA, V6 = NA, V7 = NA, V8 = NA)
  out <- plyr::rbind.fill(out, fak)
  out <- out[1:(nrow(out) - 1), ]
  
  # trim and replace NA's with blank
  out <- mutate_all(out, str_trim)
  out <- mutate_all(out, function(x) replace(x, is.na(x), ''))
  
  # create names
  nam <-c(
    'field',
    'units',
    'universe',
    'category',
    'desc',
    str_c('group', 1:3)
    )
  names(out) <- nam
  
  # identify universe/category columns
  if (out$universe[1] == toupper(out$universe[1])) {
    names(out)[3:4] <- c('category', 'universe')
  }
  
  # reorder
  out <- out[nam]
  
  # convert units
  out <- out %>%
    within({
      units[units == 'Number'] <- 'Estimate'
      units[grepl('MOE', field) & units == 'Estimate'] <- 'Margin of Error'
      units[grepl('MOE', field) & units == 'Percent'] <- 'Percent Margin of Error'
      units[units == 'Estimate Margin of Error'] <- 'Margin of Error'
    })
  
  # create id
  out$id <- apply(select(out, -field), 1, str_c, collapse = ' - ')
  out$id <- out$id
  out <- mutate_at(out, -1, tolower)
  
  # return
  out
  
}

# ------------------------------------------------------------------------------
# 106th - census 2000
# ------------------------------------------------------------------------------
files <- meta[grepl('DEC_00_SF', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
cen106 <- do.call(plyr::rbind.fill, map(files, meta_get))

# ------------------------------------------------------------------------------
# 109th - acs 2005/2006
# ------------------------------------------------------------------------------

# acs 2005
files <- meta[grepl('ACS_05', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs05 <- do.call(plyr::rbind.fill, map(files, meta_get))

# acs 2006
files <- meta[grepl('ACS_06', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs06 <- do.call(plyr::rbind.fill, map(files, meta_get))

# ------------------------------------------------------------------------------
# 110th congress - census 2000 + acs 2007/2008
# ------------------------------------------------------------------------------

# census 2000
files <- meta[grepl('DEC_00_110', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
cen110 <- do.call(plyr::rbind.fill, map(files, meta_get))

# acs 2007
files <- meta[grepl('ACS_07', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs110_07 <- do.call(plyr::rbind.fill, map(files, meta_get, m = 3))

# acs 2008
files <- meta[grepl('ACS_08', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs110_08 <- do.call(plyr::rbind.fill, map(files, meta_get, m = 3))

# ------------------------------------------------------------------------------
# 111th congress - census 2010 + acs 2009/2010
# ------------------------------------------------------------------------------

# census 2010
files <- meta[grepl('DEC_10_SF2', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
cen111 <- do.call(plyr::rbind.fill, map(files, meta_get, s = 5))

# acs 2009
files <- meta[grepl('ACS_09', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs111_09 <- do.call(plyr::rbind.fill, map(files, meta_get, m = 3))

# acs 2010
files <- meta[grepl('ACS_10', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs111_10 <- do.call(plyr::rbind.fill, map(files, meta_get))

# ------------------------------------------------------------------------------
# 112th congress - acs 2011
# ------------------------------------------------------------------------------
files <- meta[grepl('ACS_11', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs112_11 <- do.call(plyr::rbind.fill, map(files, meta_get))

files <- meta[grepl('ACS_12', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs112_12 <- do.call(plyr::rbind.fill, map(files, meta_get))

# ------------------------------------------------------------------------------
# 113th congress - census 2010 + acs 2013
# ------------------------------------------------------------------------------

# census 2010
files <- meta[grepl('DEC_10_113', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
cen113 <- do.call(plyr::rbind.fill, map(files, meta_get))

# acs 2013
files <- meta[grepl('ACS_12', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs113_12 <- do.call(plyr::rbind.fill, map(files, meta_get))

# acs 2013
files <- meta[grepl('ACS_13', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs113_13 <- do.call(plyr::rbind.fill, map(files, meta_get))

# ------------------------------------------------------------------------------
# 114th congress - acs 2014/2015
# ------------------------------------------------------------------------------

# acs 2014
files <- meta[grepl('ACS_14', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs114_14 <- do.call(plyr::rbind.fill, map(files, meta_get))

# acs 2015
files <- meta[grepl('ACS_15', meta) & grepl('metadata', meta)]
files <- str_c(census_dir, files)
acs114_15 <- do.call(plyr::rbind.fill, map(files, meta_get))

# ------------------------------------------------------------------------------
# compare acs 2015 with 2014
# ------------------------------------------------------------------------------

# acs 2015
x <- acs114_15 %>% 
  filter(str_sub(field, 1, 4) == 'HC01') %>%
  rename(field15 = field)

# acs 2014
y <- acs114_14 %>%
  filter(str_sub(field, 1, 4) == 'HC01') %>%
  rename(field14 = field)
cw1 <- full_join(x, y)
cw1 <- cw1[c(1, 10, 2:7, 9, 8)]

# manually match
mis <- filter(y, field14 %in% cw$field15[is.na(cw1$field14)])
write_csv(cw1, path = '~/Desktop/acs.csv')
mat <- mis$field14[1:19]

# recode matches and drop
cw1$field14[cw1$field15 %in% mat] <- cw1$field15[cw1$field15 %in% mat]
cw1 <- filter(cw1, !field14 %in% mat | !is.na(field15))

# ------------------------------------------------------------------------------
# compare with 2013
# ------------------------------------------------------------------------------

# acs 2013
y <- acs113 %>%
  filter(str_sub(field, 1, 4) == 'HC01') %>%
  rename(field13 = field)
cw2 <- full_join(cw1, y)
cw2 <- cw2[c(1:2, 11, 3:9, 10)]

# manually match
mis <- filter(y, field13 %in% cw2$field13[is.na(cw2$field15) & is.na(cw2$field14)])
write_csv(cw2, path = '~/Desktop/acs.csv')
mat <- mis$field13[1:19]

# recode matches and drop
cw2$field13[cw2$field15 %in% mat] <- cw2$field15[cw2$field15 %in% mat]
cw2 <- filter(cw2, !field13 %in% mat | !is.na(field15))

# ------------------------------------------------------------------------------
# compare with 2012
# ------------------------------------------------------------------------------

# census 2010
y <- acs112 %>%
  filter(str_sub(field, 1, 4) == 'HC01') %>%
  rename(field12 = field)
cw3 <- full_join(cw2, y)
cw3 <- cw3[c(1:3, 12, 4:10, 11)]
