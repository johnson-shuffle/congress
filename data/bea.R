#-------------------------------------------------------------------------------
# Preample
#-------------------------------------------------------------------------------
rm(list = ls())
source("~/GoogleDrive/Research/Congress/congress.Rprofile")

url1 <- "https://www.bea.gov/regional/zip/gsp/gsp_naics_all.zip"
url2 <- "https://www.bea.gov/regional/zip/gsp/gsp_sic_all.zip"

#-------------------------------------------------------------------------------
# State info
#-------------------------------------------------------------------------------
states <- read.csv(paste0(d$congress, "Data/us_state_info.csv"))

#-------------------------------------------------------------------------------
# NAICS 1997-2013
#-------------------------------------------------------------------------------
naics <- read.csv(paste0(d$bea, "raw_files/gsp_naics_all.csv"))

# Keep only states
naics <- subset(naics, GeoName %in% states$statedscr)

# Keep All-industry values
naics <- subset(naics, IndustryId == 1)

# Keep GSP values
naics <- subset(naics, ComponentId == 200)

# Get FIPS
naics <- merge(naics, states, by.x = "GeoName", by.y = "statedscr")

#-------------------------------------------------------------------------------
# SIC 1963-1996
#-------------------------------------------------------------------------------
sic <- read.csv(paste0(d$bea, "raw_files/gsp_sic_all.csv"))
  
# Keep only states
sic <- subset(sic, FIPS > 0 & FIPS < 57)

# Keep All-industry values
sic <- subset(sic, IndustryId == 1)

# Drop overlaping year
sic <- subset(sic,select=-c(gsp1997))

sic <- rename(sic, fips = FIPS)

#-------------------------------------------------------------------------------
# Merge NAICS and SIC
#-------------------------------------------------------------------------------
bea.gsp <- sic %>%
  merge(naics, by = "fips") %>%
  melt(id = "fips") %>%
  mutate(variable = as.character(variable)) %>%
  subset(substr(variable, 1, 3) == "gsp" | substr(variable, 1, 1) == "X") %>%
  mutate(
    variable = gsub("gsp", "", variable),
    variable = gsub("X", "", variable),
    variable = as.numeric(variable),
    value = as.numeric(value)*1E6
    ) %>%
  setNames(c("fips", "year", "gsp"))

#-------------------------------------------------------------------------------
# GSI and population data
#-------------------------------------------------------------------------------
gsi <- read.csv(paste0(d$bea, "/raw_files/SA1_1929_2014.csv"))

# Keep only states
gsi <- subset(gsi, GeoName %in% states$statedscr)

# Get FIPS
gsi <- merge(gsi, states, by.x = "GeoName", by.y = "statedscr")

# Separate data (income)
gsi.inc <- subset(gsi, LineCode == 1)

# Reshape
gsi.inc <- gsi.inc %>%
  melt(id = "fips") %>%
  subset(substr(variable, 1, 1) == "X") %>%
  mutate(
    variable = gsub("X", "", variable),
    variable = as.numeric(variable),
    value = as.numeric(value)*1000
    ) %>%
  setNames(c("fips", "year", "gsi"))

# Separate data (population)
gsi.pop <- subset(gsi, LineCode == 2)

# Reshape
gsi.pop <- gsi.pop %>%
  melt(id = "fips") %>%
  subset(substr(variable, 1, 1) == "X") %>%
  mutate(
    variable = gsub("X", "", variable),
    variable = as.numeric(variable),
    value = as.numeric(value)
  ) %>%
  setNames(c("fips", "year", "pop.bea"))

#-------------------------------------------------------------------------------
# Merge everything together
#-------------------------------------------------------------------------------
bea.all <- gsi.inc %>%
  merge(gsi.pop, by = c("fips", "year")) %>%
  merge(bea.gsp, by = c("fips", "year"), all.x = T, all.y = T) %>%
  unique

#-------------------------------------------------------------------------------
# Convert to real values
#-------------------------------------------------------------------------------
load(paste0(d$bls, "bls_cpi.Rda"))

bea.income <- merge(bea.all, cpi, by = "year", all.x = T, all.y = F) %>%
  mutate(
    rgsp = gsp / (cpi/100),
    rgsi = gsi / (cpi/100)
    ) %>%
  unique %>%
  select(-cpi)

#-------------------------------------------------------------------------------
# Save
#-------------------------------------------------------------------------------
save(bea.income, file = paste0(d$bea,"bea_income.Rda"))
