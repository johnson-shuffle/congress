#---------------------------------------------------------------------------------
# create list of districts
#---------------------------------------------------------------------------------
db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

# inputs
in1 <- tbl(db, 'voteview_memb') %>% collect()
in2 <- tbl(db, 'states') %>% collect()

# get unique districts from 2004/2009 (congress = 108/111) and add fips
dis  <- in1 %>%
  filter(chamber == 'House') %>%
  filter(congress %in% c(108, 111)) %>%
  select(congress, state_abbrev, district_code) %>%
  distinct() %>%
  left_join(states, by = c('state_abbrev' = 'statecode'))
  

#---------------------------------------------------------------------------------
# Loop over states and recover district emissions from carma.org
#---------------------------------------------------------------------------------
co2.districts<-NULL # Final object

for (i in 1:nrow(districts)) {
  
  # Read from web
  theurl <- paste("http://carma.org/region/detail/9999",districts[i,1],districts[i,2],sep="")
  tables <- readHTMLTable(theurl)
  
  # Get the correct table
  df<-data.frame(tables[[2]])
  
  # Clean up table
  df<-df[,1:2]
  names(df)<-c("year","co2.prod")
  df$year<-gsub(":","",df$year)
  df$fips<-as.numeric(districts[i,1])
  df$district<-as.numeric(districts[i,2])
  df$district[df$district==0]<-1
  
  # Convert CO2 to millions of tons
  df$co2.prod<-gsub(",","",df$co2.prod)
  df$co2.prod<-as.numeric(df$co2.prod)/1E6
  
  # Print
  print(paste(districts[i,1],"-",districts[i,2]," is done.",sep=""))
  
  # Bind to previous
  co2.districts<-rbind(co2.districts,df)
  
}

# Convert to MMt of carbon
co2.districts$carbon.prod<-co2.districts$co2.prod*12/44

# Reorder
co2.districts<-co2.districts[c("fips","district","year","co2.prod","carbon.prod")]

#---------------------------------------------------------------------------------
# Save
#---------------------------------------------------------------------------------
write.csv(co2.districts,paste(dcarma,"carma_co2.csv",sep=""),row.names=F)
