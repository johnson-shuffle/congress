source('~/.Rprofile')

.First <- function() {
  
  library(DBI)
  library(RPostgres)
  
  db_old <<- DBI::dbConnect(
    RSQLite::SQLite(),
    "/Volumes/32GB_BRO/congress.sqlite"
  )
    
  db <<- RPostgres::dbConnect(
    RPostgres::Postgres(),
    host = "localhost",
    dbname = "congress", 
    user = "JRJ",
    password = ""
  )
  
  os <<- RPostgres::dbConnect(
    RPostgres::Postgres(),
    host = "localhost",
    dbname = "opensecrets", 
    user = "JRJ",
    password = ""
  )

}
