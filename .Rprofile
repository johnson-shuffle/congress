source('~/.Rprofile')

.First <- function() {
    
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
  
  db_old <<- DBI::dbConnect(
    RSQLite::SQLite(),
    "/Volumes/32GB_BRO/congress.sqlite"
  )

}
