source('~/.Rprofile')

.First <- function() {
  
  library(DBI)
  library(RPostgreSQL)
  
  db <<- dbConnect(
    "PostgreSQL",
    host = "localhost",
    dbname = "congress", 
    user = "JRJ",
    password = ""
  )
  
  os <<- dbConnect(
    "PostgreSQL",
    host = "localhost",
    dbname = "opensecrets", 
    user = "JRJ",
    password = ""
  )

}
