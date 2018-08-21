source('~/.Rprofile')

.First <- function() {
  library(stats)
  load_tidy()
  db     <<- src_sqlite("~/Projects/congress/congress.sqlite", create = F)
  db_gis <<- src_sqlite("~/Projects/congress/congress_gis.sqlite", create = F)
  db_crp <<- src_postgres('opensecrets')
}
