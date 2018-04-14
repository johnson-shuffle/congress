source('~/.Rprofile')

.First <- function() {
  load_tidy()
  db <<- src_sqlite("~/GitHub/congress/congress.sqlite", create = F)
  db_gis <<- src_sqlite("~/GitHub/congress/congress_gis.sqlite", create = F)
  db_crp <<- src_sqlite("~/GitHub/congress/opensecrets.sqlite", create = F)
}


