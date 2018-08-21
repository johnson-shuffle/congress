z <- tbl(db_crp, sql(
  "SELECT * FROM indivs
  WHERE fecrecno='4122020121176554351'"
  )) %>% 
  collect()

duplicates(x, c('cycle', 'cmteid'))

db <- src_postgres('trumptown')
