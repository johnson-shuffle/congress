# ----- preample ----------------------------------------------------------


# ----- helper function ---------------------------------------------------

voteview_fun <- function(congress, type = NULL) {
  
  stopifnot(!is.null(type))
  
  url <- 'https://voteview.com/static/data/out/'
  pg1 <- str_c(url, type, '/', 'H', congress, '_', type, '.csv')
  pg2 <- str_c(url, type, '/', 'S', congress, '_', type, '.csv')
  
  rbind(read_csv(pg1, col_types = cols()), read_csv(pg2, col_types = cols()))

}


# ----- vote info, votes cast, and member info ----------------------------

voteview_info <- map(103:114, voteview_fun, type = 'rollcalls')
voteview_info <- do.call(rbind, voteview_info) 

voteview_cast <- map(103:114, voteview_fun, type = 'votes')
voteview_cast <- do.call(rbind, voteview_cast)

voteview_memb <- map(103:114, voteview_fun, type = 'members')
voteview_memb <- do.call(rbind, voteview_memb) %>% distinct()


# ----- tidy up -----------------------------------------------------------

voteview_memb %<>%
  rename(icpsr_code = state_icpsr, state_code = state_abbrev) %>%
  mutate(
    state_code = if_else(state_code == "USA", "US", state_code)
  )


# ----- add to database ---------------------------------------------------

dbWriteTable(db, "voteview_info", voteview_info, append = T, row.names = F)
dbWriteTable(db, "voteview_cast", voteview_cast, append = T, row.names = F)
dbWriteTable(db, "voteview_memb", voteview_memb, append = T, row.names = F)
