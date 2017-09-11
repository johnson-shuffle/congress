# ------------------------------------------------------------------------------
# voteview function
# ------------------------------------------------------------------------------
voteview_fun <- function(congress, type = NULL) {
  
  stopifnot(!is.null(type))
  
  url <- 'https://voteview.com/static/data/out/'
  url1 <- str_c(url, type, '/', 'H', congress, '_', type, '.csv')
  url2 <- str_c(url, type, '/', 'S', congress, '_', type, '.csv')
  
  rbind(read_csv(url1, col_types = cols()), read_csv(url2, col_types = cols()))

}

# ------------------------------------------------------------------------------
# house vote info & casts, and member data
# ------------------------------------------------------------------------------
voteview_info <- map(103:114, voteview_fun, type = 'rollcalls')
voteview_info <- do.call(rbind, voteview_info) 

voteview_cast <- map(103:114, voteview_fun, type = 'votes')
voteview_cast <- do.call(rbind, voteview_cast)

voteview_memb <- map(103:114, voteview_fun, type = 'members')
voteview_memb <- do.call(rbind, voteview_memb) %>% distinct()

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

copy_to(db, voteview_info, temporary = F)
copy_to(db, voteview_cast, temporary = F)
copy_to(db, voteview_memb, temporary = F)
