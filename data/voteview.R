# ----- Preample ----------------------------------------------------------


# ----- Voteview Function -------------------------------------------------

voteview_fun <- function(congress, type = NULL) {
  
  stopifnot(!is.null(type))
  
  pag <- 'https://voteview.com/static/data/out/'
  pag1 <- str_c(url, type, '/', 'H', congress, '_', type, '.csv')
  pag2 <- str_c(url, type, '/', 'S', congress, '_', type, '.csv')
  
  rbind(read_csv(pag1, col_types = cols()), read_csv(pag2, col_types = cols()))

}


# ----- Vote Info, Votes Cast, and Member Info ----------------------------

voteview_info <- map(103:114, voteview_fun, type = 'rollcalls')
voteview_info <- do.call(rbind, voteview_info) 

voteview_cast <- map(103:114, voteview_fun, type = 'votes')
voteview_cast <- do.call(rbind, voteview_cast)

voteview_memb <- map(103:114, voteview_fun, type = 'members')
voteview_memb <- do.call(rbind, voteview_memb) %>% distinct()


# ----- Tidy Up -----------------------------------------------------------

# change classes
voteview_info %<>% mutate_if(is.integer, as.numeric)
voteview_cast %<>% mutate_if(is.integer, as.numeric)
voteview_memb %<>% mutate_if(is.integer, as.numeric)


# ----- Add to Database ---------------------------------------------------

copy_to(db, voteview_info, temporary = F, overwrite = T)
copy_to(db, voteview_cast, temporary = F, overwrite = T)
copy_to(db, voteview_memb, temporary = F, overwrite = T)
