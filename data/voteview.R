# ------------------------------------------------------------------------------
# voteview function
# ------------------------------------------------------------------------------
voteview <- function(congress, chamber = NULL, type = NULL) {
  
  if (is.null(chamber) | is.null(type)) break
  
  url <- 'https://voteview.com/static/data/out/'
  url <- str_c(url, type, '/', chamber, congress, '_', type, '.csv')
  read_csv(url)
  
}

# ------------------------------------------------------------------------------
# house vote info & casts, and member data
# ------------------------------------------------------------------------------
hou_info <- map(103:114, voteview, chamber = 'H', type = 'rollcalls')
hou_info <- do.call(rbind, hou_info)

hou_cast <- map(103:114, voteview, chamber = 'H', type = 'votes')
hou_cast <- do.call(rbind, hou_cast)

hou_memb <- map(103:114, voteview, chamber = 'H', type = 'members')
hou_memb <- do.call(rbind, hou_memb)

# ------------------------------------------------------------------------------
# senate vote info & casts and member data
# ------------------------------------------------------------------------------
sen_info <- map(103:114, voteview, chamber = 'S', type = 'rollcalls')
sen_info <- do.call(rbind, sen_info)

sen_cast <- map(103:114, voteview, chamber = 'S', type = 'votes')
sen_cast <- do.call(rbind, sen_cast)

sen_memb <- map(103:114, voteview, chamber = 'S', type = 'members')
sen_memb <- do.call(rbind, sen_memb)

# ------------------------------------------------------------------------------
# add to database
# ------------------------------------------------------------------------------
db <- src_sqlite("~/GoogleDrive/Projects/congress/congress.db", create = F)

copy_to(db, hou_info, temporary = F)
copy_to(db, hou_cast, temporary = F)
copy_to(db, hou_memb, temporary = F)
copy_to(db, sen_info, temporary = F)
copy_to(db, sen_cast, temporary = F)
copy_to(db, sen_memb, temporary = F)
