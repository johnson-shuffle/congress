# ----- Preample ----------------------------------------------------------

library(yaml)


# ----- Historical Yaml ---------------------------------------------------

p <- "https://github.com/unitedstates/congress-legislators/raw/master/"
f <- "legislators-historical.yaml"
y <- yaml.load_file(file.path(p, f))

yaml_hist <- function(x, yaml_file = NULL, pb = NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  # name, id, bio, terms (ignore other_names and leadership_roles)
  nm <- grep('^name', names(yaml_file[[x]]))
  id <- grep('id', names(yaml_file[[x]]))
  bi <- grep('bio', names(yaml_file[[x]]))
  ts <- grep('terms', names(yaml_file[[x]]))
  
  # name, id, bio (collapse id's that are not unique)
  d1 <- map(yaml_file[[x]][c(nm, id, bi)], as_tibble)
  d1 <- do.call(cbind, d1)
  d1_fun <- function(z) {
    if_else(prod(z[1] == z) == 1, z[1], str_c(z, collapse = ','))
  }
  d1[1, ] <- apply(d1, 2, d1_fun)
  d1 <- d1[1, ]
  
  # terms
  d2 <- map(yaml_file[[x]][[ts]], as_tibble)
  d2 <- do.call(plyr::rbind.fill, d2)
  d2$id.govtrack <- d1$id.govtrack
  
  list(info = d1, terms = d2)
  
}
s_yaml_hist <- safely(yaml_hist)

pb <- progress_estimated(length(y))
hist <- map(1:length(y), s_yaml_hist, yaml_file = y, pb = pb)

gt1_info <- do.call(plyr::rbind.fill, map(hist, ~.x$result$info))
gt1_term <- do.call(plyr::rbind.fill, map(hist, ~.x$result$terms))
gt1_term %<>% select(-party_affiliations) %>% distinct()


# ----- Current Yaml ------------------------------------------------------

p <- "https://github.com/unitedstates/congress-legislators/raw/master/"
f <- "legislators-current.yaml"
y <- yaml.load_file(file.path(p, f))

yaml_curr <- function(x, yaml_file = NULL, pb = NULL) {
  
  if (!is.null(pb)) pb$tick()$print()
  
  # name, id, bio, terms (ignore other_names and leadership_roles)
  nm <- grep('^name', names(yaml_file[[x]]))
  id <- grep('id', names(yaml_file[[x]]))
  bi <- grep('bio', names(yaml_file[[x]]))
  ts <- grep('terms', names(yaml_file[[x]]))
  
  # name, id, bio (collapse id's that are not unique)
  d1 <- map(yaml_file[[x]][c(nm, id, bi)], as_tibble)
  d1 <- do.call(cbind, d1)
  d1_fun <- function(z) {
    if_else(prod(z[1] == z) == 1, z[1], str_c(z, collapse = ','))
  }
  d1[1, ] <- apply(d1, 2, d1_fun)
  d1 <- d1[1, ]
  
  # terms
  d2 <- map(yaml_file[[x]][[ts]], as_tibble)
  d2 <- do.call(plyr::rbind.fill, d2)
  d2$id.govtrack <- d1$id.govtrack
  
  list(info = d1, terms = d2)
  
}
s_yaml_curr <- safely(yaml_curr)

pb <- progress_estimated(length(y))
curr <- map(1:length(y), s_yaml_curr, yaml_file = y, pb = pb)

gt2_info <- do.call(plyr::rbind.fill, map(curr, ~.x$result$info))
gt2_term <- do.call(plyr::rbind.fill, map(curr, ~.x$result$terms))
gt2_term %<>% select(-party_affiliations) %>% distinct()


# ----- Tidy Up -----------------------------------------------------------

govtrack_info <- plyr::rbind.fill(gt2_info, gt1_info) %>% unique()

govtrack_term <- plyr::rbind.fill(gt2_term, gt2_term) %>% unique()

# add chamber
govtrack_term$chamber <- if_else(govtrack_term$type == 'rep', 'House', 'Senate')

# duplicates?
nrow(duplicates(govtrack_info, 'id.govtrack')) > 0 
nrow(duplicates(govtrack_term, c('id.govtrack', 'start', 'end'))) > 0


# ----- Add to Database ---------------------------------------------------

copy_to(db, govtrack_info, temporary = F, overwrite = T)
copy_to(db, govtrack_term, temporary = F, overwrite = T)
