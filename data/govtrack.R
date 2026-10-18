# ============================================================================
# Ingest Official Legislator Rosters & Terms from @unitedstates YAML Sources
# Populates govtrack_info and govtrack_term tables in congress.sqlite
# ============================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(dplyr)
  library(tibble)
  library(DBI)
  library(RSQLite)
})

source("config.R")

# ----- 1. helpers -----------------------------------------------------------

get_val <- function(x) {
  if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[1])
}

get_int <- function(x) {
  if (is.null(x) || length(x) == 0) NA_integer_ else as.integer(x[1])
}

parse_legislators_yaml <- function(legis_list) {
  n <- length(legis_list)
  
  govtrack_id <- character(n)
  bioguide_id <- character(n)
  thomas_id <- character(n)
  lis_id <- character(n)
  opensecrets_id <- character(n)
  votesmart_id <- character(n)
  fec_id <- character(n)
  cspan_id <- character(n)
  wikipedia_id <- character(n)
  house_history_id <- character(n)
  ballotpedia_id <- character(n)
  maplight_id <- character(n)
  icpsr_id <- character(n)
  wikidata_id <- character(n)
  google_entity_id <- character(n)
  first_name <- character(n)
  last_name <- character(n)
  official_full_name <- character(n)
  middle_name <- character(n)
  nickname <- character(n)
  suffix <- character(n)
  gender <- character(n)
  birthday <- character(n)
  religion <- character(n)
  
  terms_list <- vector("list", n)
  
  for (i in seq_len(n)) {
    x <- legis_list[[i]]
    id <- x$id
    nm <- x$name
    bi <- x$bio
    
    govtrack_id[i] <- get_val(id$govtrack)
    bioguide_id[i] <- get_val(id$bioguide)
    thomas_id[i] <- get_val(id$thomas)
    lis_id[i] <- get_val(id$lis)
    opensecrets_id[i] <- get_val(id$opensecrets)
    votesmart_id[i] <- get_val(id$votesmart)
    fec_id[i] <- if (is.null(id$fec)) NA_character_ else paste(unlist(id$fec), collapse = ",")
    cspan_id[i] <- get_val(id$cspan)
    wikipedia_id[i] <- get_val(id$wikipedia)
    house_history_id[i] <- get_val(id$house_history)
    ballotpedia_id[i] <- get_val(id$ballotpedia)
    maplight_id[i] <- get_val(id$maplight)
    icpsr_id[i] <- get_val(id$icpsr)
    wikidata_id[i] <- get_val(id$wikidata)
    google_entity_id[i] <- get_val(id$google_entity_id)
    first_name[i] <- get_val(nm$first)
    last_name[i] <- get_val(nm$last)
    official_full_name[i] <- get_val(nm$official_full)
    middle_name[i] <- get_val(nm$middle)
    nickname[i] <- get_val(nm$nickname)
    suffix[i] <- get_val(nm$suffix)
    gender[i] <- get_val(bi$gender)
    birthday[i] <- get_val(bi$birthday)
    religion[i] <- get_val(bi$religion)
    
    terms <- x$terms
    if (!is.null(terms) && length(terms) > 0) {
      nt <- length(terms)
      t_gid <- rep(govtrack_id[i], nt)
      t_ch <- vapply(terms, function(t) if (identical(t$type, "rep")) "House" else "Senate", character(1))
      t_start <- vapply(terms, function(t) get_val(t$start), character(1))
      t_end <- vapply(terms, function(t) get_val(t$end), character(1))
      t_st <- vapply(terms, function(t) get_val(t$state), character(1))
      t_dist <- vapply(terms, function(t) get_int(t$district), integer(1))
      t_pty <- vapply(terms, function(t) get_val(t$party), character(1))
      t_cls <- vapply(terms, function(t) get_int(t$class), integer(1))
      t_rnk <- vapply(terms, function(t) get_val(t$state_rank), character(1))
      t_cau <- vapply(terms, function(t) get_val(t$caucus), character(1))
      
      terms_list[[i]] <- tibble(
        govtrack_id = t_gid,
        chamber = t_ch,
        start_date = t_start,
        end_date = t_end,
        state_code = t_st,
        district_number = t_dist,
        party = t_pty,
        senate_class = t_cls,
        state_rank = t_rnk,
        caucus = t_cau
      )
    }
  }
  
  info_df <- tibble(
    govtrack_id, bioguide_id, thomas_id, lis_id, opensecrets_id,
    votesmart_id, fec_id, cspan_id, wikipedia_id, house_history_id,
    ballotpedia_id, maplight_id, icpsr_id, wikidata_id, google_entity_id,
    first_name, last_name, official_full_name, middle_name, nickname,
    suffix, gender, birthday, religion
  )
  
  list(
    info = info_df,
    terms = bind_rows(terms_list)
  )
}

# ----- 2. read raw YAML files -----------------------------------------------

yaml_dir <- file.path(path_raw, "govtrack")

curr_file <- file.path(yaml_dir, "legislators-current.yaml")
hist_file <- file.path(yaml_dir, "legislators-historical.yaml")

message("Reading raw YAML files...")
curr_raw <- yaml::read_yaml(curr_file)
hist_raw <- yaml::read_yaml(hist_file)

message("Parsing current legislators...")
parsed_curr <- parse_legislators_yaml(curr_raw)

message("Parsing historical legislators...")
parsed_hist <- parse_legislators_yaml(hist_raw)

# ----- 3. combine and deduplicate -------------------------------------------

govtrack_info <- bind_rows(parsed_curr$info, parsed_hist$info) |>
  filter(!is.na(govtrack_id), govtrack_id != "") |>
  distinct(govtrack_id, .keep_all = TRUE)

govtrack_term <- bind_rows(parsed_curr$terms, parsed_hist$terms) |>
  filter(!is.na(govtrack_id), govtrack_id != "", !is.na(start_date)) |>
  mutate(
    district_number = coalesce(district_number, 0L)
  ) |>
  distinct(govtrack_id, start_date, .keep_all = TRUE)

# ----- 4. write to database -------------------------------------------------

db <- get_db()

execute_sql_file(db, file.path(path_sql, "tables", "govtrack-info.sql"))
dbWriteTable(db, "govtrack_info", govtrack_info, append = TRUE)

execute_sql_file(db, file.path(path_sql, "tables", "govtrack-term.sql"))
dbWriteTable(db, "govtrack_term", govtrack_term, append = TRUE)

dbDisconnect(db)

message(sprintf(
  "Successfully loaded %d records into 'govtrack_info' and %d records into 'govtrack_term'.",
  nrow(govtrack_info),
  nrow(govtrack_term)
))
