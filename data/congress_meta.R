# ------------------------------------------------------------------------------
# census extraction function
# ------------------------------------------------------------------------------
congress_meta <- function(file) {
  
  # identify table number
  tbl <- str_extract(file, 'DP\\d+')
  tbl <- gsub('0', '', tbl)
  
  # read data
  dat <- read_csv(file, skip = 3, col_names = c('field', 'desc')) %>%
    mutate(
      field = str_c(field, tbl, sep = '_')
      )
  
  # parse out descriptives
  txt <- str_split(dat$desc, ';', n = 2, simplify = T)
  txt <- cbind(txt[, 1], str_split(txt[, 2], ' - ', simplify = T))
  txt <- cbind(dat$field, txt)
  
  # convert to data frame
  out <- as_data_frame(txt)
  
  # remove year reference for inflation, footnotes, and acronyms
  out <- out %>%
    mutate_all(~str_replace(.x, '\\(IN\\s\\d{4}\\sINFLATION', '(IN INFLATION')) %>%
    mutate_all(~str_replace(.x, '\\[\\d+\\]', '')) %>%
    mutate_all(~str_replace(.x, '\\([A-Z]+\\)', ''))
  
  # bind to fake tibble to get columns
  fak <- tibble(V1 = NA, V2 = NA, V3 = NA, V4 = NA, V5 = NA, V6 = NA, V7 = NA, V8 = NA)
  out <- plyr::rbind.fill(out, fak)
  out <- out[1:(nrow(out) - 1), ]
  
  # trim and replace NA's with blank
  out <- mutate_all(out, str_trim)
  out <- mutate_all(out, function(x) replace(x, is.na(x), ''))
  
  # create names
  nam <-c(
    'field',
    'units',
    'universe',
    str_c('category', 1:5)
    )
  names(out) <- nam
  
  # replace for cases with number/percent column
  out$universe <- gsub('Estimate; ', '', out$universe)
  out$universe <- gsub('Margin of Error; ', '', out$universe)
  
  # identify universe/category columns
  if (out$universe[1] == toupper(out$universe[1])) {
    names(out)[3:4] <- c('category1', 'universe')
  }
  
  # reorder
  out <- out[nam]
  
  # convert units
  out <- out %>%
    within({
      units[units == 'Number'] <- 'Estimate'
      units[grepl('MOE', field) & units == 'Estimate'] <- 'Margin of Error'
      units[grepl('MOE', field) & units == 'Percent'] <- 'Percent Margin of Error'
      units[units == 'Estimate Margin of Error'] <- 'Margin of Error'
    })
  
  # create id
  out$id <- apply(select(out, -field), 1, str_c, collapse = ' - ')
  out$id <- out$id
  out <- mutate_at(out, -1, tolower)
  
  # return
  out
  
}
