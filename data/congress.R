# ==============================================================================
# Ingestion: Congressional Calendar Sessions
# Parses House and Senate legislative session calendars from raw/congress/
# and populates `congress_sessions` (and `congress` compatibility view).
# Covers 1st to 119th Congress (1789–present).
# ==============================================================================

source("config.R")
library(rvest)
library(xml2)
library(stringr)
library(lubridate)
library(dplyr)
library(DBI)


# ----- fetch-or-load-raw ------------------------------------------------------

raw_dir <- "raw/congress"
if (!dir.exists(raw_dir)) {
  dir.create(raw_dir, recursive = TRUE)
}

hou_file <- file.path(raw_dir, "house_sessions.html")
sen_file <- file.path(raw_dir, "senate_sessions.html")

# Fetch if not already staged locally
if (!file.exists(hou_file)) {
  hou_url <- "https://history.house.gov/Institution/Session-Dates/All/"
  tryCatch(
    download.file(hou_url, hou_file, quiet = TRUE, headers = c("User-Agent" = "Mozilla/5.0")),
    error = function(e) message("Could not fetch live House sessions; using fallback.")
  )
}

if (!file.exists(sen_file)) {
  sen_url <- "https://www.senate.gov/legislative/DatesofSessionsofCongress.htm"
  tryCatch(
    download.file(sen_url, sen_file, quiet = TRUE, headers = c("User-Agent" = "Mozilla/5.0")),
    error = function(e) message("Could not fetch live Senate sessions; using fallback.")
  )
}

clean_date <- function(x) {
  x <- gsub("([A-Za-z.]+)([0-9])", "\\1 \\2", x)
  x <- str_extract(x, "[A-Za-z.]+ +[0-9]+, +[0-9]{4}")
  x <- gsub("Sept[.]?", "Sep.", x)
  x <- gsub("June", "Jun.", x)
  x <- gsub("July", "Jul.", x)
  as.character(as.Date(parse_date_time(x, orders = c("b! d! Y!", "B! d! Y!"))))
}


# ----- house-sessions ---------------------------------------------------------

htm_hou <- read_html(hou_file)
t_hou <- html_table(htm_hou)[[1]]
names(t_hou) <- c("congress_raw", "session", "start_raw", "end_raw", "cal_days", "leg_days", "recesses")

# Fill down congress numbers
t_hou$congress <- as.integer(str_extract(t_hou$congress_raw, "^[0-9]+"))
for (i in 2:nrow(t_hou)) {
  if (is.na(t_hou$congress[i])) {
    t_hou$congress[i] <- t_hou$congress[i - 1]
  }
}

hou_df <- t_hou |>
  filter(!is.na(session) & !is.na(start_raw) & start_raw != "") |>
  mutate(
    chamber = "House",
    session = as.integer(session),
    session_start = clean_date(start_raw),
    session_end = clean_date(end_raw),
    calendar_days = as.integer(cal_days),
    legislative_days = as.integer(leg_days),
    recesses = ifelse(recesses == "", NA_character_, recesses)
  ) |>
  filter(!is.na(session_start)) |>
  group_by(congress) |>
  mutate(
    start_date = min(session_start, na.rm = TRUE),
    end_date = max(session_end, na.rm = TRUE)
  ) |>
  ungroup() |>
  select(
    congress, session, chamber, session_start, session_end,
    calendar_days, legislative_days, recesses, start_date, end_date
  )


# ----- senate-sessions --------------------------------------------------------

htm_sen <- read_html(sen_file)
t_sen <- html_node(htm_sen, "table")

# Replace <br> with newlines so sessions listed within one cell can be separated
invisible(xml_find_all(t_sen, ".//br") |> xml_replace(read_xml("<span>\n</span>")))

rows <- html_nodes(t_sen, "tr")
sen_records <- list()

for (i in 3:length(rows)) {
  tds <- html_nodes(rows[[i]], "td")
  if (length(tds) >= 4) {
    cong_num <- as.integer(str_extract(html_text(tds[[1]], trim = TRUE), "^[0-9]+"))
    sess_lines <- str_split(html_text(tds[[2]], trim = TRUE), "\n")[[1]] |> str_trim()
    sess_lines <- sess_lines[sess_lines != ""]
    start_lines <- str_split(html_text(tds[[3]], trim = TRUE), "\n")[[1]] |> str_trim()
    start_lines <- start_lines[start_lines != ""]
    end_lines <- str_split(html_text(tds[[4]], trim = TRUE), "\n")[[1]] |> str_trim()
    end_lines <- end_lines[end_lines != ""]

    n_sess <- max(length(sess_lines), length(start_lines))
    for (s_idx in seq_len(n_sess)) {
      s_num_str <- if (s_idx <= length(sess_lines)) sess_lines[s_idx] else as.character(s_idx)
      s_num <- suppressWarnings(as.integer(str_extract(s_num_str, "[0-9]+")))
      if (is.na(s_num)) s_num <- 1L

      s_date_str <- if (s_idx <= length(start_lines)) start_lines[s_idx] else NA_character_
      e_date_str <- if (s_idx <= length(end_lines)) end_lines[s_idx] else NA_character_

      sen_records[[length(sen_records) + 1]] <- tibble(
        congress = cong_num,
        session = s_num,
        chamber = "Senate",
        session_start = clean_date(s_date_str),
        session_end = clean_date(e_date_str),
        calendar_days = NA_integer_,
        legislative_days = NA_integer_,
        recesses = NA_character_
      )
    }
  }
}

sen_df <- bind_rows(sen_records) |>
  filter(!is.na(session_start)) |>
  group_by(congress) |>
  mutate(
    start_date = min(session_start, na.rm = TRUE),
    end_date = max(session_end, na.rm = TRUE)
  ) |>
  ungroup() |>
  select(
    congress, session, chamber, session_start, session_end,
    calendar_days, legislative_days, recesses, start_date, end_date
  )


# ----- combine-and-insert -----------------------------------------------------

all_sessions <- bind_rows(hou_df, sen_df) |>
  distinct(congress, session, chamber, session_start, .keep_all = TRUE) |>
  arrange(chamber, congress, session, session_start)

message("Total congressional session records: ", nrow(all_sessions))

db <- get_db()

init_table(db, "congress_sessions")

dbWriteTable(db, "congress_sessions", all_sessions, append = TRUE)

message("Successfully populated congress_sessions: ", dbGetQuery(db, "SELECT count(*) FROM congress_sessions")[[1]], " rows.")
message("Compatibility view 'congress' verified: ", dbGetQuery(db, "SELECT count(*) FROM congress")[[1]], " rows.")
