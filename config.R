# ============================================================================
# Central Configuration & Database Helpers
# ============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

# ----- paths ----------------------------------------------------------------

project_root <- if (requireNamespace("here", quietly = TRUE)) {
  here::here()
} else {
  getwd()
}

path_db  <- file.path(project_root, "congress.sqlite")
path_sql <- file.path(project_root, "sql")
path_raw <- file.path(project_root, "raw")

# ----- database helpers -----------------------------------------------------

get_db <- function(db_path = path_db) {
  conn <- dbConnect(SQLite(), db_path)
  dbExecute(conn, "PRAGMA foreign_keys = ON;")
  conn
}

execute_sql_file <- function(conn, file_path) {
  if (!file.exists(file_path)) {
    stop(paste("SQL file not found:", file_path))
  }
  lines <- readLines(file_path, warn = FALSE)
  clean_lines <- sub("--.*$", "", lines)
  sql_content <- paste(clean_lines, collapse = "\n")
  statements <- strsplit(sql_content, ";")[[1]]
  
  for (stmt in statements) {
    trimmed <- trimws(stmt)
    if (nchar(trimmed) > 0) {
      dbExecute(conn, trimmed)
    }
  }
}

init_table <- function(conn, table_name) {
  file_base <- gsub("_", "-", table_name)
  sql_path <- file.path(path_sql, paste0(file_base, ".sql"))
  if (!file.exists(sql_path)) {
    sql_path <- file.path(path_sql, paste0("_", file_base, ".sql"))
  }
  if (!file.exists(sql_path)) {
    sql_path <- file.path(path_sql, paste0(table_name, ".sql"))
  }
  execute_sql_file(conn, sql_path)
}

