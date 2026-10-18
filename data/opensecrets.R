# ============================================================================
# OpenSecrets / Center for Responsive Politics (CRP) Campaign Finance ETL
# Ingests candidates, PAC committees, PAC-to-candidate contributions, and
# industry/category codes covering 1990–2016 election cycles.
# Sources: raw/opensecrets/codes_industries.csv, raw/opensecrets/CampaignFin*.zip
# Target tables: crp_categories, crp_candidates, crp_committees, crp_pacs
# ============================================================================

source("config.R")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
})

raw_os_dir <- file.path(path_raw, "opensecrets")
db <- get_db()

# ----- 1. Ingest crp_categories ---------------------------------------------

message("Ingesting crp_categories...")
execute_sql_file(db, file.path(path_sql, "tables", "crp-categories.sql"))

codes_path <- file.path(raw_os_dir, "codes_industries.csv")
if (file.exists(codes_path)) {
  categories <- read_csv(codes_path, show_col_types = FALSE) |>
    transmute(
      cat_code = trimws(as.character(code)),
      cat_name = trimws(as.character(name)),
      cat_group = trimws(as.character(group)),
      industry = trimws(as.character(industry)),
      sector_short = trimws(as.character(sector_short)),
      sector_long = trimws(as.character(sector_long)),
      cat_group_alt = trimws(as.character(group_alt))
    ) |>
    distinct(cat_code, .keep_all = TRUE)
  
  dbWithTransaction(db, {
    dbAppendTable(db, "crp_categories", categories)
  })
  message(sprintf("Loaded %d category records into 'crp_categories'.", nrow(categories)))
} else {
  warning("codes_industries.csv not found in raw/opensecrets/")
}

# ----- 2. Ingest crp_candidates, crp_committees, crp_pacs -------------------

execute_sql_file(db, file.path(path_sql, "tables", "crp-candidates.sql"))
execute_sql_file(db, file.path(path_sql, "tables", "crp-committees.sql"))
execute_sql_file(db, file.path(path_sql, "tables", "crp-pacs.sql"))

zip_files <- list.files(raw_os_dir, pattern = "^CampaignFin[0-9]{2}[.]zip$", full.names = TRUE) |>
  sort()

pacs_types <- cols(
  X1 = col_integer(),
  X2 = col_character(),
  X3 = col_character(),
  X4 = col_character(),
  X5 = col_double(),
  X6 = col_character(),
  X7 = col_character(),
  X8 = col_character(),
  X9 = col_character(),
  X10 = col_character()
)

os_locale <- locale(encoding = "ISO-8859-1")

message(sprintf("Processing %d election cycles from OpenSecrets zip files...", length(zip_files)))

total_cands <- 0L
total_cmtes <- 0L
total_pacs  <- 0L

for (z in zip_files) {
  t0 <- Sys.time()
  cyc_2d <- substr(basename(z), 12, 13)
  cyc <- ifelse(as.integer(cyc_2d) >= 90, 1900L + as.integer(cyc_2d), 2000L + as.integer(cyc_2d))
  
  # A. Candidates
  cmd_cands <- sprintf("unzip -p %s cands%s.txt", z, cyc_2d)
  cands <- suppressWarnings(
    read_delim(pipe(cmd_cands), delim = ",", quote = "|", col_names = FALSE, locale = os_locale, show_col_types = FALSE)
  ) |>
    transmute(
      cycle = as.integer(X1),
      fec_candidate_id = trimws(as.character(X2)),
      osid = trimws(as.character(X3)),
      candidate_name = trimws(as.character(X4)),
      party = trimws(as.character(X5)),
      distid_run = trimws(as.character(X6)),
      distid_cur = trimws(as.character(X7)),
      cand_cur = trimws(as.character(X8)),
      cand_cyc = trimws(as.character(X9)),
      incumbent_code = trimws(as.character(X10)),
      recip_code = trimws(as.character(X11)),
      no_pacs = trimws(as.character(X12))
    ) |>
    distinct(cycle, fec_candidate_id, .keep_all = TRUE)
  
  # B. Committees
  cmd_cmtes <- sprintf("unzip -p %s cmtes%s.txt", z, cyc_2d)
  cmtes <- suppressWarnings(
    read_delim(pipe(cmd_cmtes), delim = ",", quote = "|", col_names = FALSE, locale = os_locale, show_col_types = FALSE)
  ) |>
    transmute(
      cycle = as.integer(X1),
      cmte_id = trimws(as.character(X2)),
      pac_name = trimws(as.character(X3)),
      affiliate = trimws(as.character(X4)),
      parent_org = trimws(as.character(X5)),
      recip_id = trimws(as.character(X6)),
      recip_code = trimws(as.character(X7)),
      fec_candidate_id = trimws(as.character(X8)),
      party = trimws(as.character(X9)),
      prim_code = trimws(as.character(X10)),
      source = trimws(as.character(X11)),
      sensitive = trimws(as.character(X12)),
      foreign_connected = as.integer(X13),
      active = as.integer(X14)
    ) |>
    distinct(cycle, cmte_id, .keep_all = TRUE)
  
  # C. PAC Contributions
  cmd_pacs <- sprintf("unzip -p %s pacs%s.txt", z, cyc_2d)
  pacs <- suppressWarnings(
    read_delim(pipe(cmd_pacs), delim = ",", quote = "|", col_names = FALSE, col_types = pacs_types, locale = os_locale)
  ) |>
    transmute(
      cycle = as.integer(X1),
      transaction_id = trimws(as.character(X2)),
      cmte_id = trimws(as.character(X3)),
      osid = trimws(as.character(X4)),
      amount = as.numeric(X5),
      date = as.character(as.Date(X6, format = "%m/%d/%Y")),
      cat_code = trimws(as.character(X7)),
      trans_type = trimws(as.character(X8)),
      direct_indirect = trimws(as.character(X9)),
      fec_candidate_id = trimws(as.character(X10))
    )
  
  # Bulk insert within single transaction for the cycle
  dbWithTransaction(db, {
    dbAppendTable(db, "crp_candidates", cands)
    dbAppendTable(db, "crp_committees", cmtes)
    dbAppendTable(db, "crp_pacs", pacs)
  })
  
  t1 <- Sys.time()
  total_cands <- total_cands + nrow(cands)
  total_cmtes <- total_cmtes + nrow(cmtes)
  total_pacs  <- total_pacs + nrow(pacs)
  
  message(sprintf("  Cycle %d (%s): cands=%d, cmtes=%d, pacs=%d (%.2f s)",
                  cyc, cyc_2d, nrow(cands), nrow(cmtes), nrow(pacs),
                  as.numeric(t1 - t0, units = "secs")))
}

# ----- 3. Verification -------------------------------------------------------

cnt_categories <- dbGetQuery(db, "SELECT count(*) FROM crp_categories")[[1]]
cnt_candidates <- dbGetQuery(db, "SELECT count(*) FROM crp_candidates")[[1]]
cnt_committees <- dbGetQuery(db, "SELECT count(*) FROM crp_committees")[[1]]
cnt_pacs       <- dbGetQuery(db, "SELECT count(*) FROM crp_pacs")[[1]]

dbDisconnect(db)

message("\n============================================================")
message("OpenSecrets / CRP ETL Ingestion Complete:")
message(sprintf("  crp_categories:  %d rows", cnt_categories))
message(sprintf("  crp_candidates:  %d rows", cnt_candidates))
message(sprintf("  crp_committees:  %d rows", cnt_committees))
message(sprintf("  crp_pacs:        %d rows", cnt_pacs))
message("============================================================\n")
