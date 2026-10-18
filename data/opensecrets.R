# ============================================================================
# OpenSecrets / Center for Responsive Politics (CRP) Campaign Finance ETL
# Ingests candidates, PAC committees, PAC-to-candidate contributions,
# PAC-to-PAC transactions, individual contributions, and industry/category
# codes covering 1990–2016 election cycles.
# Sources: raw/opensecrets/codes_industries.csv, raw/opensecrets/CampaignFin*.zip
# Target tables: crp_categories, crp_candidates, crp_committees, crp_pacs,
#                crp_pac_other, crp_indivs
# ============================================================================

source("config.R")

suppressPackageStartupMessages({
  library(readr)
  library(vroom)
  library(dplyr)
  library(stringr)
})

raw_os_dir <- file.path(path_raw, "opensecrets")
db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

# Helper to convert MM/DD/YYYY strings to ISO YYYY-MM-DD in vectorized form
to_iso_date <- function(x) {
  ifelse(
    !is.na(x) & nchar(x) == 10L,
    paste0(substr(x, 7L, 10L), "-", substr(x, 1L, 2L), "-", substr(x, 4L, 5L)),
    NA_character_
  )
}

# ----- 1. Ingest crp_categories ---------------------------------------------

existing_cats <- if (dbExistsTable(db, "crp_categories")) {
  dbGetQuery(db, "SELECT count(*) FROM crp_categories")[[1]]
} else 0L

if (existing_cats == 0L) {
  message("Ingesting crp_categories...")
  execute_sql_file(db, file.path(path_sql, "tables", "crp-categories.sql"))

  codes_path <- file.path(raw_os_dir, "codes_industries.csv")
  if (file.exists(codes_path)) {
    categories <- read_csv(codes_path, show_col_types = FALSE) |>
      transmute(
        cat_code = toupper(trimws(as.character(code))),
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
} else {
  message(sprintf("crp_categories already populated (%d rows); skipping.", existing_cats))
}

# ----- 2. Ingest crp_candidates, crp_committees, crp_pacs -------------------

zip_files <- list.files(raw_os_dir, pattern = "^CampaignFin[0-9]{2}[.]zip$", full.names = TRUE) |>
  sort()

os_locale <- locale(encoding = "ISO-8859-1")

existing_pacs <- if (dbExistsTable(db, "crp_pacs")) {
  dbGetQuery(db, "SELECT count(*) FROM crp_pacs")[[1]]
} else 0L

if (existing_pacs == 0L) {
  execute_sql_file(db, file.path(path_sql, "tables", "crp-candidates.sql"))
  execute_sql_file(db, file.path(path_sql, "tables", "crp-committees.sql"))
  execute_sql_file(db, file.path(path_sql, "tables", "crp-pacs.sql"))

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

  message(sprintf("Processing cands, cmtes, pacs across %d cycles...", length(zip_files)))

  for (z in zip_files) {
    t0 <- Sys.time()
    cyc_2d <- substr(basename(z), 12, 13)
    cyc <- ifelse(as.integer(cyc_2d) >= 90L, 1900L + as.integer(cyc_2d), 2000L + as.integer(cyc_2d))

    # A. Candidates
    cmd_cands <- sprintf("unzip -p %s cands%s.txt", z, cyc_2d)
    cands <- suppressWarnings(
      vroom(pipe(cmd_cands), delim = ",", quote = "|", col_names = FALSE, trim_ws = TRUE, locale = os_locale, show_col_types = FALSE)
    ) |>
      transmute(
        cycle = as.integer(X1),
        fec_candidate_id = as.character(X2),
        osid = as.character(X3),
        candidate_name = as.character(X4),
        party = as.character(X5),
        distid_run = as.character(X6),
        distid_cur = as.character(X7),
        cand_cur = as.character(X8),
        cand_cyc = as.character(X9),
        incumbent_code = as.character(X10),
        recip_code = as.character(X11),
        no_pacs = as.character(X12)
      ) |>
      distinct(cycle, fec_candidate_id, .keep_all = TRUE)

    # B. Committees
    cmd_cmtes <- sprintf("unzip -p %s cmtes%s.txt", z, cyc_2d)
    cmtes <- suppressWarnings(
      vroom(pipe(cmd_cmtes), delim = ",", quote = "|", col_names = FALSE, trim_ws = TRUE, locale = os_locale, show_col_types = FALSE)
    ) |>
      transmute(
        cycle = as.integer(X1),
        cmte_id = as.character(X2),
        pac_name = as.character(X3),
        affiliate = as.character(X4),
        parent_org = as.character(X5),
        recip_id = as.character(X6),
        recip_code = as.character(X7),
        fec_candidate_id = as.character(X8),
        party = as.character(X9),
        prim_code = toupper(as.character(X10)),
        source = as.character(X11),
        sensitive = as.character(X12),
        foreign_connected = as.integer(X13),
        active = as.integer(X14)
      ) |>
      distinct(cycle, cmte_id, .keep_all = TRUE)

    # C. PAC Contributions
    cmd_pacs <- sprintf("unzip -p %s pacs%s.txt", z, cyc_2d)
    pacs <- suppressWarnings(
      vroom(pipe(cmd_pacs), delim = ",", quote = "|", col_names = FALSE, col_types = pacs_types, trim_ws = TRUE, locale = os_locale)
    ) |>
      transmute(
        cycle = as.integer(X1),
        transaction_id = as.character(X2),
        cmte_id = as.character(X3),
        osid = as.character(X4),
        amount = as.numeric(X5),
        date = to_iso_date(X6),
        cat_code = toupper(as.character(X7)),
        trans_type = as.character(X8),
        direct_indirect = as.character(X9),
        fec_candidate_id = as.character(X10)
      )

    dbWithTransaction(db, {
      dbAppendTable(db, "crp_candidates", cands)
      dbAppendTable(db, "crp_committees", cmtes)
      dbAppendTable(db, "crp_pacs", pacs)
    })

    t1 <- Sys.time()
    message(sprintf("  Cycle %d (%s): cands=%d, cmtes=%d, pacs=%d (%.2f s)",
                    cyc, cyc_2d, nrow(cands), nrow(cmtes), nrow(pacs),
                    as.numeric(t1 - t0, units = "secs")))
  }
} else {
  message(sprintf("crp_candidates, crp_committees, crp_pacs already populated (%d PAC rows); normalizing cat_code case.", existing_pacs))
  dbExecute(db, "UPDATE crp_pacs SET cat_code = UPPER(cat_code) WHERE cat_code GLOB '[a-z]*';")
}

# ----- 3. Ingest crp_pac_other ----------------------------------------------

existing_pac_other <- if (dbExistsTable(db, "crp_pac_other")) {
  dbGetQuery(db, "SELECT count(*) FROM crp_pac_other")[[1]]
} else 0L

if (existing_pac_other == 0L) {
  message(sprintf("Ingesting crp_pac_other across %d cycles...", length(zip_files)))
  execute_sql_file(db, file.path(path_sql, "tables", "crp-pac-other.sql"))

  # Drop secondary indexes during bulk insert, rebuild after
  dbExecute(db, "DROP INDEX IF EXISTS idx_crp_pac_other_cycle;")
  dbExecute(db, "DROP INDEX IF EXISTS idx_crp_pac_other_filer;")
  dbExecute(db, "DROP INDEX IF EXISTS idx_crp_pac_other_recip;")
  dbExecute(db, "DROP INDEX IF EXISTS idx_crp_pac_other_cat;")

  pac_other_types <- cols(
    .default = col_character(),
    X1 = col_integer(),
    X12 = col_double()
  )

  for (z in zip_files) {
    t0 <- Sys.time()
    cyc_2d <- substr(basename(z), 12, 13)
    cyc <- ifelse(as.integer(cyc_2d) >= 90L, 1900L + as.integer(cyc_2d), 2000L + as.integer(cyc_2d))

    cmd_po <- sprintf("unzip -p %s pac_other%s.txt", z, cyc_2d)
    po <- suppressWarnings(
      vroom(pipe(cmd_po), delim = ",", quote = "|", col_names = FALSE, col_types = pac_other_types, trim_ws = TRUE, locale = os_locale)
    ) |>
      transmute(
        cycle = as.integer(X1),
        transaction_id = X2,
        filer_id = X3,
        donor_cmte = X4,
        contrib_lend_trans = X5,
        city = X6,
        state_code = X7,
        zip_code = X8,
        fec_occ_emp = X9,
        prim_code = toupper(X10),
        date = to_iso_date(X11),
        amount = as.numeric(X12),
        recip_id = X13,
        party = X14,
        other_id = X15,
        recip_code = X16,
        recip_prim_code = toupper(X17),
        amend = X18,
        report = X19,
        prim_gen = X20,
        microfilm = X21,
        trans_type = X22,
        cat_code = toupper(X23),
        source = X24
      )

    dbWithTransaction(db, {
      dbAppendTable(db, "crp_pac_other", po)
    })

    t1 <- Sys.time()
    message(sprintf("  Cycle %d (%s): pac_other=%d (%.2f s)",
                    cyc, cyc_2d, nrow(po), as.numeric(t1 - t0, units = "secs")))
    rm(po); gc(verbose = FALSE)
  }

  message("Building indexes on crp_pac_other...")
  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_crp_pac_other_cycle ON crp_pac_other(cycle);")
  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_crp_pac_other_filer ON crp_pac_other(filer_id);")
  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_crp_pac_other_recip ON crp_pac_other(recip_id);")
  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_crp_pac_other_cat ON crp_pac_other(cat_code);")
} else {
  message(sprintf("crp_pac_other already populated (%d rows); skipping.", existing_pac_other))
}

# ----- 4. Ingest crp_indivs -------------------------------------------------

existing_indivs <- if (dbExistsTable(db, "crp_indivs")) {
  dbGetQuery(db, "SELECT count(*) FROM crp_indivs")[[1]]
} else 0L

if (existing_indivs == 0L) {
  message(sprintf("Ingesting crp_indivs across %d cycles...", length(zip_files)))
  execute_sql_file(db, file.path(path_sql, "tables", "crp-indivs.sql"))

  # Drop secondary indexes during bulk insert, rebuild after
  dbExecute(db, "DROP INDEX IF EXISTS idx_crp_indivs_cycle;")
  dbExecute(db, "DROP INDEX IF EXISTS idx_crp_indivs_recip;")
  dbExecute(db, "DROP INDEX IF EXISTS idx_crp_indivs_cat;")
  dbExecute(db, "DROP INDEX IF EXISTS idx_crp_indivs_cmte;")

  indivs_types <- cols(
    .default = col_character(),
    X1 = col_integer(),
    X10 = col_double()
  )

  for (z in zip_files) {
    t0 <- Sys.time()
    cyc_2d <- substr(basename(z), 12, 13)
    cyc <- ifelse(as.integer(cyc_2d) >= 90L, 1900L + as.integer(cyc_2d), 2000L + as.integer(cyc_2d))

    cmd_ind <- sprintf("unzip -p %s indivs%s.txt", z, cyc_2d)
    indivs <- suppressWarnings(
      vroom(pipe(cmd_ind), delim = ",", quote = "|", col_names = FALSE, col_types = indivs_types, trim_ws = TRUE, locale = os_locale)
    ) |>
      transmute(
        cycle = as.integer(X1),
        transaction_id = X2,
        contrib_id = X3,
        contrib_name = X4,
        recip_id = X5,
        org_name = X6,
        parent_org = X7,
        cat_code = toupper(X8),
        date = to_iso_date(X9),
        amount = as.numeric(X10),
        street = X11,
        city = X12,
        state_code = X13,
        zip_code = X14,
        recip_code = X15,
        trans_type = X16,
        cmte_id = X17,
        other_id = X18,
        gender = X19,
        microfilm = X20,
        occupation = X21,
        employer = X22,
        source = X23
      )

    dbWithTransaction(db, {
      dbAppendTable(db, "crp_indivs", indivs)
    })

    t1 <- Sys.time()
    message(sprintf("  Cycle %d (%s): indivs=%d (%.2f s)",
                    cyc, cyc_2d, nrow(indivs), as.numeric(t1 - t0, units = "secs")))
    rm(indivs); gc(verbose = FALSE)
  }

  message("Building indexes on crp_indivs...")
  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_crp_indivs_cycle ON crp_indivs(cycle);")
  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_crp_indivs_recip ON crp_indivs(recip_id);")
  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_crp_indivs_cat ON crp_indivs(cat_code);")
  dbExecute(db, "CREATE INDEX IF NOT EXISTS idx_crp_indivs_cmte ON crp_indivs(cmte_id);")
} else {
  message(sprintf("crp_indivs already populated (%d rows); skipping.", existing_indivs))
}

# ----- 5. Verification -------------------------------------------------------

cnt_categories <- dbGetQuery(db, "SELECT count(*) FROM crp_categories")[[1]]
cnt_candidates <- dbGetQuery(db, "SELECT count(*) FROM crp_candidates")[[1]]
cnt_committees <- dbGetQuery(db, "SELECT count(*) FROM crp_committees")[[1]]
cnt_pacs       <- dbGetQuery(db, "SELECT count(*) FROM crp_pacs")[[1]]
cnt_pac_other  <- dbGetQuery(db, "SELECT count(*) FROM crp_pac_other")[[1]]
cnt_indivs     <- dbGetQuery(db, "SELECT count(*) FROM crp_indivs")[[1]]

message("\n============================================================")
message("OpenSecrets / CRP ETL Ingestion Complete:")
message(sprintf("  crp_categories:  %d rows", cnt_categories))
message(sprintf("  crp_candidates:  %d rows", cnt_candidates))
message(sprintf("  crp_committees:  %d rows", cnt_committees))
message(sprintf("  crp_pacs:        %d rows", cnt_pacs))
message(sprintf("  crp_pac_other:   %d rows", cnt_pac_other))
message(sprintf("  crp_indivs:      %d rows", cnt_indivs))
message("============================================================\n")
