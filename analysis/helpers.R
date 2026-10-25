# ==============================================================================
# Bayesian Modeling Helpers: Data Preparation, Identification & Tidy Extraction
# Modern helper functions supporting Stan roll-call estimation models in analysis/
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(DBI)
  library(RSQLite)
})

# ----- 1. Gelman 2-SD Covariate Rescaling -------------------------------------

#' Rescale numeric vector to Gelman standard (mean 0, sd 0.5)
#' Binary 0/1 variables are centered at mean without scaling.
#'
#' @param x Numeric vector
#' @param binary_inputs Character flag ("0/1" or "center")
#' @return Rescaled numeric vector
rescale_2sd <- function(x, binary_inputs = "0/1") {
  u <- unique(na.omit(x))
  is_binary <- length(u) == 2 && all(u %in% c(0, 1))
  
  if (is_binary && binary_inputs == "0/1") {
    return(x - mean(x, na.rm = TRUE))
  }
  
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / (2 * s)
}

# ----- 2. Database Data Extraction & IRT Data Preparation ---------------------

#' Prepare hierarchical roll-call data list for Stan from congress.sqlite
#'
#' @param db SQLite database connection
#' @param chamber Character ("House" or "Senate")
#' @param max_year Maximum calendar year to include (default 2012)
#' @param min_votes Minimum votes cast per legislator threshold (default 20)
#' @return List containing dataForStan, metadata, and crosswalk lookups
prepare_hierarchical_data <- function(db, chamber = "House", max_year = 2012, min_votes = 20) {
  table_annual <- if (chamber == "House") "panel_house_annual" else "panel_senate_annual"
  view_votes   <- if (chamber == "House") "panel_house_vw" else "panel_senate_vw"

  # 1. Query voting records
  message(sprintf("Extracting %s environmental votes from %s (<= %d)...", chamber, view_votes, max_year))
  votes_df <- dbGetQuery(
    db,
    sprintf("
      SELECT 
        rollcall,
        icpsr_id,
        year,
        congress,
        vote_lcv,
        party_r
      FROM %s
      WHERE year <= %d
        AND vote_lcv IS NOT NULL
        AND vote != 9
    ", view_votes, max_year)
  ) |>
    as_tibble()

  # Filter legislators with minimum vote threshold
  leg_vote_counts <- votes_df |>
    count(icpsr_id, name = "vote_count") |>
    filter(vote_count >= min_votes)

  votes_df <- votes_df |>
    semi_join(leg_vote_counts, by = "icpsr_id")

  # Build integer index mappings
  leg_index_df <- votes_df |>
    distinct(icpsr_id, party_r) |>
    arrange(icpsr_id) |>
    mutate(leg_idx = row_number())

  rc_index_df <- votes_df |>
    distinct(rollcall) |>
    arrange(rollcall) |>
    mutate(rc_idx = row_number())

  votes_indexed <- votes_df |>
    inner_join(leg_index_df |> select(icpsr_id, leg_idx), by = "icpsr_id") |>
    inner_join(rc_index_df |> select(rollcall, rc_idx), by = "rollcall")

  # 2. Query annual member features and compute member-level averages
  message(sprintf("Extracting %s annual covariates from %s...", chamber, table_annual))
  covar_cols <- c(
    "party_r", "age", "gender_female",
    "d_leader_pacs", "r_leader_pacs", "d_cmte", "r_cmte",
    "agribusiness", "energy", "environment", "transport",
    "pop_65", "black", "hispanic",
    "highschool", "bachelor", "empl_total", "avg_unemp",
    "cl_total", "ro_total",
    paste0("census_division", 2:9)
  )

  ann_query <- sprintf(
    "SELECT icpsr_id, year, %s FROM %s WHERE year <= %d",
    paste(covar_cols, collapse = ", "),
    table_annual,
    max_year
  )
  ann_df <- dbGetQuery(db, ann_query) |> as_tibble()

  # Average across years per legislator
  member_covars <- ann_df |>
    semi_join(leg_vote_counts, by = "icpsr_id") |>
    group_by(icpsr_id) |>
    summarise(across(all_of(covar_cols), ~ mean(., na.rm = TRUE)), .groups = "drop") |>
    inner_join(leg_index_df |> select(icpsr_id, leg_idx), by = "icpsr_id") |>
    arrange(leg_idx)

  # Rescale continuous covariates using Gelman 2-SD
  rescale_vars <- c(
    "age", "d_leader_pacs", "r_leader_pacs", "d_cmte", "r_cmte",
    "agribusiness", "energy", "environment", "transport",
    "pop_65", "black", "hispanic",
    "highschool", "bachelor", "empl_total", "avg_unemp",
    "cl_total", "ro_total"
  )
  for (v in rescale_vars) {
    if (v %in% names(member_covars)) {
      member_covars[[v]] <- rescale_2sd(member_covars[[v]])
    }
  }

  # Build interaction terms matching legacy specifications
  member_covars <- member_covars |>
    mutate(
      intercept = 1.0,
      d_age = (1 - party_r) * age,
      r_age = party_r * age,
      d_female = (1 - party_r) * gender_female,
      r_female = party_r * gender_female
    )

  feature_names <- c(
    "intercept",
    paste0("census_division", 2:9),
    "party_r",
    "d_age", "r_age",
    "d_female", "r_female",
    "d_leader_pacs", "r_leader_pacs",
    "d_cmte", "r_cmte",
    "agribusiness", "energy", "environment", "transport",
    "pop_65", "black", "hispanic",
    "highschool", "bachelor", "empl_total", "avg_unemp",
    "cl_total", "ro_total"
  )

  X <- as.matrix(member_covars[, feature_names])

  N <- nrow(member_covars)
  M <- nrow(rc_index_df)
  K <- ncol(X)
  R <- nrow(votes_indexed)

  # Prior parameters
  mu_g <- rep(0.0, K)
  names(mu_g) <- feature_names
  if ("intercept" %in% names(mu_g)) mu_g["intercept"] <- -1.0
  if ("party_r" %in% names(mu_g))   mu_g["party_r"]   <- 2.0

  sigma_g <- rep(5.0, K)
  names(sigma_g) <- feature_names
  if ("intercept" %in% names(sigma_g)) sigma_g["intercept"] <- 1.0
  if ("party_r" %in% names(sigma_g))   sigma_g["party_r"]   <- 1.0

  dataForStan <- list(
    N = N,
    M = M,
    K = K,
    R = R,
    id = as.integer(votes_indexed$leg_idx),
    rc = as.integer(votes_indexed$rc_idx),
    y  = as.integer(votes_indexed$vote_lcv),
    x  = X,
    sigma_t = 1.0,
    mu_a    = rep(0.0, M),
    sigma_a = 5.0,
    mu_b    = rep(0.0, M),
    sigma_b = 5.0,
    mu_g    = as.numeric(mu_g),
    sigma_g = as.numeric(sigma_g)
  )

  list(
    data = dataForStan,
    legislators = leg_index_df,
    rollcalls = rc_index_df,
    feature_names = feature_names,
    N = N, M = M, K = K, R = R
  )
}

# ----- 3. MCMC Identification & Latent Post-Processing -----------------------

#' Post-normalize IRT ideal point draws for identification
#' Standardizes theta_s ~ N(0, 1) across draws and rescales alpha, beta, gamma accordingly.
#'
#' @param theta_draws Matrix of draws (S x N) for ideal points
#' @param alpha_draws Matrix of draws (S x M) for cutpoints
#' @param beta_draws Matrix of draws (S x M) for discriminations
#' @param gamma_draws Matrix of draws (S x K) for structural covariates
#' @param feature_names Vector of covariate names
#' @return List of post-processed draws matrices
postprocess_irt_draws <- function(theta_draws, alpha_draws, beta_draws, gamma_draws, feature_names) {
  S <- nrow(theta_draws)
  N <- ncol(theta_draws)
  M <- ncol(alpha_draws)
  K <- ncol(gamma_draws)

  theta_norm <- matrix(0.0, nrow = S, ncol = N)
  alpha_norm <- matrix(0.0, nrow = S, ncol = M)
  beta_norm  <- matrix(0.0, nrow = S, ncol = M)
  gamma_norm <- matrix(0.0, nrow = S, ncol = K)

  intercept_col <- which(feature_names %in% c("intercept", "(Intercept)"))

  for (s in 1:S) {
    xbar <- mean(theta_draws[s, ])
    sbar <- sd(theta_draws[s, ])
    if (is.na(sbar) || sbar == 0) sbar <- 1.0

    # Rescale latent ideal points
    theta_norm[s, ] <- (theta_draws[s, ] - xbar) / sbar

    # Rescale discrimination and cutpoints
    beta_norm[s, ]  <- beta_draws[s, ] * sbar
    alpha_norm[s, ] <- alpha_draws[s, ] - xbar * beta_draws[s, ]

    # Rescale structural coefficients
    gamma_norm[s, ] <- gamma_draws[s, ] / sbar
    if (length(intercept_col) > 0) {
      gamma_norm[s, intercept_col] <- (gamma_draws[s, intercept_col] - xbar) / sbar
    }
  }

  list(
    theta = theta_norm,
    alpha = alpha_norm,
    beta  = beta_norm,
    gamma = gamma_norm
  )
}

# ----- 4. Modern Tidy Summaries ----------------------------------------------

#' Summarize MCMC draws matrix into tidy tibble with credible intervals
#'
#' @param draws_mat Matrix of draws (S x P)
#' @param labels Vector of length P giving parameter names/identifiers
#' @param param_type Character string identifying the parameter type (e.g. "gamma", "theta")
#' @return Tibble of posterior summaries
summarize_draws_tidy <- function(draws_mat, labels, param_type = "param") {
  tibble(
    parameter_type = param_type,
    label          = as.character(labels),
    mean           = apply(draws_mat, 2, mean),
    sd             = apply(draws_mat, 2, sd),
    q05            = apply(draws_mat, 2, quantile, probs = 0.05),
    q10            = apply(draws_mat, 2, quantile, probs = 0.10),
    q50            = apply(draws_mat, 2, median),
    q90            = apply(draws_mat, 2, quantile, probs = 0.90),
    q95            = apply(draws_mat, 2, quantile, probs = 0.95)
  )
}
