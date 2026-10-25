# ==============================================================================
# Model: Hierarchical 2PL IRT Estimation of Environmental Ideology
# Fits structural model: theta_i ~ Normal(X_i * gamma, sigma_t)
# Data source: congress.sqlite (panel_*_annual + panel_*_vw)
# Model source: analysis/stan/hierarchical-rollcall.stan
# ==============================================================================

suppressPackageStartupMessages({
  library(rstan)
  library(posterior)
  library(bayesplot)
  library(readr)
  library(dplyr)
  library(tibble)
  library(DBI)
  library(RSQLite)
})

source("config.R")
source("analysis/helpers.R")

# Enable multicore execution and cache compiled models
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Parse command-line flags or default to House run
args <- commandArgs(trailingOnly = TRUE)
chamber <- if ("--senate" %in% args) "Senate" else "House"
is_test <- "--test" %in% args

iter   <- if (is_test) 50L else 1000L
warmup <- if (is_test) 25L else 500L
chains <- if (is_test) 1L else min(4L, parallel::detectCores())
cores  <- chains

message(sprintf("=== Running Hierarchical IRT Model for %s (Chains=%d, Iter=%d, Warmup=%d) ===",
                chamber, chains, iter, warmup))

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

# 1. Prepare data directly from SQLite master panels
prep <- prepare_hierarchical_data(db, chamber = chamber, max_year = 2012, min_votes = 20)

# 2. Compile and sample Stan model
stan_file <- "analysis/stan/hierarchical-rollcall.stan"
message(sprintf("Compiling and sampling from %s...", stan_file))

t_start <- Sys.time()

init_fun <- function(chain_id = 1) {
  list(
    theta_raw = rnorm(prep$N, 0, 0.2),
    alpha_raw = rnorm(prep$M, 0, 0.2),
    beta_raw  = rnorm(prep$M, 0, 0.2),
    gamma_raw = rnorm(prep$K, 0, 0.2)
  )
}

fit <- stan(
  file = stan_file,
  data = prep$data,
  pars = c("theta", "alpha", "beta", "gamma"),
  chains = chains,
  iter = iter,
  warmup = warmup,
  cores = cores,
  seed = 314159,
  init = init_fun,
  control = list(adapt_delta = 0.85, max_treedepth = 12)
)

t_elapsed <- round(difftime(Sys.time(), t_start, units = "mins"), 2)
message(sprintf("Sampling completed in %s minutes.", t_elapsed))

# 3. Extract MCMC draws and perform identification post-processing
message("Extracting and normalizing draws for latent scale identification...")
theta_draws <- as.matrix(fit, pars = "theta")
alpha_draws <- as.matrix(fit, pars = "alpha")
beta_draws  <- as.matrix(fit, pars = "beta")
gamma_draws <- as.matrix(fit, pars = "gamma")

norm_draws <- postprocess_irt_draws(
  theta_draws      = theta_draws,
  alpha_draws      = alpha_draws,
  beta_draws       = beta_draws,
  gamma_draws      = gamma_draws,
  feature_names    = prep$feature_names,
  positive_covar   = "party_r",
  legislator_party = prep$legislators$party_r
)

# 4. Generate tidy posterior summaries
message("Generating tidy posterior summaries...")

# Structural regression coefficients (gamma)
gamma_tidy <- summarize_draws_tidy(
  draws_mat  = norm_draws$gamma,
  labels     = prep$feature_names,
  param_type = "gamma"
)

# Latent ideal points (theta)
leg_labels <- paste(prep$legislators$icpsr_id,
                    if_else(prep$legislators$party_r == 1, "R", "D"),
                    sep = "-")
theta_tidy <- summarize_draws_tidy(
  draws_mat  = norm_draws$theta,
  labels     = leg_labels,
  param_type = "theta"
)

# 5. Display top structural determinants
message("\n=== Posterior Structural Coefficients (gamma) ===")
print(gamma_tidy |> select(label, mean, sd, q05, q50, q95), n = 35)

# 6. Save results to analysis/results/
results_dir <- file.path("analysis", "results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

suffix <- tolower(chamber)
gamma_out <- file.path(results_dir, sprintf("hierarchical-%s-gamma.csv", suffix))
theta_out <- file.path(results_dir, sprintf("hierarchical-%s-theta.csv", suffix))

write_csv(gamma_tidy, gamma_out)
write_csv(theta_tidy, theta_out)

message(sprintf("\nWrote results to:\n  - %s\n  - %s", gamma_out, theta_out))
