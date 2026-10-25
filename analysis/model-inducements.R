# ==============================================================================
# Model: Campaign Finance Inducements Estimation
# Evaluates direct effects of campaign contributions on environmental roll calls
# beyond baseline spatial ideology theta_i.
# Data source: congress.sqlite (panel_*_annual + panel_*_vw)
# Model source: analysis/stan/inducements-rollcall.stan
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

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

args <- commandArgs(trailingOnly = TRUE)
chamber <- if ("--senate" %in% args) "Senate" else "House"
is_test <- "--test" %in% args

iter   <- if (is_test) 50L else 1000L
warmup <- if (is_test) 25L else 500L
chains <- if (is_test) 1L else min(4L, parallel::detectCores())
cores  <- chains

message(sprintf("=== Running Inducements Model for %s (Chains=%d, Iter=%d, Warmup=%d) ===",
                chamber, chains, iter, warmup))

db <- get_db()
on.exit(dbDisconnect(db), add = TRUE)

view_votes <- if (chamber == "House") "panel_house_vw" else "panel_senate_vw"

# 1. Query vote-level data including contributions and voting decision
message(sprintf("Extracting vote-level data and contribution features from %s...", view_votes))
votes_raw <- dbGetQuery(
  db,
  sprintf("
    SELECT 
      rollcall,
      icpsr_id,
      year,
      congress,
      party_r,
      vote_lcv,
      agribusiness,
      energy,
      environment,
      transport,
      d_cand_cmte,
      d_leader_pacs,
      r_cand_cmte,
      r_leader_pacs
    FROM %s
    WHERE year <= 2012
      AND vote_lcv IS NOT NULL
      AND vote != 9
  ", view_votes)
) |>
  as_tibble()

# Filter minimum votes per legislator
leg_counts <- votes_raw |> count(icpsr_id, name = "nvotes") |> filter(nvotes >= 20)
votes_raw <- votes_raw |> semi_join(leg_counts, by = "icpsr_id")

# Indices
leg_map <- votes_raw |> distinct(icpsr_id, party_r) |> arrange(icpsr_id) |> mutate(leg_idx = row_number())
rc_map  <- votes_raw |> distinct(rollcall) |> arrange(rollcall) |> mutate(rc_idx = row_number())

votes_indexed <- votes_raw |>
  inner_join(leg_map |> select(icpsr_id, leg_idx), by = "icpsr_id") |>
  inner_join(rc_map |> select(rollcall, rc_idx), by = "rollcall")

# Rescale contribution features
contrib_cols <- c("agribusiness", "energy", "environment", "transport",
                  "d_cand_cmte", "d_leader_pacs", "r_cand_cmte", "r_leader_pacs")

for (c_name in contrib_cols) {
  votes_indexed[[c_name]] <- rescale_2sd(votes_indexed[[c_name]])
}

# Construct party-interacted contribution features
votes_indexed <- votes_indexed |>
  mutate(
    party_d = 1.0 - party_r,
    agr_d = agribusiness * party_d,
    ene_d = energy * party_d,
    env_d = environment * party_d,
    tra_d = transport * party_d,
    cc_d  = d_cand_cmte * party_d,
    lp_d  = d_leader_pacs * party_d,
    agr_r = agribusiness * party_r,
    ene_r = energy * party_r,
    env_r = environment * party_r,
    tra_r = transport * party_r,
    cc_r  = r_cand_cmte * party_r,
    lp_r  = r_leader_pacs * party_r
  )

feature_cols <- c(
  "agr_d", "ene_d", "env_d", "tra_d", "cc_d", "lp_d",
  "agr_r", "ene_r", "env_r", "tra_r", "cc_r", "lp_r"
)

X_induce <- as.matrix(votes_indexed[, feature_cols])

N <- nrow(leg_map)
M <- nrow(rc_map)
K <- length(feature_cols)
R <- nrow(votes_indexed)
L <- 1L # Single overall inducement effect category (can be expanded to industry/topic subsets)

# Split baseline vs inducement votes (all votes modeled with contribution interactions)
rl <- integer(0)
rn <- seq_len(R)
R_L <- length(rl)
R_N <- length(rn)

# Baseline party priors for initial theta
mu_t <- if_else(leg_map$party_r == 1, 1.0, -1.0)

dataForStan <- list(
  N = N,
  M = M,
  K = K,
  R = R,
  L = L,
  R_L = if (R_L == 0) 1L else R_L,
  R_N = R_N,
  id = as.integer(votes_indexed$leg_idx),
  rc = as.integer(votes_indexed$rc_idx),
  nl = rep(1L, R),
  rl = if (R_L == 0) array(1L, dim = 1) else as.array(as.integer(rl)),
  rn = as.array(as.integer(rn)),
  y  = as.integer(votes_indexed$vote_lcv),
  x  = X_induce,
  mu_t = as.numeric(mu_t),
  sigma_t = 1.0,
  mu_a = rep(0.0, M),
  sigma_a = 5.0,
  mu_b = rep(0.0, M),
  sigma_b = 5.0,
  mu_g = rep(0.0, K),
  sigma_g = 2.0
)

# 2. Compile and sample Stan model
stan_file <- "analysis/stan/inducements-rollcall.stan"
message(sprintf("Compiling and sampling from %s...", stan_file))

# Matching legacy initsForStan: starts theta_raw aligned with party prior
inits_for_stan <- function(chain_id = 1) {
  set.seed(chain_id * 1000 + 42)
  list(
    theta_raw = sapply(mu_t, function(p) rnorm(1, p, 1.0)),
    alpha_raw = rnorm(M, 0, 1.0),
    beta_raw  = rnorm(M, 0, 1.0),
    gamma_raw = array(rnorm(L * K, 0, 1.0), dim = c(L, K))
  )
}

t_start <- Sys.time()

fit <- stan(
  file = stan_file,
  data = dataForStan,
  pars = c("theta", "alpha", "beta", "gamma"),
  chains = chains,
  iter = iter,
  warmup = warmup,
  cores = cores,
  seed = 314159,
  init = lapply(1:chains, inits_for_stan),
  control = list(adapt_delta = 0.85, max_treedepth = 12)
)

t_elapsed <- round(difftime(Sys.time(), t_start, units = "mins"), 2)
message(sprintf("Sampling completed in %s minutes.", t_elapsed))

# 3. Extract MCMC draws and summarize inducement effects
message("Extracting inducement effect parameters (gamma)...")

# Gamma has shape [S, L, K]
gamma_draws <- as.matrix(fit, pars = "gamma")

# Extract colnames for gamma[1, k]
gamma_tidy <- summarize_draws_tidy(
  draws_mat  = gamma_draws,
  labels     = feature_cols,
  param_type = "inducement_gamma"
)

message("\n=== Direct Campaign Inducement Effects (gamma) ===")
print(gamma_tidy |> select(label, mean, sd, q05, q50, q95), n = 20)

# Save results
results_dir <- file.path("analysis", "results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

suffix <- tolower(chamber)
out_file <- file.path(results_dir, sprintf("inducements-%s-gamma.csv", suffix))
write_csv(gamma_tidy, out_file)

message(sprintf("\nWrote results to: %s", out_file))
