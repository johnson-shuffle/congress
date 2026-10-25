// ============================================================================
// Campaign Finance Inducements Roll-Call Model
// Estimates whether campaign contributions directly shift voting behavior
// on specific bills beyond a legislator's baseline spatial ideology theta_i.
// Non-inducement link: xb_r = beta_m * theta_i - alpha_m
// Inducement link:     xb_r = beta_m * theta_i - alpha_m + x_r * gamma_l
// ============================================================================

data {
  int<lower=1> N;                           // Number of legislators
  int<lower=1> M;                           // Number of roll calls
  int<lower=1> K;                           // Number of inducement features (contributions)
  int<lower=1> R;                           // Total observed votes
  int<lower=1> L;                           // Number of vote/industry categories
  int<lower=1> R_L;                         // Number of baseline (non-inducement) votes
  int<lower=1> R_N;                         // Number of inducement votes
  
  array[R] int<lower=1, upper=N> id;        // Legislator index for each vote
  array[R] int<lower=1, upper=M> rc;        // Roll call index for each vote
  array[R] int<lower=0, upper=L> nl;        // Inducement category index (0 for baseline)
  array[R_L] int<lower=1, upper=R> rl;      // Row indices of baseline votes
  array[R_N] int<lower=1, upper=R> rn;      // Row indices of inducement votes
  array[R] int<lower=0, upper=1> y;         // Vote outcome (1 = pro-environmental, 0 = against)
  matrix[R, K] x;                           // Vote-level campaign contributions
  
  // Prior hyperparameters
  vector[N] mu_t;                           // Prior mean for baseline ideal points
  real<lower=0> sigma_t;                    // Prior scale for ideal points
  vector[M] mu_a;                           // Prior mean for cutpoints
  real<lower=0> sigma_a;                    // Prior scale for cutpoints
  vector[M] mu_b;                           // Prior mean for discrimination
  real<lower=0> sigma_b;                    // Prior scale for discrimination
  vector[K] mu_g;                           // Prior mean for contribution effects
  real<lower=0> sigma_g;                    // Prior scale for contribution effects
}

parameters {
  vector[N] theta_raw;                      // Standard normal ideal points
  vector[M] alpha_raw;                      // Standard normal cutpoints
  vector[M] beta_raw;                       // Standard normal discriminations
  array[L] vector[K] gamma_raw;             // Standard normal inducement coefficients
}

transformed parameters {
  vector[N] theta = mu_t + sigma_t * theta_raw;
  vector[M] alpha = mu_a + sigma_a * alpha_raw;
  vector[M] beta  = mu_b + sigma_b * beta_raw;
  array[L] vector[K] gamma;
  for (m in 1:L) {
    gamma[m] = mu_g + sigma_g * gamma_raw[m];
  }
}

model {
  // Non-centered standard normal priors
  theta_raw ~ std_normal();
  alpha_raw ~ std_normal();
  beta_raw  ~ std_normal();
  for (m in 1:L) {
    gamma_raw[m] ~ std_normal();
  }

  // Precompute inducement contribution effects
  array[L] vector[R] xg;
  for (m in 1:L) {
    xg[m] = x * gamma[m];
  }

  // Linear link function
  vector[R] xb;
  for (l in 1:R_L) {
    int idx = rl[l];
    xb[idx] = beta[rc[idx]] * theta[id[idx]] - alpha[rc[idx]];
  }
  for (l in 1:R_N) {
    int idx = rn[l];
    xb[idx] = beta[rc[idx]] * theta[id[idx]] - alpha[rc[idx]] + xg[nl[idx], idx];
  }

  y ~ bernoulli_logit(xb);
}

generated quantities {
  vector[R] log_lik;
  for (l in 1:R_L) {
    int idx = rl[l];
    log_lik[idx] = bernoulli_logit_lpmf(y[idx] | beta[rc[idx]] * theta[id[idx]] - alpha[rc[idx]]);
  }
  for (l in 1:R_N) {
    int idx = rn[l];
    real link = beta[rc[idx]] * theta[id[idx]] - alpha[rc[idx]] + (x[idx] * gamma[nl[idx]]);
    log_lik[idx] = bernoulli_logit_lpmf(y[idx] | link);
  }
}
