// ============================================================================
// Hierarchical 2PL Item Response Theory (IRT) Model of Environmental Ideology
// Structural regression: theta_i ~ Normal(X_i * gamma, sigma_t)
// Vote link: y_r ~ Bernoulli_Logit(beta_m * theta_i - alpha_m)
// ============================================================================

data {
  int<lower=1> N;                           // Number of legislators
  int<lower=1> M;                           // Number of roll calls
  int<lower=1> K;                           // Number of structural covariates
  int<lower=1> R;                           // Number of observed votes
  array[R] int<lower=1, upper=N> id;        // Legislator index for vote r
  array[R] int<lower=1, upper=M> rc;        // Roll call index for vote r
  array[R] int<lower=0, upper=1> y;         // Vote outcome (1 = pro-environmental, 0 = against)
  matrix[N, K] x;                           // Legislator/district covariate matrix
  
  // Prior hyperparameters
  real<lower=0> sigma_t;                    // Scale for theta residual
  vector[M] mu_a;                           // Prior mean for cutpoints alpha
  real<lower=0> sigma_a;                    // Prior scale for alpha
  vector[M] mu_b;                           // Prior mean for discrimination beta
  real<lower=0> sigma_b;                    // Prior scale for beta
  vector[K] mu_g;                           // Prior mean for structural coefficients gamma
  vector<lower=0>[K] sigma_g;               // Prior scale for gamma
}

parameters {
  vector[N] theta_raw;                      // Standard normal non-centered ideal points
  vector[M] alpha_raw;                      // Standard normal cutpoints
  vector[M] beta_raw;                       // Standard normal discriminations
  vector[K] gamma_raw;                      // Standard normal structural coefficients
}

transformed parameters {
  vector[M] alpha = mu_a + sigma_a * alpha_raw;
  vector[M] beta  = mu_b + sigma_b * beta_raw;
  vector[K] gamma = mu_g + sigma_g .* gamma_raw;
  vector[N] theta = x * gamma + sigma_t * theta_raw;
}

model {
  // Non-centered standard normal priors
  theta_raw ~ std_normal();
  alpha_raw ~ std_normal();
  beta_raw  ~ std_normal();
  gamma_raw ~ std_normal();

  // Likelihood
  vector[R] xb;
  for (r in 1:R) {
    xb[r] = beta[rc[r]] * theta[id[r]] - alpha[rc[r]];
  }
  y ~ bernoulli_logit(xb);
}

generated quantities {
  // Compute log likelihood for LOO-CV and model comparison
  vector[R] log_lik;
  for (r in 1:R) {
    log_lik[r] = bernoulli_logit_lpmf(y[r] | beta[rc[r]] * theta[id[r]] - alpha[rc[r]]);
  }
}
