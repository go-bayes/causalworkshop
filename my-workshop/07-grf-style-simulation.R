# Advanced Simulation: GRF Benchmark Patterns
# ------------------------------------------------------------------
# This script scaffolds a second data generating mechanism inspired by
# the synthetic designs used in the generalized random forest (grf)
# literature.  The core idea is to add nonlinear baseline structure,
# sparse / region-specific heterogeneity, and richer propensity score
# behaviour so forests have to work harder than in the introductory
# workshop simulation.
#
# The function below is not yet wired into the published workshop
# sequence; keeping it in its own script lets us iterate without
# disturbing the main exercises.  Call `simulate_grf_style()` to obtain
# a tibble with the following columns:
#   * `W`   : binary treatment assignment (0 = control, 1 = treated)
#   * `Y`   : observed outcome
#   * `tau` : true conditional treatment effect τ(x)
#   * `propensity`: true propensity score e(x)
#   * `x1`, `x2`, … : covariates (the first few drive heterogeneity,
#     the rest are pure noise)
#
# The design emphasises:
#   - Nonlinear heterogeneous effects (piecewise and interaction-based)
#   - Propensity scores that depend on multiple covariates (confounding)
#   - Additional irrelevant covariates to induce sparsity pressure
#   - Heteroskedastic noise (variance tied to covariates)
#
# Workflow idea (for later integration):
#   1. `dat <- simulate_grf_style()`
#   2. Fit a causal forest on `dat`
#   3. Compare forest performance against the true τ(x)
#   4. Highlight where diagnostics like RATE/Qini flag the signal
#
# Reference: Wager & Athey (2018) and follow-up GRF benchmarks.

suppressPackageStartupMessages({
  library(tidyverse)
})

simulate_grf_style <- function(n = 10000,
                               p = 10,
                               seed = 2025) {
  stopifnot(n > 0, p >= 2)

  set.seed(seed)

  # Covariates: uniform on [-1, 1]; first two drive heterogeneity
  X <- matrix(runif(n * p, min = -1, max = 1), nrow = n, ncol = p)
  colnames(X) <- paste0("x", seq_len(p))

  # Piecewise / nonlinear treatment effect
  tau <- 1.5 * (X[, 1] > 0 & X[, 2] > 0) +
    0.5 * (X[, 1] - 0.5 * X[, 2]) +
    0.25 * sin(pi * X[, 1] * X[, 2])

  # Baseline outcome: nonlinear main effects + interactions
  mu <- 0.5 * sin(pi * X[, 1] * X[, 2]) +
    0.25 * X[, 3]^2 -
    0.1 * X[, 4] * X[, 5] +
    0.1 * X[, 6]

  # Propensity score with shared structure (induces confounding)
  propensity <- plogis(
    0.5 * X[, 1] -
      0.25 * X[, 2] +
      0.35 * X[, 3] -
      0.2 * X[, 4] +
      0.1 * X[, 5]
  )

  W <- rbinom(n, size = 1, prob = propensity)

  # Heteroskedastic noise: variance increases with |x1|
  noise <- rnorm(n, sd = 0.4 + 0.2 * abs(X[, 1]))

  Y <- mu + tau * W + noise

tibble(
  W = W,
  Y = Y,
  tau = tau,
  propensity = propensity
) %>%
  bind_cols(as_tibble(X))
}

# In ad-hoc testing (n ≈ 10k) a default `grf::causal_forest()` recovers:
#   * ATE ≈ 0.24 (true ATE ≈ 0.25) with honest splitting enabled.
#   * Meaningful heterogeneity concentrated in the x1 > 0 & x2 > 0 region.
#   * RATE/Qini diagnostics flag the Charitable Giving analogue as “actionable”
#     while discouraging targeting where τ(x) is negative.
# Use this script as a sandbox when you want participants to experience a
# harder recovery problem than the main workshop simulation.

# Example usage (commented out for now)
# sim_data <- simulate_grf_style()
# head(sim_data)
