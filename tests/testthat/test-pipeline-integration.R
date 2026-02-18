# integration tests for the full lab pipeline
# these tests verify that the causalworkshop functions work together
# as they will be used in PSYC 434 labs

test_that("NZAVS data -> causal forest -> ATE close to ground truth", {
  skip_on_cran()

  d <- simulate_nzavs_data(n = 2000, seed = 2026)
  d0 <- d[d$wave == 0, ]
  d1 <- d[d$wave == 1, ]
  d2 <- d[d$wave == 2, ]

  true_ate <- mean(d0$tau_community_wellbeing)

  # construct matrices
  covariate_cols <- c(
    "age", "male", "nz_european", "education", "partner", "employed",
    "log_income", "nz_dep", "agreeableness", "conscientiousness",
    "extraversion", "neuroticism", "openness",
    "community_group", "wellbeing"
  )
  X <- as.matrix(d0[, covariate_cols])
  Y <- d2$wellbeing
  W <- d1$community_group

  cf <- grf::causal_forest(
    X, Y, W,
    num.trees = 500,
    honesty = TRUE,
    seed = 2026
  )

  ate <- grf::average_treatment_effect(cf)
  expect_lt(abs(ate[["estimate"]] - true_ate), 0.10)
})

test_that("causal forest predictions -> RATE curve has expected shape", {
  skip_on_cran()

  d <- simulate_nzavs_data(n = 2000, seed = 2026)
  d0 <- d[d$wave == 0, ]
  d1 <- d[d$wave == 1, ]
  d2 <- d[d$wave == 2, ]

  covariate_cols <- c(
    "age", "male", "nz_european", "education", "partner", "employed",
    "log_income", "nz_dep", "agreeableness", "conscientiousness",
    "extraversion", "neuroticism", "openness",
    "community_group", "wellbeing"
  )
  X <- as.matrix(d0[, covariate_cols])
  Y <- d2$wellbeing
  W <- d1$community_group

  cf <- grf::causal_forest(
    X, Y, W,
    num.trees = 500,
    honesty = TRUE,
    seed = 2026
  )

  tau_hat <- predict(cf)$predictions

  # sort descending
  tau_sorted <- sort(tau_hat, decreasing = TRUE)
  n <- length(tau_sorted)

  # gain at top 10% should exceed gain at top 50%
  gain_10 <- mean(tau_sorted[seq_len(round(0.10 * n))]) - mean(tau_hat)
  gain_50 <- mean(tau_sorted[seq_len(round(0.50 * n))]) - mean(tau_hat)

  expect_gt(gain_10, gain_50)
})

test_that("policy tree produces valid predictions", {
  skip_on_cran()
  skip_if_not_installed("policytree")

  d <- simulate_nzavs_data(n = 2000, seed = 2026)
  d0 <- d[d$wave == 0, ]
  d1 <- d[d$wave == 1, ]
  d2 <- d[d$wave == 2, ]

  covariate_cols <- c(
    "age", "male", "nz_european", "education", "partner", "employed",
    "log_income", "nz_dep", "agreeableness", "conscientiousness",
    "extraversion", "neuroticism", "openness",
    "community_group", "wellbeing"
  )
  X <- as.matrix(d0[, covariate_cols])
  Y <- d2$wellbeing
  W <- d1$community_group

  cf <- grf::causal_forest(
    X, Y, W,
    num.trees = 500,
    honesty = TRUE,
    seed = 2026
  )

  tau_hat <- predict(cf)$predictions

  # gamma matrix: col 1 = control reward, col 2 = treatment reward
  gamma_matrix <- cbind(control = rep(0, length(tau_hat)),
                        treatment = tau_hat)

  # subsample for speed
  set.seed(2026)
  idx <- sample(seq_len(nrow(X)), 500)

  pt <- policytree::policy_tree(
    X[idx, ], gamma_matrix[idx, ],
    depth = 2
  )

  preds <- predict(pt, X)
  expect_true(all(preds %in% c(1L, 2L)))
  expect_equal(length(preds), nrow(X))
})

test_that("nonlinear data -> compare_ate_methods -> forest beats OLS on RMSE", {
  skip_on_cran()

  d <- simulate_nonlinear_data(n = 2000, seed = 2026)
  result <- compare_ate_methods(d)

  # extract RMSE values
  rmse_ols <- result$summary$rmse[result$summary$method == "OLS (linear)"]
  rmse_cf <- result$summary$rmse[result$summary$method == "Causal forest"]

  expect_length(rmse_ols, 1)
  expect_length(rmse_cf, 1)
  expect_lt(rmse_cf, rmse_ols)
})

test_that("measurement items -> lavaan CFA fits without error", {
  skip_on_cran()
  skip_if_not_installed("lavaan")

  d <- simulate_measurement_items(n = 1000, seed = 2026)

  model <- "distress =~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6"

  expect_no_error({
    fit <- lavaan::cfa(model, data = d)
  })

  # check that fit converged
  expect_true(lavaan::lavInspect(fit, "converged"))
})
