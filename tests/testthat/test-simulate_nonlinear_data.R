test_that("simulate_nonlinear_data returns correct dimensions", {
  d <- simulate_nonlinear_data(n = 200, seed = 42)
  expect_equal(nrow(d), 200)
  expect_equal(ncol(d), 7)
})

test_that("simulate_nonlinear_data has correct column names", {
  d <- simulate_nonlinear_data(n = 100, seed = 1)
  expect_equal(names(d), c("id", "x1", "x2", "x3", "treatment", "outcome", "tau_true"))
})

test_that("simulate_nonlinear_data is reproducible with same seed", {
  d1 <- simulate_nonlinear_data(n = 50, seed = 99)
  d2 <- simulate_nonlinear_data(n = 50, seed = 99)
  expect_identical(d1, d2)
})

test_that("simulate_nonlinear_data differs with different seeds", {
  d1 <- simulate_nonlinear_data(n = 50, seed = 1)
  d2 <- simulate_nonlinear_data(n = 50, seed = 2)
  expect_false(identical(d1$outcome, d2$outcome))
})

test_that("treatment is binary and approximately balanced", {
  d <- simulate_nonlinear_data(n = 5000, seed = 2026)
  expect_true(all(d$treatment %in% c(0L, 1L)))
  # randomised: treatment rate should be near 0.5
  expect_gt(mean(d$treatment), 0.45)
  expect_lt(mean(d$treatment), 0.55)
})

test_that("x3 is binary", {
  d <- simulate_nonlinear_data(n = 500, seed = 1)
  expect_true(all(d$x3 %in% c(0L, 1L)))
})

test_that("ATE is approximately 0.55", {
  # theoretical: 0.3 + E[0.8*sin(2*x1)] + E[0.5*pmax(x2,0)^2] - E[0.4*x1*x2*x3]
  # = 0.3 + 0 + ~0.25 - 0 = ~0.55
  d <- simulate_nonlinear_data(n = 10000, seed = 2026)
  ate <- mean(d$tau_true)
  expect_gt(ate, 0.45)
  expect_lt(ate, 0.65)
})

test_that("no confounding: difference in means recovers ATE", {
  d <- simulate_nonlinear_data(n = 5000, seed = 2026)
  true_ate <- mean(d$tau_true)
  naive_ate <- mean(d$outcome[d$treatment == 1]) - mean(d$outcome[d$treatment == 0])
  # with randomised treatment, bias should be small
  expect_lt(abs(naive_ate - true_ate), 0.10)
})

test_that("seed = NULL does not error", {
  expect_no_error(simulate_nonlinear_data(n = 50, seed = NULL))
})

test_that("compare_ate_methods returns expected structure", {
  skip_if_not_installed("mgcv")
  results <- compare_ate_methods(n = 300, seed = 42, num_trees = 200, verbose = FALSE)

  expect_type(results, "list")
  expect_named(results, c("summary", "predictions", "plot_comparison", "plot_by_x1", "data"))

  # summary table
  expect_s3_class(results$summary, "tbl_df")
  expect_equal(nrow(results$summary), 4)
  expect_true(all(c("method", "ate_estimate", "ate_true", "rmse") %in% names(results$summary)))

  # predictions
  expect_s3_class(results$predictions, "tbl_df")
  expect_equal(nrow(results$predictions), 300 * 4)

  # plots
  expect_s3_class(results$plot_comparison, "ggplot")
  expect_s3_class(results$plot_by_x1, "ggplot")
})

test_that("causal forest has lower RMSE than OLS", {
  skip_if_not_installed("mgcv")
  results <- compare_ate_methods(n = 1000, seed = 2026, num_trees = 500, verbose = FALSE)
  rmse_ols <- results$summary$rmse[results$summary$method == "OLS (linear)"]
  rmse_cf <- results$summary$rmse[results$summary$method == "Causal forest"]
  expect_lt(rmse_cf, rmse_ols)
})
