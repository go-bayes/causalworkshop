test_that("simulate_measurement_items returns correct dimensions", {
  d <- simulate_measurement_items(n = 500, seed = 42)
  expect_equal(nrow(d), 500)
  expect_equal(ncol(d), 11)
})

test_that("simulate_measurement_items has correct column names", {
  d <- simulate_measurement_items(n = 100, seed = 1)

  expected_cols <- c(
    "id", "group", "age", "male",
    "item_1", "item_2", "item_3", "item_4", "item_5", "item_6",
    "true_distress"
  )
  expect_equal(names(d), expected_cols)
})

test_that("simulate_measurement_items has correct column types", {
  d <- simulate_measurement_items(n = 100, seed = 1)

  expect_type(d$id, "integer")
  expect_type(d$group, "integer")
  expect_type(d$age, "double")
  expect_type(d$male, "integer")
  for (j in 1:6) {
    expect_type(d[[paste0("item_", j)]], "integer")
  }
  expect_type(d$true_distress, "double")
})

test_that("simulate_measurement_items is reproducible with the same seed", {
  d1 <- simulate_measurement_items(n = 100, seed = 99)
  d2 <- simulate_measurement_items(n = 100, seed = 99)
  expect_identical(d1, d2)
})

test_that("simulate_measurement_items differs with different seeds", {
  d1 <- simulate_measurement_items(n = 100, seed = 1)
  d2 <- simulate_measurement_items(n = 100, seed = 2)
  expect_false(identical(d1$item_1, d2$item_1))
})

test_that("group variable is binary and approximately balanced", {
  d <- simulate_measurement_items(n = 2000, seed = 2026)

  expect_true(all(d$group %in% c(0L, 1L)))

  prop_g1 <- mean(d$group)
  expect_gt(prop_g1, 0.40)
  expect_lt(prop_g1, 0.60)
})

test_that("items are bounded between 1 and 5", {
  d <- simulate_measurement_items(n = 1000, seed = 2026)

  for (j in 1:6) {
    col <- paste0("item_", j)
    expect_true(all(d[[col]] >= 1L), info = paste(col, "has values below 1"))
    expect_true(all(d[[col]] <= 5L), info = paste(col, "has values above 5"))
  }
})

test_that("items are integer-valued", {
  d <- simulate_measurement_items(n = 500, seed = 2026)

  for (j in 1:6) {
    col <- paste0("item_", j)
    expect_true(all(d[[col]] == as.integer(d[[col]])),
                info = paste(col, "is not integer-valued"))
  }
})

test_that("true_loadings attribute is correct", {
  d <- simulate_measurement_items(n = 100, seed = 1)

  loadings <- attr(d, "true_loadings")
  expect_equal(loadings, c(0.70, 0.80, 0.75, 0.85, 0.65, 0.80))
})

test_that("intercept non-invariance is present in attributes", {
  d <- simulate_measurement_items(n = 100, seed = 1)

  int_g0 <- attr(d, "true_intercepts_group0")
  int_g1 <- attr(d, "true_intercepts_group1")

  # items 1, 2, 4, 6 should be equal across groups
  expect_equal(int_g0[1], int_g1[1])
  expect_equal(int_g0[2], int_g1[2])
  expect_equal(int_g0[4], int_g1[4])
  expect_equal(int_g0[6], int_g1[6])

  # items 3 and 5 should differ
  expect_false(int_g0[3] == int_g1[3])
  expect_false(int_g0[5] == int_g1[5])
})

test_that("1-factor CFA fits well", {
  skip_if_not_installed("lavaan")

  d <- simulate_measurement_items(n = 2000, seed = 2026)

  model <- "distress =~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6"
  fit <- lavaan::cfa(model, data = d)

  fit_measures <- lavaan::fitmeasures(fit, c("cfi", "rmsea"))
  expect_gt(fit_measures[["cfi"]], 0.90)
  expect_lt(fit_measures[["rmsea"]], 0.10)
})

test_that("full scalar invariance fails, partial invariance holds", {
  skip_if_not_installed("lavaan")

  d <- simulate_measurement_items(n = 2000, seed = 2026)

  model <- "distress =~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6"

  # configural
  fit_config <- lavaan::cfa(model, data = d, group = "group")

  # metric (equal loadings)
  fit_metric <- lavaan::cfa(model, data = d, group = "group",
                            group.equal = "loadings")

  # scalar (equal loadings + intercepts)
  fit_scalar <- lavaan::cfa(model, data = d, group = "group",
                            group.equal = c("loadings", "intercepts"))

  # metric should fit well
  metric_cfi <- lavaan::fitmeasures(fit_metric, "cfi")
  expect_gt(metric_cfi, 0.90)

  # scalar should fit substantially worse than metric
  comp <- lavaan::lavTestLRT(fit_metric, fit_scalar)
  expect_lt(comp[["Pr(>Chisq)"]][2], 0.05)

  # partial scalar (release items 3 and 5) should fit well
  model_partial <- paste0(
    "distress =~ item_1 + item_2 + item_3 + item_4 + item_5 + item_6\n",
    "item_3 ~ c(i3a, i3b) * 1\n",
    "item_5 ~ c(i5a, i5b) * 1"
  )
  fit_partial <- lavaan::cfa(model_partial, data = d, group = "group",
                             group.equal = c("loadings", "intercepts"))
  partial_cfi <- lavaan::fitmeasures(fit_partial, "cfi")
  expect_gt(partial_cfi, 0.90)
})

test_that("seed = NULL does not error", {
  expect_no_error(simulate_measurement_items(n = 50, seed = NULL))
})
