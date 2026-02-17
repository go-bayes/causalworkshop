test_that("simulate_nzavs_data returns correct dimensions", {
  d <- simulate_nzavs_data(n = 200, seed = 42)
  expect_equal(nrow(d), 600)
  expect_equal(ncol(d), 34)
})

test_that("simulate_nzavs_data has correct column names", {
  d <- simulate_nzavs_data(n = 100, seed = 1)

  expected_cols <- c(
    "id", "wave",
    "age", "male", "nz_european", "education", "partner", "employed",
    "log_income", "nz_dep",
    "agreeableness", "conscientiousness", "extraversion", "neuroticism",
    "openness",
    "community_group", "religious_service", "volunteer_work",
    "wellbeing", "belonging", "self_esteem", "life_satisfaction",
    "tau_community_wellbeing", "tau_community_belonging",
    "tau_community_self_esteem", "tau_community_life_satisfaction",
    "tau_religious_wellbeing", "tau_religious_belonging",
    "tau_religious_self_esteem", "tau_religious_life_satisfaction",
    "tau_volunteer_wellbeing", "tau_volunteer_belonging",
    "tau_volunteer_self_esteem", "tau_volunteer_life_satisfaction"
  )
  expect_equal(names(d), expected_cols)
})

test_that("simulate_nzavs_data has three waves per individual", {
  d <- simulate_nzavs_data(n = 100, seed = 1)
  expect_equal(sort(unique(d$wave)), c(0L, 1L, 2L))

  waves_per_id <- table(d$id)
  expect_true(all(waves_per_id == 3))
})

test_that("simulate_nzavs_data is reproducible with the same seed", {
  d1 <- simulate_nzavs_data(n = 50, seed = 99)
  d2 <- simulate_nzavs_data(n = 50, seed = 99)
  expect_identical(d1, d2)
})

test_that("simulate_nzavs_data differs with different seeds", {
  d1 <- simulate_nzavs_data(n = 50, seed = 1)
  d2 <- simulate_nzavs_data(n = 50, seed = 2)
  expect_false(identical(d1$wellbeing, d2$wellbeing))
})

test_that("binary variables are 0/1", {
  d <- simulate_nzavs_data(n = 500, seed = 1)
  binary_cols <- c("male", "nz_european", "partner", "employed",
                   "community_group", "religious_service", "volunteer_work")
  for (col in binary_cols) {
    vals <- unique(d[[col]])
    expect_true(all(vals %in% c(0L, 1L)),
                info = paste(col, "contains values other than 0/1"))
  }
})

test_that("age increases by 1 across waves", {
  d <- simulate_nzavs_data(n = 100, seed = 1)
  d0 <- d[d$wave == 0, ]
  d1 <- d[d$wave == 1, ]
  d2 <- d[d$wave == 2, ]
  expect_equal(d1$age - d0$age, rep(1, 100))
  expect_equal(d2$age - d0$age, rep(2, 100))
})

test_that("tau columns are constant across waves", {
  d <- simulate_nzavs_data(n = 100, seed = 1)
  tau_cols <- grep("^tau_", names(d), value = TRUE)
  d0 <- d[d$wave == 0, ]
  d1 <- d[d$wave == 1, ]
  d2 <- d[d$wave == 2, ]
  for (tc in tau_cols) {
    expect_equal(d0[[tc]], d1[[tc]], info = paste(tc, "differs between wave 0 and 1"))
    expect_equal(d0[[tc]], d2[[tc]], info = paste(tc, "differs between wave 0 and 2"))
  }
})

test_that("ATEs are in expected range", {
  d <- simulate_nzavs_data(n = 10000, seed = 2026)
  d0 <- d[d$wave == 0, ]

  # community_group -> wellbeing: theoretical ATE ~0.20
  ate <- mean(d0$tau_community_wellbeing)
  expect_gt(ate, 0.15)
  expect_lt(ate, 0.25)

  # religious_service -> belonging: theoretical ATE ~0.25
  ate <- mean(d0$tau_religious_belonging)
  expect_gt(ate, 0.20)
  expect_lt(ate, 0.30)

  # volunteer_work -> life_satisfaction: theoretical ATE ~0.18
  ate <- mean(d0$tau_volunteer_life_satisfaction)
  expect_gt(ate, 0.13)
  expect_lt(ate, 0.23)
})

test_that("confounding is present (naive estimate biased)", {
  d <- simulate_nzavs_data(n = 5000, seed = 2026)
  d0 <- d[d$wave == 0, ]
  d1 <- d[d$wave == 1, ]
  d2 <- d[d$wave == 2, ]

  true_ate <- mean(d0$tau_community_wellbeing)
  naive <- coef(lm(d2$wellbeing ~ d1$community_group))[[2]]
  bias <- abs(naive - true_ate)

  # naive estimate should be substantially biased (> 0.10)
  expect_gt(bias, 0.10)
})

test_that("g-computation recovers true ATE", {
  d <- simulate_nzavs_data(n = 5000, seed = 2026)
  d0 <- d[d$wave == 0, ]
  d1 <- d[d$wave == 1, ]
  d2 <- d[d$wave == 2, ]

  true_ate <- mean(d0$tau_community_wellbeing)

  df <- data.frame(
    y = d2$wellbeing,
    a = d1$community_group,
    age = d0$age, male = d0$male, nz_european = d0$nz_european,
    education = d0$education, partner = d0$partner, employed = d0$employed,
    log_income = d0$log_income, nz_dep = d0$nz_dep,
    agreeableness = d0$agreeableness, conscientiousness = d0$conscientiousness,
    extraversion = d0$extraversion, neuroticism = d0$neuroticism,
    openness = d0$openness,
    community_t0 = d0$community_group, wellbeing_t0 = d0$wellbeing
  )

  fit <- lm(y ~ a + age + male + nz_european + education + partner +
              employed + log_income + nz_dep + agreeableness +
              conscientiousness + extraversion + neuroticism + openness +
              community_t0 + wellbeing_t0, data = df)

  df1 <- df0 <- df
  df1$a <- 1
  df0$a <- 0
  ate_gcomp <- mean(predict(fit, df1) - predict(fit, df0))

  # g-computation should be close to true ATE (within 0.05)
  expect_lt(abs(ate_gcomp - true_ate), 0.05)
})

test_that("seed = NULL does not error", {
  expect_no_error(simulate_nzavs_data(n = 50, seed = NULL))
})
