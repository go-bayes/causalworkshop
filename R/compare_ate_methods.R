#' Compare ATE Estimation Methods on Non-Linear Data
#'
#' Fits four estimation methods to data with non-linear heterogeneous treatment
#' effects and compares their ability to recover the true effect surface. With
#' randomised treatment, estimation error comes purely from functional form
#' misspecification.
#'
#' @param data A tibble from [simulate_nonlinear_data()], or NULL to generate
#'   data automatically.
#' @param n Integer. Sample size if generating data (default: 2000).
#' @param seed Integer. Seed if generating data (default: 2026).
#' @param num_trees Integer. Number of trees for the causal forest
#'   (default: 2000).
#' @param verbose Logical. Print progress messages (default: TRUE).
#'
#' @return A list with components:
#'   \describe{
#'     \item{summary}{Tibble with one row per method: method name,
#'       ATE estimate, true ATE, and RMSE of individual-level predictions.}
#'     \item{predictions}{Tibble with individual-level predictions from all
#'       methods (columns: tau_true, tau_hat, method, x1, x2, x3).}
#'     \item{plot_comparison}{ggplot2 object: tau_hat vs tau_true scatter,
#'       faceted by method, with 45-degree reference line.}
#'     \item{plot_by_x1}{ggplot2 object: smoothed tau_hat and tau_true vs x1,
#'       faceted by method.}
#'     \item{data}{The input or generated data.}
#'   }
#'
#' @details
#'
#' ## Methods compared
#'
#' 1. **OLS (linear):** `lm(outcome ~ treatment * (x1 + x2 + x3))`. Assumes
#'    linear interactions. Cannot capture curvature or threshold effects.
#' 2. **Polynomial (degree 3):** `lm(outcome ~ treatment * (poly(x1,3) +
#'    poly(x2,3) + x3))`. Adds flexibility but remains parametric.
#' 3. **GAM:** `mgcv::gam()` with smooth terms and smooth-by-treatment
#'    interactions. Learns non-linear main effects and treatment-covariate
#'    interactions non-parametrically.
#' 4. **Causal forest:** `grf::causal_forest()`. Directly targets the
#'    conditional average treatment effect without specifying a functional
#'    form.
#'
#' Individual treatment effects (tau_hat) are obtained via G-computation for
#' the regression-based methods (predict under treatment = 1 minus predict
#' under treatment = 0) and via direct prediction for the causal forest.
#'
#' ## Dependencies
#'
#' Requires `mgcv` (ships with R) and `grf` (package dependency).
#'
#' @examples
#' \donttest{
#' results <- compare_ate_methods(n = 500, seed = 42)
#' results$summary
#' results$plot_comparison
#' }
#'
#' @seealso [simulate_nonlinear_data()]
#'
#' @export
compare_ate_methods <- function(data = NULL, n = 2000, seed = 2026,
                                num_trees = 2000, verbose = TRUE) {

  if (!requireNamespace("mgcv", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg mgcv} is required. Install with {.code install.packages('mgcv')}.")
  }

  if (is.null(data)) {
    data <- simulate_nonlinear_data(n = n, seed = seed)
  }

  tau_true <- data$tau_true
  true_ate <- mean(tau_true)
  nn <- nrow(data)

  # data frames for counterfactual prediction
  data_1 <- data_0 <- data
  data_1$treatment <- 1L
  data_0$treatment <- 0L

  # ---- method 1: OLS with linear interactions ----
  if (verbose) cli::cli_alert_info("Fitting OLS (linear interactions)")
  fit_ols <- stats::lm(outcome ~ treatment * (x1 + x2 + x3), data = data)
  tau_ols <- stats::predict(fit_ols, data_1) - stats::predict(fit_ols, data_0)

  # ---- method 2: polynomial regression (degree 3) ----
  if (verbose) cli::cli_alert_info("Fitting polynomial regression (degree 3)")
  fit_poly <- stats::lm(
    outcome ~ treatment * (stats::poly(x1, 3) + stats::poly(x2, 3) + x3),
    data = data
  )
  tau_poly <- stats::predict(fit_poly, data_1) - stats::predict(fit_poly, data_0)

  # ---- method 3: GAM ----
  if (verbose) cli::cli_alert_info("Fitting GAM (smooth interactions)")

  # mgcv requires treatment as numeric for by= argument
  data_gam <- data
  data_gam$treatment_num <- as.numeric(data_gam$treatment)
  data_1_gam <- data_gam
  data_0_gam <- data_gam
  data_1_gam$treatment <- 1L
  data_0_gam$treatment <- 0L
  data_1_gam$treatment_num <- 1
  data_0_gam$treatment_num <- 0

  fit_gam <- mgcv::gam(
    outcome ~ treatment + s(x1) + s(x2) + x3 +
      s(x1, by = treatment_num) + s(x2, by = treatment_num),
    data = data_gam
  )
  tau_gam <- stats::predict(fit_gam, data_1_gam) -
    stats::predict(fit_gam, data_0_gam)

  # ---- method 4: causal forest ----
  if (verbose) cli::cli_alert_info("Fitting causal forest ({num_trees} trees)")
  X <- as.matrix(data[, c("x1", "x2", "x3")])
  Y <- data$outcome
  W <- data$treatment

  cf <- grf::causal_forest(
    X = X, Y = Y, W = W,
    num.trees = num_trees,
    seed = if (!is.null(seed)) seed else 42L
  )
  tau_cf <- stats::predict(cf)$predictions

  # ---- summary table ----
  methods <- c("OLS (linear)", "Polynomial (degree 3)", "GAM", "Causal forest")
  tau_list <- list(tau_ols, tau_poly, tau_gam, tau_cf)

  summary_tbl <- tibble::tibble(
    method = methods,
    ate_estimate = vapply(tau_list, mean, numeric(1)),
    ate_true = true_ate,
    rmse = vapply(tau_list, function(th) sqrt(mean((th - tau_true)^2)), numeric(1))
  )

  if (verbose) {
    cli::cli_rule("Results")
    for (i in seq_along(methods)) {
      cli::cli_alert_info(
        "{methods[i]}: ATE = {round(summary_tbl$ate_estimate[i], 3)}, RMSE = {round(summary_tbl$rmse[i], 3)}"
      )
    }
    cli::cli_alert_success("True ATE: {round(true_ate, 3)}")
  }

  # ---- predictions tibble ----
  predictions <- tibble::tibble(
    tau_true = rep(tau_true, 4),
    tau_hat = c(tau_ols, tau_poly, as.numeric(tau_gam), tau_cf),
    method = factor(
      rep(methods, each = nn),
      levels = methods
    ),
    x1 = rep(data$x1, 4),
    x2 = rep(data$x2, 4),
    x3 = rep(data$x3, 4)
  )

  # ---- plot 1: predicted vs true tau ----
  plot_comparison <- ggplot2::ggplot(
    predictions,
    ggplot2::aes(x = .data$tau_true, y = .data$tau_hat)
  ) +
    ggplot2::geom_point(alpha = 0.15, size = 0.5, colour = "#56B4E9") +
    ggplot2::geom_abline(
      slope = 1, intercept = 0,
      colour = "#D55E00", linetype = "dashed", linewidth = 0.6
    ) +
    ggplot2::facet_wrap(~ method, ncol = 2, scales = "free_y") +
    ggplot2::labs(
      x = "True treatment effect",
      y = "Estimated treatment effect",
      title = "Predicted vs true individual treatment effects",
      subtitle = "Dashed line = perfect recovery"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  # ---- plot 2: tau vs x1 (smoothed) ----
  # build a long-form tibble with both true and estimated tau
  plot_data_x1 <- tibble::tibble(
    x1 = rep(data$x1, 5),
    tau = c(tau_true, tau_ols, tau_poly, as.numeric(tau_gam), tau_cf),
    method = factor(
      c(rep("True effect", nn), rep(methods, each = nn)),
      levels = c("True effect", methods)
    )
  )

  plot_by_x1 <- ggplot2::ggplot(
    plot_data_x1,
    ggplot2::aes(x = .data$x1, y = .data$tau, colour = .data$method)
  ) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, linewidth = 0.8, span = 0.3) +
    ggplot2::scale_colour_manual(
      values = c(
        "True effect" = "#000000",
        "OLS (linear)" = "#E69F00",
        "Polynomial (degree 3)" = "#56B4E9",
        "GAM" = "#009E73",
        "Causal forest" = "#CC79A7"
      )
    ) +
    ggplot2::labs(
      x = "x1",
      y = "Treatment effect",
      title = "Treatment effect as a function of x1",
      subtitle = "Smoothed estimates vs true effect (black)",
      colour = "Method"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  list(
    summary = summary_tbl,
    predictions = predictions,
    plot_comparison = plot_comparison,
    plot_by_x1 = plot_by_x1,
    data = data
  )
}
