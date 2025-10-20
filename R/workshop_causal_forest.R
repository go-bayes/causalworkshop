#' Workshop: Causal Forest Analysis for Heterogeneous Treatment Effects
#'
#' Demonstrates how to use causal forests to estimate conditional average
#' treatment effects (CATE) and test for treatment effect heterogeneity.
#'
#' @param data A data frame with treatment, outcome, and covariate variables.
#'   If NULL, simulates data using \code{simulate_religious_data()}.
#' @param outcome_var Character. Name of outcome variable (default: "charity_outcome")
#' @param treatment_var Character. Name of treatment variable (default: "belief_god_binary")
#' @param covariate_vars Character vector. Names of covariate variables
#'   (default: c("age", "education", "income", "baseline_charity", "baseline_volunteer"))
#' @param num_trees Integer. Number of trees in the forest (default: 1000)
#' @param verbose Logical. Whether to print detailed output (default: TRUE)
#'
#' @return A list containing:
#'   \describe{
#'     \item{causal_forest}{The fitted causal forest object}
#'     \item{predictions}{Data with individual treatment effect predictions}
#'     \item{heterogeneity_stats}{Summary statistics of treatment effect variation}
#'     \item{variable_importance}{Variable importance for heterogeneity}
#'     \item{heterogeneity_test}{Test results for treatment effect heterogeneity}
#'   }
#'
#' @details
#' This function demonstrates the core workflow for causal forest analysis:
#' 1. Fits a causal forest using honest sample splitting
#' 2. Predicts individual treatment effects τ(x)
#' 3. Tests for significant treatment effect heterogeneity
#' 4. Calculates variable importance for effect modification
#' 5. Provides interpretation of results
#'
#' Causal forests use random forest methodology adapted for causal inference,
#' providing unbiased estimates of heterogeneous treatment effects while
#' maintaining valid statistical inference.
#'
#' @examples
#' # Run with simulated data
#' results <- workshop_causal_forest()
#'
#' # Check for heterogeneity
#' results$heterogeneity_stats
#'
#' # View variable importance
#' results$variable_importance
#'
#' # Use custom parameters
#' results <- workshop_causal_forest(
#'   data = my_data,
#'   num_trees = 2000,
#'   outcome_var = "volunteer_outcome"
#' )
#'
#' @export
workshop_causal_forest <- function(data = NULL,
                                 outcome_var = "charity_outcome",
                                 treatment_var = "belief_god_binary",
                                 covariate_vars = c("age", "education", "income",
                                                  "baseline_charity", "baseline_volunteer"),
                                 num_trees = 1000,
                                 verbose = TRUE) {

  if (verbose) {
    cli::cli_rule("Workshop: Causal Forest Analysis")
  }

  # Generate data if not provided
  if (is.null(data)) {
    if (verbose) {
      cli::cli_alert_info("Generating simulated data...")
    }
    data <- simulate_religious_data()
  }

  if (verbose) {
    cli::cli_alert_info("Analysing {nrow(data)} observations")
    cli::cli_alert_info("Outcome: {outcome_var}")
    cli::cli_alert_info("Treatment: {treatment_var}")
    cli::cli_alert_info("Covariates: {length(covariate_vars)} variables")
  }

  # Prepare data for causal forest
  X <- data[, covariate_vars, drop = FALSE] |> as.matrix()
  Y <- data[[outcome_var]]
  W <- data[[treatment_var]]

  if (verbose) {
    cli::cli_alert_info("Fitting causal forest with {num_trees} trees...")
  }

  # Fit causal forest
  cf <- grf::causal_forest(
    X = X,
    Y = Y,
    W = W,
    num.trees = num_trees,
    honesty = TRUE,
    tune.parameters = "all",
    seed = 2025
  )

  if (verbose) {
    cli::cli_alert_success("Causal forest fitted successfully")
  }

  # Estimate average treatment effect
  ate_est <- grf::average_treatment_effect(cf)

  if (verbose) {
    cli::cli_alert_info("Average treatment effect: {round(ate_est[1], 3)} (SE: {round(ate_est[2], 3)})")
  }

  # Predict individual treatment effects
  tau_hat <- stats::predict(cf)$predictions

  # Calculate heterogeneity statistics
  heterogeneity_stats <- tibble::tibble(
    mean_tau = mean(tau_hat),
    sd_tau = stats::sd(tau_hat),
    min_tau = min(tau_hat),
    max_tau = max(tau_hat)
  )

  if (verbose) {
    cli::cli_rule("Heterogeneity Analysis")
    cli::cli_alert_info("Treatment effect variation:")
    cli::cli_alert_info("  Mean τ(x): {round(heterogeneity_stats$mean_tau, 3)}")
    cli::cli_alert_info("  SD τ(x): {round(heterogeneity_stats$sd_tau, 3)}")
    cli::cli_alert_info("  Range: [{round(heterogeneity_stats$min_tau, 3)}, {round(heterogeneity_stats$max_tau, 3)}]")
  }

  # Test for heterogeneity
  het_test <- grf::test_calibration(cf)

  if (verbose) {
    cli::cli_alert_info("Heterogeneity test p-value: {round(het_test[2], 3)}")

    if (het_test[2] < 0.05) {
      cli::cli_alert_success("Significant treatment effect heterogeneity detected")
    } else {
      cli::cli_alert_warning("Limited evidence of heterogeneity")
    }
  }

  # Variable importance for heterogeneity
  var_imp <- grf::variable_importance(cf)
  importance_df <- tibble::tibble(
    variable = colnames(X),
    importance = as.numeric(var_imp)
  ) |>
    dplyr::arrange(dplyr::desc(importance))

  if (verbose) {
    cli::cli_alert_info("Variable importance for heterogeneity:")
    print(importance_df |>
          dplyr::mutate(importance = round(importance, 4)))
  }

  # Add predictions to data
  data_with_predictions <- data |>
    dplyr::mutate(tau_hat = tau_hat)

  # Examine extreme cases
  if (verbose) {
    high_benefit <- data_with_predictions |>
      dplyr::slice_max(tau_hat, n = 5) |>
      dplyr::select(dplyr::all_of(covariate_vars), tau_hat)

    low_benefit <- data_with_predictions |>
      dplyr::slice_min(tau_hat, n = 5) |>
      dplyr::select(dplyr::all_of(covariate_vars), tau_hat)

    cli::cli_alert_info("Highest predicted effects:")
    print(high_benefit |>
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2))))

    cli::cli_alert_info("Lowest predicted effects:")
    print(low_benefit |>
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2))))
  }

  return(list(
    causal_forest = cf,
    predictions = data_with_predictions,
    heterogeneity_stats = heterogeneity_stats,
    variable_importance = importance_df,
    heterogeneity_test = het_test
  ))
}
