#' Workshop: Baseline Adjustment for Selection Bias
#'
#' Demonstrates the importance of baseline adjustment in addressing selection 
#' bias in observational studies. Compares naive estimates with adjusted estimates.
#'
#' @param data A data frame with treatment, outcome, and baseline variables.
#'   If NULL, simulates data using \code{simulate_religious_data()}.
#' @param treatment_var Character. Name of treatment variable (default: "belief_god_binary")
#' @param outcome_vars Character vector. Names of outcome variables 
#'   (default: c("charity_outcome", "volunteer_outcome"))
#' @param baseline_vars Character vector. Names of baseline variables for adjustment
#'   (default: c("baseline_charity", "baseline_volunteer"))
#' @param true_effects Numeric vector. True treatment effects for comparison
#'   (default: c(0.25, 0.4))
#' @param verbose Logical. Whether to print detailed output (default: TRUE)
#'
#' @return A list containing:
#'   \describe{
#'     \item{results_table}{Comparison of naive vs adjusted estimates}
#'     \item{bias_reduction}{Average bias reduction achieved}
#'     \item{data}{The analysed dataset}
#'   }
#'
#' @details
#' This function demonstrates a key principle in causal inference: the importance
#' of adjusting for baseline confounders. It shows how naive estimates (simple
#' differences in means) can be severely biased due to selection effects, and
#' how baseline adjustment can reduce this bias.
#'
#' The function:
#' 1. Fits naive models (outcome ~ treatment)
#' 2. Fits adjusted models (outcome ~ treatment + baseline)
#' 3. Compares estimates to true effects
#' 4. Calculates bias reduction
#'
#' @examples
#' # Run with simulated data
#' results <- workshop_baseline_adjustment()
#' 
#' # View results
#' print(results$results_table)
#' 
#' # Use custom data
#' my_data <- simulate_religious_data(n = 2000)
#' results <- workshop_baseline_adjustment(data = my_data)
#' 
#' @export
workshop_baseline_adjustment <- function(data = NULL,
                                       treatment_var = "belief_god_binary",
                                       outcome_vars = c("charity_outcome", "volunteer_outcome"),
                                       baseline_vars = c("baseline_charity", "baseline_volunteer"),
                                       true_effects = c(0.25, 0.4),
                                       verbose = TRUE) {
  
  if (verbose) {
    cli::cli_rule("Workshop: Baseline Adjustment for Selection Bias")
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
    cli::cli_alert_info("Treatment rate: {round(mean(data[[treatment_var]]) * 100, 1)}%")
  }
  
  # Storage for results
  results_list <- list()
  
  # Analyse each outcome
  for (i in seq_along(outcome_vars)) {
    outcome <- outcome_vars[i]
    baseline <- baseline_vars[i]
    
    # Naive estimation (biased due to selection)
    naive_formula <- stats::as.formula(paste(outcome, "~", treatment_var))
    naive_model <- stats::lm(naive_formula, data = data)
    naive_estimate <- stats::coef(naive_model)[treatment_var]
    
    # Baseline-adjusted estimation
    adjusted_formula <- stats::as.formula(paste(outcome, "~", treatment_var, "+", baseline))
    adjusted_model <- stats::lm(adjusted_formula, data = data)
    adjusted_estimate <- stats::coef(adjusted_model)[treatment_var]
    
    # Store results
    results_list[[i]] <- tibble::tibble(
      outcome = outcome,
      true_effect = true_effects[i],
      naive_estimate = naive_estimate,
      adjusted_estimate = adjusted_estimate,
      naive_bias = naive_estimate - true_effects[i],
      adjusted_bias = adjusted_estimate - true_effects[i]
    )
  }
  
  # Combine results
  results_table <- dplyr::bind_rows(results_list)
  
  # Calculate bias reduction
  bias_reduction <- mean(abs(results_table$naive_bias) - abs(results_table$adjusted_bias))
  
  if (verbose) {
    cli::cli_rule("Results")
    cli::cli_alert_info("Estimation comparison:")
    print(results_table, n = Inf)
    
    cli::cli_alert_success("Average bias reduction: {round(bias_reduction, 3)}")
    
    if (bias_reduction > 0) {
      cli::cli_alert_success("Baseline adjustment successfully reduced bias")
    } else {
      cli::cli_alert_warning("Baseline adjustment did not reduce bias")
    }
  }
  
  return(list(
    results_table = results_table,
    bias_reduction = bias_reduction,
    data = data
  ))
}