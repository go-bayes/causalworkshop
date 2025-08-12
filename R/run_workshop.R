#' Run Complete Causal Inference Workshop
#'
#' Executes the full workshop pipeline demonstrating causal inference concepts
#' from baseline adjustment through advanced targeting analysis.
#'
#' @param n Integer. Sample size for simulation (default: 1000)
#' @param seed Integer. Random seed for reproducibility (default: 2025)
#' @param include_plots Logical. Whether to generate plots (default: TRUE)
#' @param include_targeting Logical. Whether to include targeting analysis (default: TRUE)
#' @param verbose Logical. Whether to print detailed output (default: TRUE)
#'
#' @return A list containing results from all workshop modules:
#'   \describe{
#'     \item{data}{The simulated dataset used throughout}
#'     \item{baseline_results}{Results from baseline adjustment analysis}
#'     \item{causal_forest_results}{Results from causal forest analysis}
#'     \item{targeting_plots}{Rate and Qini curve visualizations (if requested)}
#'     \item{heterogeneity_plot}{Treatment effect distribution plot (if requested)}
#'     \item{selection_bias_plot}{Selection bias demonstration plot (if requested)}
#'   }
#'
#' @details
#' This function provides a complete educational workflow covering:
#' 
#' 1. **Data Simulation**: Realistic data with selection bias and heterogeneous effects
#' 2. **Baseline Adjustment**: Demonstrates importance of controlling for confounders
#' 3. **Causal Forests**: Estimates individual treatment effects τ(x)
#' 4. **Targeting Analysis**: Evaluates policy targeting using Rate and Qini curves
#' 5. **Visualization**: Creates publication-ready plots for all analyses
#' 
#' The workshop is designed to be run interactively, with detailed explanations
#' and interpretations provided throughout. Each module can also be run 
#' independently using the individual workshop functions.
#'
#' @examples
#' # Run complete workshop
#' workshop_results <- run_workshop()
#' 
#' # View baseline adjustment results
#' workshop_results$baseline_results$results_table
#' 
#' # View heterogeneity statistics
#' workshop_results$causal_forest_results$heterogeneity_stats
#' 
#' # Display plots
#' workshop_results$targeting_plots$rate_plot
#' workshop_results$heterogeneity_plot
#' 
#' # Run with custom parameters
#' workshop_results <- run_workshop(
#'   n = 2000,
#'   seed = 42,
#'   include_plots = TRUE
#' )
#' 
#' @export
run_workshop <- function(n = 1000, 
                        seed = 2025,
                        include_plots = TRUE,
                        include_targeting = TRUE,
                        verbose = TRUE) {
  
  if (verbose) {
    cli::cli_rule("Causal Inference Workshop with Machine Learning")
    cli::cli_alert_info("Educational pipeline for heterogeneous treatment effects")
    cli::cli_alert_info("Following Wager & Athey (2018) methodology")
  }
  
  # Step 1: Data Simulation
  if (verbose) {
    cli::cli_h1("Step 1: Data Simulation")
  }
  
  data <- simulate_religious_data(n = n, seed = seed, verbose = verbose)
  
  # Step 2: Baseline Adjustment Analysis
  if (verbose) {
    cli::cli_h1("Step 2: Baseline Adjustment Analysis")
  }
  
  baseline_results <- workshop_baseline_adjustment(
    data = data,
    verbose = verbose
  )
  
  # Step 3: Causal Forest Analysis
  if (verbose) {
    cli::cli_h1("Step 3: Causal Forest Analysis")
  }
  
  causal_forest_results <- workshop_causal_forest(
    data = data,
    verbose = verbose
  )
  
  # Step 4: Plotting (if requested)
  plots_list <- list()
  
  if (include_plots) {
    if (verbose) {
      cli::cli_h1("Step 4: Visualization")
      cli::cli_alert_info("Generating educational plots...")
    }
    
    # Selection bias plot
    plots_list$selection_bias_plot <- plot_selection_bias(
      baseline_results$results_table,
      title = "Workshop: Selection Bias Demonstration"
    )
    
    # Heterogeneity distribution
    plots_list$heterogeneity_plot <- plot_heterogeneity_distribution(
      causal_forest_results$predictions,
      title = "Workshop: Individual Treatment Effect Distribution"
    )
    
    # Targeting analysis (if requested)
    if (include_targeting) {
      if (verbose) {
        cli::cli_alert_info("Generating targeting performance plots...")
      }
      
      plots_list$targeting_plots <- plot_rate_qini_curves(
        causal_forest_results$predictions,
        title = "Workshop: Targeting Performance Analysis"
      )
    }
    
    if (verbose) {
      cli::cli_alert_success("All plots generated successfully")
    }
  }
  
  # Step 5: Summary and Interpretation
  if (verbose) {
    cli::cli_h1("Workshop Summary")
    
    # Key findings
    bias_reduction <- baseline_results$bias_reduction
    het_sd <- causal_forest_results$heterogeneity_stats$sd_tau
    het_test_p <- causal_forest_results$heterogeneity_test[2]
    
    cli::cli_alert_info("Key Educational Findings:")
    
    if (bias_reduction > 0) {
      cli::cli_alert_success("✓ Baseline adjustment reduced bias by {round(bias_reduction, 3)}")
    } else {
      cli::cli_alert_warning("⚠ Baseline adjustment did not reduce bias")
    }
    
    if (het_sd > 0.1) {
      cli::cli_alert_success("✓ Substantial treatment effect heterogeneity detected (SD = {round(het_sd, 3)})")
    } else {
      cli::cli_alert_info("○ Limited treatment effect heterogeneity (SD = {round(het_sd, 3)})")
    }
    
    if (het_test_p < 0.05) {
      cli::cli_alert_success("✓ Heterogeneity test significant (p = {round(het_test_p, 3)})")
    } else {
      cli::cli_alert_info("○ Heterogeneity test not significant (p = {round(het_test_p, 3)})")
    }
    
    if (include_targeting && het_sd > 0.1) {
      cli::cli_alert_success("✓ Targeting analysis indicates potential for policy differentiation")
    }
    
    cli::cli_rule("Workshop Complete")
    cli::cli_alert_success("All analyses completed successfully!")
    cli::cli_alert_info("Explore results using: workshop_results$baseline_results, workshop_results$causal_forest_results")
    if (include_plots) {
      cli::cli_alert_info("View plots using: workshop_results$selection_bias_plot, workshop_results$heterogeneity_plot")
    }
  }
  
  # Compile results
  results <- list(
    data = data,
    baseline_results = baseline_results,
    causal_forest_results = causal_forest_results
  )
  
  # Add plots if generated
  if (include_plots) {
    results <- c(results, plots_list)
  }
  
  return(results)
}


#' Check Workshop Prerequisites
#'
#' Verifies that all required packages are installed and provides 
#' installation guidance if needed.
#'
#' @param install_missing Logical. Whether to automatically install missing packages (default: FALSE)
#' @param include_optional Logical. Whether to check optional packages (default: TRUE)
#'
#' @return Logical. TRUE if all required packages are available
#'
#' @examples
#' # Check prerequisites
#' check_workshop_prerequisites()
#' 
#' # Install missing packages automatically
#' check_workshop_prerequisites(install_missing = TRUE)
#' 
#' @export
check_workshop_prerequisites <- function(install_missing = FALSE, include_optional = TRUE) {
  
  cli::cli_rule("Workshop Prerequisites Check")
  
  # Required packages
  required_packages <- c("cli", "dplyr", "grf", "ggplot2", "purrr", "scales", "tibble", "tidyr")
  
  # Optional packages  
  optional_packages <- c("margot", "policytree", "knitr", "rmarkdown")
  
  all_packages <- required_packages
  if (include_optional) {
    all_packages <- c(all_packages, optional_packages)
  }
  
  # Check each package
  missing_required <- character(0)
  missing_optional <- character(0)
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_required <- c(missing_required, pkg)
      cli::cli_alert_danger("{pkg} (required) - not installed")
    } else {
      version <- utils::packageVersion(pkg)
      cli::cli_alert_success("{pkg} v{version} - available")
    }
  }
  
  if (include_optional) {
    for (pkg in optional_packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        missing_optional <- c(missing_optional, pkg)
        cli::cli_alert_warning("{pkg} (optional) - not installed")
      } else {
        version <- utils::packageVersion(pkg)
        cli::cli_alert_success("{pkg} v{version} - available")
      }
    }
  }
  
  # R version check
  r_version <- R.Version()
  current_version <- paste(r_version$major, r_version$minor, sep = ".")
  
  if (as.numeric(r_version$major) >= 4) {
    cli::cli_alert_success("R {current_version} meets requirements (>= 4.0.0)")
  } else {
    cli::cli_alert_danger("R {current_version} insufficient - please upgrade to >= 4.0.0")
    return(FALSE)
  }
  
  # Install missing packages if requested
  if (install_missing && length(missing_required) > 0) {
    cli::cli_alert_info("Installing missing required packages...")
    utils::install.packages(missing_required)
    
    # Recheck
    still_missing <- character(0)
    for (pkg in missing_required) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        still_missing <- c(still_missing, pkg)
      }
    }
    
    if (length(still_missing) > 0) {
      cli::cli_alert_danger("Failed to install: {paste(still_missing, collapse = ', ')}")
      return(FALSE)
    } else {
      cli::cli_alert_success("All required packages installed successfully")
    }
  }
  
  # Final assessment
  if (length(missing_required) == 0) {
    cli::cli_alert_success("Workshop environment ready!")
    cli::cli_alert_info("Run run_workshop() to begin the analysis")
    return(TRUE)
  } else {
    cli::cli_alert_danger("Missing required packages: {paste(missing_required, collapse = ', ')}")
    cli::cli_alert_info("Install with: install.packages(c('{paste(missing_required, collapse = '\\'', \\'')}\'))")
    return(FALSE)
  }
}