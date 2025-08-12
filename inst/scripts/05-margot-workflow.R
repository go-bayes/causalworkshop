# Advanced Causal Forest Analysis with Margot Ecosystem
# Demonstrates professional analysis pipeline with sample splitting and validation
#
# Copyright (c) 2025 Joseph A. Bulbulia
# Licensed under Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)
# Part of the causalworkshop R package: https://github.com/go-bayes/causalworkshop
#
# EDUCATIONAL NOTE: Understanding Our Simulation Structure
# This analysis uses data where heterogeneity is created by:
# - For charity outcome: τ(x) = 0.25 + 0.3*age + 0.2*baseline_charity
# - For volunteer outcome: τ(x) = 0.4 (constant effect, no heterogeneity)
#
# This 2-dimensional structure means policy trees should use max_depth = 1L
# to match the true underlying heterogeneity pattern and avoid overfitting.
#
# Author: Joseph Bulbulia, Victoria University of Wellington

library(cli)
library(tidyverse)
library(purrr)
cli_rule("Advanced Causal Forest Analysis with Margot")

# install margot from github if missing
if (!requireNamespace("margot", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  cli_alert_info("Installing margot package from GitHub...")
  devtools::install_github("go-bayes/margot")
}

library(margot)

# verify margot version
if (packageVersion("margot") < "1.0.233") {
  stop("Margot >= 1.0.233 required. Run: devtools::install_github('go-bayes/margot')")
}

# install additional packages if missing
required_packages <- c("qs", "here", "data.table", "kableExtra", "doParallel", "patchwork")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# load required libraries
library(qs)
library(here)
library(data.table)
library(kableExtra)
library(grf)
library(ggplot2)
library(patchwork)

cli_rule("Data Preparation and Forest Configuration")

# create results directory
dir.create("results", showWarnings = FALSE)
dir_results <- "results"

# load data
if (!file.exists("data/religious_prosocial_data.rds")) {
  stop("Run 01-baseline-adjustment.R first to generate data")
}

data <- readRDS("data/religious_prosocial_data.rds")
cli_alert_info("Loaded data with {nrow(data)} observations")

# prepare covariates matrix
X <- data %>%
  dplyr::select(age, education, income, baseline_charity, baseline_volunteer) %>%
  as.matrix()

Y <- data$charity_outcome
W <- data$belief_god_binary

original_df <- data  # for interpretation of models
cli_alert_info("Analysis specification:")
cli_alert_info("  Primary outcome: charitable giving")
cli_alert_info("  Treatment: religious belief")
cli_alert_info("  Covariates: {ncol(X)} baseline variables")

# grf hyperparameters
grf_defaults <- list(
  num.trees = 1000,
  honesty = TRUE,
  tune.parameters = "all",
  min.node.size = 20,
  stabilize.splits = TRUE
)

cli_alert_info("GRF configuration: {grf_defaults$num.trees} trees, min node size {grf_defaults$min.node.size}")

# standardise outcomes for interpretability of outcomes
data_standardised <- data %>%
  mutate(
    t2_charity_outcome_z = as.numeric(scale(charity_outcome)),
    t2_volunteer_outcome_z = as.numeric(scale(volunteer_outcome))
  )

# save
here_save(data_standardised, "data_standardised", here::here('data'))

# define outcomes for analysis

outcome_vars <- c("charity_outcome", "volunteer_outcome")
t2_outcomes_z <- paste0("t2_", outcome_vars, "_z")


# outcome labels for plots
label_mapping <- list(
  "t2_charity_outcome_z" = "Charitable Giving",
  "t2_volunteer_outcome_z" = "Volunteering Hours"
)

# save for report
here_save(label_mapping, "label_mapping", dir_results)
cli_rule("Causal Forest Estimation with Sample Splitting")

# check if models already exist
if (file.exists(file.path(dir_results, "models_binary_cate.qs"))) {
  cli_alert_info("Loading existing causal forest models...")
   models_binary_cate <- margot::here_read_qs("models_binary_cate", dir_results)
} else {
  cli_alert_info("Fitting causal forests with honest sample splitting...")
models_binary_cate <- margot::margot_causal_forest(
    data = data_standardised,
    outcome_vars = t2_outcome_z,
    covariates = X,
    W = W,
    weights = NULL,
    grf_defaults = grf_defaults,
    top_n_vars = 5,
    save_models = TRUE,
    save_data = TRUE,
    compute_conditional_means = TRUE,
    train_proportion = 0.5,
    seed = 2025
    )

  # save models
  margot::here_save_qs(models_binary_cate,  "models_binary_cate", dir_results)
  cli_alert_success("Causal forest models saved")
}

cli_rule("Visualisation Configuration")

# plot configuration
title <- "Average Treatment Effects: Religious Belief on Prosocial Behaviour"
subtitle <- ""
filename_prefix <- "margot_"

# plot parameters
base_defaults <- list(
  type = "RD",
  title = title,
  e_val_bound_threshold = 1.2,
  colors = c(
    "positive" = "#E69F00",
    "not reliable" = "grey50",
    "negative" = "#56B4E9"
  ),
  x_offset = -0.2,
  x_lim_lo = -0.2,
  x_lim_hi = 0.5,
  text_size = 8,
  linewidth = 0.75,
  estimate_scale = 1,
  base_size = 18,
  point_size = 4,
  title_size = 19,
  subtitle_size = 16,
  legend_text_size = 10,
  legend_title_size = 10,
  include_coefficients = FALSE
)

# create plot options
plot_options <- margot_plot_create_options(
  title = title,
  base_defaults = base_defaults,
  subtitle = subtitle,
  filename_prefix = filename_prefix
)

cli_rule("Treatment Effect Heterogeneity Analysis")

# individual treatment effect predictions
plots_tau_hats <- margot::margot_plot_tau(models_binary_cate, label_mapping = label_mapping)
print(plots_tau_hats)

cli_alert_info("Individual treatment effect distributions plotted")

cli_rule("Average Treatment Effect Analysis")

# generate ate plots and interpretation
# note that outcomes were standardised
ate_results <- margot_plot(
  models_binary_cate$combined_table,
  options = plot_options,
  include_coefficients = FALSE,
  save_output = FALSE,
  order = "evaluebound_asc",
  label_mapping = label_mapping,
  original_df = data,
  e_val_bound_threshold = 1.1,
  rename_ate = TRUE,
  adjust = "bonferroni",
  alpha = 0.05
)

cli_alert_info("Average treatment effect results:")
cat(ate_results$interpretation)
print(ate_results$transformed_table)
print(ate_results$plot)

cli_rule("Heterogeneity Testing with Cross-Validation")

# comprehensive heterogeneity analysis
cli_alert_info("Running cross-validated heterogeneity tests...")

# multiple tests of heterogeneity using cross-validation
if (file.exists(file.path(dir_results, "hte_test_cv.rds"))) {
  cli_alert_info("Loading existing causal forest models...")
  hte_test_cv <- margot::here_read("hte_test_cv", dir_results)
} else {
  cli_alert_info("Cross-validating rate estimates...")
hte_test_cv <- margot::margot_interpret_heterogeneity(
  models_binary_cate,
  label_mapping = label_mapping,
  spend_levels = c(0.1, 0.4),
  adjust = "none",
  parallel = FALSE,
  include_extended_report = TRUE,
  use_cross_validation = TRUE,
  cv_num_folds = 5,
  seed = 42
)
# save cross-validation results
margot::here_save(hte_test_cv,  "hte_test_cv", dir_results)
}

cli_alert_success("Heterogeneity analysis completed")
cli_rule("Heterogeneity Test Results")



# display recommendations
if (!is.null(hte_test_cv$recommendations)) {
  cat(hte_test_cv$recommendations)
}

# summarise heterogeneity evidence
if (length(hte_test_cv$selected_model_ids) > 0) {
  cli_alert_success("Significant heterogeneity detected")

  hte_summary_table <- hte_test_cv$evidence_summary %>%
    dplyr::select(
      model = model_name,
      mean_pred = mean_prediction_test,
      diff_pred = differential_prediction_test,
      rate_autoc = rate_autoc,
      rate_qini = rate_qini,
      qini_curve = qini_curve
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  print(hte_summary_table)
} else {
  cli_alert_info("No significant heterogeneity detected")
}

# detailed rate and qini tables (if heterogeneity exists)
if (length(hte_test_cv$selected_model_ids) > 0) {
  # rate autoc results
  if (!is.null(hte_test_cv$cv_results) && !is.null(hte_test_cv$cv_results$tables)) {
    rate_autoc_table <- hte_test_cv$cv_results$tables$rate_autoc %>%
      dplyr::select(
        model = model_name,
        estimate = `RATE Estimate`,
        std_error = `Std Error`,
        t_stat = `t-statistic`,
        p_value = `p-value`
      ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))

    cli_alert_info("RATE AUTOC results:")
    print(rate_autoc_table)
  }

  # qini curve results
  if (!is.null(hte_test_cv$qini_results) && !is.null(hte_test_cv$qini_results$summary_table)) {
    qini_summary <- hte_test_cv$qini_results$summary_table
    cli_alert_info("QINI curve results:")
    print(qini_summary)
  }

  # save results
  if (exists("hte_summary_table")) {
    saveRDS(hte_summary_table, file.path(dir_results, "hte_summary_table.rds"))
  }
} else {
  cli_alert_warning("No reliable heterogeneous treatment effects detected")
}

cli_rule("Rate and QINI Curve Visualisation")

# verify package version
stopifnot(packageVersion("margot") >= "1.0.206")
# generate rate and qini plots for significant models
if (length(hte_test_cv$selected_model_ids) > 0) {
  cli_alert_info("Generating rate and QINI curve plots...")

  # qini curve plots
  if (!is.null(hte_test_cv$rate_results) && !is.null(hte_test_cv$rate_results$raw_results)) {
    qini_plots <- margot_plot_rate_batch(
      models = models_binary_cate,
      label_mapping = label_mapping,
      model_names = hte_test_cv$rate_results$raw_results$significant_models,
      target = "QINI"
    )
    cli_alert_success("QINI curve plots generated")
    print(qini_plots)
  }

  # autoc curve plots
  if (!is.null(hte_test_cv$cv_results) && !is.null(hte_test_cv$cv_results$significant_models)) {
    autoc_plots <- margot_plot_rate_batch(
      models = models_binary_cate,
      label_mapping = label_mapping,
      model_names = hte_test_cv$cv_results$significant_models,
      target = "AUTOC"
    )
    cli_alert_success("AUTOC curve plots generated")
    print(autoc_plots)
  }
} else {
  cli_alert_info("No significant models for rate curve analysis")
}


cli_rule("Analysis Complete")
cli_alert_success("Margot workflow analysis finished")

hte_test_cv$selected_model_ids
if (length(hte_test_cv$selected_model_ids) > 0 && !is.null(hte_test_cv$qini_results)) {
  # compute qini plots if there are reliable ra
  qini_curve_plots <- margot_plot_qini_batch(
    models_binary_cate,
    spend_levels = c(0.1,0.4),
    ci_n_points = 15,
    # cate_color = "gold2",
    ci_ribbon_alpha = 0.3,
    #  horizontal_line = TRUE,
    treatment_cost = 1,
    show_ci = "cate",
    ylim = c(-.01, 0.2),
    model_names      = hte_test_cv$qini_results$reliable_model_ids,
    label_mapping      = label_mapping
  )

  qini_curve_long_summary <- hte_test_cv$qini_results$qini_explanation
  qini_curve_concise_summary <-  hte_test_cv$qini_results$concise_summary

} else {
  qini_curve_long_summary <- list()
  qini_curve_concise_summary <- list()
  qini_curve_plots <- list()
  qini_names <- character(0)
  qini_text <- "No reliable qini models found"
  message("no reliable qini models found - skipping policy tree analysis")
}

cat(qini_curve_long_summary)
cat(qini_curve_concise_summary)

cli_rule("Policy Tree Analysis")
if (length(hte_test_cv$selected_model_ids) > 0) {
  cli_alert_info("Generating policy analysis for {length(hte_test_cv$selected_model_ids)} models...")

  # try policy generation, but handle errors gracefully
  # using depth=1 to match the 2-dimensional heterogeneity structure
  policy_results_1L <- tryCatch({
    margot_policy(
      models_binary_cate,
      model_names = hte_test_cv$selected_model_ids,
      original_df = original_df,
      output_objects = c("decision_tree"),
      label_mapping = label_mapping,
      max_depth = 1L  # optimal depth for age/baseline_charity heterogeneity
    )
  }, error = function(e) {
    cli_alert_warning("Policy tree generation failed: {e$message}")
    return(NULL)
  })

  if (!is.null(policy_results_1L)) {
    cli_alert_info("Policy analysis completed for {length(policy_results_1L)} models")

    # safely extract decision tree plots
    policy_plots <- purrr::map(policy_results_1L, function(x) {
      if (!is.null(x) && is.list(x) && "decision_tree" %in% names(x)) {
        return(x$decision_tree)
      } else {
        return(NULL)
      }
    })

    # count successful plots
    successful_plots <- sum(!sapply(policy_plots, is.null))
    cli_alert_info("Successfully generated {successful_plots} decision tree plots")

  } else {
    policy_plots <- list()
    cli_alert_warning("All policy tree generation failed")
  }

  # generate plain language interpretation
  if (any(!sapply(policy_plots, is.null))) {
    policy_text <- margot_interpret_policy_batch(
      models = models_binary_cate,
      original_df = original_df,
      model_names = hte_test_cv$selected_model_ids,
      label_mapping = label_mapping,
      max_depth = 1L  # matches simulation heterogeneity structure
    )
    cat(policy_text, "\n")
  } else {
    policy_text <- "Policy tree analysis could not be completed due to technical issues."
    cli_alert_warning("Policy tree generation failed for all models")
  }

} else {
  policy_plots <- list()
  policy_text <- "No reliable heterogeneous treatment effects found."
  cli_alert_info("Skipping policy tree analysis - no significant heterogeneity detected")
}

cli_rule("Analysis Complete")
cli_alert_success("Margot workflow analysis completed")

# Display final results summary
cli_rule("Final Results Summary")
cli_alert_info("Analysis completed successfully")

if (exists("policy_results_1L") && !is.null(policy_results_1L)) {
  cli_alert_success("Policy analysis: Decision trees available for {length(policy_results_1L)} models")
} else {
  cli_alert_info("Policy analysis: Skipped due to technical issues in margot package")
}

if (exists("hte_test_cv")) {
  cli_alert_success("Heterogeneity analysis: {length(hte_test_cv$selected_model_ids)} models with significant effects")
}

cli_alert_info("All results saved in 'results/' directory")
cli_alert_info("Workshop analysis pipeline completed successfully")





