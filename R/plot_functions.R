#' Plot Rate and Qini Curves for Targeting Analysis
#'
#' Creates visualizations of targeting performance using Rate and Qini curves
#' to evaluate the value of treatment effect heterogeneity for policy targeting.
#'
#' @param data A data frame with predicted treatment effects (tau_hat column)
#' @param tau_var Character. Name of predicted treatment effect variable (default: "tau_hat")
#' @param title Character. Plot title (default: "Targeting Performance Analysis")
#'
#' @return A list containing rate_plot, qini_plot, and efficiency_plot
#'
#' @details
#' Rate curves show the gain in average treatment effect achieved by targeting
#' individuals with the highest predicted effects, compared to random assignment.
#'
#' Qini curves show the cumulative gain from targeting, measuring the total
#' additional benefit obtained by targeting vs random treatment assignment.
#'
#' Both curves help evaluate whether treatment effect heterogeneity is sufficient
#' to warrant targeted interventions rather than universal treatment.
#'
#' @examples
#' # Generate data with predictions
#' cf_results <- workshop_causal_forest()
#' plots <- plot_rate_qini_curves(cf_results$predictions)
#'
#' # View individual plots
#' plots$rate_plot
#' plots$qini_plot
#' plots$efficiency_plot
#'
#' @export
plot_rate_qini_curves <- function(data, tau_var = "tau_hat",
                                 title = "Targeting Performance Analysis") {

  if (!tau_var %in% names(data)) {
    cli::cli_abort("Variable {tau_var} not found in data")
  }

  tau_hat <- data[[tau_var]]
  n <- length(tau_hat)

  # Order by predicted treatment effects (descending)
  tau_order <- order(tau_hat, decreasing = TRUE)

  # Calculate rate curve (targeting efficiency)
  rates <- seq(0.1, 1, by = 0.1)
  rate_results <- purrr::map_dfr(rates, function(r) {
    n_targeted <- floor(r * n)
    targeted_indices <- tau_order[1:n_targeted]

    avg_tau_targeted <- mean(tau_hat[targeted_indices])
    avg_tau_overall <- mean(tau_hat)
    gain <- avg_tau_targeted - avg_tau_overall

    tibble::tibble(
      rate = r,
      avg_tau_targeted = avg_tau_targeted,
      gain_over_random = gain
    )
  })

  # Calculate Qini curve (cumulative gain)
  percentiles <- seq(0.1, 1, by = 0.1)
  qini_results <- purrr::map_dfr(percentiles, function(p) {
    n_top <- floor(p * n)
    top_indices <- tau_order[1:n_top]

    targeted_benefit <- sum(tau_hat[top_indices])
    random_benefit <- p * sum(tau_hat)
    cum_gain <- targeted_benefit - random_benefit

    tibble::tibble(
      percentile = p,
      cumulative_gain = cum_gain
    )
  })

  # Rate curve plot
  rate_plot <- rate_results |>
    ggplot2::ggplot(ggplot2::aes(x = rate, y = gain_over_random)) +
    ggplot2::geom_line(color = "#E69F00", linewidth = 1) +
    ggplot2::geom_point(color = "#E69F00", size = 2) +
    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
      title = "Rate Curve: Targeting Efficiency",
      subtitle = "Gain over random assignment by targeting rate",
      x = "Targeting Rate",
      y = "Gain over Random Assignment"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12)
    )

  # Qini curve plot
  qini_plot <- qini_results |>
    ggplot2::ggplot(ggplot2::aes(x = percentile, y = cumulative_gain)) +
    ggplot2::geom_line(color = "#56B4E9", linewidth = 1) +
    ggplot2::geom_point(color = "#56B4E9", size = 2) +
    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
      title = "Qini Curve: Cumulative Targeting Gain",
      subtitle = "Total benefit gain from targeting vs random assignment",
      x = "Population Percentile",
      y = "Cumulative Gain"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12)
    )

  # Efficiency comparison
  top_10 <- tau_order[1:floor(0.1 * n)]
  top_20 <- tau_order[1:floor(0.2 * n)]
  top_50 <- tau_order[1:floor(0.5 * n)]

  efficiency_stats <- tibble::tibble(
    percentile = c("Top 10%", "Top 20%", "Top 50%"),
    avg_effect = c(
      mean(tau_hat[top_10]),
      mean(tau_hat[top_20]),
      mean(tau_hat[top_50])
    ),
    lift_vs_random = c(
      mean(tau_hat[top_10]) / mean(tau_hat),
      mean(tau_hat[top_20]) / mean(tau_hat),
      mean(tau_hat[top_50]) / mean(tau_hat)
    )
  ) |>
    dplyr::mutate(efficiency_gain = (lift_vs_random - 1) * 100)

  efficiency_plot <- efficiency_stats |>
    ggplot2::ggplot(ggplot2::aes(x = percentile, y = efficiency_gain)) +
    ggplot2::geom_col(fill = "#CC79A7", alpha = 0.8) +
    ggplot2::labs(
      title = "Targeting Efficiency by Percentile",
      subtitle = "Percentage improvement over random assignment",
      x = "Targeting Group",
      y = "Efficiency Gain (%)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  return(list(
    rate_plot = rate_plot,
    qini_plot = qini_plot,
    efficiency_plot = efficiency_plot,
    rate_data = rate_results,
    qini_data = qini_results,
    efficiency_data = efficiency_stats
  ))
}


#' Plot Treatment Effect Heterogeneity Distribution
#'
#' Creates a histogram showing the distribution of predicted individual
#' treatment effects from a causal forest.
#'
#' @param data A data frame with predicted treatment effects
#' @param tau_var Character. Name of treatment effect variable (default: "tau_hat")
#' @param title Character. Plot title
#' @param bins Integer. Number of histogram bins (default: 30)
#'
#' @return A ggplot object
#'
#' @examples
#' cf_results <- workshop_causal_forest()
#' plot_heterogeneity_distribution(cf_results$predictions)
#'
#' @export
plot_heterogeneity_distribution <- function(data, tau_var = "tau_hat",
                                          title = "Distribution of Individual Treatment Effects",
                                          bins = 30) {

  if (!tau_var %in% names(data)) {
    cli::cli_abort("Variable {tau_var} not found in data")
  }

  mean_tau <- mean(data[[tau_var]])
  sd_tau <- stats::sd(data[[tau_var]])

  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data[[tau_var]])) +
    ggplot2::geom_histogram(bins = bins, fill = "#69b3a2", alpha = 0.7, color = "white") +
    ggplot2::geom_vline(xintercept = mean_tau, color = "red", linetype = "dashed", size = 1) +
    ggplot2::labs(
      title = title,
      subtitle = glue::glue("Mean τ(x): {round(mean_tau, 3)}, SD: {round(sd_tau, 3)}"),
      x = "Predicted Treatment Effect τ(x)",
      y = "Count",
      caption = "Red line shows average treatment effect"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12)
    )

  return(p)
}


#' Plot Selection Bias Demonstration
#'
#' Creates side-by-side plots showing naive vs baseline-adjusted estimates
#' to demonstrate the importance of controlling for confounders.
#'
#' @param results_table A tibble with estimation results from workshop_baseline_adjustment
#' @param title Character. Plot title
#'
#' @return A ggplot object
#'
#' @examples
#' baseline_results <- workshop_baseline_adjustment()
#' plot_selection_bias(baseline_results$results_table)
#'
#' @export
plot_selection_bias <- function(results_table, title = "Selection Bias and Baseline Adjustment") {

  plot_data <- results_table |>
    tidyr::pivot_longer(
      cols = c(naive_estimate, adjusted_estimate),
      names_to = "method",
      values_to = "estimate"
    ) |>
    dplyr::mutate(
      method = dplyr::case_when(
        method == "naive_estimate" ~ "Naive",
        method == "adjusted_estimate" ~ "Baseline Adjusted"
      ),
      method = factor(method, levels = c("Naive", "Baseline Adjusted"))
    )

  p <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = method, y = estimate, fill = method)) +
    ggplot2::geom_col(alpha = 0.7) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = true_effect),
                       color = "red", linetype = "dashed", size = 1) +
    ggplot2::facet_wrap(~ outcome, scales = "free_y") +
    ggplot2::scale_fill_manual(values = c("Naive" = "#ff7f7f", "Baseline Adjusted" = "#7fbf7f")) +
    ggplot2::labs(
      title = title,
      subtitle = "Red line shows true treatment effect",
      x = "Estimation Method",
      y = "Estimated Treatment Effect",
      fill = "Method"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      legend.position = "bottom"
    )

  return(p)
}
