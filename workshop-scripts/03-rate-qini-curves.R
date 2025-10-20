# Rate and Qini Curve Analysis
# Evaluates targeting performance for heterogeneous treatment effects
#
# EDUCATIONAL NOTE: Understanding the Simulation Structure
# In our simulated data, heterogeneity is primarily driven by two variables:
# - Age: Modifies treatment effects by +0.3 * age
# - Baseline charity: Modifies treatment effects by +0.2 * baseline_charity
# This creates a 2-dimensional heterogeneity structure, where simple splits
# (e.g., on age alone) capture most of the targeting value.
#
# Author: Joseph Bulbulia, Victoria University of Wellington

library(cli)
library(tidyverse)
library(grf)
library(here)

cli_rule("Rate and Qini Curve Analysis")

# load causal forest results
if (!file.exists(here::here("data", "causal_forest_results.rds"))) {
  stop("Run 01-baseline-adjustment.R and 02-causal-forest-analysis.R first")
}

results <- readRDS(here::here("data", "causal_forest_results.rds"))
cf <- results$causal_forest
data_with_tau <- results$predictions

cli_alert_info("Evaluating targeting performance with {nrow(data_with_tau)} observations")

# prepare data for rate and qini analysis
X <- data_with_tau %>%
  dplyr::select(age, education, income, baseline_charity, baseline_volunteer) %>%
  as.matrix()

Y <- data_with_tau$charity_outcome
W <- data_with_tau$belief_god_binary
tau_hat <- data_with_tau$tau_hat

cli_alert_info("Computing rate and qini curves...")

# EDUCATIONAL NOTE: Rate vs Qini Curves
# - Rate Curve: Shows efficiency gain from targeting (how much better than random)
# - Qini Curve: Shows cumulative gain from targeting (total additional benefit)
#
# In our 2-dimensional heterogeneity structure, we expect:
# - Strong gains from targeting the top 20-30% (high age/baseline charity)
# - Diminishing returns beyond that (limited remaining heterogeneity)

# compute rate curve
# ranks individuals by predicted treatment effects
n <- length(tau_hat)
tau_order <- order(tau_hat, decreasing = TRUE)

# calculate cumulative treatment effects at different targeting rates
rates <- seq(0.1, 1, by = 0.1)
rate_results <- tibble(
  rate = numeric(),
  avg_tau_targeted = numeric(),
  gain_over_random = numeric()
)

for (r in rates) {
  n_targeted <- floor(r * n)
  targeted_indices <- tau_order[1:n_targeted]

  avg_tau_targeted <- mean(tau_hat[targeted_indices])
  avg_tau_overall <- mean(tau_hat)
  gain <- avg_tau_targeted - avg_tau_overall

  rate_results <- bind_rows(rate_results,
                            tibble(
                              rate = r,
                              avg_tau_targeted = avg_tau_targeted,
                              gain_over_random = gain
                            )
  )
}

cli_alert_info("Rate curve analysis:")
print(rate_results %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

# qini curve analysis
# measures value of targeting vs random assignment
qini_results <- tibble(
  percentile = numeric(),
  qini_coefficient = numeric(),
  cumulative_gain = numeric()
)

percentiles <- seq(0.1, 1, by = 0.1)

for (p in percentiles) {
  n_top <- floor(p * n)
  top_indices <- tau_order[1:n_top]

  # qini coefficient: difference in outcomes between targeting and random
  qini_coeff <- mean(tau_hat[top_indices]) - mean(tau_hat)

  # cumulative gain from targeting
  cum_gain <- sum(tau_hat[top_indices]) - p * sum(tau_hat)

  qini_results <- bind_rows(qini_results,
                            tibble(
                              percentile = p,
                              qini_coefficient = qini_coeff,
                              cumulative_gain = cum_gain
                            )
  )
}

cli_alert_info("Qini curve analysis:")
print(qini_results %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

# calculate area under qini curve (AUQC)
auqc <- sum(qini_results$qini_coefficient) * 0.1  # trapezoidal approximation
cli_alert_info("Area Under Qini Curve (AUQC): {round(auqc, 3)}")

# targeting efficiency at key percentiles
cli_rule("Targeting Efficiency")

top_10 <- tau_order[1:floor(0.1 * n)]
top_20 <- tau_order[1:floor(0.2 * n)]
top_50 <- tau_order[1:floor(0.5 * n)]

efficiency_stats <- tibble(
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
) %>%
  mutate(
    efficiency_gain = (lift_vs_random - 1) * 100
  )

cli_alert_info("Targeting efficiency:")
print(efficiency_stats %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

# examine characteristics of high-benefit individuals
cli_rule("High-Benefit Population Characteristics")

high_benefit <- data_with_tau %>%
  slice_max(tau_hat, prop = 0.1) %>%
  summarise(
    avg_age = mean(age),
    avg_education = mean(education),
    avg_baseline_charity = mean(baseline_charity),
    avg_predicted_effect = mean(tau_hat)
  )

cli_alert_info("Top 10% predicted beneficiaries:")
cli_alert_info("  Average age: {round(high_benefit$avg_age, 2)} SD")
cli_alert_info("  Average education: {round(high_benefit$avg_education, 2)} SD")
cli_alert_info("  Average baseline charity: {round(high_benefit$avg_baseline_charity, 2)} SD")
cli_alert_info("  Average predicted effect: {round(high_benefit$avg_predicted_effect, 3)}")

# create separate visualisation data (don't combine different metrics)
rate_viz_data <- rate_results %>%
  dplyr::select(x = rate, y = gain_over_random) %>%
  mutate(curve = "Rate")

qini_viz_data <- qini_results %>%
  dplyr::select(x = percentile, y = cumulative_gain) %>%
  mutate(curve = "Qini")

# save results
targeting_analysis <- list(
  rate_curve = rate_results,
  qini_curve = qini_results,
  auqc = auqc,
  efficiency_stats = efficiency_stats,
  high_benefit_profile = high_benefit,
  rate_viz_data = rate_viz_data,
  qini_viz_data = qini_viz_data
)

# create data directory if needed
if (!dir.exists(here::here("data"))) {
  dir.create(here::here("data"), recursive = TRUE)
}

saveRDS(targeting_analysis, here::here("data", "targeting_analysis.rds"))
cli_alert_success("Rate and Qini analysis saved")

cli_rule("Targeting Performance Visualisation")

# plot rate curve
rate_plot <- rate_results %>%
  ggplot(aes(x = rate, y = gain_over_random)) +
  geom_line(color = "#E69F00", linewidth = 1) +
  geom_point(color = "#E69F00", size = 2) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Rate Curve: Targeting Efficiency",
    subtitle = "Gain over random assignment by targeting rate",
    x = "Targeting Rate",
    y = "Gain over Random Assignment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(rate_plot)

# plot qini curve (cumulative gain)
qini_plot <- qini_results %>%
  ggplot(aes(x = percentile, y = cumulative_gain)) +
  geom_line(color = "#56B4E9", linewidth = 1) +
  geom_point(color = "#56B4E9", size = 2) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Qini Curve: Cumulative Targeting Gain",
    subtitle = "Total benefit gain from targeting vs random assignment",
    x = "Population Percentile",
    y = "Cumulative Gain"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(qini_plot)

# efficiency comparison plot
efficiency_plot <- efficiency_stats %>%
  ggplot(aes(x = percentile, y = efficiency_gain)) +
  geom_col(fill = "#CC79A7", alpha = 0.8) +
  labs(
    title = "Targeting Efficiency by Percentile",
    subtitle = "Percentage improvement over random assignment",
    x = "Targeting Group",
    y = "Efficiency Gain (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(efficiency_plot)

cli_alert_success("Targeting performance plots generated")

cli_rule()

