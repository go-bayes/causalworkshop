# Heterogeneous Treatment Effects: Causal Forests
# Estimates conditional average treatment effects τ(x) for religious belief effects
# Author: Joseph Bulbulia, Victoria University of Wellington

library(cli)
library(tidyverse)
library(grf)
library(here)

cli_rule("Causal Forest Analysis")

# load data from baseline adjustment
if (!file.exists(here::here("data", "religious_prosocial_data.rds"))) {
  stop("Run 01-baseline-adjustment.R first")
}


data <- readRDS(here::here("data", "religious_prosocial_data.rds"))
cli_alert_info("Loaded data with {nrow(data)} observations")

# prepare data for causal forest
# focus on charitable giving outcome
X <- data %>%
  select(age, education, income, baseline_charity, baseline_volunteer) %>%
  as.matrix()

Y <- data$charity_outcome
W <- data$belief_god_binary

cli_alert_info("Forest specification:")
cli_alert_info("  Outcome: charitable giving")
cli_alert_info("  Treatment: religious belief")
cli_alert_info("  Covariates: {ncol(X)} baseline variables")

# fit causal forest
cli_alert_info("Fitting causal forest...")

cf <- causal_forest(
  X = X,
  Y = Y,
  W = W,
  num.trees = 1000,
  honesty = TRUE,
  tune.parameters = "all",
  seed = 2025
)

cli_alert_success("Forest fitted with {cf$`_num_trees`} trees")

# estimate average treatment effect
ate_est <- average_treatment_effect(cf)
cli_alert_info("Average treatment effect: {round(ate_est[1], 3)} (SE: {round(ate_est[2], 3)})")

# predict individual treatment effects τ(x)
tau_hat <- predict(cf)$predictions

cli_rule("Heterogeneity Analysis")

# summary of treatment effect variation
heterogeneity <- tibble(
  mean_tau = mean(tau_hat),
  sd_tau = sd(tau_hat),
  min_tau = min(tau_hat),
  max_tau = max(tau_hat)
)

cli_alert_info("Treatment effect heterogeneity:")
cli_alert_info("  Mean τ(x): {round(heterogeneity$mean_tau, 3)}")
cli_alert_info("  SD τ(x): {round(heterogeneity$sd_tau, 3)}")
cli_alert_info("  Range: [{round(heterogeneity$min_tau, 3)}, {round(heterogeneity$max_tau, 3)}]")

# test for heterogeneity
het_test <- test_calibration(cf)
cli_alert_info("Heterogeneity test p-value: {round(het_test[2], 3)}")

if (het_test[2] < 0.05) {
  cli_alert_success("Statistically significant treatment effect heterogeneity detected")
} else {
  cli_alert_warning("Limited evidence of heterogeneity")
}

# variable importance for heterogeneity
var_imp <- variable_importance(cf)
importance_df <- tibble(
  variable = colnames(X),
  importance = as.numeric(var_imp)
) %>%
  arrange(desc(importance))

cli_alert_info("Variable importance for heterogeneity:")
print(importance_df %>% mutate(importance = round(importance, 4)))

# examine high vs low benefit individuals
data_with_tau <- data %>%
  mutate(tau_hat = tau_hat)

high_benefit <- data_with_tau %>%
  slice_max(tau_hat, n = 5) %>%
  select(age, education, baseline_charity, tau_hat)

low_benefit <- data_with_tau %>%
  slice_min(tau_hat, n = 5) %>%
  select(age, education, baseline_charity, tau_hat)

cli_alert_info("Highest predicted effects:")
print(high_benefit %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

cli_alert_info("Lowest predicted effects:")
print(low_benefit %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

# save results
forest_results <- list(
  causal_forest = cf,
  predictions = data_with_tau,
  heterogeneity_stats = heterogeneity,
  variable_importance = importance_df
)

# create data directory if needed
if (!dir.exists(here::here("data"))) {
  dir.create(here::here("data"), recursive = TRUE)
}

saveRDS(forest_results, here::here("data", "causal_forest_results.rds"))
cli_alert_success("Results saved")

cli_rule()

