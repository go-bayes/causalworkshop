# Policy Tree Analysis
# Learns interpretable treatment assignment rules from causal forests
# Author: Joseph Bulbulia, Victoria University of Wellington

library(cli)
library(tidyverse)
library(grf)
library(policytree)
library(here)

cli_rule("Policy Tree Analysis")

# load causal forest results
if (!file.exists(here::here("data", "causal_forest_results.rds"))) {
  stop("Run previous analysis scripts first")
}

results <- readRDS(here::here("data", "causal_forest_results.rds"))
cf <- results$causal_forest
data_with_tau <- results$predictions

cli_alert_info("Learning policy trees from {nrow(data_with_tau)} observations")

# prepare data for policy tree
X <- data_with_tau %>%
  dplyr::select(age, education, income, baseline_charity, baseline_volunteer) %>%
  as.matrix()

Y <- data_with_tau$charity_outcome
W <- data_with_tau$belief_god_binary
tau_hat <- data_with_tau$tau_hat

# create reward matrix for policy learning
# use tau_hat directly as treatment benefit
gamma_matrix <- cbind(
  control = rep(0, length(tau_hat)),    # control baseline reward = 0
  treatment = tau_hat                   # treatment reward = predicted effect
)

cli_alert_info("Reward matrix created: {nrow(gamma_matrix)} x {ncol(gamma_matrix)}")

cli_alert_info("Learning optimal treatment assignment policy...")

# learn policy tree with different depths
# use smaller sample for faster computation
n_sample <- min(500, nrow(X))
sample_idx <- sample(nrow(X), n_sample)

X_sample <- X[sample_idx, ]
gamma_sample <- gamma_matrix[sample_idx, ]

# convert X_sample to data frame
X_sample <- as.data.frame(X_sample)

policy_depth_1 <- policy_tree(X_sample, gamma_sample, depth = 1)
policy_depth_2 <- policy_tree(X_sample, gamma_sample, depth = 2)

cli_alert_success("Policy trees learned")

# evaluate policy performance
cli_rule("Policy Tree Evaluation")

# predict treatment assignments on full sample
X_df <- as.data.frame(X)
policy_1_actions <- predict(policy_depth_1, X_df)
policy_2_actions <- predict(policy_depth_2, X_df)

# calculate value of each policy
# value = expected outcome under learned policy vs random assignment
value_random <- mean(gamma_matrix[, 1] * 0.5 + gamma_matrix[, 2] * 0.5)

# policy_tree returns 1-indexed actions, convert to match our matrix indexing
policy_1_rewards <- ifelse(policy_1_actions == 1, gamma_matrix[, 1], gamma_matrix[, 2])
policy_2_rewards <- ifelse(policy_2_actions == 1, gamma_matrix[, 1], gamma_matrix[, 2])

value_policy_1 <- mean(policy_1_rewards)
value_policy_2 <- mean(policy_2_rewards)

policy_performance <- tibble(
  policy = c("Random", "Depth 1", "Depth 2"),
  expected_value = c(value_random, value_policy_1, value_policy_2),
  improvement = c(0, value_policy_1 - value_random, value_policy_2 - value_random)
)

cli_alert_info("Policy performance comparison:")
print(policy_performance %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

# examine policy assignments
cli_rule("Treatment Assignment Analysis")

assignment_summary <- tibble(
  policy_depth_1 = policy_1_actions,
  policy_depth_2 = policy_2_actions,
  predicted_tau = tau_hat
) %>%
  mutate(
    actual_treatment = W,
    depth_1_assign = ifelse(policy_depth_1 == 1, "Treat", "Control"),
    depth_2_assign = ifelse(policy_depth_2 == 1, "Treat", "Control")
  )

# assignment rates
assign_rates <- assignment_summary %>%
  summarise(
    depth_1_treat_rate = mean(policy_depth_1),
    depth_2_treat_rate = mean(policy_depth_2),
    actual_treat_rate = mean(actual_treatment)
  )

cli_alert_info("Treatment assignment rates:")
cli_alert_info("  Depth 1 policy: {round(assign_rates$depth_1_treat_rate * 100, 1)}%")
cli_alert_info("  Depth 2 policy: {round(assign_rates$depth_2_treat_rate * 100, 1)}%")
cli_alert_info("  Observed data: {round(assign_rates$actual_treat_rate * 100, 1)}%")

# policy tree structure
cli_rule("Policy Tree Structure")

cli_alert_info("Depth 1 policy tree:")
print(policy_depth_1)
plot(policy_depth_1)

cli_alert_info("Depth 2 policy tree:")
print(policy_depth_2)
plot(policy_depth_2)

# examine disagreement between policies
disagreement <- assignment_summary %>%
  mutate(policies_agree = policy_depth_1 == policy_depth_2) %>%
  summarise(agreement_rate = mean(policies_agree))

cli_alert_info("Policy agreement rate: {round(disagreement$agreement_rate * 100, 1)}%")

# characteristics of treatment-assigned individuals
cli_rule("Policy Assignment Characteristics")

# for depth 2 policy
depth_2_treated <- data_with_tau %>%
  filter(policy_2_actions == 1) %>%
  summarise(
    n_treated = n(),
    avg_age = mean(age),
    avg_baseline_charity = mean(baseline_charity),
    avg_predicted_tau = mean(tau_hat)
  )

depth_2_control <- data_with_tau %>%
  filter(policy_2_actions == 0) %>%
  summarise(
    n_control = n(),
    avg_age = mean(age),
    avg_baseline_charity = mean(baseline_charity),
    avg_predicted_tau = mean(tau_hat)
  )

cli_alert_info("Depth 2 policy assignments:")
cli_alert_info("  Treated group (n={depth_2_treated$n_treated}):")
cli_alert_info("    Avg age: {round(depth_2_treated$avg_age, 2)}")
cli_alert_info("    Avg baseline charity: {round(depth_2_treated$avg_baseline_charity, 2)}")
cli_alert_info("    Avg predicted τ(x): {round(depth_2_treated$avg_predicted_tau, 3)}")
cli_alert_info("  Control group (n={depth_2_control$n_control}):")
cli_alert_info("    Avg age: {round(depth_2_control$avg_age, 2)}")
cli_alert_info("    Avg baseline charity: {round(depth_2_control$avg_baseline_charity, 2)}")
cli_alert_info("    Avg predicted τ(x): {round(depth_2_control$avg_predicted_tau, 3)}")

# policy regret analysis
# regret = loss from not following optimal individual treatment
individual_regret_2 <- assignment_summary %>%
  mutate(
    optimal_action = ifelse(predicted_tau > 0, 1, 0),
    regret = abs(policy_depth_2 - optimal_action) * abs(predicted_tau)
  ) %>%
  summarise(avg_regret = mean(regret))

individual_regret_1 <- assignment_summary %>%
  mutate(
    optimal_action = ifelse(predicted_tau > 0, 1, 0),
    regret = abs(policy_depth_1 - optimal_action) * abs(predicted_tau)
  ) %>%
  summarise(avg_regret = mean(regret))

cli_rule("Policy Regret Analysis")
cli_alert_info("Average regret (vs individual optimal):")
cli_alert_info("  Depth 2 policy: {round(individual_regret_2$avg_regret, 3)}")
cli_alert_info("  Depth 3 policy: {round(individual_regret_3$avg_regret, 3)}")

# save policy analysis
policy_analysis <- list(
  policy_depth_1 = policy_depth_1,
  policy_depth_2 = policy_depth_2,
  performance = policy_performance,
  assignments = assignment_summary,
  regret_analysis = list(depth_1 = individual_regret_1, depth_2 = individual_regret_2)
)

# create data directory if needed
if (!dir.exists(here::here("data"))) {
  dir.create(here::here("data"), recursive = TRUE)
}

saveRDS(policy_analysis, here::here("data", "policy_analysis.rds"))
cli_alert_success("Policy tree analysis saved")

cli_rule()

