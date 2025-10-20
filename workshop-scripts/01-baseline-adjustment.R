# Religious Belief and Prosocial Behaviour: Baseline Adjustment
# Demonstrates selection bias and covariate adjustment in causal inference
# Author: Joseph Bulbulia, Victoria University of Wellington

library(cli)
library(tidyverse)
library(here)

# Simulation blueprint -------------------------------------------------------
# - Five baseline covariates capture age, education, income, and prior prosociality.
# - Treatment (religious belief) follows a logistic propensity anchored on age,
#   education, and baseline charity: this induces confounding.
# - Charitable giving gains a heterogeneous boost from treatment: older people and
#   those already generous benefit more. Volunteering receives a constant boost.
# - We add outcome noise to keep the example realistic but still recoverable.

cli_rule("Religious Belief and Prosocial Behavior")

# simulation parameters
set.seed(2025)
n <- 20000

cli_alert_info("Simulating data for {n} observations")

# generate covariates X
# Columns correspond to: age, education, income, baseline charity donations,
# and baseline volunteering. All start standard normal to keep focus on structure.
X <- matrix(rnorm(n * 5), n, 5)
colnames(X) <- c("age", "education", "income", "baseline_charity", "baseline_volunteer")

# treatment assignment with confounding
# religious belief depends on baseline characteristics via logistic propensity
propensity_score <- plogis(0.2 * X[,1] - 0.3 * X[,2] + 0.4 * X[,4] + rnorm(n))
W <- rbinom(n, 1, propensity_score)

cli_alert_info("Treatment (religious belief) rate: {round(mean(W) * 100, 1)}%")

# define true treatment effects
tau_charity <- 0.25    # effect on charitable giving
tau_volunteer <- 0.4   # effect on volunteering hours

cli_alert_info("True causal effects:")
cli_alert_info("  Charitable giving: +{tau_charity} log units")
cli_alert_info("  Volunteering: +{tau_volunteer} hours per week")

# generate outcomes with heterogeneous treatment effects
# treatment effects vary by age and baseline charity: richer structure than ATE
tau_heterogeneous <- tau_charity + 0.3 * X[,1] + 0.2 * X[,4]  # age and baseline charity modify effects

noise_charity <- rnorm(n, 0, 0.5)
noise_volunteer <- rnorm(n, 0, 0.8)

Y_charity <- X[,4] + tau_heterogeneous * W + noise_charity
Y_volunteer <- X[,5] + tau_volunteer * W + noise_volunteer

# create analysis dataset
# The resulting tibble is used both for illustrating bias and for later scripts.
data <- tibble(
  belief_god_binary = W,
  age = X[,1],
  education = X[,2],
  income = X[,3],
  baseline_charity = X[,4],
  baseline_volunteer = X[,5],
  charity_outcome = Y_charity,
  volunteer_outcome = Y_volunteer
)

cli_rule("Estimation Results")

# naive estimation (biased due to selection)
naive_charity <- lm(charity_outcome ~ belief_god_binary, data = data)
naive_volunteer <- lm(volunteer_outcome ~ belief_god_binary, data = data)

# baseline-adjusted estimation (controls for confounding)
adjusted_charity <- lm(charity_outcome ~ belief_god_binary + baseline_charity, data = data)
adjusted_volunteer <- lm(volunteer_outcome ~ belief_god_binary + baseline_volunteer, data = data)

# extract estimates
results <- tibble(
  outcome = c("charity", "volunteer"),
  true_effect = c(tau_charity, tau_volunteer),
  naive_est = c(coef(naive_charity)["belief_god_binary"],
                coef(naive_volunteer)["belief_god_binary"]),
  adjusted_est = c(coef(adjusted_charity)["belief_god_binary"],
                   coef(adjusted_volunteer)["belief_god_binary"])
) %>%
  mutate(
    naive_bias = naive_est - true_effect,
    adjusted_bias = adjusted_est - true_effect
  )

cli_alert_info("Comparison of estimation approaches:")
print(results %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

# bias reduction
avg_bias_reduction <- mean(abs(results$naive_bias) - abs(results$adjusted_bias))
cli_alert_success("Average bias reduction: {round(avg_bias_reduction, 3)}")

# create data directory if needed
if (!dir.exists(here::here("data"))) {
  dir.create(here::here("data"), recursive = TRUE)
}

# save data for causal forest analysis
saveRDS(data, here::here("data", "religious_prosocial_data.rds"))
cli_alert_success("Data saved for heterogeneity analysis")

cli_rule()

hist(data$volunteer_outcome)
