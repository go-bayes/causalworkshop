#' Simulate Religious Belief and Prosocial Behaviour Data
#'
#' Generates realistic simulated data for studying causal effects of religious
#' belief on prosocial outcomes. Creates correlated baseline measures with
#' observed confounding and heterogeneous treatment effects.
#'
#' @param n Integer. Sample size (default: 1000)
#' @param seed Integer. Random seed for reproducibility (default: 2025)
#' @param heterogeneous_effects Logical. Whether to include heterogeneous
#'   treatment effects (default: TRUE)
#' @param strong_selection Logical. Whether treatment assignment should depend
#'   strongly on observed baseline covariates (default: TRUE)
#'
#' @return A tibble with simulated data including:
#'   \describe{
#'     \item{belief_god_binary}{Binary treatment variable (religious belief)}
#'     \item{age, education, income}{Baseline covariates}
#'     \item{baseline_charity, baseline_volunteer}{Baseline outcome measures}
#'     \item{charity_outcome, volunteer_outcome}{Primary outcomes}
#'     \item{tau_charity, tau_volunteer}{True individual treatment effects}
#'   }
#'
#' @details
#' This function simulates realistic data with:
#' - Correlated baseline characteristics
#' - Treatment assignment that depends on baseline covariates
#' - Heterogeneous treatment effects (effects vary by individual characteristics)
#' - Realistic outcome distributions
#'
#' The simulation is designed for educational purposes to demonstrate:
#' - Bias from observed pre-treatment confounding in observational studies
#' - How baseline adjustment can reduce bias under measured confounding
#' - Heterogeneous treatment effects
#' - Policy targeting applications
#'
#' @examples
#' # Basic simulation
#' data <- simulate_religious_data()
#'
#' # Larger sample with homogeneous effects
#' data <- simulate_religious_data(n = 5000, heterogeneous_effects = FALSE)
#'
#' # Check treatment assignment
#' table(data$belief_god_binary)
#'
#' @export
simulate_religious_data <- function(n = 1000, seed = 2025,
                                   heterogeneous_effects = TRUE,
                                   strong_selection = TRUE) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  cli::cli_alert_info("Simulating data for {n} observations")

  # Generate baseline covariates
  X <- matrix(stats::rnorm(n * 5), n, 5)
  colnames(X) <- c("age", "education", "income", "baseline_charity", "baseline_volunteer")

  # Create treatment assignment mechanism from observed baseline covariates
  if (strong_selection) {
    propensity_score <- stats::plogis(
      0.2 * X[,1] - 0.3 * X[,2] + 0.4 * X[,4] + stats::rnorm(n)
    )
  } else {
    propensity_score <- rep(0.5, n)  # Random assignment
  }

  W <- stats::rbinom(n, 1, propensity_score)

  # Define treatment effects
  if (heterogeneous_effects) {
    # Treatment effects vary by age and baseline charity
    tau_charity <- 0.25 + 0.3 * X[,1] + 0.2 * X[,4]
    tau_volunteer <- 0.4 + 0.2 * X[,1] + 0.1 * X[,4]
  } else {
    # Homogeneous effects
    tau_charity <- rep(0.25, n)
    tau_volunteer <- rep(0.4, n)
  }

  # Generate outcomes
  charity_outcome <- X[,4] + tau_charity * W + stats::rnorm(n, 0, 0.5)
  volunteer_outcome <- X[,5] + tau_volunteer * W + stats::rnorm(n, 0, 0.8)

  # Create analysis dataset
  data <- tibble::tibble(
    belief_god_binary = W,
    age = X[,1],
    education = X[,2],
    income = X[,3],
    baseline_charity = X[,4],
    baseline_volunteer = X[,5],
    charity_outcome = charity_outcome,
    volunteer_outcome = volunteer_outcome,
    tau_charity = tau_charity,
    tau_volunteer = tau_volunteer
  )

  cli::cli_alert_success("Data simulation complete")
  cli::cli_alert_info("Treatment rate: {round(mean(W) * 100, 1)}%")

  return(data)
}


#' Simulate Data for Average Treatment Effect (ATE) with Sample Weights
#'
#' Generates simulated data for a sample and a population to study average
#' treatment effects (ATE), considering the presence of an effect modifier.
#' The sample and the population differ in the distribution of the effect
#' modifier; the treatment effect and effect modification are held fixed.
#' Inverse-probability sample weights are returned so that the sample can be
#' reweighted to match the population.
#'
#' This function mirrors `margot::simulate_ate_data_with_weights()` exactly so
#' that course materials remain consistent across packages.
#'
#' @param n_sample Integer. Sample size (default: 10000).
#' @param n_population Integer. Population size for comparison (default: 100000).
#' @param p_z_sample Numeric. Probability of the effect modifier in the
#'   sample (default: 0.1).
#' @param p_z_population Numeric. Probability of the effect modifier in the
#'   population (default: 0.5).
#' @param beta_a Numeric. Treatment effect (default: 1).
#' @param beta_z Numeric. Effect-modifier coefficient (default: 2.5).
#' @param beta_az Numeric. Interaction term capturing effect modification of
#'   treatment by the modifier (default: 0.5).
#' @param noise_sd Numeric. Outcome noise standard deviation (default: 0.5).
#' @param seed Integer. Random seed for reproducibility (default: 12345).
#'   Pass `NULL` to leave the seed untouched.
#'
#' @return A list with two data frames: `sample_data` (`y_sample`, `a_sample`,
#'   `z_sample`, `weights`) and `population_data` (`y_population`,
#'   `a_population`, `z_population`).
#'
#' @examples
#' data <- simulate_ate_data_with_weights()
#' head(data$sample_data)
#' head(data$population_data)
#'
#' @export
simulate_ate_data_with_weights <- function(n_sample = 10000,
                                           n_population = 100000,
                                           p_z_sample = 0.1,
                                           p_z_population = 0.5,
                                           beta_a = 1,
                                           beta_z = 2.5,
                                           beta_az = 0.5,
                                           noise_sd = 0.5,
                                           seed = 12345) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # sample data
  z_sample <- stats::rbinom(n_sample, 1, p_z_sample)
  a_sample <- stats::rbinom(n_sample, 1, 0.5)
  y_sample <- beta_a * a_sample + beta_z * z_sample +
    beta_az * (a_sample * z_sample) +
    stats::rnorm(n_sample, mean = 0, sd = noise_sd)
  sample_data <- data.frame(y_sample, a_sample, z_sample)

  # population data: same treatment effect, different distribution of z
  z_population <- stats::rbinom(n_population, 1, p_z_population)
  a_population <- stats::rbinom(n_population, 1, 0.5)
  y_population <- beta_a * a_population + beta_z * z_population +
    beta_az * (a_population * z_population) +
    stats::rnorm(n_population, mean = 0, sd = noise_sd)
  population_data <- data.frame(y_population, a_population, z_population)

  # inverse-probability weights to map sample distribution of z to population
  weight_z_1 <- p_z_population / p_z_sample
  weight_z_0 <- (1 - p_z_population) / (1 - p_z_sample)
  weights <- ifelse(z_sample == 1, weight_z_1, weight_z_0)
  sample_data$weights <- weights

  list(sample_data = sample_data, population_data = population_data)
}


#' Simulate Cross-Sectional Mediation Example
#'
#' Generates cross-sectional data in which `L` is a mediator on the path
#' `A -> L -> Y`. With cross-sectional data alone, the analyst cannot
#' distinguish a fork (`A <- L -> Y`) from a chain (`A -> L -> Y`); the two
#' produce the same observed associations. This simulation makes the
#' ambiguity concrete by generating data from the chain and inviting analyses
#' that condition on `L` (incorrectly treating it as a confounder) versus
#' omitting `L` (correctly treating it as a mediator and recovering the total
#' effect of `A` on `Y`).
#'
#' @param n Integer. Sample size (default: 1000).
#' @param p Numeric. Probability that `A = 1` (default: 0.5).
#' @param alpha Numeric. Intercept for `L` (default: 0).
#' @param beta Numeric. Effect of `A` on `L` (default: 2).
#' @param gamma Numeric. Intercept for `Y` (default: 1).
#' @param delta Numeric. Effect of `L` on `Y` (default: 1.5).
#' @param sigma_L Numeric. Standard deviation of noise on `L` (default: 1).
#' @param sigma_Y Numeric. Standard deviation of noise on `Y` (default: 1.5).
#' @param seed Integer. Random seed for reproducibility (default: 123).
#'   Pass `NULL` to leave the seed untouched.
#'
#' @return A data frame with columns `A` (binary treatment), `L` (mediator),
#'   and `Y` (outcome).
#'
#' @details
#' The data-generating process is:
#'   - `A ~ Bernoulli(p)`
#'   - `L = alpha + beta * A + N(0, sigma_L)`
#'   - `Y = gamma + delta * L + N(0, sigma_Y)`
#'
#' so the total effect of `A` on `Y` is `beta * delta`.
#'
#' @examples
#' data <- simulate_mediation_example()
#' fit_total <- lm(Y ~ A, data = data)             # correct: total effect
#' fit_blocked <- lm(Y ~ A + L, data = data)        # blocks the mediator
#' coef(fit_total)["A"]
#' coef(fit_blocked)["A"]
#'
#' @export
simulate_mediation_example <- function(n = 1000, p = 0.5,
                                       alpha = 0, beta = 2,
                                       gamma = 1, delta = 1.5,
                                       sigma_L = 1, sigma_Y = 1.5,
                                       seed = 123) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  A <- stats::rbinom(n, 1, p)
  L <- alpha + beta * A + stats::rnorm(n, 0, sigma_L)
  Y <- gamma + delta * L + stats::rnorm(n, 0, sigma_Y)

  data.frame(A = A, L = L, Y = Y)
}


#' Simulate a Three-Wave Panel for Confounding-Control Demonstrations
#'
#' Generates a synthetic three-wave panel (baseline, exposure wave, outcome
#' wave) used to demonstrate confounding-control strategies and causal-forest
#' estimation in the simulation guide. Treatment assignment depends on the
#' baseline covariate `L_0`, the prior exposure `A_0`, the prior outcome
#' `Y_0`, and an unmeasured confounder `U`. The continuous outcome `Y_2`
#' depends on `A_1` (the focal treatment), all three baseline measures, an
#' interaction among `Y_0`, `A_0`, and `L_0`, and `U`. The true average
#' treatment effect of `A_1` on `Y_2` is `delta_A1`.
#'
#' @param n Integer. Sample size (default: 10000).
#' @param beta_A0,beta_Y0,beta_L0,beta_U Numeric. Coefficients in the
#'   logistic model for `A_1` on `A_0`, `Y_0`, `L_0`, and `U` (defaults:
#'   0.25, 0.3, 0.2, 0.1).
#' @param delta_A1,delta_Y0,delta_A0,delta_L0,delta_U Numeric. Coefficients
#'   in the linear model for `Y_2` on `A_1`, `Y_0`, `A_0`, `L_0`, and `U`
#'   (defaults: 0.3, 0.9, 0.1, 0.3, 0.05).
#' @param theta_A0Y0L0 Numeric. Three-way interaction coefficient on
#'   `Y_0 * A_0 * L_0` in the outcome model (default: 0.5).
#' @param sigma_Y2 Numeric. Standard deviation of noise on `Y_2` (default:
#'   0.5).
#' @param seed Integer. Random seed for reproducibility (default: 123).
#'   Pass `NULL` to leave the seed untouched.
#'
#' @return A data frame with columns `Y_2` (outcome), `A_0` (baseline
#'   treatment), `A_1` (focal treatment), `L_0` (baseline covariate), `Y_0`
#'   (baseline outcome), and `U` (unmeasured confounder).
#'
#' @details
#' The data-generating process is:
#'   - `U ~ N(0, 1)`
#'   - `A_0 ~ Bernoulli(plogis(U))`
#'   - `Y_0 ~ N(U, 1)` and `L_0 ~ N(U, 1)`
#'   - `A_1 ~ Bernoulli(plogis(-0.5 + beta_A0*A_0 + beta_Y0*Y_0 + beta_L0*L_0 + beta_U*U))`
#'   - `Y_2 ~ N(delta_A1*A_1 + delta_Y0*Y_0 + delta_A0*A_0 + delta_L0*L_0 + theta_A0Y0L0*Y_0*A_0*L_0 + delta_U*U, sigma_Y2)`
#'
#' The true ATE of `A_1` on `Y_2` is `delta_A1` (default 0.3). `U` is
#' included so analysts can compare the bias from omitting it with the bias
#' from omitting other components of the adjustment set.
#'
#' @examples
#' data <- simulate_three_wave_panel()
#' fit_no_control <- lm(Y_2 ~ A_1, data = data)
#' fit_interaction <- lm(Y_2 ~ A_1 * (L_0 + A_0 + Y_0), data = data)
#' coef(fit_no_control)["A_1"]
#' coef(fit_interaction)["A_1"]
#'
#' @export
simulate_three_wave_panel <- function(n = 10000,
                                      beta_A0 = 0.25, beta_Y0 = 0.3,
                                      beta_L0 = 0.2, beta_U = 0.1,
                                      delta_A1 = 0.3, delta_Y0 = 0.9,
                                      delta_A0 = 0.1, delta_L0 = 0.3,
                                      theta_A0Y0L0 = 0.5, delta_U = 0.05,
                                      sigma_Y2 = 0.5,
                                      seed = 123) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  U <- stats::rnorm(n)
  A_0 <- stats::rbinom(n, 1, prob = stats::plogis(U))
  Y_0 <- stats::rnorm(n, mean = U, sd = 1)
  L_0 <- stats::rnorm(n, mean = U, sd = 1)

  A_1 <- stats::rbinom(n, 1, prob = stats::plogis(
    -0.5 + beta_A0 * A_0 + beta_Y0 * Y_0 + beta_L0 * L_0 + beta_U * U
  ))

  Y_2 <- stats::rnorm(n,
    mean = delta_A1 * A_1 + delta_Y0 * Y_0 + delta_A0 * A_0 +
      delta_L0 * L_0 + theta_A0Y0L0 * Y_0 * A_0 * L_0 + delta_U * U,
    sd = sigma_Y2
  )

  data.frame(Y_2 = Y_2, A_0 = A_0, A_1 = A_1, L_0 = L_0, Y_0 = Y_0, U = U)
}
