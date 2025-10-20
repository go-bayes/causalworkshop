#' Simulate Religious Belief and Prosocial Behaviour Data
#'
#' Generates realistic simulated data for studying causal effects of religious
#' belief on prosocial outcomes. Creates correlated baseline measures with
#' selection bias and heterogeneous treatment effects.
#'
#' @param n Integer. Sample size (default: 1000)
#' @param seed Integer. Random seed for reproducibility (default: 2025)
#' @param heterogeneous_effects Logical. Whether to include heterogeneous
#'   treatment effects (default: TRUE)
#' @param strong_selection Logical. Whether to include strong selection bias
#'   (default: TRUE)
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
#' - Selection bias (religious people differ systematically)
#' - Heterogeneous treatment effects (effects vary by individual characteristics)
#' - Realistic outcome distributions
#'
#' The simulation is designed for educational purposes to demonstrate:
#' - Selection bias in observational studies
#' - Importance of baseline adjustment
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

  # Create selection mechanism for treatment assignment
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


#' Simulate Data for Average Treatment Effect with Sample Weights
#'
#' Generates simulated data for studying average treatment effects with
#' sample weights to adjust for population differences.
#'
#' @param n_sample Integer. Sample size (default: 10000)
#' @param n_population Integer. Population size for comparison (default: 100000)
#' @param p_z_sample Numeric. Probability of effect modifier in sample (default: 0.1)
#' @param p_z_population Numeric. Probability of effect modifier in population (default: 0.5)
#' @param beta_a Numeric. Treatment effect (default: 1)
#' @param beta_z Numeric. Effect modifier coefficient (default: 2.5)
#' @param beta_az Numeric. Interaction effect (default: 0.5)
#' @param noise_sd Numeric. Outcome noise standard deviation (default: 0.5)
#' @param seed Integer. Random seed (default: 12345)
#'
#' @return A list containing sample_data and population_data tibbles
#'
#' @examples
#' data <- simulate_ate_data_with_weights()
#' head(data$sample_data)
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

  # Create sample data
  z_sample <- stats::rbinom(n_sample, 1, p_z_sample)
  a_sample <- stats::rbinom(n_sample, 1, 0.5)

  # Simulate outcome
  y_sample <- beta_a * a_sample + beta_z * z_sample +
              beta_az * (a_sample * z_sample) +
              stats::rnorm(n_sample, mean = 0, sd = noise_sd)

  # Create sample data frame
  sample_data <- tibble::tibble(
    y = y_sample,
    treatment = a_sample,
    effect_modifier = z_sample
  )

  # Simulate population data
  z_population <- stats::rbinom(n_population, 1, p_z_population)
  a_population <- stats::rbinom(n_population, 1, 0.5)
  y_population <- beta_a * a_population + beta_z * z_population +
                  beta_az * (a_population * z_population) +
                  stats::rnorm(n_population, mean = 0, sd = noise_sd)

  # Create population data frame
  population_data <- tibble::tibble(
    y = y_population,
    treatment = a_population,
    effect_modifier = z_population
  )

  # Calculate weights
  weight_z_1 <- p_z_population / p_z_sample
  weight_z_0 <- (1 - p_z_population) / (1 - p_z_sample)
  weights <- ifelse(z_sample == 1, weight_z_1, weight_z_0)

  # Add weights to sample data
  sample_data$weights <- weights

  return(list(sample_data = sample_data, population_data = population_data))
}
