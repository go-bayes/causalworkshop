#' Simulate Data with Non-Linear Heterogeneous Treatment Effects
#'
#' Generates data with randomised treatment assignment and a highly non-linear
#' treatment effect surface. Because treatment is randomised, confounding is
#' absent: any estimation error comes purely from functional form
#' misspecification. This makes the data ideal for comparing estimation methods
#' (OLS, polynomial regression, GAM, causal forest) and demonstrating why
#' flexible methods matter for heterogeneous effects.
#'
#' @param n Integer. Sample size (default: 2000).
#' @param seed Integer or NULL. Random seed for reproducibility (default: 2026).
#'   Set to NULL to skip setting the seed.
#'
#' @return A tibble with `n` rows and the following columns:
#'   \describe{
#'     \item{id}{Integer. Observation identifier.}
#'     \item{x1}{Numeric. Continuous covariate (standard normal).}
#'     \item{x2}{Numeric. Continuous covariate (standard normal).}
#'     \item{x3}{Integer. Binary covariate (Bernoulli, p = 0.5).}
#'     \item{treatment}{Integer. Randomised binary treatment (Bernoulli,
#'       p = 0.5). No confounding by design.}
#'     \item{outcome}{Numeric. Observed outcome.}
#'     \item{tau_true}{Numeric. True individual treatment effect.}
#'   }
#'
#' @details
#'
#' ## Data-generating process
#'
#' The true treatment effect varies across individuals as:
#'
#' \deqn{\tau(x) = 0.3 + 0.8 \sin(2 x_1) + 0.5 \max(x_2, 0)^2 -
#'   0.4 \, x_1 \, x_2 \, I(x_3 = 1)}
#'
#' This surface combines three features that standard regression cannot
#' capture: a sinusoidal dependence on \eqn{x_1}, a rectified quadratic in
#' \eqn{x_2} (zero for negative values, accelerating for positive), and a
#' three-way interaction modulated by the binary \eqn{x_3}.
#'
#' The baseline outcome (under control) is:
#'
#' \deqn{\mu_0(x) = 1.0 + 0.8 \, x_1 - 0.3 \, x_2^2 + 0.5 \, x_3}
#'
#' The observed outcome is \eqn{Y = \mu_0(x) + \tau(x) \cdot A +
#'   \varepsilon}, where \eqn{A} is the randomised treatment and
#'   \eqn{\varepsilon \sim N(0, 0.5)}.
#'
#' ## Pedagogical purpose
#'
#' With no confounding, the average treatment effect (ATE) is recoverable
#' by a simple difference in means. The challenge is recovering the
#' \emph{individual-level} effect surface \eqn{\tau(x)}. OLS with linear
#' interactions collapses heterogeneity to a plane; polynomial regression
#' and GAMs improve progressively; causal forests recover the full
#' non-linear surface.
#'
#' Use [compare_ate_methods()] to fit all four methods and visualise the
#' comparison.
#'
#' @examples
#' d <- simulate_nonlinear_data(n = 500)
#' mean(d$tau_true)
#'
#' # no confounding: difference in means recovers ATE
#' mean(d$outcome[d$treatment == 1]) - mean(d$outcome[d$treatment == 0])
#'
#' @seealso [compare_ate_methods()]
#'
#' @export
simulate_nonlinear_data <- function(n = 2000, seed = 2026) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  cli::cli_alert_info("Simulating non-linear treatment effect data for {n} observations")

  # covariates
  x1 <- stats::rnorm(n, 0, 1)
  x2 <- stats::rnorm(n, 0, 1)
  x3 <- stats::rbinom(n, 1, 0.5)

  # randomised treatment (no confounding)
  treatment <- stats::rbinom(n, 1, 0.5)

  # true individual treatment effect (highly non-linear)
  tau_true <- 0.3 +
    0.8 * sin(2 * x1) +
    0.5 * pmax(x2, 0)^2 -
    0.4 * x1 * x2 * x3

  # baseline outcome under control
  mu0 <- 1.0 + 0.8 * x1 - 0.3 * x2^2 + 0.5 * x3

  # observed outcome
  outcome <- mu0 + tau_true * treatment + stats::rnorm(n, 0, 0.5)

  data <- tibble::tibble(
    id = seq_len(n),
    x1 = x1,
    x2 = x2,
    x3 = x3,
    treatment = treatment,
    outcome = outcome,
    tau_true = tau_true
  )

  cli::cli_alert_success("Generated {n} observations (treatment rate: {round(mean(treatment) * 100, 1)}%)")
  cli::cli_alert_info("True ATE: {round(mean(tau_true), 3)}")

  data
}
