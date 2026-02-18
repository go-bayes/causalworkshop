#' Simulate Multi-Item Scale Data with Measurement Non-Invariance
#'
#' Generates synthetic multi-item psychological distress scale data with a
#' known factor structure and built-in measurement non-invariance across two
#' groups. Suitable for teaching exploratory factor analysis (EFA),
#' confirmatory factor analysis (CFA), and multigroup CFA invariance testing.
#'
#' @param n Integer. Number of observations to simulate (default: 2000).
#'   Observations are split approximately equally across two groups.
#' @param seed Integer or NULL. Random seed for reproducibility (default: 2026).
#'   Set to NULL to skip setting the seed.
#'
#' @return A tibble with `n` rows and the following columns:
#'
#'   **Identifiers and demographics:**
#'   \describe{
#'     \item{id}{Integer. Unique participant identifier.}
#'     \item{group}{Integer. Binary group indicator (0 or 1).}
#'     \item{age}{Numeric. Standardised age.}
#'     \item{male}{Integer. Binary indicator (1 = male).}
#'   }
#'
#'   **Scale items:**
#'   \describe{
#'     \item{item_1}{Integer. "Nervous" (1-5 scale).}
#'     \item{item_2}{Integer. "Hopeless" (1-5 scale).}
#'     \item{item_3}{Integer. "Restless" (1-5 scale).}
#'     \item{item_4}{Integer. "Depressed" (1-5 scale).}
#'     \item{item_5}{Integer. "Effort" (1-5 scale).}
#'     \item{item_6}{Integer. "Worthless" (1-5 scale).}
#'   }
#'
#'   **Ground truth:**
#'   \describe{
#'     \item{true_distress}{Numeric. Latent distress score used to generate
#'       item responses.}
#'   }
#'
#'   The returned tibble also carries three attributes:
#'   \describe{
#'     \item{true_loadings}{Numeric vector of length 6. The true factor
#'       loadings used in the data-generating process.}
#'     \item{true_intercepts_group0}{Numeric vector of length 6. Item
#'       intercepts for group 0.}
#'     \item{true_intercepts_group1}{Numeric vector of length 6. Item
#'       intercepts for group 1. Items 3 and 5 differ from group 0
#'       (partial scalar non-invariance).}
#'   }
#'
#' @details
#'
#' ## Data-generating process
#'
#' Each observation is assigned to one of two groups (approximately 50/50
#' split). A latent distress score is drawn from a standard normal
#' distribution, shifted slightly by group membership to create a small
#' mean difference.
#'
#' Item responses are generated as:
#' \deqn{item_j^* = intercept_j + loading_j \times distress + error_j}
#'
#' where errors are independent normal with SD = 0.40. The continuous
#' response is then discretised to a 1-5 ordinal scale using floor and
#' clipping.
#'
#' ## Non-invariance structure
#'
#' Items 3 ("restless") and 5 ("effort") have different intercepts across
#' groups (shifted by +0.50 in group 1), representing partial scalar
#' non-invariance. All factor loadings are invariant across groups
#' (metric invariance holds). This design allows students to discover
#' that configural and metric invariance hold but full scalar invariance
#' fails, and that releasing intercepts for items 3 and 5 restores fit.
#'
#' ## Factor loadings
#'
#' The true loadings are: 0.70, 0.80, 0.75, 0.85, 0.65, 0.80
#' (corresponding to items 1-6).
#'
#' @examples
#' # generate data with default settings
#' d <- simulate_measurement_items()
#' dim(d)
#'
#' # check true loadings
#' attr(d, "true_loadings")
#'
#' # check intercept non-invariance
#' attr(d, "true_intercepts_group0")
#' attr(d, "true_intercepts_group1")
#'
#' # smaller dataset for testing
#' d_small <- simulate_measurement_items(n = 500, seed = 42)
#'
#' @export
simulate_measurement_items <- function(n = 2000, seed = 2026) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  cli::cli_alert_info(
    "Simulating measurement items for {n} observations (2 groups)"
  )


  # ---- true parameters ----

  true_loadings <- c(0.70, 0.80, 0.75, 0.85, 0.65, 0.80)
  n_items <- length(true_loadings)

  # intercepts for group 0
  intercepts_g0 <- c(2.50, 2.80, 2.60, 3.00, 2.40, 2.70)


  # intercepts for group 1: items 3 and 5 shifted (non-invariance)
  intercepts_g1 <- intercepts_g0
  intercepts_g1[3] <- intercepts_g0[3] + 0.50
  intercepts_g1[5] <- intercepts_g0[5] + 0.50


  # ---- demographics ----

  group <- stats::rbinom(n, 1, 0.50)
  age <- stats::rnorm(n, 0, 1)
  male <- stats::rbinom(n, 1, 0.45)


  # ---- latent factor ----

  # small group mean difference in distress
  true_distress <- stats::rnorm(n, 0, 1) + 0.20 * group


  # ---- generate item responses ----

  item_error_sd <- 0.40

  items <- matrix(NA_real_, nrow = n, ncol = n_items)

  for (j in seq_len(n_items)) {
    # select intercept based on group
    intercept_j <- ifelse(
      group == 0, intercepts_g0[j], intercepts_g1[j]
    )

    # continuous latent response
    items[, j] <- intercept_j +
      true_loadings[j] * true_distress +
      stats::rnorm(n, 0, item_error_sd)

    # discretise to 1-5 ordinal scale
    items[, j] <- round(items[, j])
    items[, j] <- pmax(pmin(items[, j], 5L), 1L)
  }

  # convert to integer
  storage.mode(items) <- "integer"


  # ---- assemble tibble ----

  data <- tibble::tibble(
    id = seq_len(n),
    group = group,
    age = age,
    male = male,
    item_1 = items[, 1],
    item_2 = items[, 2],
    item_3 = items[, 3],
    item_4 = items[, 4],
    item_5 = items[, 5],
    item_6 = items[, 6],
    true_distress = true_distress
  )

  # attach ground truth as attributes
  attr(data, "true_loadings") <- true_loadings
  attr(data, "true_intercepts_group0") <- intercepts_g0
  attr(data, "true_intercepts_group1") <- intercepts_g1

  cli::cli_alert_success("Generated {n} observations with {n_items} items")
  cli::cli_alert_info("Items: nervous, hopeless, restless, depressed, effort, worthless")
  cli::cli_alert_info("Non-invariant items: 3 (restless) and 5 (effort)")
  cli::cli_alert_info("Ground truth in true_distress column and attributes")

  data
}
