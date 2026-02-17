#' Simulate NZAVS-Style Panel Data for Causal Inference
#'
#' Generates a synthetic three-wave panel dataset modelled on the New Zealand
#' Attitudes and Values Study (NZAVS). The data includes baseline confounders,
#' multiple candidate exposures, and multiple candidate outcomes with built-in
#' confounding structure and known ground-truth treatment effects.
#'
#' @param n Integer. Number of individuals to simulate (default: 5000).
#'   Total rows returned will be `n * 3` (three waves per individual).
#' @param seed Integer or NULL. Random seed for reproducibility (default: 2026).
#'   Set to NULL to skip setting the seed.
#'
#' @return A tibble in long panel format with `n * 3` rows and the following
#'   columns:
#'
#'   **Identifiers:**
#'   \describe{
#'     \item{id}{Integer. Unique participant identifier.}
#'     \item{wave}{Integer. Wave indicator (0, 1, or 2).}
#'   }
#'
#'   **Demographics and baseline confounders (L):**
#'   \describe{
#'     \item{age}{Numeric. Standardised age (increases by 1 each wave).}
#'     \item{male}{Integer. Binary indicator (1 = male).}
#'     \item{nz_european}{Integer. Binary ethnicity indicator.}
#'     \item{education}{Numeric. Standardised years of education.}
#'     \item{partner}{Integer. Binary indicator (1 = partnered).}
#'     \item{employed}{Integer. Binary indicator (1 = employed).}
#'     \item{log_income}{Numeric. Log household income.}
#'     \item{nz_dep}{Numeric. Neighbourhood deprivation index.}
#'     \item{agreeableness, conscientiousness, extraversion, neuroticism,
#'       openness}{Numeric. Big Five personality traits (standardised).}
#'   }
#'
#'   **Candidate exposures (A):**
#'   \describe{
#'     \item{community_group}{Integer. Regular community group participation
#'       (binary).}
#'     \item{religious_service}{Integer. Regular religious service attendance
#'       (binary).}
#'     \item{volunteer_work}{Integer. Regular volunteer work (binary).}
#'   }
#'
#'   **Candidate outcomes (Y):**
#'   \describe{
#'     \item{wellbeing}{Numeric. General psychological wellbeing.}
#'     \item{belonging}{Numeric. Sense of community belonging.}
#'     \item{self_esteem}{Numeric. Self-esteem measure.}
#'     \item{life_satisfaction}{Numeric. Life satisfaction measure.}
#'   }
#'
#'   **Ground-truth treatment effects (tau):**
#'   Twelve columns named \code{tau_\{exposure\}_\{outcome\}} giving the true
#'   individual-level causal effect of each exposure on each outcome. These
#'   are constant across waves for a given individual (they depend on baseline
#'   characteristics only).
#'
#' @details
#'
#' ## Causal structure
#'
#' The data-generating process follows a three-wave panel design:
#'
#' - **Wave 0 (baseline):** Confounders and pre-treatment outcome values.
#'   Personality traits and demographics are drawn independently. Income and
#'   deprivation depend on education and age.
#' - **Wave 1 (exposure):** Binary exposures are assigned via logistic models
#'   that depend on baseline confounders, creating realistic selection bias.
#'   Outcome values drift slightly from baseline.
#' - **Wave 2 (outcome):** Outcomes are generated as a function of baseline
#'   values plus heterogeneous treatment effects of wave-1 exposures.
#'
#' ## Treatment effects
#'
#' Effects are heterogeneous: they vary across individuals as a function of
#' baseline personality traits and partnership status. Some effects include
#' non-linear terms (e.g., squared neuroticism, interaction between
#' conscientiousness and openness) to create effect modification patterns
#' detectable by causal forests.
#'
#' The average treatment effect (ATE) of any single exposure on any outcome
#' equals the population mean of the corresponding \code{tau_*} column.
#'
#' ## Intended use
#'
#' Analysts choose one exposure-outcome pair for causal estimation. Multiple
#' options support teaching settings where each participant works on a
#' different question. The \code{tau_*} columns allow validation of causal
#' estimates against ground truth.
#'
#' @examples
#' # generate data with default settings
#' d <- simulate_nzavs_data()
#' dim(d)
#'
#' # check average treatment effect of community group on wellbeing
#' mean(d$tau_community_wellbeing[d$wave == 0])
#'
#' # smaller dataset for testing
#' d_small <- simulate_nzavs_data(n = 500, seed = 42)
#'
#' @export
simulate_nzavs_data <- function(n = 5000, seed = 2026) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  cli::cli_alert_info("Simulating NZAVS-style panel data for {n} individuals")

  # ---- wave 0: baseline confounders ----

  # demographics
  age <- stats::rnorm(n, 0, 1)
  male <- stats::rbinom(n, 1, 0.45)
  nz_european <- stats::rbinom(n, 1, 0.70)
  education <- stats::rnorm(n, 0, 1)
  partner <- stats::rbinom(n, 1, 0.60)
  employed <- stats::rbinom(n, 1, 0.65)

  # socioeconomic (depend on demographics)
  log_income <- 0.30 * education + 0.20 * age + 0.15 * employed +
    stats::rnorm(n, 0, 0.80)
  nz_dep <- -0.20 * education - 0.15 * log_income + 0.10 * (1 - employed) +
    stats::rnorm(n, 0, 0.70)

  # personality (big five, standardised)
  agreeableness <- stats::rnorm(n, 0, 1)
  conscientiousness <- stats::rnorm(n, 0, 1)
  extraversion <- stats::rnorm(n, 0, 1)
  neuroticism <- stats::rnorm(n, 0, 1)
  openness <- stats::rnorm(n, 0, 1)

  # baseline outcome measures (pre-treatment values)
  wellbeing_t0 <- 0.30 * extraversion - 0.40 * neuroticism +
    0.20 * partner + 0.10 * log_income - 0.15 * nz_dep +
    stats::rnorm(n, 0, 0.60)
  belonging_t0 <- 0.20 * extraversion + 0.15 * agreeableness -
    0.20 * neuroticism + 0.10 * partner - 0.10 * nz_dep +
    stats::rnorm(n, 0, 0.60)
  self_esteem_t0 <- 0.15 * extraversion - 0.35 * neuroticism +
    0.10 * education + 0.10 * conscientiousness +
    stats::rnorm(n, 0, 0.60)
  life_satisfaction_t0 <- 0.25 * extraversion - 0.30 * neuroticism +
    0.20 * partner + 0.15 * log_income - 0.10 * nz_dep +
    stats::rnorm(n, 0, 0.60)

  # baseline exposure levels (pre-treatment)
  community_t0 <- stats::rbinom(n, 1, stats::plogis(
    -0.50 + 0.30 * extraversion + 0.20 * agreeableness
  ))
  religious_t0 <- stats::rbinom(n, 1, stats::plogis(
    -1.00 + 0.20 * agreeableness + 0.15 * conscientiousness + 0.10 * age
  ))
  volunteer_t0 <- stats::rbinom(n, 1, stats::plogis(
    -0.80 + 0.20 * agreeableness + 0.15 * conscientiousness + 0.10 * openness
  ))

  # ---- wave 1: exposure assignment (selection bias) ----

  # community group participation (depends on confounders)
  p_community <- stats::plogis(
    -0.50 + 0.40 * extraversion + 0.30 * agreeableness -
      0.20 * neuroticism + 0.15 * partner + 0.10 * belonging_t0 -
      0.10 * nz_dep + 0.30 * community_t0
  )
  community_t1 <- stats::rbinom(n, 1, p_community)

  # religious service attendance (depends on confounders)
  p_religious <- stats::plogis(
    -1.00 + 0.25 * agreeableness + 0.20 * conscientiousness +
      0.15 * age - 0.10 * education - 0.20 * nz_european +
      0.15 * partner + 0.40 * religious_t0
  )
  religious_t1 <- stats::rbinom(n, 1, p_religious)

  # volunteer work (depends on confounders)
  p_volunteer <- stats::plogis(
    -0.80 + 0.25 * agreeableness + 0.20 * conscientiousness +
      0.15 * openness + 0.10 * education + 0.10 * extraversion +
      0.30 * volunteer_t0
  )
  volunteer_t1 <- stats::rbinom(n, 1, p_volunteer)

  # outcomes at t1 (drift from baseline, no treatment effect yet)
  wellbeing_t1 <- wellbeing_t0 + stats::rnorm(n, 0, 0.30)
  belonging_t1 <- belonging_t0 + stats::rnorm(n, 0, 0.30)
  self_esteem_t1 <- self_esteem_t0 + stats::rnorm(n, 0, 0.30)
  life_satisfaction_t1 <- life_satisfaction_t0 + stats::rnorm(n, 0, 0.30)

  # ---- heterogeneous treatment effects ----

  # community_group -> outcomes
  tau_community_wellbeing <- 0.20 + 0.10 * extraversion +
    0.05 * partner - 0.03 * neuroticism^2
  tau_community_belonging <- 0.30 + 0.08 * agreeableness +
    0.06 * extraversion
  tau_community_self_esteem <- 0.10 + 0.04 * extraversion -
    0.02 * neuroticism
  tau_community_life_satisfaction <- 0.15 + 0.06 * partner +
    0.04 * extraversion

  # religious_service -> outcomes
  tau_religious_wellbeing <- 0.12 + 0.06 * agreeableness -
    0.04 * openness
  tau_religious_belonging <- 0.25 + 0.10 * agreeableness +
    0.04 * conscientiousness
  tau_religious_self_esteem <- 0.08 + 0.04 * conscientiousness
  tau_religious_life_satisfaction <- 0.14 + 0.06 * agreeableness +
    0.03 * partner

  # volunteer_work -> outcomes
  tau_volunteer_wellbeing <- 0.15 + 0.08 * openness +
    0.04 * conscientiousness
  tau_volunteer_belonging <- 0.20 + 0.06 * agreeableness +
    0.04 * openness
  tau_volunteer_self_esteem <- 0.14 + 0.06 * conscientiousness +
    0.04 * openness + 0.03 * conscientiousness * openness
  tau_volunteer_life_satisfaction <- 0.18 + 0.06 * openness +
    0.04 * conscientiousness

  # ---- wave 2: outcomes (include causal effects of t1 exposures) ----

  wellbeing_t2 <- wellbeing_t0 +
    tau_community_wellbeing * community_t1 +
    tau_religious_wellbeing * religious_t1 +
    tau_volunteer_wellbeing * volunteer_t1 +
    stats::rnorm(n, 0, 0.50)

  belonging_t2 <- belonging_t0 +
    tau_community_belonging * community_t1 +
    tau_religious_belonging * religious_t1 +
    tau_volunteer_belonging * volunteer_t1 +
    stats::rnorm(n, 0, 0.50)

  self_esteem_t2 <- self_esteem_t0 +
    tau_community_self_esteem * community_t1 +
    tau_religious_self_esteem * religious_t1 +
    tau_volunteer_self_esteem * volunteer_t1 +
    stats::rnorm(n, 0, 0.50)

  life_satisfaction_t2 <- life_satisfaction_t0 +
    tau_community_life_satisfaction * community_t1 +
    tau_religious_life_satisfaction * religious_t1 +
    tau_volunteer_life_satisfaction * volunteer_t1 +
    stats::rnorm(n, 0, 0.50)

  # exposures persist at t2
  community_t2 <- community_t1
  religious_t2 <- religious_t1
  volunteer_t2 <- volunteer_t1

  # ---- assemble long panel ----

  # helper to build one wave
  make_wave <- function(w, wellbeing, belonging, self_esteem, life_satisfaction,
                        community, religious, volunteer, age_offset = 0L) {
    tibble::tibble(
      id = seq_len(n),
      wave = w,
      age = age + age_offset,
      male = male,
      nz_european = nz_european,
      education = education,
      partner = partner,
      employed = employed,
      log_income = log_income,
      nz_dep = nz_dep,
      agreeableness = agreeableness,
      conscientiousness = conscientiousness,
      extraversion = extraversion,
      neuroticism = neuroticism,
      openness = openness,
      community_group = community,
      religious_service = religious,
      volunteer_work = volunteer,
      wellbeing = wellbeing,
      belonging = belonging,
      self_esteem = self_esteem,
      life_satisfaction = life_satisfaction,
      tau_community_wellbeing = tau_community_wellbeing,
      tau_community_belonging = tau_community_belonging,
      tau_community_self_esteem = tau_community_self_esteem,
      tau_community_life_satisfaction = tau_community_life_satisfaction,
      tau_religious_wellbeing = tau_religious_wellbeing,
      tau_religious_belonging = tau_religious_belonging,
      tau_religious_self_esteem = tau_religious_self_esteem,
      tau_religious_life_satisfaction = tau_religious_life_satisfaction,
      tau_volunteer_wellbeing = tau_volunteer_wellbeing,
      tau_volunteer_belonging = tau_volunteer_belonging,
      tau_volunteer_self_esteem = tau_volunteer_self_esteem,
      tau_volunteer_life_satisfaction = tau_volunteer_life_satisfaction
    )
  }

  data <- dplyr::bind_rows(
    make_wave(0L, wellbeing_t0, belonging_t0, self_esteem_t0,
              life_satisfaction_t0, community_t0, religious_t0, volunteer_t0,
              age_offset = 0L),
    make_wave(1L, wellbeing_t1, belonging_t1, self_esteem_t1,
              life_satisfaction_t1, community_t1, religious_t1, volunteer_t1,
              age_offset = 1L),
    make_wave(2L, wellbeing_t2, belonging_t2, self_esteem_t2,
              life_satisfaction_t2, community_t2, religious_t2, volunteer_t2,
              age_offset = 2L)
  )

  data <- data[order(data$id, data$wave), ]

  cli::cli_alert_success(
    "Generated {nrow(data)} rows ({n} individuals x 3 waves)"
  )
  cli::cli_alert_info(
    "Exposures: community_group, religious_service, volunteer_work"
  )
  cli::cli_alert_info(
    "Outcomes: wellbeing, belonging, self_esteem, life_satisfaction"
  )
  cli::cli_alert_info("Ground-truth effects in tau_* columns")

  data
}
