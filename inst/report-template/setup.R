# ============================================================================
# PSYC 434 -- research-report-template / setup.R
# ----------------------------------------------------------------------------
# Single source of truth for the Option A research report.
# Change the exposure here. Helpers fit the four-outcome causal forest,
# build the policy-tree workflow, apply the graphing rule, and produce
# margot_plot() outputs (plot + transformed table + interpretation).
# Re-renders read a cached fit instead of refitting; set
# `invalidate_fit_cache <- TRUE` near the top to force a refit.
# ============================================================================

# --- packages ---------------------------------------------------------------

required_packages <- c(
  "causalworkshop", "margot", "grf",
  "ggdag", "dagitty", "ggplot2", "dplyr", "tibble", "tidyr",
  "knitr", "kableExtra"
)

missing <- required_packages[
  !vapply(required_packages, \(p) requireNamespace(p, quietly = TRUE), logical(1))
]

if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")

if ("margot" %in% missing || packageVersion("margot") < "1.0.322") {
  pak::pak("go-bayes/margot")
  missing <- setdiff(missing, "margot")
}

if ("causalworkshop" %in% missing || packageVersion("causalworkshop") < "0.6.0") {
  pak::pak("go-bayes/causalworkshop")
  missing <- setdiff(missing, "causalworkshop")
  if ("causalworkshop" %in% loadedNamespaces()) {
    stop("causalworkshop was upgraded; please restart R and re-render.",
         call. = FALSE)
  }
}

if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(causalworkshop)
  library(margot)
  library(grf)
  library(ggdag)
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(knitr)
  library(kableExtra)
})

# --- study decisions --------------------------------------------------------

# The template ships with the Lab 9/10 demo exposure ("community_group")
# so that a fresh download renders out-of-the-box. **For your submitted
# report you must change this to one of the two report options below.**
# Allowed report values:
#   - "religious_service"
#   - "volunteer_work"
# The lab-only value "community_group" is not a valid choice for the
# report and will trigger a startup message reminding you to switch.
name_exposure <- "community_group"  # change to "religious_service" or "volunteer_work" for the report

stopifnot(name_exposure %in% c("religious_service", "volunteer_work", "community_group"))
if (identical(name_exposure, "community_group")) {
  message(
    "name_exposure = 'community_group': lab demo only. ",
    "Before rendering your final report, set name_exposure to ",
    "'religious_service' or 'volunteer_work' in setup.R."
  )
}

study_seed <- 2026
study_n <- 2000

# fitting controls — raise these for the final render
num_trees <- 500
n_iterations_stability <- 50

# multiplicity and parsimony controls
alpha_family_wise <- 0.05
min_gain_for_depth_switch <- 0.01

# graphing-rule thresholds (margot_select_grf_policy_trees)
policy_value_lower_threshold <- 0
treated_uplift_lower_threshold <- 0

# E-value bound threshold used by margot_plot() (1.0 == "any robustness").
e_val_bound_threshold <- 1.0

# fixed by the assignment — do not change
outcome_short_names <- c("purpose", "belonging", "self_esteem", "life_satisfaction")

covariate_cols <- c(
  "age", "male", "nz_european", "education", "partner", "employed",
  "log_income", "nz_dep", "agreeableness", "conscientiousness",
  "extraversion", "neuroticism", "openness",
  name_exposure,
  outcome_short_names
)

label_mapping <- list(
  model_t2_purpose = "Sense of purpose",
  model_t2_belonging = "Belonging",
  model_t2_self_esteem = "Self-esteem",
  model_t2_life_satisfaction = "Life satisfaction"
)

exposure_label <- switch(
  name_exposure,
  religious_service = "religious-service attendance",
  volunteer_work = "volunteer work",
  community_group = "community-group participation"
)

# --- plot defaults ---------------------------------------------------------
# Pass to margot_plot() via margot_plot_create_options(base_defaults = ...).
# Tweak any of these values to restyle the forest plot without editing the
# manuscript chunks. See ?margot::margot_plot for the full set of options.

plot_defaults <- list(
  type = "RD",
  title = "",
  subtitle = "",
  order = "magnitude_asc",
  colors = c(
    "positive"     = "#E69F00",
    "not reliable" = "grey50",
    "negative"     = "#56B4E9"
  ),
  text_size = 4,
  linewidth = 0.5,
  estimate_scale = 1,
  base_size = 14,
  point_size = 2,
  title_size = 14,
  subtitle_size = 14,
  legend_text_size = 10,
  legend_title_size = 10,
  include_coefficients = TRUE,
  rename_ate = TRUE,
  rename_evalue = TRUE
)

theme_set(
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title.position = "plot",
      strip.text = element_text(face = "bold")
    )
)

# --- data -------------------------------------------------------------------

simulate_panel <- function(n = study_n, seed = study_seed,
                           exposure = name_exposure) {
  d <- causalworkshop::simulate_nzavs_data(n = n, seed = seed)
  d0 <- d |> filter(wave == 0)
  d1 <- d |> filter(wave == 1)
  d2 <- d |> filter(wave == 2)

  bind_cols(
    d0 |> select(all_of(covariate_cols)),
    tibble(
      exposure_t1 = d1[[exposure]],
      t2_purpose = d2$purpose,
      t2_belonging = d2$belonging,
      t2_self_esteem = d2$self_esteem,
      t2_life_satisfaction = d2$life_satisfaction
    )
  )
}

# --- pipeline ---------------------------------------------------------------

fit_pipeline <- function(panel,
                         num_trees_local = num_trees,
                         n_iter_local = n_iterations_stability,
                         seed = study_seed) {
  X <- as.matrix(panel[, covariate_cols])
  W <- panel$exposure_t1
  weights <- rep(1, nrow(panel))

  outcome_vars <- c("t2_purpose", "t2_belonging",
                    "t2_self_esteem", "t2_life_satisfaction")

  models_binary <- margot::margot_causal_forest(
    data = panel,
    outcome_vars = outcome_vars,
    covariates = X,
    W = W,
    weights = weights,
    grf_defaults = list(num.trees = num_trees_local, honesty = TRUE),
    top_n_vars = 12,
    save_models = TRUE,
    save_data = TRUE,
    compute_conditional_means = TRUE,
    train_proportion = 0.5,
    use_train_test_split = TRUE,
    seed = seed
  )

  policy_tree_stability <- margot::margot_policy_tree_stability(
    model_results = models_binary,
    depth = 2,
    n_iterations = n_iter_local,
    vary_type = "split_only",
    parallel = FALSE,
    label_mapping = label_mapping,
    seed = seed
  )

  wf <- margot::margot_policy_workflow(
    stability = policy_tree_stability,
    original_df = panel,
    label_mapping = label_mapping,
    audience = "policy",
    prefer_stability = TRUE,
    min_gain_for_depth_switch = min_gain_for_depth_switch,
    signal_score = "pv_snr",
    signals_k = 3,
    interpret_models = "wins_borderline",
    plot_models = "wins_borderline"
  )

  selection <- margot::margot_select_grf_policy_trees(
    policy_brief = wf$policy_brief_df,
    policy_value_lower_threshold = policy_value_lower_threshold,
    treated_uplift_lower_threshold = treated_uplift_lower_threshold
  )

  list(
    models_binary = models_binary,
    policy_tree_stability = policy_tree_stability,
    wf = wf,
    selection = selection
  )
}

# --- fit cache --------------------------------------------------------------
# The fit takes a few minutes. Save it to disk after the first successful
# render so subsequent renders are near-instant. The cache key encodes every
# input *and* every piece of code that affects the fit:
#   1. The explicit constants (exposure, n, seed, num_trees, etc.).
#   2. The list of covariate columns (so adding/removing a covariate
#      invalidates the cache).
#   3. A short hash of the body of `fit_pipeline()` (so any edit to the
#      fitting code — e.g., changing top_n_vars, train_proportion, or the
#      workflow audience — also invalidates the cache).
# If anything that changes the fit changes, the cache filename changes too,
# and a fresh fit runs. Set `invalidate_fit_cache <- TRUE` to force a refit.

fit_cache_dir <- "_cache"
invalidate_fit_cache <- FALSE   # set to TRUE to force a refit

fit_pipeline_hash <- function() {
  body_str <- paste(deparse(body(fit_pipeline)), collapse = "\n")
  cov_str  <- paste(covariate_cols, collapse = ",")
  digest_str <- paste(body_str, cov_str, sep = "||")
  # md5 of the combined string
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeLines(digest_str, tmp)
  substring(unname(tools::md5sum(tmp)), 1, 10)
}

fit_cache_key <- function() {
  paste(name_exposure, study_n, study_seed, num_trees,
        n_iterations_stability, min_gain_for_depth_switch,
        policy_value_lower_threshold, treated_uplift_lower_threshold,
        fit_pipeline_hash(),
        sep = "_")
}

fit_cache_path <- function() {
  file.path(fit_cache_dir, paste0("fit_", fit_cache_key(), ".rds"))
}

run_fit_pipeline <- function(panel) {
  cache <- fit_cache_path()
  if (!invalidate_fit_cache && file.exists(cache)) {
    message("Loading cached fit: ", cache)
    return(readRDS(cache))
  }
  message("Fitting causal forests + policy-tree workflow; this takes a few minutes.")
  fit <- fit_pipeline(panel)
  dir.create(fit_cache_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(fit, cache)
  message("Cached fit to: ", cache)
  fit
}

# --- ATE plot, table, and interpretation via margot_plot() -----------------
# margot_plot() returns three things:
#   $plot              — the forest plot (ggplot)
#   $transformed_table — outcomes ordered by magnitude with renamed columns
#   $interpretation    — short prose summarising direction, magnitude, and
#                        E-value bounds (use this verbatim or as a draft).
# `adjust = "bonferroni"` widens the CIs and recomputes the bound-side
# E-value at the multiplicity-adjusted lower bound, so we don't have to
# do the correction ourselves.

ate_plot_objects <- function(models_binary,
                             plot_options = plot_defaults) {
  margot::margot_plot(
    models_binary$combined_table,
    options = margot::margot_plot_create_options(
      title = sprintf("Outcome-wide effects of %s", exposure_label),
      subtitle = sprintf(
        "Bonferroni-adjusted across %d outcomes (alpha_FW = %g)",
        length(outcome_short_names), alpha_family_wise
      ),
      base_defaults = plot_options
    ),
    label_mapping = label_mapping,
    include_coefficients = TRUE,
    save_output = FALSE,
    e_val_bound_threshold = e_val_bound_threshold,
    rename_ate = "ATE",
    order = "magnitude_asc",
    adjust = "bonferroni",
    alpha = alpha_family_wise
  )
}

# --- descriptives -----------------------------------------------------------

descriptives_table <- function(panel) {
  vars <- c("age", "male", "partner", "log_income",
            "exposure_t1",
            "t2_purpose", "t2_belonging", "t2_self_esteem", "t2_life_satisfaction")
  labels <- c("age", "male (proportion)", "partner (proportion)",
              "log income",
              paste0(exposure_label, " at wave 1 (proportion)"),
              "purpose at wave 2", "belonging at wave 2",
              "self-esteem at wave 2", "life satisfaction at wave 2")
  bind_rows(lapply(seq_along(vars), function(i) {
    x <- panel[[vars[[i]]]]
    tibble(variable = labels[[i]], mean = mean(x), sd = sd(x))
  }))
}

# --- ground-truth audit (teaching only; remove from final report) ----------

ground_truth_audit <- function(panel_seed = study_seed,
                               n_audit = study_n,
                               exposure = name_exposure) {
  short <- switch(exposure,
    religious_service = "religious",
    volunteer_work = "volunteer",
    community_group = "community"
  )
  d <- causalworkshop::simulate_nzavs_data(n = n_audit, seed = panel_seed)
  d0 <- d |> filter(wave == 0)
  tibble(
    outcome_label = c("Sense of purpose", "Belonging",
                      "Self-esteem", "Life satisfaction"),
    true_mean_tau = c(
      mean(d0[[paste0("tau_", short, "_purpose")]]),
      mean(d0[[paste0("tau_", short, "_belonging")]]),
      mean(d0[[paste0("tau_", short, "_self_esteem")]]),
      mean(d0[[paste0("tau_", short, "_life_satisfaction")]])
    )
  )
}
