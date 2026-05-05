# private helper shared by fit_lab_09() and fit_option_a(). fits the
# multi-outcome causal-forest batch, the policy-tree stability check,
# and the policy workflow on a freshly simulated NZAVS-style panel.
# returns the same three-artefact list shape as load_lab_09_cache() and
# load_option_a_cache(). expensive: ~10-25 minutes on an M-series
# laptop at default settings.

.fit_psyc434 <- function(
  exposure = c("community_group", "religious_service", "volunteer_work"),
  n = 5000,
  seed = 2026,
  n_iterations = 100,
  num_trees = 1000,
  parallel = FALSE
) {
  exposure <- match.arg(exposure)

  for (pkg in c("margot", "grf", "dplyr", "tibble")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "package '", pkg, "' is required to fit the cache locally.\n",
        "install with install.packages('", pkg, "')",
        if (pkg == "margot") " or pak::pak('go-bayes/margot')" else "",
        "."
      )
    }
  }

  d <- simulate_nzavs_data(n = n, seed = seed)
  d0 <- dplyr::filter(d, .data$wave == 0)
  d1 <- dplyr::filter(d, .data$wave == 1)
  d2 <- dplyr::filter(d, .data$wave == 2)

  covariate_cols <- c(
    "age", "male", "nz_european", "education", "partner", "employed",
    "log_income", "nz_dep", "agreeableness", "conscientiousness",
    "extraversion", "neuroticism", "openness",
    exposure, "purpose"
  )

  exposure_t1 <- paste0(exposure, "_t1")
  outcomes_t2 <- tibble::tibble(
    t2_purpose = d2$purpose,
    t2_belonging = d2$belonging,
    t2_self_esteem = d2$self_esteem,
    t2_life_satisfaction = d2$life_satisfaction
  )
  outcomes_t2[[exposure_t1]] <- d1[[exposure]]
  # column order: covariates, then exposure_t1, then outcomes
  outcomes_t2 <- outcomes_t2[, c(exposure_t1, setdiff(names(outcomes_t2), exposure_t1))]

  df_grf <- dplyr::bind_cols(
    dplyr::select(d0, dplyr::all_of(covariate_cols)),
    outcomes_t2
  )

  X <- as.matrix(df_grf[, covariate_cols])
  W <- df_grf[[exposure_t1]]
  weights <- rep(1, nrow(df_grf))

  outcome_vars <- c(
    "t2_purpose", "t2_belonging", "t2_self_esteem", "t2_life_satisfaction"
  )
  label_mapping <- list(
    model_t2_purpose = "Sense of Purpose",
    model_t2_belonging = "Belonging",
    model_t2_self_esteem = "Self-esteem",
    model_t2_life_satisfaction = "Life satisfaction"
  )

  grf_defaults <- list(
    num.trees = num_trees,
    honesty = TRUE,
    tune.parameters = "all"
  )

  message(
    "fitting locally — expect ~10-25 min on an M-series laptop ",
    "(num.trees = ", num_trees, ", n_iterations = ", n_iterations, ")"
  )

  message("[1/3] causal forests over ", length(outcome_vars), " outcomes ...")
  models_binary <- margot::margot_causal_forest(
    data = df_grf,
    outcome_vars = outcome_vars,
    covariates = X,
    W = W,
    weights = weights,
    grf_defaults = grf_defaults,
    top_n_vars = 12,
    save_models = TRUE,
    save_data = TRUE,
    compute_conditional_means = TRUE,
    train_proportion = 0.5,
    use_train_test_split = TRUE,
    seed = seed
  )

  message("[2/3] policy-tree stability (", n_iterations, " iterations) ...")
  policy_tree_stability <- margot::margot_policy_tree_stability(
    model_results = models_binary,
    depth = 2,
    n_iterations = n_iterations,
    vary_type = "split_only",
    parallel = parallel,
    label_mapping = label_mapping,
    seed = seed
  )

  message("[3/3] policy workflow (prose + brief) ...")
  wf <- margot::margot_policy_workflow(
    stability = policy_tree_stability,
    original_df = df_grf,
    label_mapping = label_mapping,
    audience = "policy",
    interpret_models = "recommended",
    plot_models = "recommended"
  )
  # strip heavy ggplot objects; downstream code re-renders from
  # policy_tree_stability via margot_plot_decision_tree() etc.
  wf$plots <- NULL

  list(
    models_binary = models_binary,
    policy_tree_stability = policy_tree_stability,
    policy_workflow = wf
  )
}
