# Margot Analysis Workflow ----------------------------------------------------
# Companion script for `06-interpretation-2.qmd`.
# Runs ATE and CATE summaries using cached outputs produced by the workshop
# pipeline. Designed for code review sessions in R / RStudio.
suppressPackageStartupMessages({
  library(cli)
  library(glue)
  library(here)
  library(margot)
  library(tidyverse)
  library(kableExtra)
})


devtools::load_all("/Users/joseph/GIT/margot/")

# only use for nvim
setwd("/Users/joseph/GIT/2025-workshop-ci-sasp/")
here::i_am("index.qmd")

cli_rule("Margot causal forest analysis")

# ---- Paths ------------------------------------------------------------------
results_dir <- here("results")
data_dir <- here("data")

required_dirs <- c(results_dir, data_dir)
missing_dirs <- required_dirs[!dir.exists(required_dirs)]

if (length(missing_dirs) > 0) {
  cli_abort(c(
    "x" = "Required directory/ies missing.",
    "i" = "Run the workshop preparation scripts to populate: {paste(missing_dirs, collapse = ', ')}"
  ))
}

# Helper to read cached objects safely
read_cached <- function(name, reader = margot::here_read, ...) {
  tryCatch(
    reader(name, ...),
    error = function(e) {
      cli_abort(c(
        "x" = "Could not read cached object {.code {name}}.",
        "i" = "Re-run the upstream pipeline to regenerate it.",
        "!" = conditionMessage(e)
      ))
    }
  )
}

# ---- Load inputs ------------------------------------------------------------
cli_alert_info("Loading cached data and models")

original_df <- read_cached(
  here("data", "religious_prosocial_data.rds"),
  reader = readRDS
)

models_binary_cate <- read_cached(
  "models_binary_cate",
  reader = margot::here_read_qs,
  dir_path = results_dir
)

label_mapping <- read_cached(
  "label_mapping",
  reader = margot::here_read,
  dir_path = results_dir
)

# Optional flipped results
models_binary_flipped <- tryCatch(
  margot::here_read_qs("models_binary_flipped_all", dir_path = results_dir),
  error = function(e) NULL
)
flip_outcomes <- tryCatch(
  margot::here_read("flip_outcomes", dir_path = results_dir),
  error = function(e) NULL
)
flipped_names <- tryCatch(
  margot::here_read("flipped_names", dir_path = results_dir),
  error = function(e) NULL
)

use_flipped <- !is.null(models_binary_flipped) && !is.null(flip_outcomes)

if (use_flipped) {
  cli_alert_info("Using flipped outcome models for analysis")
  models_for_analysis <- models_binary_flipped
  labels_for_analysis <- margot::margot_reversed_labels(label_mapping, flip_outcomes)
} else {
  models_for_analysis <- models_binary_cate
  labels_for_analysis <- label_mapping
  flipped_names <- character()
}

# ---- ATE analysis -----------------------------------------------------------
cli_alert_info("Summarising average treatment effects")

ate_title <- glue("ATE Effects of Belief in God on Cooperative Outcomes")

base_defaults_binary <- list(
  type                     = "RD",
  title                    = ate_title,
  e_val_bound_threshold    = 1.2,
  colors                   = c("positive" = "#E69F00", "not reliable" = "grey50", "negative" = "#56B4E9"),
  x_offset                 = -.5,
  x_lim_lo                 = -.5,
  x_lim_hi                 = 1,
  text_size                = 5,
  linewidth                = 0.75,
  estimate_scale           = 1,
  base_size                = 20,
  point_size               = 4,
  title_size               = 20,
  subtitle_size            = 16,
  legend_text_size         = 10,
  legend_title_size        = 10,
  include_coefficients     = FALSE
)

outcomes_options_all <- margot::margot_plot_create_options(
  title           = ate_title,
  base_defaults   = base_defaults_binary,
  subtitle        = "",
  filename_prefix = "grf"
)

ate_results <- margot::margot_plot(
  models_binary_cate$combined_table,
  options               = outcomes_options_all,
  label_mapping         = label_mapping,
  include_coefficients  = FALSE,
  order                 = "evaluebound_asc",
  original_df           = original_df,
  e_val_bound_threshold = 1.2,
  rename_ate            = TRUE,
  adjust                = "bonferroni",
  alpha                 = 0.05
)

ate_table <- ate_results$transformed_table |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cli_h2("ATE summary")
print(ate_table)

# ---- Heterogeneity (RATE + Qini) -------------------------------------------
cli_alert_info("Screening for heterogeneous treatment effects")

rate_results <- margot::margot_rate(
  models        = models_for_analysis,
  policy        = "treat_best",
  alpha         = 0.20,
  adjust        = "fdr",
  label_mapping = labels_for_analysis
)

rate_interp <- margot::margot_interpret_rate(
  rate_results,
  flipped_outcomes      = if (use_flipped) flipped_names else NULL,
  adjust_positives_only = TRUE
)

cli_h2("RATE interpretation")
cat(rate_interp$comparison, "\n")

model_groups <- list(
  autoc       = rate_interp$autoc_model_names,
  qini        = rate_interp$qini_model_names,
  either      = rate_interp$either_model_names,
  exploratory = rate_interp$not_excluded_either
)

qini_results <- margot::margot_policy(
  models_for_analysis,
  policy_tree_args = list(
    point_alpha              = 0.5,
    title_size               = 30,
    subtitle_size            = 25,
    axis_title_size          = 25,
    legend_title_size        = 18,
    split_line_color         = "red",
    split_line_alpha         = 0.8,
    split_label_color        = "red",
    split_label_nudge_factor = 0.007
  ),
  model_names = names(models_for_analysis$results),
  original_df = original_df,
  label_mapping = labels_for_analysis,
  qini_args = list(show_ci = "both"),
  max_depth = 1L,
  output_objects = c("qini_plot", "diff_gain_summaries")
)

qini_gain <- margot::margot_interpret_qini(
  qini_results,
  label_mapping = labels_for_analysis
)

cli_h2("Qini gain summary")
print(qini_gain$summary_table |> mutate(across(where(is.numeric), ~ round(.x, 3))))
cat(qini_gain$qini_explanation, "\n")

reliable_ids <- qini_gain$reliable_model_ids

# ---- Policy trees -----------------------------------------------------------
if (length(reliable_ids) > 0) {
  cli_alert_success("Reliable heterogeneous effects detected: {paste(reliable_ids, collapse = ', ')}")

  policy_results <- margot::margot_policy(
    models_for_analysis,
    decision_tree_args = list(
      span_ratio        = 0.1,
      text_size         = 4,
      y_padding         = 0.5,
      edge_label_offset = 0.02,
      border_size       = 0.01
    ),
    policy_tree_args = list(
      # point_alpha              = 0.5,
      # title_size               = 30,
      # subtitle_size            = 25,
      # axis_title_size          = 25,
      # legend_title_size        = 18,
      # split_line_color         = "red",
      # split_line_alpha         = 0.8,
      # split_label_color        = "red",
      # split_label_nudge_factor = 0.007
    ),
    # model_names        = reliable_ids, Uncomment for valid results only
    original_df = original_df,
    label_mapping = labels_for_analysis,
    max_depth = 2L,
    output_objects = c("combined_plot", "policy_tree")
  )

  policy_text <- margot::margot_interpret_policy_batch(
    models = models_for_analysis,
    original_df = original_df,
    # model_names   = reliable_ids, uncommon for valid result only
    label_mapping = labels_for_analysis,
    max_depth = 2L
  )

  cli_h2("Policy tree interpretation")
  cat(policy_text, "\n")

  # Save plots to images/ for optional use in the Quarto site
  images_dir <- here("images")
  if (!dir.exists(images_dir)) dir.create(images_dir, recursive = TRUE)

  walk2(policy_results, reliable_ids, function(res, id) {
    plot_path <- file.path(images_dir, glue("{id}_policy_tree.png"))
    ggplot2::ggsave(plot_path, res$combined_plot, width = 10, height = 8, dpi = 300)
    cli_alert_info("Saved policy tree for {id} -> {plot_path}")
  })
} else {
  cli_alert_warning("No reliable heterogeneous treatment effects detected; skipping policy trees.")
}

# ---- Optional: stability + policy workflow summary -------------------------
cli_rule("Policy Tree Stability (optional)")


# check if results already exist
results_ready <- all(
  file.exists(file.path(results_dir, "policy_tree_result_stability.qs")),
  file.exists(file.path(results_dir, "policy_workflow_summary.rds"))
)


if (results_ready) {
  cli_alert_info("Skipping stability/policy workflow – cached results already exist.")
} else if (!requireNamespace("fastpolicytree", quietly = TRUE)) {
  cli_alert_warning("Skipping stability analysis (package 'fastpolicytree' not installed)")
} else {
  # run policy tree stability analysis
  policy_tree_result_stability <- tryCatch(
    {
      cli_alert_info("Running stability analysis (100 iterations, depth 1–2) ...")
      margot::margot_policy_tree_stability(
        models_for_analysis,
        label_mapping = label_mapping,
        n_iterations = 300L,
        train_proportion = 0.5,
        tree_method = "fastpolicytree",
        seed = 42,
        metaseed = 12345,
        depth = "both",
        parallel = TRUE,
        policy_value_baseline = "control_all"
      )
    },
    error = function(e) {
      cli_alert_warning("Stability analysis failed: {conditionMessage(e)}")
      NULL
    }
  )
  # save stability results and generate workflow summary
  if (!is.null(policy_tree_result_stability)) {
    margot::here_save_qs(policy_tree_result_stability, "policy_tree_result_stability", results_dir)
    cli_alert_success("Stability results saved to results/policy_tree_result_stability.qs")
    policy_workflow <- tryCatch(
      margot::margot_policy_workflow(
        policy_tree_result_stability,
        original_df = original_df,
        label_mapping = label_mapping,
        se_method = "plugin",
        dominance_threshold = 0.8,
        strict_branch = FALSE,
        min_gain_for_depth_switch = 0.005,
        include_interpretation = TRUE,
        audience = "policy"
      ),
      error = function(e) {
        cli_alert_warning("Policy workflow summary failed: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.null(policy_workflow)) {
      margot::here_save(policy_workflow, "policy_workflow_summary", results_dir)
      cli_alert_success("Policy workflow summary saved to results/policy_workflow_summary.rds")
      # ... depth_map / coverage logging ...
    }
  }
}

cli_rule("Margot analysis complete ✔")

#
# # Charity Result (where there are genuine CATEs)
# policy_results[[1]]
#
#
# # Volunteering Result (where there are no genuine CATEs)
# policy_results[[2]]
#
# diagnostics <- margot_stability_diagnostics(
#   policy_tree_result_stability, # Your bootstrap results
#   model_results = models_for_analysis, # Original causal forest results
#   model_name = reliable_ids
# )
#
# diagnostics
#
# Quantify effects
# ============================================================


policy_tree_result_stability <- here_read_qs("policy_tree_result_stability", results_dir)


wf_ready <- all(
  file.exists(file.path(results_dir, "wf.rds"))
)

if (wf_ready) {
  # read result
  wf <- margot::here_read("wf", results_dir)

  cli_alert_info("Skipping stability/policy workflow – cached results already exist.")
} else {
  # policy_tree_result_stability
  wf <- margot_policy_workflow(
    policy_tree_result_stability,
    min_gain_for_depth_switch = -Inf,
    include_split_breakdown = "leaf",
    split_top_only = FALSE,
    original_df = original_df,
    label_mapping = label_mapping,
    se_method = "plugin",
    dominance_threshold = 0.6,
    include_interpretation = TRUE,
    audience = "policy"
  )

  margot::here_save(wf, "wf", results_dir)
}


cat(wf$summary$recommendations_text)

policy_results[[1]]
