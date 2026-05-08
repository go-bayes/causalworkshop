#' Fit a PSYC 434 report workflow locally
#'
#' Local-fit analogue of [load_report_workflow_cache()] for the PSYC 434
#' report assessment. Refits the multi-outcome causal forest,
#' policy-tree stability check, and policy workflow for one of the report
#' exposures, returning the same list shape as the loader.
#'
#' @inheritParams fit_policy_learning_workflow
#' @param exposure One of `"religious_service"` or `"volunteer_work"`.
#'
#' @return A named list with `models_binary`, `policy_tree_stability`,
#'   `policy_workflow`, `cache_dir = NA_character_`, and `exposure`.
#'
#' @details Expect ~10-25 min on an M-series laptop at defaults.
#'
#' @examples
#' \dontrun{
#' cache_rel <- fit_report_workflow("religious_service")
#' cache_vol <- fit_report_workflow("volunteer_work")
#' }
#'
#' @seealso [load_report_workflow_cache()] for the Drive-hosted alternative.
#'
#' @export
fit_report_workflow <- function(
  exposure = c("religious_service", "volunteer_work"),
  n = 5000,
  seed = 2026,
  n_iterations = 100,
  num_trees = 1000,
  parallel = FALSE
) {
  exposure <- match.arg(exposure)
  out <- .fit_psyc434(
    exposure = exposure,
    n = n,
    seed = seed,
    n_iterations = n_iterations,
    num_trees = num_trees,
    parallel = parallel
  )
  c(out, list(cache_dir = NA_character_, exposure = exposure))
}

#' Fit an Option A validation cache locally
#'
#' `fit_option_a()` is soft-deprecated. Use [fit_report_workflow()] instead.
#'
#' @inheritParams fit_report_workflow
#' @inherit fit_report_workflow return
#' @export
fit_option_a <- function(...) {
  warning(
    "`fit_option_a()` is soft-deprecated; ",
    "use `fit_report_workflow()` instead.",
    call. = FALSE
  )
  fit_report_workflow(...)
}
