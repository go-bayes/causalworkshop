#' Fit an Option A validation cache locally
#'
#' Local-fit analogue of [load_option_a_cache()] for the PSYC 434
#' Option A assessment. Refits the multi-outcome causal forest,
#' policy-tree stability check, and policy workflow for one of the two
#' Option A exposures, returning the same list shape as the loader.
#'
#' @inheritParams fit_lab_09
#' @param exposure One of `"religious_service"` or `"volunteer_work"`.
#'
#' @return A named list with `models_binary`, `policy_tree_stability`,
#'   `policy_workflow`, `cache_dir = NA_character_`, and `exposure`.
#'
#' @details Expect ~10-25 min on an M-series laptop at defaults.
#'
#' @examples
#' \dontrun{
#' cache_rel <- fit_option_a("religious_service")
#' cache_vol <- fit_option_a("volunteer_work")
#' }
#'
#' @seealso [load_option_a_cache()] for the Drive-hosted alternative.
#'
#' @export
fit_option_a <- function(
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
