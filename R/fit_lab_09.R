#' Fit the Lab 9 cache locally
#'
#' Refits the Lab 9 multi-outcome causal forest, policy-tree stability
#' check, and policy workflow on the caller's machine, returning the
#' same list shape as [load_policy_learning_cache()]. Use this when you would
#' rather not deserialise the author's pre-fitted artefacts from Google
#' Drive — for example, if you would prefer not to run
#' [qs2::qs_deserialize()] on a third-party blob, or you want to test a
#' local change to the simulator.
#'
#' @param n Sample size for [simulate_nzavs_data()].
#' @param seed Master seed; passed to the simulator and to grf.
#' @param n_iterations Bootstrap iterations for
#'   `margot::margot_policy_tree_stability()`. Lower for a faster fit;
#'   the canonical cache uses 100.
#' @param num_trees Per-outcome tree count for
#'   `margot::margot_causal_forest()`.
#' @param parallel Forwarded to
#'   `margot::margot_policy_tree_stability(parallel = ...)`.
#'
#' @return A named list with `models_binary`, `policy_tree_stability`,
#'   `policy_workflow`, and `cache_dir = NA_character_`.
#'
#' @details Expect ~10-25 min on an M-series laptop at defaults.
#'
#' @examples
#' \dontrun{
#' cache <- fit_lab_09()
#' # equivalent to causalworkshop::load_policy_learning_cache(refit = TRUE)
#' }
#'
#' @seealso [load_policy_learning_cache()] for the Drive-hosted alternative.
#'
#' @export
fit_lab_09 <- function(
  n = 5000,
  seed = 2026,
  n_iterations = 100,
  num_trees = 1000,
  parallel = FALSE
) {
  out <- .fit_psyc434(
    exposure = "community_group",
    n = n,
    seed = seed,
    n_iterations = n_iterations,
    num_trees = num_trees,
    parallel = parallel
  )
  c(out, list(cache_dir = NA_character_))
}
