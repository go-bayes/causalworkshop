#' Load the PSYC 434 policy-learning cache
#'
#' Downloads (once) and reads the pre-fitted causal-forest artefacts that
#' the PSYC 434 policy-learning materials build on. The cache lives as a
#' public Google Drive zip and contains three parquet files written by
#' [margot::here_save_arrow()]: `models_binary.parquet`,
#' `policy_tree_stability.parquet`, `policy_workflow.parquet`. On first
#' call the zip is fetched into a per-user cache directory, unpacked,
#' and the three artefacts are read with [margot::here_read_arrow()]; on
#' subsequent calls the local cache is read directly.
#'
#' @param url Google Drive share URL of `lab-09-cache.zip`. Defaults to
#'   the package's canonical URL; can be overridden via the
#'   `PSYC434_POLICY_LEARNING_CACHE_URL` environment variable.
#' @param cache_dir Directory where the cache is stored on disk.
#'   Defaults to a per-user location returned by [tools::R_user_dir()].
#' @param refresh If `TRUE`, re-download the cache even if it is already
#'   on disk.
#' @param refit If `TRUE`, skip the Drive download and refit the cache
#'   locally via [fit_lab_09()]. Use this when you would prefer not to
#'   deserialise a third-party blob, or to test a change to the
#'   simulator. Default settings take ~10-25 min on an M-series laptop.
#' @param ... Forwarded to [fit_lab_09()] when `refit = TRUE`.
#'
#' @return A named list with elements `models_binary`,
#'   `policy_tree_stability`, `policy_workflow`, and `cache_dir`.
#'
#' @details The instructor shortcut: set `PSYC434_POLICY_LEARNING_CACHE` in your
#'   `~/.Renviron` to a local Drive directory holding the three parquet
#'   files, and the function will read them directly without touching
#'   the network.
#'
#' @examples
#' \dontrun{
#' cache <- load_policy_learning_cache()
#' cache$models_binary
#' }
#'
#' @export
load_policy_learning_cache <- function(
  url = .policy_learning_cache_url(),
  cache_dir = tools::R_user_dir("psyc434", which = "cache"),
  refresh = FALSE,
  refit = FALSE,
  ...
) {
  if (isTRUE(refit)) {
    return(fit_lab_09(...))
  }
  .ensure_arrow_stack()

  expected_files <- c(
    "models_binary.parquet",
    "policy_tree_stability.parquet",
    "policy_workflow.parquet"
  )

  # author shortcut: read from a local Drive mirror if the env var
  # points at a directory holding all three parquet files.
  local_drive <- Sys.getenv("PSYC434_POLICY_LEARNING_CACHE", "")
  if (!nzchar(local_drive)) {
    local_drive <- Sys.getenv("PSYC434_LAB09_CACHE", "")
  }
  if (nzchar(local_drive) &&
    all(file.exists(file.path(local_drive, expected_files)))) {
    return(list(
      models_binary = margot::here_read_arrow("models_binary", dir_path = local_drive, quiet = TRUE),
      policy_tree_stability = margot::here_read_arrow("policy_tree_stability", dir_path = local_drive, quiet = TRUE),
      policy_workflow = margot::here_read_arrow("policy_workflow", dir_path = local_drive, quiet = TRUE),
      cache_dir = local_drive
    ))
  }

  cache_dir <- path.expand(cache_dir)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  on_disk <- file.path(cache_dir, expected_files)
  needs_download <- refresh || !all(file.exists(on_disk))

  if (needs_download) {
    if (identical(url, "<paste-share-url-here>") || nchar(url) == 0) {
      stop(
        "no cache URL configured. set PSYC434_POLICY_LEARNING_CACHE_URL ",
        "to a Google Drive share URL of lab-09-cache.zip, or update the ",
        "default in causalworkshop's R/load_lab_09_cache.R."
      )
    }
    file_id <- .gdrive_file_id(url)
    zip_path <- file.path(cache_dir, "lab-09-cache.zip")
    message("downloading lab 9 cache (about 80 MB) ...")
    .gdrive_download(file_id, zip_path)
    message("unzipping ...")
    utils::unzip(zip_path, exdir = cache_dir, overwrite = TRUE)
    file.remove(zip_path)
  }

  missing <- expected_files[!file.exists(on_disk)]
  if (length(missing) > 0) {
    stop(
      "cache download finished but the following files are missing:\n  ",
      paste(missing, collapse = "\n  "),
      "\nthe download may have hit Google Drive's virus-scan page; ",
      "rebuild the cache so the zip stays under 100 MB."
    )
  }

  list(
    models_binary = margot::here_read_arrow("models_binary", dir_path = cache_dir, quiet = TRUE),
    policy_tree_stability = margot::here_read_arrow("policy_tree_stability", dir_path = cache_dir, quiet = TRUE),
    policy_workflow = margot::here_read_arrow("policy_workflow", dir_path = cache_dir, quiet = TRUE),
    cache_dir = cache_dir
  )
}

#' Load the PSYC 434 Lab 9 cache
#'
#' `load_lab_09_cache()` is soft-deprecated. Use
#' [load_policy_learning_cache()] instead.
#'
#' @inheritParams load_policy_learning_cache
#' @inherit load_policy_learning_cache return
#' @export
load_lab_09_cache <- function(...) {
  warning(
    "`load_lab_09_cache()` is soft-deprecated; ",
    "use `load_policy_learning_cache()` instead.",
    call. = FALSE
  )
  load_policy_learning_cache(...)
}

# default share URL for the policy-learning cache. update after re-fitting
# and uploading the parquet-based zip.
.LAB_09_CACHE_URL_DEFAULT <- "https://drive.google.com/file/d/1IvlurjbqcHSdSQ7KLcb0mAeXa1X4M26H/view?usp=sharing"

.policy_learning_cache_url <- function() {
  user_url <- Sys.getenv("PSYC434_POLICY_LEARNING_CACHE_URL", "")
  if (nzchar(user_url)) return(user_url)
  user_url <- Sys.getenv("PSYC434_LAB09_CACHE_URL", "")
  if (nzchar(user_url)) return(user_url)
  .LAB_09_CACHE_URL_DEFAULT
}
