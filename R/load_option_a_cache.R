#' Load a PSYC 434 Option A validation cache
#'
#' Parameterised analogue of [load_policy_learning_cache()] for the Option A
#' assessment, where students choose one of two exposures
#' (`"religious_service"` or `"volunteer_work"`). Each exposure has its
#' own zip on Google Drive containing three parquet files written by
#' [margot::here_save_arrow()]: `models_binary.parquet`,
#' `policy_tree_stability.parquet`, `policy_workflow.parquet`.
#'
#' @param exposure One of `"religious_service"` or `"volunteer_work"`.
#' @param cache_root Root directory where per-exposure caches are stored
#'   on disk. Defaults to a per-user location under
#'   [tools::R_user_dir()].
#' @param refresh If `TRUE`, re-download the selected cache.
#' @param refit If `TRUE`, skip the Drive download and refit locally via
#'   [fit_option_a()]. Use this when you would prefer not to deserialise
#'   a third-party blob, or to test a change to the simulator. Default
#'   settings take ~10-25 min on an M-series laptop.
#' @param ... Forwarded to [fit_option_a()] when `refit = TRUE`.
#'
#' @return A named list with elements `models_binary`,
#'   `policy_tree_stability`, `policy_workflow`, `cache_dir`, and
#'   `exposure`.
#'
#' @details The instructor's local Drive mirror is checked first
#'   (under `~/Library/CloudStorage/.../My Drive/courses/26-434/2026/option-a/<exposure>/`)
#'   so re-uploads do not stall the workflow while Drive syncs.
#'
#' @examples
#' \dontrun{
#' cache_rel <- load_option_a_cache("religious_service")
#' cache_vol <- load_option_a_cache("volunteer_work")
#' }
#'
#' @export
load_option_a_cache <- function(
  exposure = c("religious_service", "volunteer_work"),
  cache_root = tools::R_user_dir("psyc434", which = "cache"),
  refresh = FALSE,
  refit = FALSE,
  ...
) {
  exposure <- match.arg(exposure)
  if (isTRUE(refit)) {
    return(fit_option_a(exposure = exposure, ...))
  }
  .ensure_arrow_stack()

  meta <- .option_a_meta(exposure)

  expected_files <- c(
    "models_binary.parquet",
    "policy_tree_stability.parquet",
    "policy_workflow.parquet"
  )

  # local Drive mirror takes precedence on the author's machine.
  local_drive <- path.expand(file.path(
    "~/Library/CloudStorage/GoogleDrive-joseph.bulbulia@gmail.com",
    "My Drive/courses/26-434/2026/option-a",
    meta$drive_subdir
  ))
  if (all(file.exists(file.path(local_drive, expected_files)))) {
    return(list(
      models_binary = margot::here_read_arrow("models_binary", dir_path = local_drive, quiet = TRUE),
      policy_tree_stability = margot::here_read_arrow("policy_tree_stability", dir_path = local_drive, quiet = TRUE),
      policy_workflow = margot::here_read_arrow("policy_workflow", dir_path = local_drive, quiet = TRUE),
      cache_dir = local_drive,
      exposure = exposure
    ))
  }

  cache_dir <- path.expand(file.path(cache_root, meta$cache_subdir))
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  on_disk <- file.path(cache_dir, expected_files)
  needs_download <- refresh || !all(file.exists(on_disk))

  if (needs_download) {
    if (identical(meta$url, "<paste-share-url-here>") || nchar(meta$url) == 0) {
      stop(
        "no cache URL configured for ", exposure, ".\n",
        "set ", meta$env_var, " to a Google Drive share URL, or update ",
        "the default in causalworkshop's R/load_option_a_cache.R."
      )
    }
    file_id <- .gdrive_file_id(meta$url)
    zip_path <- file.path(cache_dir, meta$zip_name)
    message("downloading option-a ", exposure, " cache (about 80 MB) ...")
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
    cache_dir = cache_dir,
    exposure = exposure
  )
}

# default share URLs per exposure. update after re-fitting and uploading
# each parquet-based zip.
.OPT_A_RELIGIOUS_URL_DEFAULT <- "<paste-share-url-here>"
.OPT_A_VOLUNTEER_URL_DEFAULT <- "<paste-share-url-here>"

.option_a_meta <- function(exposure) {
  switch(
    exposure,
    religious_service = list(
      url = .opt_a_url("religious"),
      env_var = "PSYC434_OPT_A_RELIGIOUS_URL",
      drive_subdir = "religious",
      zip_name = "option-a-religious-cache.zip",
      cache_subdir = "option-a-religious"
    ),
    volunteer_work = list(
      url = .opt_a_url("volunteer"),
      env_var = "PSYC434_OPT_A_VOLUNTEER_URL",
      drive_subdir = "volunteer",
      zip_name = "option-a-volunteer-cache.zip",
      cache_subdir = "option-a-volunteer"
    ),
    stop(
      "exposure must be \"religious_service\" or \"volunteer_work\"; got: ",
      exposure
    )
  )
}

.opt_a_url <- function(which) {
  env_name <- switch(
    which,
    religious = "PSYC434_OPT_A_RELIGIOUS_URL",
    volunteer = "PSYC434_OPT_A_VOLUNTEER_URL",
    stop("unknown exposure: ", which)
  )
  user_url <- Sys.getenv(env_name, "")
  if (nzchar(user_url)) return(user_url)
  switch(
    which,
    religious = .OPT_A_RELIGIOUS_URL_DEFAULT,
    volunteer = .OPT_A_VOLUNTEER_URL_DEFAULT
  )
}
