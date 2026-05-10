#' Get the PSYC 434 report template
#'
#' Copies the Option A research-report template bundled with the package to a
#' local directory. The template contains `setup.R`, `manuscript.qmd`,
#' `_quarto.yml`, references, and a local cache rule so students can fit the
#' model once and reuse it on later renders.
#'
#' @param dest_dir Character. Directory where the template should be copied.
#'   Relative paths are resolved from the current project root using
#'   [here::here()].
#' @param overwrite Logical. Whether to overwrite existing files.
#'
#' @return Character vector of copied file paths (invisibly).
#'
#' @examples
#' template_dir <- file.path(tempdir(), "research-report-template")
#' get_report_template(template_dir)
#'
#' @export
get_report_template <- function(dest_dir = "research-report-template",
                                overwrite = FALSE) {
  if (!is.character(dest_dir) || length(dest_dir) != 1 || !nzchar(dest_dir)) {
    cli::cli_abort("{.arg dest_dir} must be a single non-empty string.")
  }

  template_dir <- system.file("report-template", package = "causalworkshop")
  if (!dir.exists(template_dir)) {
    cli::cli_abort("Report template not found in package installation.")
  }

  dest_path <- .resolve_template_dest(dest_dir)
  if (!dir.exists(dest_path)) {
    dir.create(dest_path, recursive = TRUE)
    cli::cli_alert_success("Created directory: {dest_path}")
  }

  template_files <- list.files(
    template_dir,
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )

  copied_files <- character()
  for (file in template_files) {
    rel <- substring(file, nchar(template_dir) + 2L)
    if (identical(rel, "gitignore")) rel <- ".gitignore"

    dest_file <- file.path(dest_path, rel)
    if (file.exists(dest_file) && !overwrite) {
      cli::cli_alert_warning("Skipping {rel} (already exists)")
      next
    }

    dir.create(dirname(dest_file), showWarnings = FALSE, recursive = TRUE)
    success <- file.copy(file, dest_file, overwrite = overwrite)
    if (success) {
      cli::cli_alert_success("Copied {rel}")
      copied_files <- c(copied_files, dest_file)
    } else {
      cli::cli_alert_danger("Failed to copy {rel}")
    }
  }

  if (length(copied_files) > 0) {
    cli::cli_rule("Report Template Ready")
    cli::cli_alert_info("Template copied to: {dest_path}")
    cli::cli_alert_info("Open manuscript.qmd and edit setup.R first.")
  }

  invisible(copied_files)
}

.resolve_template_dest <- function(dest_dir) {
  expanded <- path.expand(dest_dir)
  is_absolute <- grepl("^/|^[A-Za-z]:[\\\\/]", expanded)
  if (is_absolute) {
    expanded
  } else {
    here::here(dest_dir)
  }
}
