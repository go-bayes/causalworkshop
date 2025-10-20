#' Get Workshop Scripts
#'
#' Copies all workshop scripts to a local directory for easy access.
#' This includes the complete educational progression from baseline adjustment
#' through advanced margot workflow analysis.
#'
#' @param dest_dir Character. Directory where scripts should be copied.
#'   Default is "workshop-scripts" in the project root directory (using here::here()).
#' @param overwrite Logical. Whether to overwrite existing files (default: FALSE)
#'
#' @return Character vector of copied file paths (invisibly)
#'
#' @details
#' This function copies the complete set of workshop scripts:
#' - 01-baseline-adjustment.R: Demonstrates selection bias and covariate adjustment
#' - 02-causal-forest-analysis.R: Basic causal forest implementation
#' - 03-rate-qini-curves.R: Targeting performance evaluation
#' - 04-policy-trees.R: Policy tree learning and interpretation
#' - 05-margot-workflow.R: Advanced workflow with margot ecosystem
#' - 06-margot-analysis.R: Stand-alone code review of ATE/CATE reporting
#' - 07-grf-style-simulation.R: Advanced data generator (GRF-style benchmark)
#'
#' @examples
#' # Copy scripts to default location
#' get_workshop_scripts()
#'
#' # Copy scripts to specific directory
#' get_workshop_scripts("my-workshop")
#'
#' # Overwrite existing scripts
#' get_workshop_scripts(overwrite = TRUE)
#'
#' @export
get_workshop_scripts <- function(dest_dir = "workshop-scripts", overwrite = FALSE) {

  # Use here::here() to resolve path relative to project root
  dest_path <- here::here(dest_dir)

  # Create destination directory
  if (!dir.exists(dest_path)) {
    dir.create(dest_path, recursive = TRUE)
    cli::cli_alert_success("Created directory: {dest_path}")
  }

  # Find source scripts directory
  scripts_dir <- system.file("scripts", package = "causalworkshop")

  if (!dir.exists(scripts_dir)) {
    cli::cli_abort("Workshop scripts not found in package installation")
  }

  # Get list of script files
  script_files <- list.files(scripts_dir, pattern = "\\.(R|qmd)$", full.names = TRUE)

  if (length(script_files) == 0) {
    cli::cli_abort("No script files found in package")
  }

  # Copy each file
  copied_files <- character()

  for (file in script_files) {
    filename <- basename(file)
    dest_file <- file.path(dest_path, filename)

    if (file.exists(dest_file) && !overwrite) {
      cli::cli_alert_warning("Skipping {filename} (already exists)")
      next
    }

    success <- file.copy(file, dest_file, overwrite = overwrite)

    if (success) {
      cli::cli_alert_success("Copied {filename}")
      copied_files <- c(copied_files, dest_file)
    } else {
      cli::cli_alert_danger("Failed to copy {filename}")
    }
  }

  if (length(copied_files) > 0) {
    cli::cli_rule("Workshop Scripts Ready")
    cli::cli_alert_info("Scripts copied to: {dest_path}")
    cli::cli_alert_info("Start with: 01-baseline-adjustment.R")
    cli::cli_alert_info("For help: ?get_workshop_scripts")
  }

  invisible(copied_files)
}


#' List Available Workshop Scripts
#'
#' Shows the workshop scripts included in the package with brief descriptions.
#'
#' @return Data frame with script information (invisibly)
#'
#' @examples
#' list_workshop_scripts()
#'
#' @export
list_workshop_scripts <- function() {

  scripts_info <- data.frame(
    Script = c(
      "01-baseline-adjustment.R",
      "02-causal-forest-analysis.R",
      "03-rate-qini-curves.R",
      "04-policy-trees.R",
      "05-margot-workflow.R",
      "06-margot-analysis.R",
      "07-grf-style-simulation.R"
    ),
    Description = c(
      "Demonstrates selection bias and baseline covariate adjustment",
      "Basic causal forest implementation with GRF",
      "Rate and Qini curve analysis for targeting evaluation",
      "Policy tree learning for treatment assignment rules",
      "Advanced workflow using the margot ecosystem",
      "Code review script for margot-based ATE/CATE analysis",
      "Alternative simulation matching GRF benchmark designs"
    ),
    Purpose = c(
      "Foundation concepts",
      "Core methodology",
      "Performance evaluation",
      "Decision rules",
      "Professional analysis",
      "Results interpretation",
      "Stress-testing forests"
    ),
    stringsAsFactors = FALSE
  )

  cli::cli_rule("Available Workshop Scripts")

  for (i in 1:nrow(scripts_info)) {
    cli::cli_alert_info("{scripts_info$Script[i]}")
    cli::cli_alert("  {scripts_info$Description[i]}")
    cli::cli_alert("  Purpose: {scripts_info$Purpose[i]}")
    cli::cli_text("")
  }

  cli::cli_alert_info("Use get_workshop_scripts() to copy all scripts to your working directory")

  invisible(scripts_info)
}
