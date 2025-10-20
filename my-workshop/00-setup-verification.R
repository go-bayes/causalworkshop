# Setup Verification for Causal Inference Workshop
# Verifies installation of required packages for causal forest analysis
# Author: Joseph Bulbulia, Victoria University of Wellington

# Load CLI for professional output
if (!requireNamespace("cli", quietly = TRUE)) {
  cat("Installing cli package for enhanced output...\n")
  install.packages("cli", quiet = TRUE)
}
library(cli)
cli_rule(left = "Workshop Setup Verification")

# Essential packages for causal inference (margot, script 5 is optional)
required_packages <- list(
  "tidyverse" = "Data manipulation and visualisation",
  "grf" = "Generalised Random Forests for causal inference",
  "policytree" = "Policy learning for treatment assignment",
  "cli" = "Enhanced command line interface",
  "devtools" = "Package development tools (for margot installation)",
  "purrr" = "Mapping plot labels",
  "here" = "Set paths",
  "fastpolicytree" = "Faster policy tree search (stability analysis)"
)

cli_alert_info("Checking installation of {length(required_packages)} required packages")

# Verification results
verification_results <- vector("logical", length(required_packages))
names(verification_results) <- names(required_packages)

cli_h2("Package Verification")

for (pkg in names(required_packages)) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    version <- packageVersion(pkg)
    cli_alert_success("{pkg} v{version} - {required_packages[[pkg]]}")
    verification_results[pkg] <- TRUE
  } else {
    cli_alert_danger("{pkg} not found - {required_packages[[pkg]]}")
    cli_alert_info("Install with: install.packages('{pkg}')")
    verification_results[pkg] <- FALSE
  }
}

# R version check
cli_h2("R Version Verification")
r_version <- R.Version()
current_version <- paste(r_version$major, r_version$minor, sep = ".")

if (as.numeric(r_version$major) >= 4) {
  cli_alert_success("R {current_version} meets requirements (>= 4.0.0)")
  r_ok <- TRUE
} else {
  cli_alert_danger("R {current_version} insufficient - please upgrade to >= 4.0.0")
  r_ok <- FALSE
}

# Final assessment
cli_rule(left = "Setup Assessment")

if (all(verification_results) && r_ok) {
  cli_alert_success("Environment ready for causal inference workshop")
  cli_alert_info("Proceed to run the demonstration scripts")
} else {
  missing_packages <- names(verification_results)[!verification_results]
  if (length(missing_packages) > 0) {
    cli_alert_warning("Missing packages: {paste(missing_packages, collapse = ', ')}")
  }
  if (!r_ok) {
    cli_alert_warning("R version insufficient")
  }
  cli_alert_info("Please install missing components before proceeding")
}

cli_rule()
