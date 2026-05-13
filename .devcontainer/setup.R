# prepare a Codespaces R environment for causalworkshop students
options(
  repos = c(CRAN = "https://p3m.dev/cran/__linux__/noble/latest"),
  Ncpus = max(1L, parallel::detectCores(logical = TRUE) - 1L)
)

cran_packages <- c(
  "arrow",
  "devtools",
  "googledrive",
  "httpgd",
  "knitr",
  "languageserver",
  "lavaan",
  "mgcv",
  "pak",
  "policytree",
  "fastpolicytree",
  "qs2",
  "rmarkdown",
  "testthat"
)

missing_cran <- setdiff(cran_packages, rownames(installed.packages()))
if (length(missing_cran) > 0L) {
  install.packages(missing_cran)
}

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

# install the GitHub-only dependency before installing the local package
if (!requireNamespace("margot", quietly = TRUE)) {
  pak::pkg_install("go-bayes/margot", ask = FALSE)
}

# install the checked-out repository so examples work from a fresh Codespace
pak::local_install(".", dependencies = TRUE, ask = FALSE)

library(causalworkshop)
check_workshop_prerequisites(include_optional = TRUE)
