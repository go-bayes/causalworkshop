skip_if_lavaan_unavailable <- function() {
  skip_if_not_installed("lavaan")
  skip_if(
    is.na(parallel::detectCores()),
    "lavaan requires parallel::detectCores() to report available CPUs"
  )
}
