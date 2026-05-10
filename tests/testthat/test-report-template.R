test_that("report template copies the student workflow", {
  dest <- file.path(tempdir(), paste0("research-report-template-", Sys.getpid()))
  copied <- get_report_template(dest)

  expect_true(file.exists(file.path(dest, "setup.R")))
  expect_true(file.exists(file.path(dest, "manuscript.qmd")))
  expect_true(file.exists(file.path(dest, "_quarto.yml")))
  expect_true(file.exists(file.path(dest, ".gitignore")))
  expect_true(any(basename(copied) == "setup.R"))
  expect_false(any(grepl("_cache|_output", copied)))
})
