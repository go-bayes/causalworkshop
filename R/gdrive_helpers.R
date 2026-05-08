# private helpers shared by load_policy_learning_cache() and load_report_workflow_cache().
# extract a Google Drive file ID from a share URL and download a public
# file via googledrive's authenticated-but-deauthed flow, which handles
# the virus-scan confirmation page that trips simple curl downloads on
# zips larger than ~25 MB.

.ensure_arrow_stack <- function() {
  for (pkg in c("arrow", "qs2", "margot")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "package '", pkg, "' is required to read the parquet-based ",
        "cache. install with install.packages('", pkg, "')",
        if (pkg == "margot") " or pak::pak('go-bayes/margot')" else "",
        "."
      )
    }
  }
  invisible(TRUE)
}

.gdrive_file_id <- function(share_url) {
  if (grepl("/drive/folders/", share_url)) {
    stop(
      "the URL points at a Google Drive *folder*, not a *file*:\n  ",
      share_url,
      "\nopen Drive in a browser, right-click the cache zip ",
      "(not the parent folder), choose Get link > Anyone with the link > ",
      "Viewer, and paste THAT URL. it should look like\n",
      "  https://drive.google.com/file/d/<FILE_ID>/view?usp=sharing"
    )
  }
  # accept both canonical "/d/<ID>/..." and query-form "?id=<ID>" URLs.
  m_path <- regmatches(share_url, regexec("/d/([^/?#]+)", share_url))[[1]]
  if (length(m_path) >= 2 && nchar(m_path[2]) > 0) return(m_path[2])
  m_qry <- regmatches(share_url, regexec("[?&]id=([^&#]+)", share_url))[[1]]
  if (length(m_qry) >= 2 && nchar(m_qry[2]) > 0) return(m_qry[2])
  stop(
    "could not extract file ID from URL:\n  ", share_url,
    "\nexpected one of:\n",
    "  https://drive.google.com/file/d/<FILE_ID>/view?usp=sharing\n",
    "  https://drive.google.com/open?id=<FILE_ID>"
  )
}

.gdrive_download <- function(file_id, dest) {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    install.packages("googledrive")
  }
  suppressMessages(googledrive::drive_deauth())
  googledrive::drive_download(
    googledrive::as_id(file_id),
    path = dest,
    overwrite = TRUE
  )
  invisible(dest)
}
