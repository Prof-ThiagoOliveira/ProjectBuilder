write_template_file <- function(path, content, overwrite = FALSE) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  existed <- fs::file_exists(path)

  if (existed && !isTRUE(overwrite)) {
    return(list(path = path, status = "skipped"))
  }

  fs::dir_create(fs::path_dir(path), recurse = TRUE)
  writeLines(enc2utf8(content), path, useBytes = TRUE)

  list(
    path = path,
    status = if (existed) "overwritten" else "created"
  )
}

collect_template_results <- function(results) {
  paths <- vapply(results, `[[`, character(1), "path")
  statuses <- vapply(results, `[[`, character(1), "status")

  list(
    files_created = paths[statuses %in% c("created", "overwritten")],
    files_skipped = paths[!statuses %in% c("created", "overwritten")]
  )
}
