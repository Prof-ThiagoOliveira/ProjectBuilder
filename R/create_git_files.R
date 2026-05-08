gitignore_template <- function(
    use_internal_data_dirs = FALSE,
    deliverables = character()) {
  lines <- c(
    ".projectSetupR/local.yml",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    ".Rproj.user/",
    "renv/library/",
    "*.RData",
    "*.rds",
    "*.qs",
    "*.parquet",
    "*.fst",
    "*.xlsx"
  )

  if (isTRUE(use_internal_data_dirs)) {
    lines <- c(lines, "data/")
  }

  if ("tables" %in% deliverables) {
    lines <- c(
      lines,
      "*.csv",
      "*.tsv",
      "!outputs/tables/*.csv",
      "!outputs/tables/*.tsv"
    )
  } else {
    lines <- c(lines, "*.csv", "*.tsv")
  }

  paste(unique(lines), collapse = "\n")
}

#' Create Git support files
#'
#' @param path Project root path.
#' @param overwrite Should existing files be overwritten?
#' @param use_internal_data_dirs Should internal data directories be ignored
#'   explicitly?
#' @param deliverables Project deliverables used to shape allow-lists for final
#'   tracked outputs.
#'
#' @return A list with created and skipped file paths.
create_git_files <- function(
    path,
    overwrite = FALSE,
    use_internal_data_dirs = FALSE,
    deliverables = character()) {
  results <- list(
    write_template_file(
      fs::path(path, ".gitignore"),
      gitignore_template(
        use_internal_data_dirs = use_internal_data_dirs,
        deliverables = deliverables
      ),
      overwrite = overwrite
    ),
    write_template_file(
      fs::path(path, ".Rbuildignore"),
      "^.*\\.Rproj$\n^\\.Rproj\\.user$\n^\\.git$\n^\\.gitignore$\n^\\.Rhistory$\n^\\.RData$",
      overwrite = overwrite
    ),
    write_template_file(
      fs::path(path, ".gitattributes"),
      "*.R text eol=lf\n*.qmd text eol=lf\n*.yml text eol=lf",
      overwrite = overwrite
    )
  )

  collect_template_results(results)
}
