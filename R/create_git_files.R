gitignore_template <- function(use_internal_data_dirs = FALSE) {
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
    "*.csv",
    "*.tsv",
    "*.xlsx"
  )

  if (isTRUE(use_internal_data_dirs)) {
    lines <- c(lines, "data/raw/", "data/processed/")
  }

  paste(unique(lines), collapse = "\n")
}

#' Create Git support files
#'
#' @param path Project root path.
#' @param overwrite Should existing files be overwritten?
#' @param use_internal_data_dirs Should internal data directories be ignored
#'   explicitly?
#'
#' @return A list with created and skipped file paths.
create_git_files <- function(path, overwrite = FALSE, use_internal_data_dirs = FALSE) {
  results <- list(
    write_template_file(
      fs::path(path, ".gitignore"),
      gitignore_template(use_internal_data_dirs = use_internal_data_dirs),
      overwrite = overwrite
    ),
    write_template_file(
      fs::path(path, ".Rbuildignore"),
      ".Rproj.user\n.Rhistory\n.RData",
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
