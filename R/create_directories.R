#' Create standard project directories
#'
#' @param path Project root path.
#' @param code_loading Code loading strategy.
#' @param dependency_profile Dependency profile for the generated project.
#' @param use_pkgdown Logical. Whether to create pkgdown output directories.
#' @param use_quarto Logical. Whether Quarto reports are enabled.
#' @param use_rmarkdown Logical. Whether R Markdown reports are enabled.
#'
#' @return Character vector of directories created during the call.
create_project_directories <- function(
    path,
    code_loading = "package",
    dependency_profile = "minimal",
    use_pkgdown = FALSE,
    use_quarto = TRUE,
    use_rmarkdown = FALSE) {
  directories <- c(
    "scripts",
    "data/raw",
    "data/external",
    "data/interim",
    "data/processed",
    "data/metadata",
    "outputs",
    "outputs/tables",
    "outputs/figures",
    "outputs/models",
    "outputs/reports",
    "outputs/logs",
    "tests/testthat",
    "reports"
  )

  if (isTRUE(use_quarto) || isTRUE(use_rmarkdown)) {
    directories <- c(directories, "reports/templates")
  }

  if (identical(code_loading, "package") || identical(code_loading, "source")) {
    directories <- c("R", directories)
  }

  if (identical(code_loading, "box")) {
    directories <- c("modules", directories)
  }

  if (isTRUE(use_pkgdown)) {
    directories <- c(directories, "docs")
  }

  if (identical(dependency_profile, "package-development")) {
    directories <- c(directories, "data-raw", "inst/extdata", "vignettes", "man")
  } else if (isTRUE(use_pkgdown)) {
    directories <- c(directories, "vignettes")
  }

  directories <- unique(directories)
  created <- character()

  for (directory in directories) {
    full_path <- fs::path(path, directory)

    if (!fs::dir_exists(full_path)) {
      fs::dir_create(full_path, recurse = TRUE)
      created <- c(created, full_path)
    }
  }

  created
}
