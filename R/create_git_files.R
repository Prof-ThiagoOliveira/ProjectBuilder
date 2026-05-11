gitignore_template <- function(
    use_internal_data_dirs = FALSE,
    deliverables = character()) {
  lines <- c(
    ".projflow/local.yml",
    ".projflow/activity_log.yml",
    ".projflow/backups/",
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
#' @param path Project root path where \code{.gitignore}, \code{.Rbuildignore}, and
#'   \code{.gitattributes} should be written.
#' @param overwrite Logical scalar. If \code{TRUE}, existing Git support files are
#'   replaced; if \code{FALSE}, existing files are preserved.
#' @param use_internal_data_dirs Logical scalar indicating whether the scaffold
#'   includes internal \code{data/} folders that should be ignored explicitly in the
#'   generated \code{.gitignore}.
#' @param deliverables Character vector of project deliverables. This is used to
#'   decide whether output tables should remain ignored broadly or whether
#'   selected \code{outputs/tables/} files should be allow-listed for tracking.
#'
#' @return A list with created and skipped file paths.
#' @examples
#' \dontrun{
#' tmp <- tempfile("projflow-git-")
#' dir.create(tmp)
#' projflow:::create_git_files(
#'   path = tmp,
#'   use_internal_data_dirs = FALSE,
#'   deliverables = c("html_report", "tables")
#' )
#' }
#' @author Thiago de Paula Oliveira
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
      paste(
        c(
          "^.*\\.Rproj$",
          "^\\.Rproj\\.user$",
          "^\\.git$",
          "^\\.gitignore$",
          "^\\.Rhistory$",
          "^\\.RData$",
          "^projflow\\.Rcheck$",
          "^projflow_.*\\.tar\\.gz$",
          "^.*~$",
          "^.*\\.tmp$",
          "^.*\\.bak$",
          "^\\.DS_Store$"
        ),
        collapse = "\n"
      ),
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
