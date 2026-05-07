new_analysis_project_scaffold <- function(
    path,
    project_name,
    template,
    dependency_profile,
    code_loading,
    packages,
    directories_created,
    files_created,
    files_skipped,
    warnings,
    use_quarto,
    use_rmarkdown,
    use_targets,
    use_renv,
    use_git) {
  structure(
    list(
      path = path,
      project_name = project_name,
      template = template,
      dependency_profile = dependency_profile,
      code_loading = code_loading,
      packages = packages,
      directories_created = directories_created,
      files_created = files_created,
      files_skipped = files_skipped,
      warnings = warnings,
      use_quarto = use_quarto,
      use_rmarkdown = use_rmarkdown,
      use_targets = use_targets,
      use_renv = use_renv,
      use_git = use_git
    ),
    class = "analysis_project_scaffold"
  )
}

#' Print an analysis project scaffold result
#'
#' @param x An object of class `"analysis_project_scaffold"`.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
print.analysis_project_scaffold <- function(x, ...) {
  cat("Project name: ", x$project_name, "\n", sep = "")
  cat("Project path: ", x$path, "\n", sep = "")
  cat("Template: ", x$template, "\n", sep = "")
  cat("Dependency profile: ", x$dependency_profile, "\n", sep = "")
  cat("Code loading: ", x$code_loading, "\n", sep = "")
  cat("Packages: ", length(x$packages), "\n", sep = "")
  cat("Directories created: ", length(x$directories_created), "\n", sep = "")
  cat("Files created: ", length(x$files_created), "\n", sep = "")
  cat("Files skipped: ", length(x$files_skipped), "\n", sep = "")

  if (length(x$warnings) > 0L) {
    cat("\nWarnings:\n")
    cat(paste0("- ", x$warnings, collapse = "\n"), "\n", sep = "")
  }

  cat("\nStart here:\n")
  cat("1. Open: ", paste0(x$project_name, ".Rproj"), "\n", sep = "")
  cat("2. Run in R: source(\"scripts/00_start_here.R\")\n")
  cat("3. Read: PROJECT_GUIDE.md\n")
  cat("4. Add raw data to: data/raw/\n")
  cat("5. Edit: scripts/01_import_data.R\n")

  if (isTRUE(x$use_quarto)) {
    cat("* Render reports with: Rscript scripts/render_reports.R\n")
  }

  if (isTRUE(x$use_rmarkdown)) {
    cat("* Render R Markdown reports with: Rscript scripts/render_rmarkdown_reports.R\n")
  }

  if (isTRUE(x$use_targets)) {
    cat("* Run pipeline with: Rscript scripts/run_pipeline.R\n")
  }

  if (isTRUE(x$use_renv)) {
    cat("* Restore packages with: Rscript scripts/restore_environment.R\n")
  }

  invisible(x)
}
