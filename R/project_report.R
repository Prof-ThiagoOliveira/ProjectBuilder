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
    use_git,
    preset = "analysis",
    mode = "simple",
    entrypoint = "run_project.R",
    guide = "README.md") {
  structure(
    list(
      path = path,
      project_name = project_name,
      preset = preset,
      mode = mode,
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
      use_git = use_git,
      entrypoint = entrypoint,
      guide = guide
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
  cat("Preset: ", x$preset, "\n", sep = "")
  cat("Mode: ", x$mode, "\n", sep = "")
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
  cat("2. Add raw data to: data/raw/\n")

  if (identical(x$entrypoint, "run_project.R")) {
    cat('3. Run in R: source("run_project.R")', "\n", sep = "")
    cat("4. Read: README.md\n")
  } else {
    cat('3. Run in R: source("scripts/00_start_here.R")', "\n", sep = "")
    cat("4. Read: PROJECT_GUIDE.md\n")
  }

  if (isTRUE(x$use_quarto)) {
    if (identical(x$mode, "simple")) {
      cat("* Render reports with: projectSetupR::render_project_reports()\n")
    } else {
      cat("* Render reports with: Rscript scripts/render_reports.R\n")
    }
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
