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
    preset = "basic_analysis",
    scaffold_level = "simple",
    entrypoint = "run_project.R",
    guide = "README.md") {
  structure(
    list(
      path = path,
      project_name = project_name,
      preset = preset,
      scaffold_level = scaffold_level,
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
#' @param x An object of class \code{"analysis_project_scaffold"}, typically the
#'   result returned by \code{new_project()}.
#' @param ... Additional arguments accepted for S3 compatibility but ignored by
#'   this method.
#'
#' @return \code{x}, invisibly.
#' @examples
#' \dontrun{
#' result <- new_project(
#'   path = "demo-project",
#'   components = c("statistical_analysis", "report"),
#'   infrastructure = character(),
#'   open = FALSE
#' )
#' print(result)
#' }
#' @author Thiago de Paula Oliveira
#' @export
print.analysis_project_scaffold <- function(x, ...) {
  cat("Project name: ", x$project_name, "\n", sep = "")
  cat("Project path: ", x$path, "\n", sep = "")
  cat("Preset: ", x$preset, "\n", sep = "")
  cat("Scaffold level: ", x$scaffold_level, "\n", sep = "")
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
  cat("2. Configure external data with: projflow::set_project_data_root(\"path/to/external/data\")\n")
  cat("3. Check the project with: projflow::check_project()\n")
  cat("4. Build the project with: projflow::build_project()\n")

  if (isTRUE(x$use_quarto)) {
    cat("* Serve reports or dashboards with: projflow::serve_project()\n")
  }

  if (isTRUE(x$use_renv)) {
    cat("* Restore packages with: renv::restore()\n")
  }

  invisible(x)
}

#' Print a guided start-project result
#'
#' @param x An object of class \code{"projflow_started_project"} returned by
#'   \code{\link{start_project}()}.
#' @param ... Additional arguments accepted for S3 compatibility but ignored.
#'
#' @return \code{x}, invisibly.
#' @examples
#' \dontrun{
#' result <- start_project(
#'   path = "demo-project",
#'   type = "analysis",
#'   data_location = "external",
#'   use_quarto = TRUE,
#'   use_renv = FALSE,
#'   use_git = TRUE,
#'   use_github_actions = FALSE,
#'   open = FALSE
#' )
#' print(result)
#' }
#' @author Thiago de Paula Oliveira
#' @export
print.projflow_started_project <- function(x, ...) {
  details <- x$start_project %||% list()
  type_specs <- get("start_project_type_specs", mode = "function")()
  type_label <- type_specs[[details$type %||% "analysis"]]$label %||% (details$type %||% "analysis")

  cat("projflow started project\n")
  cat("------------------------\n")
  cat("Project: ", x$project_name, "\n", sep = "")
  cat("Root: ", x$path, "\n", sep = "")
  cat("Workflow: ", type_label, "\n", sep = "")
  cat("Data: ", if (identical(details$data_location, "internal")) "internal repository folders" else "external data root", "\n", sep = "")
  cat("Reports: ", if (isTRUE(details$use_quarto)) "Quarto" else "R Markdown", "\n", sep = "")

  infrastructure <- c(
    if (isTRUE(details$use_git)) "git" else character(),
    if (isTRUE(details$use_github_actions)) "github_actions" else character(),
    if (isTRUE(details$use_renv)) "renv" else character()
  )
  cat("Infrastructure: ", if (length(infrastructure) == 0L) "none" else paste(infrastructure, collapse = ", "), "\n", sep = "")
  cat("Created: ", length(x$directories_created), " director", if (length(x$directories_created) == 1L) "y" else "ies", "; ", length(x$files_created), " file", if (length(x$files_created) == 1L) "" else "s", "\n", sep = "")

  starter_files <- unique(details$starter_files %||% character())
  if (length(starter_files) > 0L) {
    cat("\nStarter files:\n")
    for (path in utils::head(starter_files, 5L)) {
      cat("  - ", path, "\n", sep = "")
    }
    if (length(starter_files) > 5L) {
      cat("  - ...\n")
    }
  }

  if (length(x$warnings) > 0L) {
    cat("\nWarnings:\n")
    cat(paste0("- ", x$warnings, collapse = "\n"), "\n", sep = "")
  }

  cat("\nNext action:\n")
  cat("  ", details$next_action %||% paste0("projflow::setup_project(root = \"", x$path, "\")"), "\n", sep = "")
  cat("  projflow::check_project(root = \"", x$path, "\")\n", sep = "")

  invisible(x)
}
