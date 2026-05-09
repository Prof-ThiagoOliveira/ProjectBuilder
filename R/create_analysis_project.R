filesystem_normalize <- function(paths) {
  if (length(paths) == 0L) {
    return(character())
  }

  fs::path_norm(paths)
}

#' Create a component-driven project
#'
#' @param path Target project path. The directory is created if needed and then
#'   populated with the scaffold selected by the remaining arguments.
#' @param title Optional human-readable project title written into project
#'   metadata. If omitted, the project folder name is used.
#' @param components Character vector of project components to include, such as
#'   `"statistical_analysis"`, `"report"`, `"tables"`, or
#'   `"project_management"`.
#' @param deliverables Optional character vector of deliverables to prepare,
#'   such as `"html_report"`, `"tables"`, or `"dashboard"`. When `NULL`,
#'   deliverables are inferred from components.
#' @param infrastructure Optional character vector of technical features to
#'   enable, such as `"git"`, `"tests"`, `"quarto"`, or `"renv"`. Use
#'   `NULL` to accept default infrastructure or `character()` to disable
#'   inferred infrastructure.
#' @param preset Optional preset name that expands to a predefined combination
#'   of components, deliverables, and infrastructure before explicit arguments
#'   are merged in.
#' @param component_specs Optional custom component spec path, a single spec
#'   list, or a list of spec objects used to extend the built-in component
#'   registry for this project.
#' @param use_internal_data_dirs Logical scalar. If `TRUE`, create internal
#'   `data/raw/` and `data/processed/` folders. The default `FALSE` keeps the
#'   package's external-data-first behavior.
#' @param include_example Logical scalar. If `TRUE`, include the built-in
#'   `analysis/example_analysis.R` example script. The default `FALSE` keeps the
#'   scaffold minimal and avoids generating starter artefacts unless requested.
#' @param open Logical scalar kept for API compatibility. `projflow` does not
#'   automatically open the project in an IDE or editor.
#' @param overwrite Logical scalar indicating whether an existing non-empty
#'   target directory may be populated or updated.
#' @param repair Logical scalar. If `TRUE`, allow `projflow` to populate or
#'   register missing standard files in an existing project directory without
#'   overwriting files already present.
#' @param dry_run Logical scalar. If `TRUE`, return the computed project plan
#'   without writing any files.
#'
#' @return An object of class `"analysis_project_scaffold"`.
#' @examples
#' \dontrun{
#' new_project(
#'   path = "demo-project",
#'   preset = "basic_analysis",
#'   components = c("statistical_analysis", "report"),
#'   infrastructure = character(),
#'   open = FALSE
#' )
#' }
#' @author Thiago de Paula Oliveira
#' @export
new_project <- function(
    path,
    title = NULL,
    components = c("statistical_analysis", "report"),
    deliverables = NULL,
    infrastructure = NULL,
    preset = NULL,
    component_specs = NULL,
    use_internal_data_dirs = FALSE,
    include_example = FALSE,
    open = interactive(),
    overwrite = FALSE,
    repair = FALSE,
    dry_run = FALSE) {
  validate_character_vector(path, "path")
  validate_character_vector(title, "title", allow_null = TRUE)
  validate_logical_scalar(use_internal_data_dirs, "use_internal_data_dirs")
  validate_logical_scalar(include_example, "include_example")
  validate_logical_scalar(open, "open")
  validate_logical_scalar(overwrite, "overwrite")
  validate_logical_scalar(repair, "repair")
  validate_logical_scalar(dry_run, "dry_run")

  raw_path <- trimws(path)
  path <- resolve_project_path(path)
  if (!isTRUE(repair)) {
    validate_project_path(path, overwrite = overwrite)
  } else if (fs::file_exists(path) && !fs::dir_exists(path)) {
    rlang::abort("`path` points to an existing file, not a directory.")
  }
  path_warning <- detect_path_construction_warning(raw_path, path)

  plan <- build_project_plan(
    path = path,
    title = title,
    components = components,
    deliverables = deliverables,
    infrastructure = infrastructure,
    preset = preset,
    component_specs = component_specs,
    use_internal_data_dirs = use_internal_data_dirs,
    include_example = include_example
  )

  if (isTRUE(dry_run)) {
    return(plan)
  }

  result <- apply_project_plan(
    plan = plan,
    open = open,
    overwrite = overwrite,
    dry_run = FALSE
  )

  if (!is.null(path_warning)) {
    result$warnings <- unique(c(result$warnings, path_warning))
  }

  invisible(result)
}
