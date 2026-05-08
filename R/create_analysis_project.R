filesystem_normalize <- function(paths) {
  if (length(paths) == 0L) {
    return(character())
  }

  fs::path_norm(paths)
}

#' Create a component-driven project
#'
#' @param path Target project path.
#' @param title Optional project title.
#' @param components Project components to include.
#' @param deliverables Deliverables to prepare.
#' @param infrastructure Technical support features to enable.
#' @param preset Optional preset shortcut.
#' @param use_internal_data_dirs Should internal `data/raw/` and
#'   `data/processed/` folders be created? Defaults to `FALSE`.
#' @param include_example Should example files be created?
#' @param open Included for API compatibility. Project opening is not
#'   automated.
#' @param overwrite Should existing files be overwritten?
#'
#' @return An object of class `"analysis_project_scaffold"`.
#' @export
new_project <- function(
    path,
    title = NULL,
    components = c("statistical_analysis", "report"),
    deliverables = NULL,
    infrastructure = NULL,
    preset = NULL,
    use_internal_data_dirs = FALSE,
    include_example = TRUE,
    open = interactive(),
    overwrite = FALSE) {
  validate_character_vector(path, "path")
  validate_character_vector(title, "title", allow_null = TRUE)
  validate_logical_scalar(use_internal_data_dirs, "use_internal_data_dirs")
  validate_logical_scalar(include_example, "include_example")
  validate_logical_scalar(open, "open")
  validate_logical_scalar(overwrite, "overwrite")

  raw_path <- trimws(path)
  path <- resolve_project_path(path)
  validate_project_path(path, overwrite = overwrite)
  path_warning <- detect_path_construction_warning(raw_path, path)

  plan <- build_project_plan(
    path = path,
    title = title,
    components = components,
    deliverables = deliverables,
    infrastructure = infrastructure,
    preset = preset,
    use_internal_data_dirs = use_internal_data_dirs,
    include_example = include_example
  )

  result <- apply_project_plan(
    plan = plan,
    open = open,
    overwrite = overwrite
  )

  if (!is.null(path_warning)) {
    result$warnings <- unique(c(result$warnings, path_warning))
  }

  invisible(result)
}
