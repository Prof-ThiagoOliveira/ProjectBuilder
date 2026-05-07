#' Create targets scaffold files
#'
#' @param path Project root path.
#' @param project_name Project name.
#' @param template_data Template data used for rendering.
#' @param overwrite Logical. Should existing files be overwritten?
#'
#' @return A list with created and skipped file paths.
scaffold_targets <- function(
    path,
    project_name,
    template_data = list(),
    overwrite = FALSE) {
  write_registered_templates(
    path = path,
    project_name = project_name,
    registry = template_registry("targets"),
    overwrite = overwrite,
    template_data = template_data
  )
}
