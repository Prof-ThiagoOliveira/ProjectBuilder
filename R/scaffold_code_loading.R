#' Create code loading scaffold files
#'
#' @param path Project root path.
#' @param project_name Project name.
#' @param code_loading Code loading strategy.
#' @param template_data Template data used for rendering.
#' @param overwrite Logical. Should existing files be overwritten?
#'
#' @return A list with created and skipped file paths.
scaffold_code_loading <- function(
    path,
    project_name,
    code_loading = c("package", "box", "source"),
    template_data = list(),
    overwrite = FALSE) {
  code_loading <- match.arg(code_loading)

  registry_group <- switch(
    code_loading,
    package = "code_loading_package",
    box = "code_loading_box",
    source = "code_loading_source"
  )

  write_registered_templates(
    path = path,
    project_name = project_name,
    registry = template_registry(registry_group),
    overwrite = overwrite,
    template_data = template_data
  )
}
