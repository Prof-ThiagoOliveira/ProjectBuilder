#' Project paths
#'
#' Returns standard project paths relative to the project root.
#'
#' @param root Project root. Defaults to `here::here()`.
#'
#' @return A named list of paths.
project_paths <- function(root = here::here()) {
  list(
    root = root,
    data_raw = file.path(root, "data", "raw"),
    data_external = file.path(root, "data", "external"),
    data_interim = file.path(root, "data", "interim"),
    data_processed = file.path(root, "data", "processed"),
    data_metadata = file.path(root, "data", "metadata"),
    outputs = file.path(root, "outputs"),
    tables = file.path(root, "outputs", "tables"),
    figures = file.path(root, "outputs", "figures"),
    models = file.path(root, "outputs", "models"),
    reports = file.path(root, "outputs", "reports"),
    logs = file.path(root, "outputs", "logs")
  )
}

#' Check that project paths exist
#'
#' @param paths Named list of paths from `project_paths()`.
#'
#' @return Invisibly returns TRUE if all paths exist.
check_project_paths <- function(paths = project_paths()) {
  path_values <- unlist(paths, use.names = FALSE)
  missing_paths <- path_values[!dir.exists(path_values)]

  if (length(missing_paths) > 0L) {
    stop(
      "Missing project directories: ",
      paste(missing_paths, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
