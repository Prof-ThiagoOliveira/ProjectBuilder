#' Set up project session
#'
#' Loads project packages, project parameters, paths, and optional checks.
#'
#' @param install_missing Logical. Should missing project packages be installed?
#' @param check_paths Logical. Should standard project paths be checked?
#' @param set_seed Logical. Should the project random seed be set?
#'
#' @return A named list with project parameters and paths.
#' @export
setup_project <- function(
    install_missing = FALSE,
    check_paths = TRUE,
    set_seed = TRUE) {
  if (isTRUE(install_missing)) {
    install_project_packages()
  }

  load_project_packages()

  params <- get_global_parameters()
  paths <- project_paths()

  if (isTRUE(set_seed)) {
    set.seed(params$random_seed)
  }

  if (isTRUE(check_paths)) {
    check_project_paths(paths)
  }

  invisible(
    list(
      parameters = params,
      paths = paths
    )
  )
}
