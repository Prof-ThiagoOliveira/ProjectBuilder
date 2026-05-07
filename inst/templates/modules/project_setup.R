box::use(
  params = ./project_settings[get_global_parameters],
  pkgs = ./dependencies[install_project_packages, load_project_packages],
  pathmod = ./paths[project_paths, check_project_paths]
)

setup_project <- function(
    install_missing = FALSE,
    check_paths = TRUE,
    set_seed = TRUE) {
  if (isTRUE(install_missing)) {
    pkgs$install_project_packages()
  }

  pkgs$load_project_packages()

  project_params <- params$get_global_parameters()
  project_paths <- pathmod$project_paths()

  if (isTRUE(set_seed)) {
    set.seed(project_params$random_seed)
  }

  if (isTRUE(check_paths)) {
    pathmod$check_project_paths(project_paths)
  }

  invisible(
    list(
      parameters = project_params,
      paths = project_paths
    )
  )
}

box::export(setup_project)
