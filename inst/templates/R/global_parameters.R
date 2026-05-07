#' Global project parameters
#'
#' Centralises project-level constants used across scripts and reports.
#' Do not store passwords, API keys, database credentials, or other secrets here.
#'
#' @return A named list of project parameters.
#' @export
get_global_parameters <- function() {
  if (requireNamespace("config", quietly = TRUE) && file.exists("config.yml")) {
    cfg <- config::get()

    return(
      list(
        project_name = cfg$project_name,
        dependency_profile = cfg$dependency_profile,
        project_version = "0.0.1",
        project_owner = "name_or_team",
        report_date = Sys.Date(),
        random_seed = cfg$seed,
        timezone = cfg$timezone,
        default_output_format = "html"
      )
    )
  }

  list(
    project_name = "{{ project_name }}",
    dependency_profile = "{{ dependency_profile }}",
    project_version = "0.0.1",
    project_owner = "name_or_team",
    report_date = Sys.Date(),
    random_seed = 12345,
    timezone = "Europe/London",
    default_output_format = "html"
  )
}
