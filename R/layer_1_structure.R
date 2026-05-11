# Layer 1: project structure -------------------------------------------------
#
# This layer describes the stable filesystem and metadata contract of a
# projflow project. It deliberately avoids running analysis code.

#' Describe the four projflow architecture layers
#'
#' @description
#' `project_layers()` returns the package architecture used by the redesigned
#' interface. The layers separate durable project structure, the executable
#' analysis DAG, governance records, and interactive/integration interfaces.
#'
#' @return A data frame with one row per architectural layer.
#' @examples
#' project_layers()
#' @author Thiago de Paula Oliveira
#' @export
project_layers <- function() {
  data.frame(
    layer = 1:4,
    name = c(
      "project_structure",
      "analysis_dag",
      "governance",
      "interfaces"
    ),
    responsibility = c(
      "Folders, project.yml, .projflow metadata, registry, local data roots and path conventions.",
      "Executable dependencies from data inputs to scripts, outputs, reports and deliverables.",
      "Tasks, risks, decisions, milestones, activity logs and project status records.",
      "User interfaces, dashboards, diagnostics, GitHub Actions, renv and targets integration."
    ),
    primary_functions = c(
      "plan_project(), new_project(), setup_project(), project_structure()",
      "project_analysis_dag(), validate_project_dag(), topological_project_order(), run_project()",
      "project_tasks(), project_risks(), project_decisions(), project_milestones()",
      "open_dashboard(), diagnose_project(), write_targets_pipeline(), use_github_actions()"
    ),
    stringsAsFactors = FALSE
  )
}

#' Inspect the project structure layer
#'
#' @description
#' `project_structure()` returns the canonical filesystem and metadata contract
#' for an existing projflow project. It is intended for users who need a compact
#' view of what the scaffold owns and what is missing before they inspect the
#' analysis DAG or governance layer.
#'
#' @param root Path inside an existing projflow project.
#'
#' @return A data frame describing canonical project paths and their existence.
#' @examples
#' \dontrun{
#' project_structure()
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_structure <- function(root = ".") {
  root <- find_project_root(root)
  paths <- project_paths(root)
  registry <- read_project_registry(root)
  local_config <- read_project_local_config(root)

  core <- data.frame(
    role = c(
      "root",
      "project_config",
      "metadata_directory",
      "registry",
      "local_config",
      "analysis_code",
      "reports",
      "outputs"
    ),
    path = normalize_relative_path(c(
      ".",
      "project.yml",
      project_metadata_relative_dir(root),
      project_registry_relative_path(root),
      project_local_config_relative_path(root),
      fs::path_rel(paths$analysis, start = root),
      fs::path_rel(paths$reports, start = root),
      fs::path_rel(paths$outputs, start = root)
    )),
    layer = "project_structure",
    stringsAsFactors = FALSE
  )

  if (!is.null(paths$data_raw)) {
    core <- rbind(
      core,
      data.frame(
        role = c("raw_data", "processed_data"),
        path = normalize_relative_path(c(
          fs::path_rel(paths$data_raw, start = root),
          fs::path_rel(paths$data_processed, start = root)
        )),
        layer = "project_structure",
        stringsAsFactors = FALSE
      )
    )
  }

  data_sources <- names(local_config$data_sources %||% list())
  if (length(data_sources) > 0L) {
    core <- rbind(
      core,
      data.frame(
        role = paste0("external_data_root:", data_sources),
        path = vapply(local_config$data_sources, function(x) x$path %||% "", character(1)),
        layer = "project_structure",
        stringsAsFactors = FALSE
      )
    )
  }

  core$exists <- vapply(
    core$path,
    function(path) {
      if (identical(path, ".")) {
        return(TRUE)
      }
      if (is_absolute_path(path)) {
        return(fs::file_exists(path) || fs::dir_exists(path))
      }
      fs::file_exists(fs::path(root, path)) || fs::dir_exists(fs::path(root, path))
    },
    logical(1)
  )
  core$registered_scripts <- length(registry$scripts %||% list())
  core$registered_reports <- length(registry$reports %||% list())
  core$registered_outputs <- length(registry$outputs %||% list())

  core
}
