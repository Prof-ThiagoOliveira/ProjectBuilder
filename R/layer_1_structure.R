# Layer 1: project structure -------------------------------------------------
#
# This layer describes the stable filesystem and metadata contract of a
# projflow project. It deliberately avoids running analysis code.

#' Describe the four projflow architecture layers
#'
#' @description
#' \code{project_layers()} returns the package architecture used by the redesigned
#' interface. The layers separate durable project structure, the executable
#' analysis DAG, governance records, and interactive/integration interfaces.
#'
#' @return A data frame of class \code{"projflow_layers"} with one row per architectural layer. It prints as a compact user-facing summary.
#' @examples
#' project_layers()
#' @author Thiago de Paula Oliveira
#' @export
project_layers <- function() {
  layers <- data.frame(
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

  structure(layers, class = c("projflow_layers", class(layers)))
}

#' Print projflow architecture layers
#'
#' @description
#' Prints \code{project_layers()} as a compact user-facing summary rather than a
#' wide data frame. The underlying object remains tabular and can be inspected
#' with \code{as.data.frame()} or \code{unclass()}.
#'
#' @param x A \code{"projflow_layers"} object returned by \code{project_layers()}.
#' @param ... Additional arguments accepted for S3 compatibility but ignored.
#'
#' @return \code{x}, invisibly.
#' @examples
#' layers <- project_layers()
#' print(layers)
#' as.data.frame(layers)
#' @author Thiago de Paula Oliveira
#' @export
print.projflow_layers <- function(x, ...) {
  cat("projflow architecture layers\n")
  cat("----------------------------\n")

  for (i in seq_len(nrow(x))) {
    layer_label <- paste0("Layer ", x$layer[[i]], ": ", x$name[[i]])
    cat("\n", layer_label, "\n", sep = "")
    cat(paste(strwrap(
      paste0("Responsibility: ", x$responsibility[[i]]),
      width = getOption("width", 80L),
      indent = 2L,
      exdent = 2L
    ), collapse = "\n"), "\n", sep = "")
    cat(paste(strwrap(
      paste0("Primary functions: ", x$primary_functions[[i]]),
      width = getOption("width", 80L),
      indent = 2L,
      exdent = 2L
    ), collapse = "\n"), "\n", sep = "")
  }

  cat("\nUse as.data.frame(x) for the underlying table.\n")
  invisible(x)
}

#' Inspect the project structure layer
#'
#' @description
#' \code{project_structure()} returns the canonical filesystem and metadata contract
#' for an existing projflow project. It is intended for users who need a compact
#' view of what the scaffold owns and what is missing before they inspect the
#' analysis DAG or governance layer.
#'
#' @param root Path inside an existing projflow project.
#'
#' @return A data frame of class \code{"projflow_structure"} describing canonical project paths and their existence. It prints as a compact checklist.
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
  registered <- c(
    scripts = length(registry$scripts %||% list()),
    reports = length(registry$reports %||% list()),
    outputs = length(registry$outputs %||% list())
  )

  core$registered_scripts <- unname(registered[["scripts"]])
  core$registered_reports <- unname(registered[["reports"]])
  core$registered_outputs <- unname(registered[["outputs"]])

  structure(
    core,
    class = c("projflow_structure", class(core)),
    root = normalizePath(root, winslash = "/", mustWork = FALSE),
    registered = registered
  )
}

#' Print a projflow project structure summary
#'
#' @description
#' Prints \code{project_structure()} as a compact project-structure checklist.
#' The underlying object remains tabular and can be inspected with
#' \code{as.data.frame()} or \code{unclass()}.
#'
#' @param x A \code{"projflow_structure"} object returned by
#'   \code{project_structure()}.
#' @param ... Additional arguments accepted for S3 compatibility but ignored.
#'
#' @return \code{x}, invisibly.
#' @examples
#' \dontrun{
#' structure <- project_structure()
#' print(structure)
#' as.data.frame(structure)
#' }
#' @author Thiago de Paula Oliveira
#' @export
print.projflow_structure <- function(x, ...) {
  root <- attr(x, "root", exact = TRUE)
  registered <- attr(x, "registered", exact = TRUE)

  if (is.null(root) || !nzchar(root)) {
    root <- "."
  }

  if (is.null(registered)) {
    registered <- c(
      scripts = if ("registered_scripts" %in% names(x)) x$registered_scripts[[1]] else NA_integer_,
      reports = if ("registered_reports" %in% names(x)) x$registered_reports[[1]] else NA_integer_,
      outputs = if ("registered_outputs" %in% names(x)) x$registered_outputs[[1]] else NA_integer_
    )
  }

  cat("projflow project structure\n")
  cat("--------------------------\n")
  cat("Root: ", root, "\n", sep = "")
  cat(
    "Registered objects: ",
    registered[["scripts"]], " scripts; ",
    registered[["reports"]], " reports; ",
    registered[["outputs"]], " outputs\n",
    sep = ""
  )

  cat("\nStructure checklist:\n")

  roles <- as.character(x$role)
  paths <- as.character(x$path)
  exists <- as.logical(x$exists)
  exists[is.na(exists)] <- FALSE
  status <- ifelse(exists, "ok", "missing")
  role_width <- max(nchar(roles), na.rm = TRUE)

  for (i in seq_along(roles)) {
    cat(sprintf(
      "  [%-7s] %-*s %s\n",
      status[[i]],
      role_width,
      roles[[i]],
      paths[[i]]
    ))
  }

  missing_n <- sum(!exists)
  if (missing_n > 0L) {
    cat("\nMissing paths: ", missing_n, "\n", sep = "")
  } else {
    cat("\nAll registered structure paths exist.\n")
  }

  cat("Use as.data.frame(x) for the underlying table.\n")
  invisible(x)
}
