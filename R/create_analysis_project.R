filesystem_normalize <- function(paths) {
  if (length(paths) == 0L) {
    return(character())
  }

  fs::path_norm(paths)
}

#' Create a project from a project plan or scaffold arguments
#'
#' @description
#' \code{new_project()} materialises a \pkg{projflow} project on disk. The
#' recommended workflow is to first call \code{\link{plan_project}()} to
#' create and inspect a \code{"project_plan"} object, and then pass that object
#' to \code{new_project()} with the \code{plan} argument.
#'
#' @details
#' \code{new_project()} supports two equivalent user workflows:
#' \itemize{
#'   \item \strong{Planned workflow}: call \code{\link{plan_project}()} first,
#'     inspect the returned plan, and then call \code{new_project(plan = plan)}.
#'   \item \strong{Shortcut workflow}: call \code{new_project()} directly with
#'     scaffold arguments such as \code{path}, \code{components},
#'     \code{deliverables}, and \code{infrastructure}. In this case,
#'     \code{new_project()} constructs the plan internally and then applies it.
#' }
#'
#' The planned workflow is the clearest and safest interface for substantial
#' analytical projects because it separates project design from file creation.
#' The shortcut workflow is retained for convenience and backwards
#' compatibility.
#'
#' When \code{plan} is supplied, the scaffold-design arguments
#' \code{path}, \code{title}, \code{components}, \code{deliverables},
#' \code{infrastructure}, \code{preset}, \code{component_specs},
#' \code{use_internal_data_dirs}, and \code{include_example} must not be
#' supplied. This rule prevents accidental divergence between an inspected plan
#' and the project that is created.
#'
#' @param path Character scalar. Target project directory used when
#'   \code{plan = NULL}. The directory is created if needed and then populated
#'   with the scaffold described by the computed project plan. If \code{plan} is
#'   supplied, the target path is taken from \code{plan$path} and \code{path}
#'   must be omitted.
#' @param title Optional character scalar. Human-readable project title written
#'   into project metadata when \code{plan = NULL}. If omitted, the project
#'   folder name is used. Ignored when a plan is supplied.
#' @param components Character vector. Project components to include when
#'   \code{plan = NULL}, for example \code{"data_preparation"},
#'   \code{"statistical_analysis"}, \code{"report"}, \code{"tables"},
#'   or \code{"project_management"}. Components are validated by
#'   \code{\link{plan_project}()} before any files are written.
#' @param deliverables Optional character vector. Deliverables to prepare when
#'   \code{plan = NULL}, for example \code{"html_report"},
#'   \code{"tables"}, \code{"figures"}, or \code{"dashboard"}. When
#'   \code{NULL}, deliverables are inferred from the selected components and
#'   preset.
#' @param infrastructure Optional character vector. Technical infrastructure to
#'   enable when \code{plan = NULL}, for example \code{"git"},
#'   \code{"renv"}, \code{"quarto"}, \code{"tests"}, or
#'   \code{"github_actions"}. Use \code{NULL} to accept package defaults and
#'   \code{character()} to request no optional infrastructure.
#' @param preset Optional character scalar. Preset name used when
#'   \code{plan = NULL}. A preset expands to a predefined combination of
#'   components, deliverables, and infrastructure before explicit arguments are
#'   merged.
#' @param component_specs Optional custom component specification used when
#'   \code{plan = NULL}. This may be a YAML file path, a single
#'   component-specification list, or a list of component-specification objects.
#' @param use_internal_data_dirs Logical scalar. If \code{TRUE} and
#'   \code{plan = NULL}, create internal \file{data/raw/} and
#'   \file{data/processed/} directories. The default \code{FALSE} reflects the
#'   package preference for external data roots.
#' @param include_example Logical scalar. If \code{TRUE} and \code{plan = NULL},
#'   include the built-in \file{analysis/example_analysis.R} script.
#' @param plan Optional object of class \code{"project_plan"}, usually created
#'   by \code{\link{plan_project}()}. When supplied, \code{new_project()}
#'   applies this exact plan to disk and does not recompute the scaffold from
#'   separate component arguments.
#' @param open Logical scalar. Kept for API compatibility. \pkg{projflow} does
#'   not automatically open the project in an IDE or editor. If \code{TRUE}, a
#'   warning is added to the returned scaffold object.
#' @param overwrite Logical scalar. If \code{TRUE}, existing scaffold files may
#'   be overwritten where the underlying writer supports overwriting. The default
#'   \code{FALSE} is safer for existing directories.
#' @param repair Logical scalar. If \code{TRUE}, allow \pkg{projflow} to
#'   populate or register missing standard files in an existing project
#'   directory without overwriting files already present. This argument is
#'   relevant only when \code{plan = NULL}; a supplied plan is assumed to be the
#'   explicit creation contract.
#' @param dry_run Logical scalar. If \code{TRUE}, return the project plan without
#'   writing files. This is primarily useful for compatibility with direct
#'   \code{new_project()} calls; for new code, prefer
#'   \code{\link{plan_project}()}.
#'
#' @return
#' If \code{dry_run = TRUE}, an object of class \code{"project_plan"}. Otherwise,
#' an object of class \code{"analysis_project_scaffold"} describing the project
#' that was created or updated, including created directories, created files,
#' skipped files, selected infrastructure, package suggestions, and warnings.
#'
#' @seealso
#' \code{\link{plan_project}()}, \code{\link{plot.project_plan}()},
#' \code{\link{project_plan_network_data}()}, \code{\link{new_component}()},
#' \code{\link{new_script}()}, \code{\link{new_report}()},
#' \code{\link{new_output}()}
#'
#' @examples
#' \dontrun{
#' plan <- plan_project(
#'   path = "demo-project",
#'   components = c("data_preparation", "statistical_analysis", "report"),
#'   infrastructure = character()
#' )
#'
#' plot(plan)
#' new_project(plan = plan, open = FALSE)
#'
#' # Convenience shortcut: this computes the same type of plan internally and
#' # then creates the project.
#' new_project(
#'   path = "demo-project-shortcut",
#'   components = c("statistical_analysis", "report"),
#'   infrastructure = character(),
#'   open = FALSE
#' )
#' }
#'
#' @author Thiago de Paula Oliveira
#' @export
new_project <- function(
    path = NULL,
    title = NULL,
    components = c("statistical_analysis", "report"),
    deliverables = NULL,
    infrastructure = NULL,
    preset = NULL,
    component_specs = NULL,
    use_internal_data_dirs = FALSE,
    include_example = FALSE,
    plan = NULL,
    open = interactive(),
    overwrite = FALSE,
    repair = FALSE,
    dry_run = FALSE) {
  validate_logical_scalar(open, "open")
  validate_logical_scalar(overwrite, "overwrite")
  validate_logical_scalar(repair, "repair")
  validate_logical_scalar(dry_run, "dry_run")

  path_was_plan <- FALSE
  if (is.null(plan) && is_project_plan(path)) {
    plan <- path
    path <- NULL
    path_was_plan <- TRUE
  }

  if (!is.null(plan)) {
    supplied_design_args <- c(
      if (!path_was_plan && !missing(path)) "path" else character(),
      if (!missing(title)) "title" else character(),
      if (!missing(components)) "components" else character(),
      if (!missing(deliverables)) "deliverables" else character(),
      if (!missing(infrastructure)) "infrastructure" else character(),
      if (!missing(preset)) "preset" else character(),
      if (!missing(component_specs)) "component_specs" else character(),
      if (!missing(use_internal_data_dirs)) "use_internal_data_dirs" else character(),
      if (!missing(include_example)) "include_example" else character()
    )

    if (length(supplied_design_args) > 0L) {
      rlang::abort(paste0(
        "When `plan` is supplied, do not also supply scaffold-design arguments: ",
        paste(supplied_design_args, collapse = ", "),
        ". Inspect or modify the plan before calling `new_project(plan = plan)`."
      ))
    }

    validate_project_plan(plan)

    if (isTRUE(repair)) {
      rlang::abort("`repair = TRUE` cannot be combined with `plan`. Use `repair_project()` for an existing project, or create a new plan explicitly.")
    }

    path <- plan$path
    validate_project_path(path, overwrite = overwrite)

    if (isTRUE(dry_run)) {
      return(plan)
    }

    return(invisible(apply_project_plan(
      plan = plan,
      open = open,
      overwrite = overwrite,
      dry_run = FALSE
    )))
  }

  if (is.null(path)) {
    rlang::abort("`path` is required when `plan` is not supplied. Use `plan_project()` first, then call `new_project(plan = plan)`, or call `new_project(path = ...)`.")
  }

  validate_character_vector(path, "path")
  validate_character_vector(title, "title", allow_null = TRUE)
  validate_logical_scalar(use_internal_data_dirs, "use_internal_data_dirs")
  validate_logical_scalar(include_example, "include_example")

  raw_path <- trimws(path)
  path <- resolve_project_path(path)
  if (!isTRUE(repair)) {
    validate_project_path(path, overwrite = overwrite)
  } else if (fs::file_exists(path) && !fs::dir_exists(path)) {
    rlang::abort("`path` points to an existing file, not a directory.")
  }
  path_warning <- detect_path_construction_warning(raw_path, path)

  plan <- plan_project(
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
