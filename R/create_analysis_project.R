filesystem_normalize <- function(paths) {
  if (length(paths) == 0L) {
    return(character())
  }

  fs::path_norm(paths)
}

start_project_type_specs <- function() {
  list(
    analysis = list(label = "Analysis report", preset = "basic_analysis"),
    data_preparation = list(label = "Data preparation report", preset = "data_preparation_report"),
    reproducible_research = list(label = "Reproducible research", preset = "reproducible_research"),
    client_report = list(label = "Client report", preset = "client_report"),
    scientific_paper = list(label = "Scientific paper", preset = "scientific_paper"),
    dashboard = list(label = "Dashboard", preset = "dashboard")
  )
}

normalize_start_project_type <- function(type) {
  validate_choice(type, names(start_project_type_specs()), "type")
}

normalize_start_project_data_location <- function(data_location) {
  validate_choice(data_location, c("external", "internal"), "data_location")
}

start_project_prompt_select <- function(prompt, choices, labels = choices, default = choices[[1]]) {
  if (!interactive()) {
    rlang::abort(paste0("`", prompt, "` must be supplied when `interactive()` is FALSE."))
  }

  cat(prompt, "\n", sep = "")
  for (i in seq_along(choices)) {
    cat(i, ". ", labels[[i]], "\n", sep = "")
  }

  default_index <- match(default, choices)
  response <- trimws(readline(paste0("Choose [", default_index, "]: ")))
  if (!nzchar(response)) {
    return(default)
  }

  choice_index <- suppressWarnings(as.integer(response))
  if (is.na(choice_index) || choice_index < 1L || choice_index > length(choices)) {
    rlang::abort("Invalid selection.")
  }

  choices[[choice_index]]
}

start_project_prompt_yes_no <- function(prompt, default = TRUE) {
  if (!interactive()) {
    rlang::abort(paste0("`", prompt, "` must be supplied when `interactive()` is FALSE."))
  }

  suffix <- if (isTRUE(default)) "[Y/n]" else "[y/N]"
  response <- tolower(trimws(readline(paste0(prompt, " ", suffix, ": "))))

  if (!nzchar(response)) {
    return(isTRUE(default))
  }

  if (response %in% c("y", "yes")) {
    return(TRUE)
  }

  if (response %in% c("n", "no")) {
    return(FALSE)
  }

  rlang::abort("Please answer yes or no.")
}

start_project_report_paths <- function(plan) {
  if (length(plan$reports) == 0L) {
    return(character())
  }

  vapply(plan$reports, `[[`, character(1), "path")
}

start_project_switch_report_format <- function(plan, use_quarto = TRUE) {
  validate_logical_scalar(use_quarto, "use_quarto")

  if (isTRUE(use_quarto) || length(plan$reports) == 0L) {
    return(plan)
  }

  report_types <- vapply(plan$reports, `[[`, character(1), "type")
  if ("dashboard" %in% report_types) {
    rlang::abort(
      "Guided dashboard projects currently require Quarto. Use `start_project(..., use_quarto = TRUE)` or create a Shiny app workflow instead."
    )
  }

  old_paths <- start_project_report_paths(plan)
  new_paths <- sub("\\.qmd$", ".Rmd", old_paths)

  for (i in seq_along(plan$reports)) {
    plan$reports[[i]]$path <- new_paths[[i]]
  }

  if (length(plan$registry$reports %||% list()) > 0L) {
    for (name in names(plan$registry$reports)) {
      report_path <- plan$registry$reports[[name]]$path %||% NA_character_
      match_index <- match(report_path, old_paths)
      if (!is.na(match_index)) {
        plan$registry$reports[[name]]$path <- new_paths[[match_index]]
      }
    }
  }

  if (length(plan$files) > 0L) {
    matched <- match(plan$files, old_paths)
    replace_index <- which(!is.na(matched))
    if (length(replace_index) > 0L) {
      plan$files[[replace_index]] <- new_paths[matched[[replace_index]]]
    }
  }

  plan$files <- unique(setdiff(plan$files, "_quarto.yml"))
  plan$packages <- unique(c(setdiff(plan$packages, "quarto"), "rmarkdown"))

  plan
}

start_project_next_action <- function(root, data_location = "external") {
  root <- normalize_absolute_path(root)
  data_location <- normalize_start_project_data_location(data_location)

  if (identical(data_location, "external")) {
    return(paste0(
      "projflow::set_project_data_root(\"path/to/external/data\", root = \"",
      root,
      "\")"
    ))
  }

  paste0("projflow::setup_project(root = \"", root, "\")")
}

new_started_project <- function(result,
                                type,
                                data_location,
                                use_quarto,
                                use_renv,
                                use_git,
                                use_github_actions,
                                starter_files = character(),
                                next_action = NULL) {
  structure(
    utils::modifyList(
      result,
      list(
        start_project = list(
          type = type,
          data_location = data_location,
          use_quarto = isTRUE(use_quarto),
          use_renv = isTRUE(use_renv),
          use_git = isTRUE(use_git),
          use_github_actions = isTRUE(use_github_actions),
          starter_files = starter_files,
          next_action = next_action
        )
      )
    ),
    class = c("projflow_started_project", class(result))
  )
}

#' Start a project with a guided setup workflow
#'
#' @description
#' \code{start_project()} is an opinionated entry point for new users. It asks
#' only a small set of setup questions, translates them into a concrete
#' \pkg{projflow} scaffold, creates the project on disk, and prints the next
#' recommended action.
#'
#' The guided workflow is intentionally narrower than \code{\link{new_project}()}.
#' It is designed to reduce setup friction, not to expose every scaffold
#' combination. Advanced customisation should still use
#' \code{\link{plan_project}()} and \code{\link{new_project}()} directly.
#'
#' @param path Character scalar. Target project directory. If omitted in an
#'   interactive session, the function prompts for it.
#' @param title Optional character scalar. Human-readable project title written
#'   into project metadata.
#' @param type Character scalar. Guided project type. Supported values are
#'   \code{"analysis"}, \code{"data_preparation"},
#'   \code{"reproducible_research"}, \code{"client_report"},
#'   \code{"scientific_paper"}, and \code{"dashboard"}. If omitted in an
#'   interactive session, the function prompts for it.
#' @param data_location Character scalar. Either \code{"external"} or
#'   \code{"internal"}. External keeps raw data outside the repository; internal
#'   creates \file{data/raw/} and \file{data/processed/}.
#' @param use_quarto Logical scalar. If \code{TRUE}, starter reports are created
#'   as \file{.qmd} files and Quarto support is enabled. If \code{FALSE}, guided
#'   report projects are initialised with \file{.Rmd} reports instead.
#' @param use_renv Logical scalar. If \code{TRUE}, initialise \pkg{renv}.
#' @param use_git Logical scalar. If \code{TRUE}, configure Git support.
#' @param use_github_actions Logical scalar. If \code{TRUE}, configure the
#'   default GitHub Actions workflow. This implies \code{use_git = TRUE}.
#' @param open Logical scalar. Passed through to \code{\link{new_project}()}.
#' @param overwrite Logical scalar. Passed through to \code{\link{new_project}()}.
#'
#' @return
#' An object of class \code{"projflow_started_project"} and
#' \code{"analysis_project_scaffold"}.
#'
#' @examples
#' \dontrun{
#' start_project(
#'   path = "demo-project",
#'   type = "analysis",
#'   data_location = "external",
#'   use_quarto = TRUE,
#'   use_renv = FALSE,
#'   use_git = TRUE,
#'   use_github_actions = FALSE,
#'   open = FALSE
#' )
#' }
#'
#' @author Thiago de Paula Oliveira
#' @export
start_project <- function(path = NULL,
                          title = NULL,
                          type = NULL,
                          data_location = NULL,
                          use_quarto = NULL,
                          use_renv = NULL,
                          use_git = NULL,
                          use_github_actions = NULL,
                          open = interactive(),
                          overwrite = FALSE) {
  validate_character_vector(title, "title", allow_null = TRUE)
  validate_logical_scalar(open, "open")
  validate_logical_scalar(overwrite, "overwrite")

  type_specs <- start_project_type_specs()

  if (is.null(path)) {
    if (!interactive()) {
      rlang::abort("`path` is required when `interactive()` is FALSE.")
    }
    path <- trimws(readline("Project path: "))
    if (!nzchar(path)) {
      rlang::abort("A non-empty `path` is required.")
    }
  }

  if (is.null(type)) {
    type <- start_project_prompt_select(
      prompt = "What type of project is this?",
      choices = names(type_specs),
      labels = vapply(type_specs, `[[`, character(1), "label"),
      default = "analysis"
    )
  }
  type <- normalize_start_project_type(type)

  if (is.null(data_location)) {
    data_location <- start_project_prompt_select(
      prompt = "Will data be internal or external?",
      choices = c("external", "internal"),
      labels = c("External data outside the repository", "Internal data folders inside the repository"),
      default = "external"
    )
  }
  data_location <- normalize_start_project_data_location(data_location)

  if (is.null(use_quarto)) {
    use_quarto <- start_project_prompt_yes_no("Should reports use Quarto?", default = TRUE)
  }
  validate_logical_scalar(use_quarto, "use_quarto")

  if (is.null(use_renv)) {
    use_renv <- start_project_prompt_yes_no("Should renv be initialised?", default = FALSE)
  }
  validate_logical_scalar(use_renv, "use_renv")

  if (is.null(use_git) && is.null(use_github_actions)) {
    git_mode <- start_project_prompt_select(
      prompt = "Should Git or GitHub Actions be configured?",
      choices = c("none", "git", "github_actions"),
      labels = c("Neither", "Git only", "Git and GitHub Actions"),
      default = "git"
    )
    use_git <- !identical(git_mode, "none")
    use_github_actions <- identical(git_mode, "github_actions")
  } else {
    if (is.null(use_git)) {
      use_git <- FALSE
    }
    if (is.null(use_github_actions)) {
      use_github_actions <- FALSE
    }
  }
  validate_logical_scalar(use_git, "use_git")
  validate_logical_scalar(use_github_actions, "use_github_actions")

  if (isTRUE(use_github_actions)) {
    use_git <- TRUE
  }

  base_spec <- project_presets()[[type_specs[[type]]$preset]]
  infrastructure <- c(
    if (isTRUE(use_git)) "git" else character(),
    if (isTRUE(use_renv)) "renv" else character(),
    if (isTRUE(use_quarto)) "quarto" else character(),
    if (isTRUE(use_github_actions)) "github_actions" else character()
  )

  plan <- plan_project(
    path = path,
    title = title,
    components = base_spec$components,
    deliverables = base_spec$deliverables,
    infrastructure = unique(infrastructure),
    use_internal_data_dirs = identical(data_location, "internal"),
    include_example = FALSE
  )
  plan <- start_project_switch_report_format(plan, use_quarto = use_quarto)

  starter_files <- c(
    vapply(plan$scripts, `[[`, character(1), "path"),
    vapply(plan$reports, `[[`, character(1), "path")
  )

  result <- new_project(
    plan = plan,
    open = open,
    overwrite = overwrite
  )

  new_started_project(
    result = result,
    type = type,
    data_location = data_location,
    use_quarto = use_quarto,
    use_renv = use_renv,
    use_git = use_git,
    use_github_actions = use_github_actions,
    starter_files = starter_files,
    next_action = start_project_next_action(plan$path, data_location = data_location)
  )
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
#'   include a numbered, comment-only example analysis script.
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
