#' List machine-readable project check items
#'
#' @param root Existing project root.
#' @param deep Logical scalar passed to [check_project()].
#' @param render_reports Logical scalar passed to [check_project()].
#'
#' @return Data frame of project check items.
#' @examples
#' \dontrun{
#' project_check_items()
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_check_items <- function(root = ".", deep = FALSE, render_reports = FALSE) {
  check_project(root = root, deep = deep, render_reports = render_reports, strict = FALSE, repair = FALSE)$issues
}

#' Apply conservative project repairs
#'
#' @param root Existing project root.
#' @param dry_run Logical scalar. If `TRUE`, return planned repairs without
#'   modifying files.
#' @param confirm Logical scalar. Must be `TRUE` to perform the repair.
#'
#' @return A list describing the repair actions.
#' @examples
#' \dontrun{
#' repair_project(dry_run = TRUE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
repair_project <- function(root = ".", dry_run = FALSE, confirm = FALSE) {
  validate_logical_scalar(dry_run, "dry_run")
  validate_logical_scalar(confirm, "confirm")
  root <- find_project_root(root)

  planned <- list(
    metadata_dir = project_metadata_relative_dir(root),
    ensure_dirs = c(
      "analysis",
      "reports",
      "outputs",
      fs::path("outputs", project_output_subdirs()),
      default_project_metadata_dir()
    ),
    ensure_files = c(project_registry_relative_path(root), project_local_config_relative_path(root))
  )

  if (isTRUE(dry_run)) {
    return(planned)
  }
  if (!isTRUE(confirm)) {
    rlang::abort("Set `confirm = TRUE` to apply project repairs.")
  }

  backup_project_registry(root)
  backup_project_local_config(root)
  for (directory in planned$ensure_dirs) {
    fs::dir_create(fs::path(root, directory), recurse = TRUE)
  }
  ensure_registry_file(root, overwrite = FALSE)
  ensure_local_config_file(root, overwrite = FALSE)
  ensure_gitignore_entries(root)
  append_project_activity(
    action = "repair_project",
    object_type = "project",
    object_id = safe_basename(root),
    object_name = safe_basename(root),
    details = planned,
    root = root
  )
  planned
}

#' Reorganise generated project outputs into the canonical layout
#'
#' @description
#' `organise_project_outputs()` migrates registered output paths and rendered
#' report artefacts to the current `projflow` output layout. The function is
#' conservative: it does not delete analytical outputs, and it only removes known
#' Quarto duplicate report artefacts after a canonical report copy exists.
#'
#' The canonical layout is:
#' \preformatted{
#' outputs/
#'   data/
#'   analysis/
#'   models/
#'   diagnostics/
#'   tables/
#'   figures/
#'   reports/<report-name>/<report-name>.html
#'   logs/
#'   project_management/
#'   deliverables/
#' }
#'
#' @param root Existing project root.
#' @param dry_run Logical scalar. If `TRUE`, return the planned actions without
#'   changing files or the registry.
#' @param move Logical scalar. If `TRUE`, move existing registered output files
#'   from old paths to canonical paths when possible. If `FALSE`, update only the
#'   planned action table when `dry_run = TRUE`.
#' @param clean_report_noise Logical scalar. If `TRUE`, remove known duplicated
#'   Quarto report artefacts such as `outputs/reports/reports/<name>.html` after
#'   the canonical report exists.
#'
#' @return A data frame describing the planned or applied actions.
#' @examples
#' \dontrun{
#' organise_project_outputs(dry_run = TRUE)
#' organise_project_outputs()
#' }
#' @author Thiago de Paula Oliveira
#' @export
organise_project_outputs <- function(root = ".",
                                     dry_run = FALSE,
                                     move = TRUE,
                                     clean_report_noise = TRUE) {
  validate_logical_scalar(dry_run, "dry_run")
  validate_logical_scalar(move, "move")
  validate_logical_scalar(clean_report_noise, "clean_report_noise")

  root <- find_project_root(root)
  registry <- read_project_registry(root)

  canonical_dirs <- fs::path(root, "outputs", project_output_subdirs())
  actions <- data.frame(
    section = character(),
    name = character(),
    old_path = character(),
    new_path = character(),
    action = character(),
    stringsAsFactors = FALSE
  )

  add_action <- function(section, name, old_path, new_path, action) {
    actions <<- rbind(
      actions,
      data.frame(
        section = section,
        name = name,
        old_path = old_path %||% "",
        new_path = new_path %||% "",
        action = action,
        stringsAsFactors = FALSE
      )
    )
  }

  if (!isTRUE(dry_run)) {
    fs::dir_create(canonical_dirs, recurse = TRUE)
  }

  output_names <- names(registry$outputs %||% list())
  for (name in output_names) {
    entry <- registry$outputs[[name]]
    type <- canonical_output_type(name, entry, registry)
    canonical <- normalize_relative_path(default_output_path(name, type))
    current <- normalize_relative_path(entry$path %||% canonical)

    if (!identical(current, canonical)) {
      current_abs <- fs::path(root, current)
      canonical_abs <- fs::path(root, canonical)
      current_exists <- fs::file_exists(current_abs)
      canonical_exists <- fs::file_exists(canonical_abs)

      planned_action <- if (isTRUE(current_exists) && !isTRUE(canonical_exists) && isTRUE(move)) {
        "move_file_and_update_registry"
      } else if (isTRUE(current_exists) && isTRUE(canonical_exists)) {
        "update_registry_keep_existing_files"
      } else {
        "update_registry_only"
      }

      add_action("outputs", name, current, canonical, planned_action)

      if (!isTRUE(dry_run)) {
        if (isTRUE(current_exists) && !isTRUE(canonical_exists) && isTRUE(move)) {
          fs::dir_create(fs::path_dir(canonical_abs), recurse = TRUE)
          fs::file_move(current_abs, canonical_abs)
        }
        registry$outputs[[name]]$path <- canonical
        registry$outputs[[name]]$type <- type
      }
    } else if (!identical(entry$type %||% "output", type) && !isTRUE(dry_run)) {
      registry$outputs[[name]]$type <- type
    }
  }

  report_names <- names(registry$reports %||% list())
  for (name in report_names) {
    entry <- registry$reports[[name]]
    source_path <- entry$path %||% ""
    canonical <- normalize_relative_path(default_output_path(name, "report"))
    canonical_abs <- fs::path(root, canonical)
    output_file <- fs::path_file(canonical_abs)
    report_root <- fs::path(root, "outputs", "reports")
    source_parent <- if (nzchar(source_path)) fs::path_file(fs::path_dir(source_path)) else "reports"
    candidates <- unique(normalize_relative_path(c(
      fs::path("outputs", "reports", output_file),
      fs::path("outputs", "reports", source_parent, output_file),
      fs::path("outputs", "reports", name, output_file)
    )))
    candidates <- setdiff(candidates, canonical)
    candidates_abs <- fs::path(root, candidates)
    existing_candidates <- candidates[fs::file_exists(candidates_abs)]

    if (length(existing_candidates) > 0L) {
      add_action("reports", name, existing_candidates[[1]], canonical, "copy_report_artifacts_to_canonical_path")
      if (!isTRUE(dry_run)) {
        copy_report_companion_files(fs::path(root, existing_candidates[[1]]), canonical_abs)
      }
    }

    if (isTRUE(clean_report_noise)) {
      noise <- candidates[fs::file_exists(fs::path(root, candidates))]
      for (noise_path in noise) {
        add_action("reports", name, noise_path, canonical, "remove_duplicate_report_artifact")
        if (!isTRUE(dry_run) && fs::file_exists(canonical_abs)) {
          fs::file_delete(fs::path(root, noise_path))
          files_dir <- fs::path(fs::path_dir(fs::path(root, noise_path)), paste0(name, "_files"))
          if (fs::dir_exists(files_dir)) {
            fs::dir_delete(files_dir)
          }
        }
      }

      if (!isTRUE(dry_run) && fs::file_exists(canonical_abs)) {
        noisy_source_dir <- fs::path(report_root, source_parent)
        if (!identical(normalize_absolute_path(noisy_source_dir), fs::path_dir(canonical_abs)) && fs::dir_exists(noisy_source_dir)) {
          remaining <- list.files(noisy_source_dir, all.files = TRUE, no.. = TRUE)
          if (length(remaining) == 0L) {
            fs::dir_delete(noisy_source_dir)
          }
        }
      }
    }
  }

  if (!isTRUE(dry_run)) {
    write_project_registry(registry, root = root, overwrite = TRUE)
    append_project_activity(
      action = "organise_project_outputs",
      object_type = "project",
      object_id = safe_basename(root),
      object_name = safe_basename(root),
      details = list(actions = actions),
      root = root
    )
  }

  actions
}
