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
    ensure_dirs = c("analysis", "reports", "outputs", default_project_metadata_dir()),
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
