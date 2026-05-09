activity_log_path <- function(root = ".") {
  project_metadata_path(find_project_root(root), "activity_log.yml", create_dir = TRUE, prefer_existing = FALSE)
}

project_backups_dir <- function(root = ".") {
  path <- project_metadata_path(find_project_root(root), "backups", create_dir = TRUE, prefer_existing = FALSE)
  fs::dir_create(path, recurse = TRUE)
  path
}

timestamp_slug <- function(time = Sys.time()) {
  value <- format(as.POSIXct(time, tz = "UTC"), "%Y%m%d_%H%M%OS3")
  gsub("\\.", "", value)
}

current_action_source <- function() {
  source <- getOption("projflow.action_source", "cli")
  if (!is.character(source) || length(source) != 1L || is.na(source) || !nzchar(source)) {
    return("cli")
  }
  source
}

read_yaml_if_exists <- function(path, default) {
  if (!fs::file_exists(path)) {
    return(default)
  }
  yaml::read_yaml(path)
}

append_yaml_entry <- function(path, entry) {
  existing <- read_yaml_if_exists(path, list())
  if (is.null(existing)) {
    existing <- list()
  }
  existing[[length(existing) + 1L]] <- entry
  write_yaml_file(path, existing, overwrite = TRUE)
  invisible(path)
}

backup_project_file <- function(path, prefix, root = ".") {
  root <- find_project_root(root)
  validate_character_vector(prefix, "prefix")
  if (!fs::file_exists(path)) {
    return(NULL)
  }

  backup_dir <- project_backups_dir(root)
  extension <- fs::path_ext(path)
  if (!nzchar(extension)) {
    extension <- "yml"
  }
  backup_path <- fs::path(backup_dir, paste0(prefix[[1]], "_", timestamp_slug(), ".", extension))
  while (fs::file_exists(backup_path)) {
    backup_path <- fs::path(
      backup_dir,
      paste0(
        prefix[[1]],
        "_",
        timestamp_slug(Sys.time() + stats::runif(1, 0, 0.999)),
        ".",
        extension
      )
    )
  }
  fs::file_copy(path, backup_path, overwrite = FALSE)
  normalize_absolute_path(backup_path)
}

#' Back up the project registry
#'
#' @param root Existing project root.
#'
#' @return Absolute backup path, or `NULL` if the source file does not exist.
#' @examples
#' \dontrun{
#' backup_project_registry()
#' }
#' @author Thiago de Paula Oliveira
#' @export
backup_project_registry <- function(root = ".") {
  backup_project_file(registry_path(root), "project_registry", root = root)
}

#' Back up the local project configuration
#'
#' @param root Existing project root.
#'
#' @return Absolute backup path, or `NULL` if the source file does not exist.
#' @examples
#' \dontrun{
#' backup_project_local_config()
#' }
#' @author Thiago de Paula Oliveira
#' @export
backup_project_local_config <- function(root = ".") {
  backup_project_file(local_config_path(root), "local", root = root)
}

#' Back up the project governance file
#'
#' @param root Existing project root.
#'
#' @return Absolute backup path, or `NULL` if the source file does not exist.
#' @examples
#' \dontrun{
#' backup_project_tasks_data()
#' }
#' @author Thiago de Paula Oliveira
#' @export
backup_project_tasks_data <- function(root = ".") {
  backup_project_file(tasks_path(root), "tasks", root = root)
}

#' List available project backups
#'
#' @param root Existing project root.
#'
#' @return Data frame of backup files.
#' @examples
#' \dontrun{
#' list_project_backups()
#' }
#' @author Thiago de Paula Oliveira
#' @export
list_project_backups <- function(root = ".") {
  root <- find_project_root(root)
  backup_dir <- project_backups_dir(root)
  files <- list.files(backup_dir, full.names = TRUE)
  if (length(files) == 0L) {
    return(data.frame(
      name = character(),
      path = character(),
      size = numeric(),
      modified = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ))
  }

  info <- file.info(files)
  data.frame(
    name = basename(files),
    path = normalize_absolute_path(files),
    size = info$size,
    modified = info$mtime,
    stringsAsFactors = FALSE
  )
}

#' Restore a project backup
#'
#' @param backup Backup file path or backup file name.
#' @param root Existing project root.
#' @param confirm Logical scalar. Must be `TRUE` to perform the restore.
#'
#' @return Invisibly returns the restored destination path.
#' @examples
#' \dontrun{
#' backups <- list_project_backups()
#' restore_project_backup(backups$path[[1]], confirm = TRUE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
restore_project_backup <- function(backup, root = ".", confirm = FALSE) {
  validate_character_vector(backup, "backup")
  validate_logical_scalar(confirm, "confirm")
  if (!isTRUE(confirm)) {
    rlang::abort("Set `confirm = TRUE` to restore a project backup.")
  }

  root <- find_project_root(root)
  backup_value <- backup[[1]]
  backup_path <- if (fs::file_exists(backup_value)) {
    backup_value
  } else {
    fs::path(project_backups_dir(root), backup_value)
  }

  if (!fs::file_exists(backup_path)) {
    rlang::abort(paste0("Backup file does not exist: ", backup_value))
  }

  file_name <- safe_basename(backup_path)
  destination <- if (startsWith(file_name, "project_registry_")) {
    registry_path(root)
  } else if (startsWith(file_name, "local_")) {
    local_config_path(root)
  } else if (startsWith(file_name, "tasks_")) {
    tasks_path(root)
  } else {
    rlang::abort("Could not infer the restore target from the backup file name.")
  }

  fs::file_copy(backup_path, destination, overwrite = TRUE)
  invisible(destination)
}

#' Append an activity-log entry
#'
#' @param action Short action identifier.
#' @param object_type Object type involved in the action.
#' @param object_id Optional object identifier.
#' @param object_name Optional display name.
#' @param source Action source, usually `"cli"` or `"dashboard"`.
#' @param details Optional named list with extra action metadata.
#' @param root Existing project root.
#'
#' @return Invisibly returns the appended entry.
#' @examples
#' \dontrun{
#' append_project_activity("check_project", "project", source = "cli")
#' }
#' @author Thiago de Paula Oliveira
#' @export
append_project_activity <- function(action,
                                    object_type,
                                    object_id = NULL,
                                    object_name = NULL,
                                    source = current_action_source(),
                                    details = list(),
                                    root = ".") {
  validate_character_vector(action, "action")
  validate_character_vector(object_type, "object_type")
  validate_character_vector(source, "source")
  root <- find_project_root(root)

  entry <- list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    action = action[[1]],
    object_type = object_type[[1]],
    object_id = if (is.null(object_id)) NA_character_ else as.character(object_id[[1]]),
    object_name = if (is.null(object_name)) NA_character_ else as.character(object_name[[1]]),
    source = source[[1]],
    details = if (length(details) == 0L) list() else details
  )

  append_yaml_entry(activity_log_path(root), entry)
  invisible(entry)
}

#' List the project activity log
#'
#' @param root Existing project root.
#'
#' @return Data frame of activity-log entries.
#' @examples
#' \dontrun{
#' list_project_activity()
#' }
#' @author Thiago de Paula Oliveira
#' @export
list_project_activity <- function(root = ".") {
  root <- find_project_root(root)
  entries <- read_yaml_if_exists(activity_log_path(root), list())
  if (length(entries) == 0L) {
    return(data.frame(
      timestamp = character(),
      action = character(),
      object_type = character(),
      object_id = character(),
      object_name = character(),
      source = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(
    rbind,
    lapply(entries, function(entry) {
      data.frame(
        timestamp = as.character(entry$timestamp %||% NA_character_),
        action = as.character(entry$action %||% NA_character_),
        object_type = as.character(entry$object_type %||% NA_character_),
        object_id = as.character(entry$object_id %||% NA_character_),
        object_name = as.character(entry$object_name %||% NA_character_),
        source = as.character(entry$source %||% NA_character_),
        stringsAsFactors = FALSE
      )
    })
  )
}

#' Clear the project activity log
#'
#' @param root Existing project root.
#' @param confirm Logical scalar. Must be `TRUE` to clear the log.
#'
#' @return Invisibly returns the cleared log path.
#' @examples
#' \dontrun{
#' clear_project_activity(confirm = TRUE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
clear_project_activity <- function(root = ".", confirm = FALSE) {
  validate_logical_scalar(confirm, "confirm")
  if (!isTRUE(confirm)) {
    rlang::abort("Set `confirm = TRUE` to clear the project activity log.")
  }
  path <- activity_log_path(root)
  write_yaml_file(path, list(), overwrite = TRUE)
  invisible(path)
}
