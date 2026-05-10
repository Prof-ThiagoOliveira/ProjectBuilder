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

activity_log_scalar <- function(x, collapse = ", ") {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  if (is.data.frame(x) || is.list(x)) {
    return(paste(utils::capture.output(utils::str(x, give.attr = FALSE, vec.len = 8L)), collapse = "\n"))
  }

  value <- as.character(x)
  value <- value[!is.na(value) & nzchar(value)]
  if (length(value) == 0L) NA_character_ else paste(value, collapse = collapse)
}

activity_log_detail_value <- function(details, candidates) {
  if (!is.list(details) || length(details) == 0L) {
    return(NA_character_)
  }

  detail_names <- names(details) %||% character(length(details))
  detail_names <- tolower(detail_names)
  for (candidate in candidates) {
    idx <- which(detail_names == tolower(candidate))
    if (length(idx) > 0L) {
      value <- activity_log_scalar(details[[idx[[1]]]])
      if (!is.na(value) && nzchar(value)) {
        return(value)
      }
    }
  }

  NA_character_
}

activity_log_details_text <- function(details) {
  if (is.null(details) || length(details) == 0L) {
    return(NA_character_)
  }
  if (!is.list(details)) {
    return(activity_log_scalar(details))
  }

  detail_names <- names(details)
  if (is.null(detail_names)) {
    detail_names <- paste0("item_", seq_along(details))
  }
  missing_names <- is.na(detail_names) | !nzchar(detail_names)
  detail_names[missing_names] <- paste0("item_", which(missing_names))

  lines <- unlist(
    lapply(seq_along(details), function(i) {
      value <- activity_log_scalar(details[[i]])
      if (is.na(value) || !nzchar(value)) {
        return(character())
      }
      paste0(detail_names[[i]], ": ", value)
    }),
    use.names = FALSE
  )

  if (length(lines) == 0L) NA_character_ else paste(lines, collapse = "\n")
}

activity_log_action_label <- function(action) {
  action <- activity_log_scalar(action)
  if (is.na(action) || !nzchar(action)) {
    return(NA_character_)
  }
  label <- gsub("_", " ", action, fixed = TRUE)
  paste0(toupper(substr(label, 1L, 1L)), substring(label, 2L))
}

activity_log_status <- function(action, details) {
  explicit <- activity_log_detail_value(details, c("status", "state", "result"))
  if (!is.na(explicit) && nzchar(explicit)) {
    return(explicit)
  }

  action <- activity_log_scalar(action)
  action <- if (is.na(action)) "" else tolower(action)
  if (grepl("remove|delete|clear|unregister", action)) {
    "removed"
  } else if (grepl("repair", action)) {
    "repair applied"
  } else if (grepl("render|build|run", action)) {
    "execution recorded"
  } else if (grepl("update|rename|mark|close", action)) {
    "updated"
  } else if (grepl("create|add|register|record|set", action)) {
    "created"
  } else {
    "recorded"
  }
}

activity_log_summary <- function(action, object_type, object_name, path, status) {
  action_label <- activity_log_action_label(action)
  object <- activity_log_scalar(object_name)
  if (is.na(object) || !nzchar(object)) {
    object <- activity_log_scalar(object_type)
  }
  pieces <- c(action_label, object)
  pieces <- pieces[!is.na(pieces) & nzchar(pieces)]
  summary <- paste(pieces, collapse = " - ")
  if (!is.na(path) && nzchar(path)) {
    summary <- paste0(summary, " (", path, ")")
  }
  if (!is.na(status) && nzchar(status)) {
    summary <- paste0(summary, " [", status, "]")
  }
  summary
}

activity_log_recommendation <- function(action, object_type, details) {
  action <- activity_log_scalar(action)
  action <- if (is.na(action)) "" else tolower(action)
  object_type <- activity_log_scalar(object_type)
  object_type <- if (is.na(object_type)) "" else tolower(object_type)
  path <- activity_log_detail_value(details, c("path", "file", "destination", "target", "output_path"))

  if (grepl("register_output|new_output|create_output|update_output", action) || identical(object_type, "output")) {
    if (!is.na(path) && nzchar(path)) {
      return(paste0("Check whether `", path, "` is generated by the intended script; run the generating step or update the registry path if the file is expected elsewhere."))
    }
    return("Check whether the registered output is still required, generated by a script, and correctly linked in the registry.")
  }
  if (grepl("create_report|update_report|render", action) || identical(object_type, "report")) {
    return("Open the report entry, verify the source path and render target, then render the selected report if the source is complete.")
  }
  if (grepl("create_script|repair_script|rename_script", action) || identical(object_type, "script")) {
    return("Review the script path, declared type and generated outputs; then run the relevant project step if downstream artefacts are missing.")
  }
  if (grepl("task", action) || identical(object_type, "task")) {
    return("Review the task status, due date, priority and dependencies; update blockers before marking downstream work as ready.")
  }
  if (grepl("risk", action) || identical(object_type, "risk")) {
    return("Review the mitigation or closure status and confirm whether any linked tasks or outputs need follow-up.")
  }
  if (grepl("milestone", action) || identical(object_type, "milestone")) {
    return("Check whether the milestone status is consistent with completed tasks and delivered outputs.")
  }
  if (grepl("decision", action) || identical(object_type, "decision")) {
    return("Review the decision rationale and ensure any affected scripts, reports or outputs have been updated accordingly.")
  }
  if (grepl("data_source", action) || identical(object_type, "data_source")) {
    return("Verify that the configured data root exists on this machine and remains outside the version-controlled project when appropriate.")
  }
  if (grepl("repair", action)) {
    return("Review the repair details and inspect the generated backups before making further structural changes.")
  }
  if (grepl("remove|delete|clear|unregister", action)) {
    return("Confirm that the removal was intentional and that no downstream registry entries still depend on the removed item.")
  }

  "Review the metadata for this event and confirm whether a project check, rebuild or registry update is needed."
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
#' @return A data frame of activity-log entries. In addition to the core audit
#'   fields (`timestamp`, `action`, `object_type`, `object_id`, `object_name`,
#'   and `source`), the returned table includes derived fields used by the
#'   dashboard: `entry`, `action_label`, `affected_path`, `status`, `summary`,
#'   `details`, and `recommendation`.
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
      entry = integer(),
      timestamp = character(),
      action = character(),
      action_label = character(),
      object_type = character(),
      object_id = character(),
      object_name = character(),
      affected_path = character(),
      status = character(),
      source = character(),
      summary = character(),
      details = character(),
      recommendation = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(
    rbind,
    lapply(seq_along(entries), function(i) {
      entry <- entries[[i]]
      details <- entry$details %||% list()
      if (!is.list(details)) {
        details <- list(value = details)
      }
      action <- as.character(entry$action %||% NA_character_)
      object_type <- as.character(entry$object_type %||% NA_character_)
      object_name <- as.character(entry$object_name %||% NA_character_)
      affected_path <- activity_log_detail_value(
        details,
        c("path", "file", "destination", "target", "output_path", "source_path")
      )
      status <- activity_log_status(action, details)

      data.frame(
        entry = i,
        timestamp = as.character(entry$timestamp %||% NA_character_),
        action = action,
        action_label = activity_log_action_label(action),
        object_type = object_type,
        object_id = as.character(entry$object_id %||% NA_character_),
        object_name = object_name,
        affected_path = affected_path,
        status = status,
        source = as.character(entry$source %||% NA_character_),
        summary = activity_log_summary(action, object_type, object_name, affected_path, status),
        details = activity_log_details_text(details),
        recommendation = activity_log_recommendation(action, object_type, details),
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
