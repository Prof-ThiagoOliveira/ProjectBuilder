tasks_path <- function(root = ".") {
  project_metadata_path(find_project_root(root), "tasks.yml", create_dir = TRUE, prefer_existing = FALSE)
}

default_tasks_data <- function() {
  list(
    version = 2L,
    tasks = list(),
    milestones = list(),
    decisions = list(),
    risks = list()
  )
}

read_project_tasks_data <- function(root = ".") {
  path <- tasks_path(root)
  if (!fs::file_exists(path)) {
    return(default_tasks_data())
  }

  data <- yaml::read_yaml(path)
  data$version <- data$version %||% 2L
  data$tasks <- data$tasks %||% list()
  data$milestones <- data$milestones %||% list()
  data$decisions <- data$decisions %||% list()
  data$risks <- data$risks %||% list()
  data
}

write_project_tasks_data <- function(data, root = ".", overwrite = TRUE) {
  write_yaml_file(tasks_path(root), data, overwrite = overwrite)
}

governance_status_levels <- function(type) {
  switch(
    type,
    task = c("backlog", "todo", "in_progress", "blocked", "done", "cancelled"),
    risk = c("open", "mitigating", "mitigated", "accepted", "closed"),
    milestone = c("planned", "in_progress", "done", "delayed", "cancelled"),
    decision = c("active", "superseded", "withdrawn"),
    rlang::abort(paste0("Unsupported governance type: ", type))
  )
}

task_priority_levels <- function() {
  c("low", "medium", "high", "critical")
}

validate_governance_status <- function(status, type) {
  validate_choice(status, governance_status_levels(type), "status")
}

validate_task_priority <- function(priority) {
  validate_choice(priority, task_priority_levels(), "priority")
}

validate_optional_text <- function(x, arg) {
  validate_character_vector(x, arg, allow_null = TRUE)
  if (is.null(x)) {
    return(NULL)
  }
  trimws(as.character(x[[1]]))
}

validate_optional_date <- function(x, arg) {
  if (is.null(x)) {
    return(NULL)
  }
  value <- validate_optional_text(as.character(x), arg)
  if (is.null(value) || identical(value, "")) {
    return(NULL)
  }
  parsed <- tryCatch(as.Date(value), error = function(error) error)
  if (inherits(parsed, "error") || is.na(parsed)) {
    rlang::abort(paste0("`", arg, "` must be a valid ISO date."))
  }
  as.character(parsed)
}

validate_linked_objects <- function(linked_objects) {
  if (is.null(linked_objects)) {
    return(character())
  }
  validate_character_vector(linked_objects, "linked_objects")
  unique(trimws(linked_objects))
}

next_governance_id <- function(entries, prefix) {
  ids <- vapply(entries, function(entry) as.character(entry$id %||% NA_character_), character(1))
  ids <- ids[!is.na(ids)]
  if (length(ids) == 0L) {
    return(sprintf("%s_%04d", prefix, 1L))
  }

  numbers <- suppressWarnings(as.integer(sub(paste0("^", prefix, "_"), "", ids)))
  numbers <- numbers[!is.na(numbers)]
  sprintf("%s_%04d", prefix, if (length(numbers) == 0L) 1L else max(numbers) + 1L)
}

timestamp_now <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

governance_entry_by_id <- function(entries, id_or_title) {
  key <- task_key_from_title(id_or_title)
  for (entry in entries) {
    if (identical(entry$id %||% "", id_or_title) || identical(entry$id %||% "", key)) {
      return(entry)
    }
    if (identical(task_key_from_title(entry$title %||% ""), key)) {
      return(entry)
    }
  }
  NULL
}

governance_entry_index <- function(entries, id_or_title) {
  key <- task_key_from_title(id_or_title)
  for (index in seq_along(entries)) {
    entry <- entries[[index]]
    if (identical(entry$id %||% "", id_or_title) || identical(entry$id %||% "", key)) {
      return(index)
    }
    if (identical(task_key_from_title(entry$title %||% ""), key)) {
      return(index)
    }
  }
  NA_integer_
}

governance_entries_to_df <- function(entries, fields) {
  if (length(entries) == 0L) {
    out <- as.data.frame(stats::setNames(replicate(length(fields), character(), simplify = FALSE), fields), stringsAsFactors = FALSE)
    return(out)
  }

  rows <- lapply(entries, function(entry) {
    values <- lapply(fields, function(field) {
      value <- entry[[field]]
      if (is.null(value)) {
        return(NA_character_)
      }
      if (is.character(value) && length(value) > 1L) {
        return(paste(value, collapse = "; "))
      }
      as.character(value)
    })
    stats::setNames(values, fields)
  })

  as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
}

backup_governance_state <- function(root = ".") {
  backup_project_tasks_data(root)
}

log_governance_action <- function(action, object_type, object_id, object_name, details = list(), root = ".") {
  append_project_activity(
    action = action,
    object_type = object_type,
    object_id = object_id,
    object_name = object_name,
    details = details,
    root = root
  )
}

update_governance_docs <- function(root = ".") {
  root <- find_project_root(root)
  status <- project_status_report(root, output = "markdown")
  if (fs::dir_exists(fs::path(root, "docs"))) {
    write_template_file(fs::path(root, "docs", "status.md"), status, overwrite = TRUE)
  }
  invisible(root)
}

list_project_tasks <- function(root = ".") {
  project_tasks(root)
}

#' List project tasks
#'
#' @param root Existing project root.
#'
#' @return A data frame with one row per task.
#' @examples
#' \dontrun{
#' project_tasks()
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_tasks <- function(root = ".") {
  governance_entries_to_df(
    read_project_tasks_data(root)$tasks,
    c("id", "title", "description", "status", "priority", "due_date", "assigned_to", "linked_objects", "created_at", "updated_at", "completed_at", "source")
  )
}

#' Add a project task
#'
#' @param title Human-readable task title.
#' @param root Existing project root.
#' @param description Optional task description.
#' @param status Task status.
#' @param priority Task priority.
#' @param due_date Optional due date stored as an ISO date string.
#' @param assigned_to Optional owner or responsible person.
#' @param linked_objects Optional character vector of linked project objects.
#' @param source Action source label.
#'
#' @return Invisibly returns the created task id.
#' @examples
#' \dontrun{
#' add_project_task("Review outputs", priority = "high")
#' }
#' @author Thiago de Paula Oliveira
#' @export
add_project_task <- function(title,
                             root = ".",
                             description = NULL,
                             status = "todo",
                             priority = "medium",
                             due_date = NULL,
                             assigned_to = NULL,
                             linked_objects = NULL,
                             source = current_action_source()) {
  validate_character_vector(title, "title")
  status <- validate_governance_status(status, "task")
  priority <- validate_task_priority(priority)
  description <- validate_optional_text(description, "description")
  due_date <- validate_optional_date(due_date, "due_date")
  assigned_to <- validate_optional_text(assigned_to, "assigned_to")
  linked_objects <- validate_linked_objects(linked_objects)

  root <- find_project_root(root)
  backup_governance_state(root)
  data <- read_project_tasks_data(root)

  if (any(vapply(data$tasks, function(entry) identical(task_key_from_title(entry$title %||% ""), task_key_from_title(title[[1]])), logical(1)))) {
    rlang::abort(paste0("A task with title `", title[[1]], "` already exists."))
  }

  entry <- list(
    id = next_governance_id(data$tasks, "task"),
    title = trimws(title[[1]]),
    description = description,
    status = status,
    priority = priority,
    due_date = due_date,
    assigned_to = assigned_to,
    linked_objects = linked_objects,
    created_at = timestamp_now(),
    updated_at = timestamp_now(),
    completed_at = if (identical(status, "done")) timestamp_now() else NULL,
    source = source
  )

  data$tasks[[length(data$tasks) + 1L]] <- entry
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  update_governance_docs(root)
  log_governance_action("add_task", "task", entry$id, entry$title, list(status = entry$status), root = root)
  invisible(entry$id)
}

#' Update a project task
#'
#' @param task Task id or task title.
#' @param root Existing project root.
#' @param title Optional replacement title.
#' @param description Optional replacement description.
#' @param status Optional replacement status.
#' @param priority Optional replacement priority.
#' @param due_date Optional replacement due date.
#' @param assigned_to Optional replacement owner.
#' @param linked_objects Optional replacement linked objects.
#'
#' @return Invisibly returns the updated task id.
#' @examples
#' \dontrun{
#' update_project_task("task_0001", status = "in_progress")
#' }
#' @author Thiago de Paula Oliveira
#' @export
update_project_task <- function(task,
                                root = ".",
                                title = NULL,
                                description = NULL,
                                status = NULL,
                                priority = NULL,
                                due_date = NULL,
                                assigned_to = NULL,
                                linked_objects = NULL) {
  validate_character_vector(task, "task")
  root <- find_project_root(root)
  data <- read_project_tasks_data(root)
  index <- governance_entry_index(data$tasks, task[[1]])
  if (is.na(index)) {
    rlang::abort(paste0("Task `", task[[1]], "` does not exist."))
  }

  backup_governance_state(root)
  entry <- data$tasks[[index]]
  previous_status <- entry$status %||% NA_character_
  if (!is.null(title)) entry$title <- trimws(title[[1]])
  if (!is.null(description)) entry$description <- validate_optional_text(description, "description")
  if (!is.null(status)) entry$status <- validate_governance_status(status, "task")
  if (!is.null(priority)) entry$priority <- validate_task_priority(priority)
  if (!is.null(due_date)) entry$due_date <- validate_optional_date(due_date, "due_date")
  if (!is.null(assigned_to)) entry$assigned_to <- validate_optional_text(assigned_to, "assigned_to")
  if (!is.null(linked_objects)) entry$linked_objects <- validate_linked_objects(linked_objects)
  entry$updated_at <- timestamp_now()
  if (identical(entry$status, "done") && !identical(previous_status, "done")) {
    entry$completed_at <- timestamp_now()
  }
  if (!identical(entry$status, "done")) {
    entry$completed_at <- NULL
  }

  data$tasks[[index]] <- entry
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  update_governance_docs(root)
  log_governance_action("update_task", "task", entry$id, entry$title, list(previous_status = previous_status, new_status = entry$status), root = root)
  invisible(entry$id)
}

#' Mark a project task as done
#'
#' @param task Task id or task title.
#' @param root Existing project root.
#'
#' @return Invisibly returns the updated task id.
#' @examples
#' \dontrun{
#' mark_project_task_done("task_0001")
#' }
#' @author Thiago de Paula Oliveira
#' @export
mark_project_task_done <- function(task, root = ".") {
  update_project_task(task, root = root, status = "done")
}

#' Complete a project task
#'
#' @inheritParams mark_project_task_done
#'
#' @return Invisibly returns the updated task id.
#' @examples
#' \dontrun{
#' complete_project_task("task_0001")
#' }
#' @author Thiago de Paula Oliveira
#' @export
complete_project_task <- function(task, root = ".") {
  mark_project_task_done(task, root = root)
}

#' Remove a project task
#'
#' @param task Task id or task title.
#' @param root Existing project root.
#'
#' @return Invisibly returns the removed task id.
#' @examples
#' \dontrun{
#' remove_project_task("task_0001")
#' }
#' @author Thiago de Paula Oliveira
#' @export
remove_project_task <- function(task, root = ".") {
  validate_character_vector(task, "task")
  root <- find_project_root(root)
  data <- read_project_tasks_data(root)
  index <- governance_entry_index(data$tasks, task[[1]])
  if (is.na(index)) {
    rlang::abort(paste0("Task `", task[[1]], "` does not exist."))
  }

  backup_governance_state(root)
  entry <- data$tasks[[index]]
  data$tasks[[index]] <- NULL
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  update_governance_docs(root)
  log_governance_action("remove_task", "task", entry$id, entry$title, root = root)
  invisible(entry$id)
}

list_project_milestones <- function(root = ".") {
  project_milestones(root)
}

#' List project milestones
#'
#' @param root Existing project root.
#'
#' @return A data frame with one row per milestone.
#' @examples
#' \dontrun{
#' project_milestones()
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_milestones <- function(root = ".") {
  governance_entries_to_df(
    read_project_tasks_data(root)$milestones,
    c("id", "title", "description", "status", "due_date", "completed_at", "linked_objects", "created_at", "updated_at")
  )
}

#' Add a project milestone
#'
#' @param title Human-readable milestone title.
#' @param root Existing project root.
#' @param description Optional milestone description.
#' @param status Milestone status.
#' @param due_date Optional due date stored as an ISO date string.
#' @param linked_objects Optional linked objects.
#'
#' @return Invisibly returns the created milestone id.
#' @examples
#' \dontrun{
#' add_project_milestone("Draft report")
#' }
#' @author Thiago de Paula Oliveira
#' @export
add_project_milestone <- function(title,
                                  root = ".",
                                  description = NULL,
                                  status = "planned",
                                  due_date = NULL,
                                  linked_objects = NULL) {
  validate_character_vector(title, "title")
  status <- validate_governance_status(status, "milestone")
  description <- validate_optional_text(description, "description")
  due_date <- validate_optional_date(due_date, "due_date")
  linked_objects <- validate_linked_objects(linked_objects)

  root <- find_project_root(root)
  backup_governance_state(root)
  data <- read_project_tasks_data(root)
  entry <- list(
    id = next_governance_id(data$milestones, "milestone"),
    title = trimws(title[[1]]),
    description = description,
    status = status,
    due_date = due_date,
    completed_at = if (identical(status, "done")) timestamp_now() else NULL,
    linked_objects = linked_objects,
    created_at = timestamp_now(),
    updated_at = timestamp_now()
  )
  data$milestones[[length(data$milestones) + 1L]] <- entry
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  log_governance_action("add_milestone", "milestone", entry$id, entry$title, list(status = entry$status), root = root)
  invisible(entry$id)
}

#' Update a project milestone
#'
#' @param milestone Milestone id or title.
#' @param root Existing project root.
#' @param title Optional replacement title.
#' @param description Optional replacement description.
#' @param status Optional replacement status.
#' @param due_date Optional replacement due date.
#' @param linked_objects Optional replacement linked objects.
#'
#' @return Invisibly returns the updated milestone id.
#' @examples
#' \dontrun{
#' update_project_milestone("milestone_0001", status = "in_progress")
#' }
#' @author Thiago de Paula Oliveira
#' @export
update_project_milestone <- function(milestone,
                                     root = ".",
                                     title = NULL,
                                     description = NULL,
                                     status = NULL,
                                     due_date = NULL,
                                     linked_objects = NULL) {
  validate_character_vector(milestone, "milestone")
  root <- find_project_root(root)
  data <- read_project_tasks_data(root)
  index <- governance_entry_index(data$milestones, milestone[[1]])
  if (is.na(index)) {
    rlang::abort(paste0("Milestone `", milestone[[1]], "` does not exist."))
  }

  backup_governance_state(root)
  entry <- data$milestones[[index]]
  previous_status <- entry$status %||% NA_character_
  if (!is.null(title)) entry$title <- trimws(title[[1]])
  if (!is.null(description)) entry$description <- validate_optional_text(description, "description")
  if (!is.null(status)) entry$status <- validate_governance_status(status, "milestone")
  if (!is.null(due_date)) entry$due_date <- validate_optional_date(due_date, "due_date")
  if (!is.null(linked_objects)) entry$linked_objects <- validate_linked_objects(linked_objects)
  entry$updated_at <- timestamp_now()
  if (identical(entry$status, "done") && !identical(previous_status, "done")) {
    entry$completed_at <- timestamp_now()
  }
  if (!identical(entry$status, "done")) {
    entry$completed_at <- NULL
  }
  data$milestones[[index]] <- entry
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  log_governance_action("update_milestone", "milestone", entry$id, entry$title, list(previous_status = previous_status, new_status = entry$status), root = root)
  invisible(entry$id)
}

#' Mark a project milestone as done
#'
#' @param milestone Milestone id or title.
#' @param root Existing project root.
#'
#' @return Invisibly returns the updated milestone id.
#' @examples
#' \dontrun{
#' mark_project_milestone_done("milestone_0001")
#' }
#' @author Thiago de Paula Oliveira
#' @export
mark_project_milestone_done <- function(milestone, root = ".") {
  update_project_milestone(milestone, root = root, status = "done")
}

#' Remove a project milestone
#'
#' @param milestone Milestone id or title.
#' @param root Existing project root.
#'
#' @return Invisibly returns the removed milestone id.
#' @examples
#' \dontrun{
#' remove_project_milestone("milestone_0001")
#' }
#' @author Thiago de Paula Oliveira
#' @export
remove_project_milestone <- function(milestone, root = ".") {
  validate_character_vector(milestone, "milestone")
  root <- find_project_root(root)
  data <- read_project_tasks_data(root)
  index <- governance_entry_index(data$milestones, milestone[[1]])
  if (is.na(index)) {
    rlang::abort(paste0("Milestone `", milestone[[1]], "` does not exist."))
  }
  backup_governance_state(root)
  entry <- data$milestones[[index]]
  data$milestones[[index]] <- NULL
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  log_governance_action("remove_milestone", "milestone", entry$id, entry$title, root = root)
  invisible(entry$id)
}

list_project_risks <- function(root = ".") {
  project_risks(root)
}

#' List project risks
#'
#' @param root Existing project root.
#'
#' @return A data frame with one row per risk.
#' @examples
#' \dontrun{
#' project_risks()
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_risks <- function(root = ".") {
  governance_entries_to_df(
    read_project_tasks_data(root)$risks,
    c("id", "title", "description", "probability", "impact", "severity", "status", "mitigation", "owner", "due_date", "linked_objects", "created_at", "updated_at", "closed_at")
  )
}

compute_risk_severity <- function(probability, impact, severity = NULL) {
  if (!is.null(severity)) {
    return(validate_optional_text(severity, "severity"))
  }
  probability <- validate_optional_text(probability, "probability")
  impact <- validate_optional_text(impact, "impact")
  if (is.null(probability) || is.null(impact)) {
    return(NULL)
  }
  if (probability %in% c("high", "critical") || impact %in% c("high", "critical")) {
    return("high")
  }
  if (probability %in% c("medium") || impact %in% c("medium")) {
    return("medium")
  }
  "low"
}

#' Add a project risk
#'
#' @param title Human-readable risk title.
#' @param root Existing project root.
#' @param description Optional risk description.
#' @param probability Optional qualitative probability.
#' @param impact Optional qualitative impact.
#' @param severity Optional explicit severity.
#' @param status Risk status.
#' @param mitigation Optional mitigation plan.
#' @param owner Optional risk owner.
#' @param due_date Optional due date.
#' @param linked_objects Optional linked objects.
#'
#' @return Invisibly returns the created risk id.
#' @examples
#' \dontrun{
#' add_project_risk("Missing external data", mitigation = "Confirm access before analysis")
#' }
#' @author Thiago de Paula Oliveira
#' @export
add_project_risk <- function(title,
                             root = ".",
                             description = NULL,
                             probability = NULL,
                             impact = NULL,
                             severity = NULL,
                             status = "open",
                             mitigation = NULL,
                             owner = NULL,
                             due_date = NULL,
                             linked_objects = NULL) {
  validate_character_vector(title, "title")
  status <- validate_governance_status(status, "risk")
  description <- validate_optional_text(description, "description")
  probability <- validate_optional_text(probability, "probability")
  impact <- validate_optional_text(impact, "impact")
  severity <- compute_risk_severity(probability, impact, severity)
  mitigation <- validate_optional_text(mitigation, "mitigation")
  owner <- validate_optional_text(owner, "owner")
  due_date <- validate_optional_date(due_date, "due_date")
  linked_objects <- validate_linked_objects(linked_objects)

  root <- find_project_root(root)
  backup_governance_state(root)
  data <- read_project_tasks_data(root)
  entry <- list(
    id = next_governance_id(data$risks, "risk"),
    title = trimws(title[[1]]),
    description = description,
    probability = probability,
    impact = impact,
    severity = severity,
    status = status,
    mitigation = mitigation,
    owner = owner,
    due_date = due_date,
    linked_objects = linked_objects,
    created_at = timestamp_now(),
    updated_at = timestamp_now(),
    closed_at = if (status %in% c("mitigated", "accepted", "closed")) timestamp_now() else NULL
  )
  data$risks[[length(data$risks) + 1L]] <- entry
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  log_governance_action("add_risk", "risk", entry$id, entry$title, list(status = entry$status), root = root)
  invisible(entry$id)
}

#' Update a project risk
#'
#' @param risk Risk id or title.
#' @param root Existing project root.
#' @param title Optional replacement title.
#' @param description Optional replacement description.
#' @param probability Optional replacement probability.
#' @param impact Optional replacement impact.
#' @param severity Optional replacement severity.
#' @param status Optional replacement status.
#' @param mitigation Optional replacement mitigation.
#' @param owner Optional replacement owner.
#' @param due_date Optional replacement due date.
#' @param linked_objects Optional replacement linked objects.
#'
#' @return Invisibly returns the updated risk id.
#' @examples
#' \dontrun{
#' update_project_risk("risk_0001", status = "mitigating")
#' }
#' @author Thiago de Paula Oliveira
#' @export
update_project_risk <- function(risk,
                                root = ".",
                                title = NULL,
                                description = NULL,
                                probability = NULL,
                                impact = NULL,
                                severity = NULL,
                                status = NULL,
                                mitigation = NULL,
                                owner = NULL,
                                due_date = NULL,
                                linked_objects = NULL) {
  validate_character_vector(risk, "risk")
  root <- find_project_root(root)
  data <- read_project_tasks_data(root)
  index <- governance_entry_index(data$risks, risk[[1]])
  if (is.na(index)) {
    rlang::abort(paste0("Risk `", risk[[1]], "` does not exist."))
  }

  backup_governance_state(root)
  entry <- data$risks[[index]]
  previous_status <- entry$status %||% NA_character_
  if (!is.null(title)) entry$title <- trimws(title[[1]])
  if (!is.null(description)) entry$description <- validate_optional_text(description, "description")
  if (!is.null(probability)) entry$probability <- validate_optional_text(probability, "probability")
  if (!is.null(impact)) entry$impact <- validate_optional_text(impact, "impact")
  if (!is.null(severity) || !is.null(probability) || !is.null(impact)) {
    entry$severity <- compute_risk_severity(entry$probability, entry$impact, if (is.null(severity)) entry$severity else severity)
  }
  if (!is.null(status)) entry$status <- validate_governance_status(status, "risk")
  if (!is.null(mitigation)) entry$mitigation <- validate_optional_text(mitigation, "mitigation")
  if (!is.null(owner)) entry$owner <- validate_optional_text(owner, "owner")
  if (!is.null(due_date)) entry$due_date <- validate_optional_date(due_date, "due_date")
  if (!is.null(linked_objects)) entry$linked_objects <- validate_linked_objects(linked_objects)
  entry$updated_at <- timestamp_now()
  if (entry$status %in% c("mitigated", "accepted", "closed")) {
    entry$closed_at <- entry$closed_at %||% timestamp_now()
  } else {
    entry$closed_at <- NULL
  }
  data$risks[[index]] <- entry
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  log_governance_action("update_risk", "risk", entry$id, entry$title, list(previous_status = previous_status, new_status = entry$status), root = root)
  invisible(entry$id)
}

#' Mark a project risk as mitigated
#'
#' @param risk Risk id or title.
#' @param root Existing project root.
#'
#' @return Invisibly returns the updated risk id.
#' @examples
#' \dontrun{
#' mark_project_risk_mitigated("risk_0001")
#' }
#' @author Thiago de Paula Oliveira
#' @export
mark_project_risk_mitigated <- function(risk, root = ".") {
  update_project_risk(risk, root = root, status = "mitigated")
}

#' Close a project risk
#'
#' @param risk Risk id or title.
#' @param root Existing project root.
#'
#' @return Invisibly returns the updated risk id.
#' @examples
#' \dontrun{
#' close_project_risk("risk_0001")
#' }
#' @author Thiago de Paula Oliveira
#' @export
close_project_risk <- function(risk, root = ".") {
  update_project_risk(risk, root = root, status = "closed")
}

#' Remove a project risk
#'
#' @param risk Risk id or title.
#' @param root Existing project root.
#'
#' @return Invisibly returns the removed risk id.
#' @examples
#' \dontrun{
#' remove_project_risk("risk_0001")
#' }
#' @author Thiago de Paula Oliveira
#' @export
remove_project_risk <- function(risk, root = ".") {
  validate_character_vector(risk, "risk")
  root <- find_project_root(root)
  data <- read_project_tasks_data(root)
  index <- governance_entry_index(data$risks, risk[[1]])
  if (is.na(index)) {
    rlang::abort(paste0("Risk `", risk[[1]], "` does not exist."))
  }
  backup_governance_state(root)
  entry <- data$risks[[index]]
  data$risks[[index]] <- NULL
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  log_governance_action("remove_risk", "risk", entry$id, entry$title, root = root)
  invisible(entry$id)
}

list_project_decisions <- function(root = ".") {
  project_decisions(root)
}

#' List project decisions
#'
#' @param root Existing project root.
#'
#' @return A data frame with one row per decision.
#' @examples
#' \dontrun{
#' project_decisions()
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_decisions <- function(root = ".") {
  governance_entries_to_df(
    read_project_tasks_data(root)$decisions,
    c("id", "title", "decision", "rationale", "consequences", "linked_objects", "created_at", "updated_at", "status")
  )
}

#' Record a project decision
#'
#' @param title Human-readable decision title.
#' @param decision Optional decision statement. If omitted, the decision statement defaults to \code{title}.
#' @param root Existing project root.
#' @param rationale Optional rationale.
#' @param consequences Optional consequences.
#' @param linked_objects Optional linked objects.
#' @param status Decision status.
#'
#' @return Invisibly returns the created decision id.
#' @examples
#' \dontrun{
#' record_project_decision("Keep raw data outside the repository")
#' }
#' @author Thiago de Paula Oliveira
#' @export
record_project_decision <- function(title,
                                    decision = NULL,
                                    root = ".",
                                    rationale = NULL,
                                    consequences = NULL,
                                    linked_objects = NULL,
                                    status = "active") {
  validate_character_vector(title, "title")
  if (is.null(decision)) {
    decision <- title
  }
  validate_character_vector(decision, "decision")
  status <- validate_governance_status(status, "decision")
  rationale <- validate_optional_text(rationale, "rationale")
  consequences <- validate_optional_text(consequences, "consequences")
  linked_objects <- validate_linked_objects(linked_objects)

  root <- find_project_root(root)
  backup_governance_state(root)
  data <- read_project_tasks_data(root)
  entry <- list(
    id = next_governance_id(data$decisions, "decision"),
    title = trimws(title[[1]]),
    decision = trimws(decision[[1]]),
    rationale = rationale,
    consequences = consequences,
    linked_objects = linked_objects,
    created_at = timestamp_now(),
    updated_at = timestamp_now(),
    status = status
  )
  data$decisions[[length(data$decisions) + 1L]] <- entry
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  log_governance_action("record_decision", "decision", entry$id, entry$title, list(status = entry$status), root = root)
  invisible(entry$id)
}

#' Add a project decision
#'
#' @inheritParams record_project_decision
#'
#' @return Invisibly returns the created decision id.
#' @examples
#' \dontrun{
#' add_project_decision("Keep raw data outside the repository")
#' }
#' @author Thiago de Paula Oliveira
#' @export
add_project_decision <- function(title,
                                 decision = NULL,
                                 root = ".",
                                 rationale = NULL,
                                 consequences = NULL,
                                 linked_objects = NULL,
                                 status = "active") {
  record_project_decision(
    title = title,
    decision = decision,
    root = root,
    rationale = rationale,
    consequences = consequences,
    linked_objects = linked_objects,
    status = status
  )
}

#' Update a project decision
#'
#' @param decision_id Decision id or title.
#' @param root Existing project root.
#' @param title Optional replacement title.
#' @param decision Optional replacement decision statement.
#' @param rationale Optional replacement rationale.
#' @param consequences Optional replacement consequences.
#' @param linked_objects Optional replacement linked objects.
#' @param status Optional replacement status.
#'
#' @return Invisibly returns the updated decision id.
#' @examples
#' \dontrun{
#' update_project_decision("decision_0001", status = "superseded")
#' }
#' @author Thiago de Paula Oliveira
#' @export
update_project_decision <- function(decision_id,
                                    root = ".",
                                    title = NULL,
                                    decision = NULL,
                                    rationale = NULL,
                                    consequences = NULL,
                                    linked_objects = NULL,
                                    status = NULL) {
  validate_character_vector(decision_id, "decision_id")
  root <- find_project_root(root)
  data <- read_project_tasks_data(root)
  index <- governance_entry_index(data$decisions, decision_id[[1]])
  if (is.na(index)) {
    rlang::abort(paste0("Decision `", decision_id[[1]], "` does not exist."))
  }
  backup_governance_state(root)
  entry <- data$decisions[[index]]
  if (!is.null(title)) entry$title <- trimws(title[[1]])
  if (!is.null(decision)) entry$decision <- trimws(decision[[1]])
  if (!is.null(rationale)) entry$rationale <- validate_optional_text(rationale, "rationale")
  if (!is.null(consequences)) entry$consequences <- validate_optional_text(consequences, "consequences")
  if (!is.null(linked_objects)) entry$linked_objects <- validate_linked_objects(linked_objects)
  if (!is.null(status)) entry$status <- validate_governance_status(status, "decision")
  entry$updated_at <- timestamp_now()
  data$decisions[[index]] <- entry
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  log_governance_action("update_decision", "decision", entry$id, entry$title, list(status = entry$status), root = root)
  invisible(entry$id)
}

#' Remove a project decision
#'
#' @param decision_id Decision id or title.
#' @param root Existing project root.
#'
#' @return Invisibly returns the removed decision id.
#' @examples
#' \dontrun{
#' remove_project_decision("decision_0001")
#' }
#' @author Thiago de Paula Oliveira
#' @export
remove_project_decision <- function(decision_id, root = ".") {
  validate_character_vector(decision_id, "decision_id")
  root <- find_project_root(root)
  data <- read_project_tasks_data(root)
  index <- governance_entry_index(data$decisions, decision_id[[1]])
  if (is.na(index)) {
    rlang::abort(paste0("Decision `", decision_id[[1]], "` does not exist."))
  }
  backup_governance_state(root)
  entry <- data$decisions[[index]]
  data$decisions[[index]] <- NULL
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  log_governance_action("remove_decision", "decision", entry$id, entry$title, root = root)
  invisible(entry$id)
}

risk_status_summary <- function(risks_df) {
  statuses <- risks_df$status %||% character()
  list(
    open = sum(statuses == "open", na.rm = TRUE),
    mitigating = sum(statuses == "mitigating", na.rm = TRUE),
    mitigated = sum(statuses == "mitigated", na.rm = TRUE),
    accepted = sum(statuses == "accepted", na.rm = TRUE),
    closed = sum(statuses == "closed", na.rm = TRUE)
  )
}

count_overdue_tasks <- function(tasks_df) {
  if (nrow(tasks_df) == 0L || !"due_date" %in% names(tasks_df)) {
    return(0L)
  }
  due <- suppressWarnings(as.Date(as.character(tasks_df$due_date)))
  sum(!is.na(due) & due < Sys.Date() & !tasks_df$status %in% c("done", "cancelled"), na.rm = TRUE)
}

#' Create a project status report
#'
#' @param root Existing project root.
#' @param output Output format to return.
#'
#' @return Either a markdown string, HTML string, or a structured list.
#' @examples
#' \dontrun{
#' project_status_report(output = "data")
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_status_report <- function(root = ".", output = c("markdown", "html", "data")) {
  output <- match.arg(output)
  tasks <- project_tasks(root)
  milestones <- project_milestones(root)
  decisions <- project_decisions(root)
  risks <- project_risks(root)
  status <- check_project(root = root, deep = FALSE, render_reports = FALSE, strict = FALSE, repair = FALSE)
  risk_counts <- risk_status_summary(risks)

  summary <- list(
    tasks = tasks,
    milestones = milestones,
    decisions = decisions,
    risks = risks,
    project_check = status,
    counts = list(
      open_tasks = sum(tasks$status %in% c("backlog", "todo", "in_progress", "blocked"), na.rm = TRUE),
      overdue_tasks = count_overdue_tasks(tasks),
      completed_tasks = sum(tasks$status == "done", na.rm = TRUE),
      blocked_tasks = sum(tasks$status == "blocked", na.rm = TRUE),
      open_risks = risk_counts$open,
      mitigating_risks = risk_counts$mitigating,
      mitigated_risks = risk_counts$mitigated,
      accepted_risks = risk_counts$accepted,
      closed_risks = risk_counts$closed,
      decisions = nrow(decisions)
    )
  )

  if (identical(output, "data")) {
    return(summary)
  }

  markdown <- paste(
    "# Project Status Report",
    "",
    paste0("- Open tasks: ", summary$counts$open_tasks),
    paste0("- Overdue tasks: ", summary$counts$overdue_tasks),
    paste0("- Completed tasks: ", summary$counts$completed_tasks),
    paste0("- Blocked tasks: ", summary$counts$blocked_tasks),
    paste0("- Open risks: ", summary$counts$open_risks),
    paste0("- Mitigating risks: ", summary$counts$mitigating_risks),
    paste0("- Mitigated risks: ", summary$counts$mitigated_risks),
    paste0("- Accepted risks: ", summary$counts$accepted_risks),
    paste0("- Closed risks: ", summary$counts$closed_risks),
    paste0("- Recorded decisions: ", summary$counts$decisions),
    "",
    "## Project checks",
    "",
    paste0("- Errors: ", nrow(status$errors)),
    paste0("- Warnings: ", nrow(status$warnings)),
    paste0("- Suggestions: ", nrow(status$suggestions)),
    "",
    "## Tasks",
    "",
    if (nrow(tasks) == 0L) "- No tasks recorded." else paste0("- ", tasks$title, " [", tasks$status, "]"),
    "",
    "## Risks",
    "",
    if (nrow(risks) == 0L) "- No risks recorded." else paste0("- ", risks$title, " [", risks$status, "]"),
    "",
    "## Milestones",
    "",
    if (nrow(milestones) == 0L) "- No milestones recorded." else paste0("- ", milestones$title, " [", milestones$status, "]"),
    sep = "\n"
  )

  if (identical(output, "markdown")) {
    return(markdown)
  }

  paste0("<pre>", escape_html_text(markdown), "</pre>")
}

sync_project_tasks_to_github <- function(root = ".", dry_run = TRUE) {
  validate_logical_scalar(dry_run, "dry_run")
  list(dry_run = dry_run, tasks = project_tasks(root))
}

sync_github_issues_to_project_tasks <- function(root = ".", dry_run = TRUE) {
  validate_logical_scalar(dry_run, "dry_run")
  list(dry_run = dry_run)
}

create_github_milestones_from_project <- function(root = ".", dry_run = TRUE) {
  validate_logical_scalar(dry_run, "dry_run")
  list(dry_run = dry_run, milestones = project_milestones(root))
}
