tasks_path <- function(root = ".") {
  fs::path(find_project_root(root), ".projectSetupR", "tasks.yml")
}

default_tasks_data <- function() {
  list(
    version = 1L,
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
  data$tasks <- data$tasks %||% list()
  data$milestones <- data$milestones %||% list()
  data$decisions <- data$decisions %||% list()
  data$risks <- data$risks %||% list()
  data
}

write_project_tasks_data <- function(data, root = ".", overwrite = TRUE) {
  write_yaml_file(tasks_path(root), data, overwrite = overwrite)
}

project_tasks <- function(root = ".") {
  tasks <- read_project_tasks_data(root)$tasks
  if (length(tasks) == 0L) {
    return(data.frame(
      task = character(),
      title = character(),
      status = character(),
      owner = character(),
      due = character(),
      priority = character(),
      related_component = character(),
      related_file = character(),
      notes = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(
    rbind,
    lapply(names(tasks), function(name) {
      task <- tasks[[name]]
      data.frame(
        task = name,
        title = task$title %||% NA_character_,
        status = task$status %||% NA_character_,
        owner = task$owner %||% NA_character_,
        due = as.character(task$due %||% NA_character_),
        priority = task$priority %||% NA_character_,
        related_component = task$related_component %||% NA_character_,
        related_file = task$related_file %||% task$related_script %||% task$related_report %||% NA_character_,
        notes = task$notes %||% NA_character_,
        stringsAsFactors = FALSE
      )
    })
  )
}

validate_task_status <- function(status) {
  validate_choice(status, c("todo", "in_progress", "blocked", "done", "cancelled"), "status")
}

validate_task_priority <- function(priority) {
  validate_choice(priority, c("low", "medium", "high", "critical"), "priority")
}

update_governance_docs <- function(root = ".") {
  root <- find_project_root(root)
  tasks <- project_tasks(root)
  task_lines <- if (nrow(tasks) == 0L) {
    "No tasks recorded."
  } else {
    paste0("- [", tasks$status, "] ", tasks$title)
  }

  write_template_file(
    fs::path(root, "docs", "status.md"),
    paste(c("# Status", "", task_lines), collapse = "\n"),
    overwrite = TRUE
  )

  invisible(root)
}

add_project_task <- function(
    title,
    root = ".",
    status = "todo",
    owner = NULL,
    due = NULL,
    priority = "medium",
    related_component = NULL,
    related_file = NULL,
    notes = NULL) {
  validate_character_vector(title, "title")
  validate_task_status(status)
  validate_task_priority(priority)
  validate_character_vector(owner, "owner", allow_null = TRUE)
  validate_character_vector(related_component, "related_component", allow_null = TRUE)
  validate_character_vector(related_file, "related_file", allow_null = TRUE)
  validate_character_vector(notes, "notes", allow_null = TRUE)

  data <- read_project_tasks_data(root)
  key <- task_key_from_title(title)
  data$tasks[[key]] <- list(
    title = title,
    status = status,
    owner = owner,
    due = due,
    related_component = related_component,
    related_file = related_file,
    priority = priority,
    notes = notes
  )
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  update_governance_docs(root)
  invisible(key)
}

update_project_task <- function(task, root = ".", status = NULL, owner = NULL, due = NULL, priority = NULL, notes = NULL) {
  validate_character_vector(task, "task")
  data <- read_project_tasks_data(root)
  key <- task_key_from_title(task[[1]])
  if (is.null(data$tasks[[key]]) && !is.null(data$tasks[[task[[1]]]])) {
    key <- task[[1]]
  }
  if (is.null(data$tasks[[key]])) {
    rlang::abort(paste0("Task `", task[[1]], "` does not exist."))
  }

  entry <- data$tasks[[key]]
  if (!is.null(status)) entry$status <- validate_task_status(status)
  if (!is.null(owner)) entry$owner <- owner
  if (!is.null(due)) entry$due <- due
  if (!is.null(priority)) entry$priority <- validate_task_priority(priority)
  if (!is.null(notes)) entry$notes <- notes

  data$tasks[[key]] <- entry
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  update_governance_docs(root)
  invisible(key)
}

complete_project_task <- function(task, root = ".") {
  update_project_task(task, root = root, status = "done")
}

remove_project_task <- function(task, root = ".") {
  validate_character_vector(task, "task")
  data <- read_project_tasks_data(root)
  key <- task_key_from_title(task[[1]])
  data$tasks[[key]] <- NULL
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  update_governance_docs(root)
  invisible(key)
}

project_milestones <- function(root = ".") {
  read_project_tasks_data(root)$milestones
}

add_project_milestone <- function(title, root = ".", due = NULL, notes = NULL) {
  validate_character_vector(title, "title")
  data <- read_project_tasks_data(root)
  key <- task_key_from_title(title)
  data$milestones[[key]] <- list(title = title, status = "todo", due = due, notes = notes)
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  invisible(key)
}

update_project_milestone <- function(milestone, root = ".", status = NULL, due = NULL, notes = NULL) {
  validate_character_vector(milestone, "milestone")
  data <- read_project_tasks_data(root)
  key <- task_key_from_title(milestone[[1]])
  entry <- data$milestones[[key]]
  if (is.null(entry)) {
    rlang::abort(paste0("Milestone `", milestone[[1]], "` does not exist."))
  }
  if (!is.null(status)) entry$status <- status
  if (!is.null(due)) entry$due <- due
  if (!is.null(notes)) entry$notes <- notes
  data$milestones[[key]] <- entry
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  invisible(key)
}

project_decisions <- function(root = ".") {
  read_project_tasks_data(root)$decisions
}

add_project_decision <- function(title, decision, root = ".", rationale = NULL, date = Sys.Date(), owner = NULL) {
  validate_character_vector(title, "title")
  validate_character_vector(decision, "decision")
  data <- read_project_tasks_data(root)
  key <- task_key_from_title(title)
  data$decisions[[key]] <- list(
    title = title,
    decision = decision,
    rationale = rationale,
    date = as.character(date),
    owner = owner
  )
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  invisible(key)
}

project_risks <- function(root = ".") {
  read_project_tasks_data(root)$risks
}

add_project_risk <- function(title, root = ".", probability = NULL, impact = NULL, mitigation = NULL, owner = NULL, status = "open") {
  validate_character_vector(title, "title")
  data <- read_project_tasks_data(root)
  key <- task_key_from_title(title)
  data$risks[[key]] <- list(
    title = title,
    probability = probability,
    impact = impact,
    mitigation = mitigation,
    owner = owner,
    status = status
  )
  write_project_tasks_data(data, root = root, overwrite = TRUE)
  invisible(key)
}

project_status_report <- function(root = ".", output = c("markdown", "html", "data")) {
  output <- match.arg(output)
  tasks <- project_tasks(root)
  data <- read_project_tasks_data(root)
  status <- check_project(root = root, deep = FALSE, strict = FALSE, repair = FALSE)

  summary <- list(
    tasks = tasks,
    milestones = data$milestones,
    decisions = data$decisions,
    risks = data$risks,
    project_check = status
  )

  if (identical(output, "data")) {
    return(summary)
  }

  markdown <- paste(
    "# Project Status Report",
    "",
    paste0("- Open tasks: ", sum(tasks$status %in% c("todo", "in_progress", "blocked"), na.rm = TRUE)),
    paste0("- Completed tasks: ", sum(tasks$status == "done", na.rm = TRUE)),
    paste0("- Blocked tasks: ", sum(tasks$status == "blocked", na.rm = TRUE)),
    paste0("- Open risks: ", length(data$risks)),
    paste0("- Recent decisions: ", length(data$decisions)),
    "",
    "## Project checks",
    "",
    paste0("- Errors: ", nrow(status$errors)),
    paste0("- Warnings: ", nrow(status$warnings)),
    sep = "\n"
  )

  if (identical(output, "markdown")) {
    return(markdown)
  }

  paste("<pre>", markdown, "</pre>")
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
