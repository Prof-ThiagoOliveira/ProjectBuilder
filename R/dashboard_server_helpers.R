with_project_action_source <- function(source = "dashboard", expr) {
  old <- getOption("projflow.action_source", "cli")
  options(projflow.action_source = source)
  on.exit(options(projflow.action_source = old), add = TRUE)
  force(expr)
}

new_dashboard_state <- function(root, mode) {
  shiny::reactiveValues(
    root = root,
    mode = mode,
    diagnostics = NULL,
    last_action = NULL,
    refresh_token = 0L,
    selected_object = NULL
  )
}

refresh_dashboard_state <- function(state) {
  state$diagnostics <- project_diagnostics_data(state$root)
  state$refresh_token <- shiny::isolate(state$refresh_token %||% 0L) + 1L
  invisible(state$diagnostics)
}

dashboard_run_action <- function(state, session, label, expr) {
  tryCatch(
    {
      result <- with_project_action_source("dashboard", expr)
      refresh_dashboard_state(state)
      state$last_action <- paste0(label, " completed.")
      shiny::showNotification(state$last_action, type = "message")
      result
    },
    error = function(error) {
      state$last_action <- conditionMessage(error)
      shiny::showNotification(conditionMessage(error), type = "error", duration = 8)
      NULL
    }
  )
}
