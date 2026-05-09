mod_activity_log_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "projflow-actions",
      if (manage) shiny::actionButton(ns("clear_log"), "Clear activity log")
    ),
    dashboard_table_ui(ns("activity_table"))
  )
}

mod_activity_log_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    activity_data <- shiny::reactive(state$diagnostics$activity)
    dashboard_render_table(output, "activity_table", activity_data)

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$clear_log, {
      dashboard_run_action(
        state,
        session,
        "Activity log clear",
        clear_project_activity(root = state$root, confirm = TRUE)
      )
    })
  })
}

mod_settings_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::verbatimTextOutput(ns("project_settings")),
    shiny::verbatimTextOutput(ns("backup_settings"))
  )
}

mod_settings_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    output$project_settings <- shiny::renderPrint({
      list(
        project = state$diagnostics$project,
        summary = state$diagnostics$summary
      )
    })

    output$backup_settings <- shiny::renderPrint({
      list_project_backups(state$root)
    })
  })
}
