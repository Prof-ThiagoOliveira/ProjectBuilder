mod_activity_log_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dashboard_card(
      "Activity log",
      subtitle = "Audit trail of dashboard, governance and registry actions.",
      actions = if (manage) shiny::actionButton(ns("clear_log"), "Clear activity log", class = "btn-outline-danger"),
      dashboard_table_ui(ns("activity_table"))
    )
  )
}

mod_activity_log_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    activity_data <- shiny::reactive(dashboard_safe_data_frame(dashboard_diagnostics(state)$activity))
    dashboard_render_table(output, "activity_table", activity_data, selection = "none", page_length = 15)

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$clear_log, {
      dashboard_run_action(
        state,
        session,
        "Activity log clear",
        clear_project_activity(root = dashboard_root(state), confirm = TRUE)
      )
    })
  })
}

mod_settings_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        dashboard_card(
          "Project metadata",
          subtitle = "Core diagnostic metadata for the active project.",
          shiny::verbatimTextOutput(ns("project_settings"))
        )
      ),
      shiny::column(
        width = 6,
        dashboard_card(
          "Backups",
          subtitle = "Available registry, local configuration and governance backups.",
          shiny::verbatimTextOutput(ns("backup_settings"))
        )
      )
    )
  )
}

mod_settings_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    output$project_settings <- shiny::renderPrint({
      diagnostics <- dashboard_diagnostics(state)
      list(
        project = diagnostics$project,
        summary = diagnostics$summary
      )
    })

    output$backup_settings <- shiny::renderPrint({
      state$refresh_token
      list_project_backups(dashboard_root(state))
    })
  })
}
