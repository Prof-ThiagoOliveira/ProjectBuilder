mod_checks_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "projflow-actions",
      shiny::actionButton(ns("refresh_checks"), "Refresh checks"),
      if (manage) shiny::actionButton(ns("repair_dry_run"), "Preview safe repairs"),
      if (manage) shiny::actionButton(ns("repair_apply"), "Apply safe repairs")
    ),
    dashboard_table_ui(ns("checks_table")),
    shiny::verbatimTextOutput(ns("repair_preview"))
  )
}

mod_checks_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    checks_data <- shiny::reactive(state$diagnostics$checks)
    dashboard_render_table(output, "checks_table", checks_data)

    output$repair_preview <- shiny::renderPrint(invisible(NULL))

    shiny::observeEvent(input$refresh_checks, {
      refresh_dashboard_state(state)
      shiny::showNotification("Checks refreshed.", type = "message")
    })

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$repair_dry_run, {
      output$repair_preview <- shiny::renderPrint({
        repair_project(root = state$root, dry_run = TRUE)
      })
    })

    shiny::observeEvent(input$repair_apply, {
      dashboard_run_action(
        state,
        session,
        "Project repair",
        repair_project(root = state$root, dry_run = FALSE, confirm = TRUE)
      )
    })
  })
}
