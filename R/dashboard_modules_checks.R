mod_checks_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dashboard_card(
      "Checks and fixes",
      subtitle = "Review structural, registry and output issues detected in the project.",
      shiny::fluidRow(
        shiny::column(3, shiny::uiOutput(ns("errors_box"))),
        shiny::column(3, shiny::uiOutput(ns("warnings_box"))),
        shiny::column(3, shiny::uiOutput(ns("suggestions_box"))),
        shiny::column(3, shiny::uiOutput(ns("repair_box")))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        dashboard_card(
          "Actions",
          subtitle = "Preview repairs before applying them.",
          shiny::div(
            class = "projflow-actions",
            shiny::actionButton(ns("refresh_checks"), "Refresh checks", class = "btn-outline-primary"),
            if (manage) shiny::actionButton(ns("repair_dry_run"), "Preview repairs", class = "btn-outline-secondary"),
            if (manage) shiny::actionButton(ns("repair_apply"), "Apply safe repairs", class = "btn-outline-danger")
          ),
          shiny::hr(),
          shiny::verbatimTextOutput(ns("repair_preview"))
        )
      ),
      shiny::column(
        width = 8,
        dashboard_card(
          "Detected issues",
          subtitle = "Use severity and message columns to prioritise repairs.",
          dashboard_table_ui(ns("checks_table"))
        )
      )
    )
  )
}

mod_checks_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    checks_data <- shiny::reactive(dashboard_safe_data_frame(dashboard_diagnostics(state)$checks))
    dashboard_render_table(output, "checks_table", checks_data)

    issue_counts <- shiny::reactive(dashboard_issue_counts(dashboard_diagnostics(state)))
    output$errors_box <- shiny::renderUI(dashboard_value_box("Errors", issue_counts()[["errors"]], if (issue_counts()[["errors"]] > 0L) "danger" else "success"))
    output$warnings_box <- shiny::renderUI(dashboard_value_box("Warnings", issue_counts()[["warnings"]], if (issue_counts()[["warnings"]] > 0L) "warning" else "success"))
    output$suggestions_box <- shiny::renderUI(dashboard_value_box("Suggestions", issue_counts()[["suggestions"]], "secondary"))
    output$repair_box <- shiny::renderUI({
      label <- if (isTRUE(manage)) "Available" else "Read-only"
      dashboard_value_box("Repair mode", label, if (isTRUE(manage)) "primary" else "secondary", note = "Repairs are limited to safe metadata operations.")
    })

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
        repair_project(root = dashboard_root(state), dry_run = TRUE)
      })
    })

    shiny::observeEvent(input$repair_apply, {
      dashboard_run_action(
        state,
        session,
        "Project repair",
        repair_project(root = dashboard_root(state), dry_run = FALSE, confirm = TRUE)
      )
    })
  })
}
