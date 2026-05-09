mod_reports_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::textInput(ns("report_name"), "Report name"),
        if (manage) shiny::actionButton(ns("add_report"), "Add report"),
        shiny::actionButton(ns("render_selected"), "Render selected report"),
        shiny::actionButton(ns("render_all"), "Render all reports"),
        if (manage) shiny::actionButton(ns("remove_report"), "Remove selected report")
      ),
      shiny::column(
        width = 8,
        dashboard_table_ui(ns("reports_table"))
      )
    )
  )
}

mod_reports_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    report_data <- shiny::reactive({
      state$diagnostics$reports
    })

    dashboard_render_table(output, "reports_table", report_data)

    selected_report <- shiny::reactive({
      sel <- input$reports_table_rows_selected
      reports <- report_data()
      if (length(sel) != 1L || nrow(reports) < sel[[1]]) {
        return(NULL)
      }
      reports[sel[[1]], , drop = FALSE]
    })

    if (isTRUE(manage)) {
      shiny::observeEvent(input$add_report, {
        dashboard_run_action(
          state = state,
          session = session,
          label = "Report creation",
          expr = new_report(input$report_name, root = state$root, open = FALSE)
        )
      })

      shiny::observeEvent(input$remove_report, {
        report <- selected_report()
        shiny::req(report)
        dashboard_run_action(
          state = state,
          session = session,
          label = "Report removal",
          expr = remove_project_report(report$name[[1]], root = state$root)
        )
      })
    }

    shiny::observeEvent(input$render_selected, {
      report <- selected_report()
      shiny::req(report)
      dashboard_run_action(
        state = state,
        session = session,
        label = "Report render",
        expr = render_one_report(
          fs::path(state$root, report$path[[1]] %||% report$source[[1]]),
          fs::path(state$root, default_output_path(report$name[[1]], "report"))
        )
      )
    })

    shiny::observeEvent(input$render_all, {
      dashboard_run_action(
        state = state,
        session = session,
        label = "Report render",
        expr = render_project_reports(state$root)
      )
    })
  })
}
