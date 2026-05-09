mod_reports_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dashboard_card(
      "Reports",
      subtitle = "Create Quarto reports and render selected or all registered reports.",
      shiny::fluidRow(
        shiny::column(4, shiny::uiOutput(ns("reports_box"))),
        shiny::column(4, shiny::uiOutput(ns("source_box"))),
        shiny::column(4, shiny::uiOutput(ns("render_box")))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        dashboard_form_card(
          "Add report",
          subtitle = "Create a report scaffold and register it in the project metadata.",
          shiny::textInput(ns("report_name"), "Report name", placeholder = "e.g. analysis_report"),
          if (manage) shiny::actionButton(ns("add_report"), "Add report", class = "btn-primary") else dashboard_empty_state("Read-only diagnostic mode.")
        ),
        dashboard_card(
          "Report charts",
          subtitle = "Source and render-output availability.",
          shiny::plotOutput(ns("report_source_plot"), height = "210px"),
          shiny::plotOutput(ns("report_output_plot"), height = "210px")
        ),
        dashboard_card(
          "Render actions",
          subtitle = "Rendering may call Quarto and write output files.",
          if (manage) shiny::div(
            class = "projflow-actions",
            shiny::actionButton(ns("render_selected"), "Render selected", class = "btn-outline-primary"),
            shiny::actionButton(ns("render_all"), "Render all", class = "btn-outline-primary")
          ) else dashboard_empty_state("Read-only diagnostic mode.")
        )
      ),
      shiny::column(
        width = 8,
        dashboard_card(
          "Registered reports",
          subtitle = "Select a report before rendering, editing or removing it.",
          dashboard_table_ui(ns("reports_table"))
        ),
        dashboard_form_card(
          "Selected report editor",
          subtitle = "Update the selected report source path or type.",
          shiny::uiOutput(ns("selected_report_label")),
          shiny::fluidRow(
            shiny::column(6, shiny::textInput(ns("edit_report_path"), "Source path")),
            shiny::column(6, shiny::textInput(ns("edit_report_type"), "Report type"))
          ),
          shiny::checkboxInput(ns("edit_report_overwrite"), "Allow file-path overwrite/rename when changing path", value = FALSE),
          shiny::div(
            class = "projflow-actions",
            if (manage) shiny::actionButton(ns("update_report"), "Save report changes", class = "btn-primary"),
            if (manage) shiny::actionButton(ns("remove_report"), "Remove report", class = "btn-outline-danger") else dashboard_empty_state("Read-only diagnostic mode.")
          )
        )
      )
    )
  )
}

mod_reports_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    report_data <- shiny::reactive({
      dashboard_safe_data_frame(dashboard_diagnostics(state)$reports)
    })

    dashboard_render_table(output, "reports_table", report_data)

    output$reports_box <- shiny::renderUI({
      dashboard_value_box("Registered", nrow(report_data()), "secondary", note = "Reports in registry.")
    })
    output$source_box <- shiny::renderUI({
      reports <- report_data()
      value <- if ("source_exists" %in% names(reports)) sum(reports$source_exists, na.rm = TRUE) else 0L
      dashboard_value_box("Sources exist", value, if (value == nrow(reports)) "success" else "warning", note = "Report source files present.")
    })
    output$render_box <- shiny::renderUI({
      value <- dashboard_summary_value(state, "reports_needing_render", 0L)
      dashboard_value_box("Need rendering", value, if (as.integer(value) > 0L) "warning" else "success", note = "Source exists but output is absent.")
    })

    output$report_source_plot <- shiny::renderPlot({
      data <- report_data()
      if ("source_exists" %in% names(data)) data$source_exists <- as.character(data$source_exists)
      dashboard_plot_categorical_counts(data, "source_exists", title = "Report sources")
    })
    output$report_output_plot <- shiny::renderPlot({
      data <- report_data()
      if ("output_exists" %in% names(data)) data$output_exists <- as.character(data$output_exists)
      dashboard_plot_categorical_counts(data, "output_exists", title = "Rendered outputs")
    })

    selected_report <- shiny::reactive({
      dashboard_selected_row(input, "reports_table", report_data())
    })

    output$selected_report_label <- shiny::renderUI({
      report <- selected_report()
      if (is.null(report)) {
        return(dashboard_empty_state("Select a report row to edit its path or type."))
      }
      shiny::div(class = "projflow-selected-record", shiny::strong("Selected report: "), report$name[[1]])
    })

    shiny::observeEvent(selected_report(), {
      report <- selected_report()
      if (is.null(report)) return(invisible(NULL))
      shiny::updateTextInput(session, "edit_report_path", value = dashboard_cell(report, "path"))
      shiny::updateTextInput(session, "edit_report_type", value = dashboard_cell(report, "type", "report"))
    }, ignoreInit = TRUE)

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$add_report, {
      dashboard_run_action(
        state = state,
        session = session,
        label = "Report creation",
        expr = new_report(
          dashboard_required_text(input$report_name, "Report name"),
          root = dashboard_root(state),
          open = FALSE
        )
      )
    })

    shiny::observeEvent(input$update_report, {
      report <- selected_report()
      shiny::req(report)
      dashboard_run_action(
        state = state,
        session = session,
        label = "Report update",
        expr = update_project_report(
          name = report$name[[1]],
          root = dashboard_root(state),
          path = dashboard_required_text(input$edit_report_path, "Report path"),
          type = dashboard_required_text(input$edit_report_type, "Report type"),
          overwrite = isTRUE(input$edit_report_overwrite)
        )
      )
    })

    shiny::observeEvent(input$remove_report, {
      report <- selected_report()
      shiny::req(report)
      dashboard_run_action(
        state = state,
        session = session,
        label = "Report removal",
        expr = remove_project_report(report$name[[1]], root = dashboard_root(state))
      )
    })

    shiny::observeEvent(input$render_selected, {
      report <- selected_report()
      shiny::req(report)
      dashboard_run_action(
        state = state,
        session = session,
        label = "Report render",
        expr = render_one_report(
          fs::path(dashboard_root(state), report$path[[1]] %||% report$source[[1]]),
          fs::path(dashboard_root(state), default_output_path(report$name[[1]], "report"))
        )
      )
    })

    shiny::observeEvent(input$render_all, {
      dashboard_run_action(
        state = state,
        session = session,
        label = "Report render",
        expr = render_project_reports(dashboard_root(state))
      )
    })
  })
}
