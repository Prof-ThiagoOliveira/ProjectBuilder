mod_data_sources_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dashboard_card(
      "Data sources",
      subtitle = "Register external data roots and verify whether they exist and are readable.",
      shiny::fluidRow(
        shiny::column(4, shiny::uiOutput(ns("sources_box"))),
        shiny::column(4, shiny::uiOutput(ns("available_box"))),
        shiny::column(4, shiny::uiOutput(ns("unavailable_box")))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        dashboard_form_card(
          "Register data source",
          subtitle = "Use external paths for raw or sensitive data that should not be committed to the project repository.",
          shiny::textInput(ns("source_name"), "Source name", value = "default"),
          shiny::textInput(ns("source_path"), "Path", placeholder = "C:/data/project or /mnt/data/project"),
          if (manage) shiny::actionButton(ns("add_source"), "Add or update", class = "btn-primary"),
          if (manage) shiny::actionButton(ns("remove_source"), "Remove selected", class = "btn-outline-danger"),
          shiny::actionButton(ns("check_source"), "Refresh access checks", class = "btn-outline-secondary")
        )
      ),
      shiny::column(
        width = 8,
        dashboard_card(
          "Registered sources",
          subtitle = "The table indicates whether each configured path exists and can be read.",
          dashboard_table_ui(ns("sources_table"))
        )
      )
    )
  )
}

mod_data_sources_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    source_data <- shiny::reactive({
      dashboard_safe_data_frame(dashboard_diagnostics(state)$data_sources)
    })

    dashboard_render_table(output, "sources_table", source_data)

    output$sources_box <- shiny::renderUI({
      dashboard_value_box("Sources", nrow(source_data()), "secondary", note = "Configured external roots.")
    })
    output$available_box <- shiny::renderUI({
      sources <- source_data()
      value <- if (all(c("exists", "readable") %in% names(sources))) sum(sources$exists & sources$readable, na.rm = TRUE) else 0L
      dashboard_value_box("Available", value, "success", note = "Exists and readable.")
    })
    output$unavailable_box <- shiny::renderUI({
      value <- dashboard_summary_value(state, "data_sources_unavailable", 0L)
      dashboard_value_box("Unavailable", value, if (as.integer(value) > 0L) "danger" else "success", note = "Needs path or permission review.")
    })

    selected_source_row <- shiny::reactive({
      dashboard_selected_row(input, "sources_table", source_data())
    })

    selected_source <- shiny::reactive({
      source <- selected_source_row()
      if (is.null(source) || !"name" %in% names(source)) {
        return(NULL)
      }
      source$name[[1]]
    })

    shiny::observeEvent(selected_source_row(), {
      source <- selected_source_row()
      if (is.null(source)) return(invisible(NULL))
      shiny::updateTextInput(session, "source_name", value = dashboard_cell(source, "name", "default"))
      shiny::updateTextInput(session, "source_path", value = dashboard_cell(source, "path"))
    }, ignoreInit = TRUE)

    if (isTRUE(manage)) {
      shiny::observeEvent(input$add_source, {
        dashboard_run_action(
          state = state,
          session = session,
          label = "Data source update",
          expr = set_project_data_root(
            path = dashboard_required_text(input$source_path, "Data-source path"),
            name = dashboard_required_text(input$source_name, "Data-source name"),
            root = dashboard_root(state)
          )
        )
      })

      shiny::observeEvent(input$remove_source, {
        shiny::req(selected_source())
        dashboard_run_action(
          state = state,
          session = session,
          label = "Data source removal",
          expr = remove_project_data_source(selected_source(), root = dashboard_root(state))
        )
      })
    }

    shiny::observeEvent(input$check_source, {
      refresh_dashboard_state(state)
      shiny::showNotification("Data-source checks refreshed.", type = "message")
    })
  })
}
