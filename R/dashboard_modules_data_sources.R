mod_data_sources_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::textInput(ns("source_name"), "Source name", value = "default"),
        shiny::textInput(ns("source_path"), "Path"),
        if (manage) shiny::actionButton(ns("add_source"), "Add data source"),
        if (manage) shiny::actionButton(ns("remove_source"), "Remove selected source"),
        shiny::actionButton(ns("check_source"), "Refresh access checks")
      ),
      shiny::column(
        width = 8,
        dashboard_table_ui(ns("sources_table"))
      )
    )
  )
}

mod_data_sources_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    source_data <- shiny::reactive({
      state$diagnostics$data_sources
    })

    dashboard_render_table(output, "sources_table", source_data)

    selected_source <- shiny::reactive({
      sel <- input$sources_table_rows_selected
      sources <- source_data()
      if (length(sel) != 1L || nrow(sources) < sel[[1]]) {
        return(NULL)
      }
      sources$name[[sel[[1]]]]
    })

    if (isTRUE(manage)) {
      shiny::observeEvent(input$add_source, {
        dashboard_run_action(
          state = state,
          session = session,
          label = "Data source update",
          expr = set_project_data_root(
            path = input$source_path,
            name = input$source_name,
            root = state$root
          )
        )
      })

      shiny::observeEvent(input$remove_source, {
        shiny::req(selected_source())
        dashboard_run_action(
          state = state,
          session = session,
          label = "Data source removal",
          expr = remove_project_data_source(selected_source(), root = state$root)
        )
      })
    }

    shiny::observeEvent(input$check_source, {
      refresh_dashboard_state(state)
      shiny::showNotification("Data-source checks refreshed.", type = "message")
    })
  })
}
