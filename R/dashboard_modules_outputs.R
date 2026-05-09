mod_outputs_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::textInput(ns("output_name"), "Output name"),
        shiny::textInput(ns("output_type"), "Type", value = "output"),
        shiny::textInput(ns("output_path"), "Path"),
        if (manage) shiny::actionButton(ns("register_output"), "Register output"),
        if (manage) shiny::actionButton(ns("remove_output"), "Remove selected output")
      ),
      shiny::column(
        width = 8,
        dashboard_table_ui(ns("outputs_table")),
        shiny::verbatimTextOutput(ns("output_summary"))
      )
    )
  )
}

mod_outputs_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    output_data <- shiny::reactive({
      state$diagnostics$outputs
    })

    dashboard_render_table(output, "outputs_table", output_data)

    output$output_summary <- shiny::renderPrint({
      list(
        missing_outputs = missing_project_outputs(state$root),
        stale_outputs = stale_project_outputs(state$root)
      )
    })

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$register_output, {
      dashboard_run_action(
        state = state,
        session = session,
        label = "Output registration",
        expr = new_project_output(
          name = input$output_name,
          type = input$output_type,
          path = if (nzchar(input$output_path)) input$output_path else NULL,
          root = state$root
        )
      )
    })

    selected_output <- shiny::reactive({
      sel <- input$outputs_table_rows_selected
      outputs <- output_data()
      if (length(sel) != 1L || nrow(outputs) < sel[[1]]) {
        return(NULL)
      }
      outputs$name[[sel[[1]]]]
    })

    shiny::observeEvent(input$remove_output, {
      shiny::req(selected_output())
      dashboard_run_action(
        state = state,
        session = session,
        label = "Output removal",
        expr = remove_project_output(selected_output(), root = state$root)
      )
    })
  })
}
