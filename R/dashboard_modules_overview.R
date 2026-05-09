mod_overview_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      shiny::uiOutput(ns("health_box")),
      shiny::uiOutput(ns("tasks_box")),
      shiny::uiOutput(ns("outputs_box"))
    ),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      shiny::uiOutput(ns("risks_box")),
      shiny::uiOutput(ns("packages_box")),
      shiny::uiOutput(ns("data_box"))
    ),
    if (isTRUE(manage)) {
      shiny::fluidRow(
        shiny::column(12,
          shiny::actionButton(ns("go_tasks"), "Add task"),
          shiny::actionButton(ns("go_objects"), "Add object"),
          shiny::actionButton(ns("go_reports"), "Reports"),
          shiny::actionButton(ns("go_network"), "Open network")
        )
      )
    }
  )
}

mod_overview_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$health_box <- shiny::renderUI({
      diagnostics <- state$diagnostics
      dashboard_value_box("Overall health", diagnostics$summary$overall_status[[1]], if (identical(diagnostics$summary$overall_status[[1]], "Healthy")) "success" else if (identical(diagnostics$summary$overall_status[[1]], "Broken")) "danger" else "warning")
    })
    output$tasks_box <- shiny::renderUI({
      diagnostics <- state$diagnostics
      dashboard_value_box("Open / overdue tasks", paste(diagnostics$summary$open_tasks[[1]], "/", diagnostics$summary$overdue_tasks[[1]]), "primary")
    })
    output$outputs_box <- shiny::renderUI({
      diagnostics <- state$diagnostics
      dashboard_value_box("Missing / stale outputs", paste(diagnostics$summary$missing_outputs[[1]], "/", diagnostics$summary$stale_outputs[[1]]), "warning")
    })
    output$risks_box <- shiny::renderUI({
      diagnostics <- state$diagnostics
      dashboard_value_box("Open risks", diagnostics$summary$open_risks[[1]], "danger")
    })
    output$packages_box <- shiny::renderUI({
      diagnostics <- state$diagnostics
      dashboard_value_box("Missing packages", diagnostics$summary$missing_packages[[1]], "secondary")
    })
    output$data_box <- shiny::renderUI({
      diagnostics <- state$diagnostics
      dashboard_value_box("Unavailable data sources", diagnostics$summary$data_sources_unavailable[[1]], "secondary")
    })

        shiny::observeEvent(input$go_tasks, {
            shiny::updateTabsetPanel(session$parent, "main_tabs", selected = "Task board")
        })
        shiny::observeEvent(input$go_objects, {
            shiny::updateTabsetPanel(session$parent, "main_tabs", selected = "Add object")
        })
        shiny::observeEvent(input$go_reports, {
            shiny::updateTabsetPanel(session$parent, "main_tabs", selected = "Reports")
        })
        shiny::observeEvent(input$go_network, {
            shiny::updateTabsetPanel(session$parent, "main_tabs", selected = "Network")
        })
  })
}
