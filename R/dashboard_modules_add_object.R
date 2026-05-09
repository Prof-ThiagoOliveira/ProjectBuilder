mod_add_object_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      ns("object_type"),
      "Object type",
      choices = c("script", "report", "output", "table", "figure", "app", "task", "risk", "milestone", "decision", "data_source")
    ),
    shiny::textInput(ns("object_name"), "Name or title"),
    shiny::textInput(ns("object_subtype"), "Subtype / type", placeholder = "e.g. statistical_analysis, model, shiny"),
    shiny::textInput(ns("object_path"), "Optional path"),
    shiny::checkboxInput(ns("object_dry_run"), "Dry-run only", value = TRUE),
    if (manage) shiny::actionButton(ns("create_object"), "Create object"),
    shiny::verbatimTextOutput(ns("preview"))
  )
}

mod_add_object_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    output$preview <- shiny::renderPrint({
      list(
        object_type = input$object_type,
        name = input$object_name,
        subtype = input$object_subtype,
        path = input$object_path,
        dry_run = input$object_dry_run
      )
    })

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$create_object, {
      call_expr <- switch(
        input$object_type,
        script = quote(new_script(
          name = input$object_name,
          type = if (nzchar(input$object_subtype)) input$object_subtype else "analysis",
          root = state$root,
          output = if (nzchar(input$object_path)) input$object_path else NULL,
          dry_run = isTRUE(input$object_dry_run),
          open = FALSE
        )),
        report = quote(new_report(
          name = input$object_name,
          root = state$root,
          dry_run = isTRUE(input$object_dry_run),
          open = FALSE
        )),
        output = quote(new_output(
          name = input$object_name,
          type = if (nzchar(input$object_subtype)) input$object_subtype else "output",
          path = if (nzchar(input$object_path)) input$object_path else NULL,
          root = state$root,
          dry_run = isTRUE(input$object_dry_run)
        )),
        table = quote(new_table(
          name = input$object_name,
          path = if (nzchar(input$object_path)) input$object_path else NULL,
          root = state$root,
          dry_run = isTRUE(input$object_dry_run)
        )),
        figure = quote(new_figure(
          name = input$object_name,
          path = if (nzchar(input$object_path)) input$object_path else NULL,
          root = state$root,
          dry_run = isTRUE(input$object_dry_run)
        )),
        app = quote(new_app(
          name = if (nzchar(input$object_name)) input$object_name else "app",
          type = if (nzchar(input$object_subtype)) input$object_subtype else "shiny",
          root = state$root,
          dry_run = isTRUE(input$object_dry_run),
          open = FALSE
        )),
        task = quote(add_project_task(
          title = input$object_name,
          root = state$root
        )),
        risk = quote(add_project_risk(
          title = input$object_name,
          root = state$root
        )),
        milestone = quote(add_project_milestone(
          title = input$object_name,
          root = state$root
        )),
        decision = quote(record_project_decision(
          title = input$object_name,
          decision = if (nzchar(input$object_subtype)) input$object_subtype else input$object_name,
          root = state$root
        )),
        data_source = quote(set_project_data_root(
          path = input$object_path,
          name = if (nzchar(input$object_name)) input$object_name else "default",
          root = state$root
        ))
      )

      dashboard_run_action(
        state = state,
        session = session,
        label = "Object creation",
        expr = eval(call_expr)
      )
    })
  })
}
