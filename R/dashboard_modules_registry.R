mod_registry_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::textInput(ns("rename_to"), "Rename selected object to"),
        if (manage) shiny::actionButton(ns("rename_object"), "Rename selected object"),
        if (manage) shiny::actionButton(ns("remove_object"), "Remove selected object")
      ),
      shiny::column(
        width = 8,
        dashboard_table_ui(ns("registry_table"))
      )
    )
  )
}

mod_registry_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    object_data <- shiny::reactive({
      state$diagnostics$objects
    })

    dashboard_render_table(output, "registry_table", object_data)

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    selected_object <- shiny::reactive({
      sel <- input$registry_table_rows_selected
      objects <- object_data()
      if (length(sel) != 1L || nrow(objects) < sel[[1]]) {
        return(NULL)
      }
      objects[sel[[1]], , drop = FALSE]
    })

    shiny::observeEvent(input$rename_object, {
      obj <- selected_object()
      shiny::req(obj)
      section <- switch(
        obj$section[[1]],
        script = "script",
        report = "report",
        output = "output",
        obj$section[[1]]
      )
      dashboard_run_action(
        state = state,
        session = session,
        label = "Object rename",
        expr = rename_project_object(
          from = obj$name[[1]],
          to = input$rename_to,
          section = section,
          root = state$root
        )
      )
    })

    shiny::observeEvent(input$remove_object, {
      obj <- selected_object()
      shiny::req(obj)
      section <- switch(
        obj$section[[1]],
        script = "script",
        report = "report",
        output = "output",
        obj$section[[1]]
      )
      dashboard_run_action(
        state = state,
        session = session,
        label = "Object removal",
        expr = remove_project_object(
          name = obj$name[[1]],
          section = section,
          root = state$root,
          delete_files = FALSE,
          confirm = FALSE,
          dry_run = FALSE
        )
      )
    })
  })
}
