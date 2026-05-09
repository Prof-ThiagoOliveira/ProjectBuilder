mod_registry_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dashboard_card(
      "Registry",
      subtitle = "Review the canonical record of scripts, reports, outputs and other project objects.",
      shiny::fluidRow(
        shiny::column(3, shiny::uiOutput(ns("objects_box"))),
        shiny::column(3, shiny::uiOutput(ns("scripts_box"))),
        shiny::column(3, shiny::uiOutput(ns("reports_box"))),
        shiny::column(3, shiny::uiOutput(ns("outputs_box")))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        dashboard_form_card(
          "Selected object actions",
          subtitle = "Rename or remove registry entries without deleting files.",
          shiny::selectInput(ns("section_filter"), "Section", choices = "All", selected = "All"),
          shiny::textInput(ns("rename_to"), "Rename selected object to"),
          if (manage) shiny::actionButton(ns("rename_object"), "Rename selected", class = "btn-primary"),
          if (manage) shiny::actionButton(ns("remove_object"), "Remove selected", class = "btn-outline-danger") else dashboard_empty_state("Read-only diagnostic mode.")
        )
      ),
      shiny::column(
        width = 8,
        dashboard_card(
          "Registered objects",
          subtitle = "Use the filters to find objects by section, type, name or path.",
          dashboard_table_ui(ns("registry_table"))
        )
      )
    )
  )
}

mod_registry_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    object_data_raw <- shiny::reactive({
      dashboard_safe_data_frame(dashboard_diagnostics(state)$objects)
    })

    shiny::observe({
      objects <- object_data_raw()
      choices <- if ("section" %in% names(objects)) c("All", sort(unique(objects$section))) else "All"
      shiny::updateSelectInput(session, "section_filter", choices = choices, selected = input$section_filter %||% "All")
    })

    object_data <- shiny::reactive({
      dashboard_filter_data(object_data_raw(), "section", input$section_filter)
    })

    dashboard_render_table(output, "registry_table", object_data)

    output$objects_box <- shiny::renderUI({
      dashboard_value_box("Objects", nrow(object_data_raw()), "secondary", note = "All registry sections.")
    })
    output$scripts_box <- shiny::renderUI({
      objects <- object_data_raw()
      value <- if ("section" %in% names(objects)) sum(objects$section == "script", na.rm = TRUE) else 0L
      dashboard_value_box("Scripts", value, "secondary")
    })
    output$reports_box <- shiny::renderUI({
      objects <- object_data_raw()
      value <- if ("section" %in% names(objects)) sum(objects$section == "report", na.rm = TRUE) else 0L
      dashboard_value_box("Reports", value, "secondary")
    })
    output$outputs_box <- shiny::renderUI({
      objects <- object_data_raw()
      value <- if ("section" %in% names(objects)) sum(objects$section == "output", na.rm = TRUE) else 0L
      dashboard_value_box("Outputs", value, "secondary")
    })

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    selected_object <- shiny::reactive({
      dashboard_selected_row(input, "registry_table", object_data())
    })

    shiny::observeEvent(selected_object(), {
      obj <- selected_object()
      if (is.null(obj) || !"name" %in% names(obj)) return(invisible(NULL))
      shiny::updateTextInput(session, "rename_to", value = dashboard_cell(obj, "name"))
    }, ignoreInit = TRUE)

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
          to = dashboard_required_text(input$rename_to, "New object name"),
          section = section,
          root = dashboard_root(state)
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
          root = dashboard_root(state),
          delete_files = FALSE,
          confirm = FALSE,
          dry_run = FALSE
        )
      )
    })
  })
}
