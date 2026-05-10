mod_outputs_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dashboard_card(
      "Outputs",
      subtitle = "Monitor expected deliverables and identify missing or stale files before reporting.",
      shiny::fluidRow(
        shiny::column(4, shiny::uiOutput(ns("registered_box"))),
        shiny::column(4, shiny::uiOutput(ns("missing_box"))),
        shiny::column(4, shiny::uiOutput(ns("stale_box")))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        dashboard_form_card(
          "Register output",
          subtitle = "Add a new expected project output to the registry.",
          shiny::textInput(ns("output_name"), "Output name", placeholder = "e.g. model_fit"),
          shiny::textInput(ns("output_type"), "Type", value = "output", placeholder = "table, figure, model, output"),
          shiny::textInput(ns("output_path"), "Path", placeholder = "outputs/..."),
          if (manage) shiny::actionButton(ns("register_output"), "Register output", class = "btn-primary") else dashboard_empty_state("Read-only diagnostic mode.")
        ),
        dashboard_card(
          "Output charts",
          subtitle = "Deliverable state and object type distribution.",
          shiny::plotOutput(ns("output_exists_plot"), height = "210px"),
          shiny::plotOutput(ns("output_type_plot"), height = "210px")
        ),
        dashboard_card(
          "Build status",
          subtitle = "Lists outputs that should be regenerated or created.",
          shiny::verbatimTextOutput(ns("output_summary"))
        ),
        dashboard_card(
          "Output organisation",
          subtitle = "Move registered outputs to the canonical typed layout and clean duplicated Quarto report artefacts.",
          shiny::div(
            class = "projflow-actions",
            if (manage) shiny::actionButton(ns("preview_output_organisation"), "Preview organisation", class = "btn-outline-secondary"),
            if (manage) shiny::actionButton(ns("apply_output_organisation"), "Apply organisation", class = "btn-outline-primary") else dashboard_empty_state("Read-only diagnostic mode.")
          ),
          shiny::verbatimTextOutput(ns("output_organisation_preview"))
        )
      ),
      shiny::column(
        width = 8,
        dashboard_card(
          "Registered outputs",
          subtitle = "Select a row to edit or remove it from the registry. File deletion is not performed here.",
          dashboard_table_ui(ns("outputs_table"))
        ),
        dashboard_form_card(
          "Selected output editor",
          subtitle = "Update the selected output path or type without editing the project registry manually.",
          shiny::uiOutput(ns("selected_output_label")),
          shiny::fluidRow(
            shiny::column(6, shiny::textInput(ns("edit_output_type"), "Type")),
            shiny::column(6, shiny::textInput(ns("edit_output_path"), "Path"))
          ),
          shiny::div(
            class = "projflow-actions",
            if (manage) shiny::actionButton(ns("update_output"), "Save output changes", class = "btn-primary"),
            if (manage) shiny::actionButton(ns("remove_output"), "Remove output", class = "btn-outline-danger") else dashboard_empty_state("Read-only diagnostic mode.")
          )
        )
      )
    )
  )
}

mod_outputs_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    output_data <- shiny::reactive({
      dashboard_safe_data_frame(dashboard_diagnostics(state)$outputs)
    })

    dashboard_render_table(output, "outputs_table", output_data)

    output$registered_box <- shiny::renderUI({
      dashboard_value_box("Registered", nrow(output_data()), "secondary", note = "Outputs tracked in the registry.")
    })
    output$missing_box <- shiny::renderUI({
      value <- dashboard_summary_value(state, "missing_outputs", 0L)
      dashboard_value_box("Missing", value, if (as.integer(value) > 0L) "danger" else "success", note = "Expected files not present.")
    })
    output$stale_box <- shiny::renderUI({
      value <- dashboard_summary_value(state, "stale_outputs", 0L)
      dashboard_value_box("Stale", value, if (as.integer(value) > 0L) "warning" else "success", note = "Older than upstream inputs.")
    })

    output$output_exists_plot <- shiny::renderPlot({
      data <- output_data()
      if ("exists" %in% names(data)) {
        data$exists <- as.character(data$exists)
      }
      dashboard_plot_categorical_counts(data, "exists", title = "Outputs by file existence")
    })
    output$output_type_plot <- shiny::renderPlot({
      dashboard_plot_categorical_counts(output_data(), "type", title = "Outputs by type")
    })

    output$output_summary <- shiny::renderPrint({
      state$refresh_token
      list(
        missing_outputs = missing_project_outputs(dashboard_root(state)),
        stale_outputs = stale_project_outputs(dashboard_root(state))
      )
    })

    output$output_organisation_preview <- shiny::renderPrint(invisible(NULL))

    selected_output_row <- shiny::reactive({
      dashboard_selected_row(input, "outputs_table", output_data())
    })

    selected_output <- shiny::reactive({
      output_row <- selected_output_row()
      if (is.null(output_row) || !"name" %in% names(output_row)) {
        return(NULL)
      }
      output_row$name[[1]]
    })

    output$selected_output_label <- shiny::renderUI({
      output_row <- selected_output_row()
      if (is.null(output_row)) {
        return(dashboard_empty_state("Select an output row to edit its type or path."))
      }
      shiny::div(class = "projflow-selected-record", shiny::strong("Selected output: "), output_row$name[[1]])
    })

    shiny::observeEvent(selected_output_row(), {
      output_row <- selected_output_row()
      if (is.null(output_row)) return(invisible(NULL))
      shiny::updateTextInput(session, "edit_output_type", value = dashboard_cell(output_row, "type", "output"))
      shiny::updateTextInput(session, "edit_output_path", value = dashboard_cell(output_row, "output"))
    }, ignoreInit = TRUE)

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$preview_output_organisation, {
      output$output_organisation_preview <- shiny::renderPrint({
        organise_project_outputs(root = dashboard_root(state), dry_run = TRUE)
      })
    })

    shiny::observeEvent(input$apply_output_organisation, {
      dashboard_run_action(
        state = state,
        session = session,
        label = "Output organisation",
        expr = organise_project_outputs(root = dashboard_root(state), dry_run = FALSE)
      )
    })

    shiny::observeEvent(input$register_output, {
      dashboard_run_action(
        state = state,
        session = session,
        label = "Output registration",
        expr = new_project_output(
          name = dashboard_required_text(input$output_name, "Output name"),
          type = dashboard_required_text(input$output_type, "Output type"),
          path = dashboard_optional_text(input$output_path),
          root = dashboard_root(state)
        )
      )
    })

    shiny::observeEvent(input$update_output, {
      shiny::req(selected_output())
      dashboard_run_action(
        state = state,
        session = session,
        label = "Output update",
        expr = update_project_output(
          name = selected_output(),
          type = dashboard_required_text(input$edit_output_type, "Output type"),
          path = dashboard_required_text(input$edit_output_path, "Output path"),
          root = dashboard_root(state)
        )
      )
    })

    shiny::observeEvent(input$remove_output, {
      shiny::req(selected_output())
      dashboard_run_action(
        state = state,
        session = session,
        label = "Output removal",
        expr = remove_project_output(selected_output(), root = dashboard_root(state))
      )
    })
  })
}
