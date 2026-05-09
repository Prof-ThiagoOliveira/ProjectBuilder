mod_add_object_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 5,
        dashboard_form_card(
          "Create or register an object",
          subtitle = "Use the guided fields to choose a valid object type, subtype and location. The dashboard prevents invalid subtype values before the action is run.",
          shiny::selectInput(
            ns("object_type"),
            "Object type",
            choices = c(
              "script", "report", "output", "table", "figure", "app",
              "task", "risk", "milestone", "decision", "data_source"
            )
          ),
          shiny::uiOutput(ns("object_guidance")),
          shiny::textInput(ns("object_name"), "Name or title", placeholder = "e.g. fit_model or final_analysis_report"),
          shiny::uiOutput(ns("object_subtype_ui")),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                ns("path_area"),
                "Location",
                choices = c("Auto", "analysis", "reports", "outputs", "outputs/tables", "outputs/figures", "app", "dashboard", "data", "docs", "references", "Project root", "Custom"),
                selected = "Auto"
              )
            ),
            shiny::column(6, shiny::textInput(ns("path_filename"), "Filename", placeholder = "Leave blank for default"))
          ),
          shiny::textInput(ns("object_path"), "Custom path", placeholder = "Optional explicit relative path or external data-source path"),
          shiny::uiOutput(ns("path_preview")),
          shiny::checkboxInput(ns("object_dry_run"), "Preview only; do not write files", value = TRUE),
          if (manage) shiny::actionButton(ns("create_object"), "Run action", class = "btn-primary") else dashboard_empty_state("Read-only diagnostic mode.")
        )
      ),
      shiny::column(
        width = 7,
        dashboard_card(
          "Action preview",
          subtitle = "Review the interpreted action before clearing the dry-run option.",
          shiny::verbatimTextOutput(ns("preview"))
        ),
        dashboard_card(
          "Creation rules",
          subtitle = "The general Add object form deliberately uses safe defaults. Use specialist tabs for detailed governance edits.",
          shiny::tags$ul(
            class = "projflow-list",
            shiny::tags$li("Script subtype is restricted to the script types accepted by projflow."),
            shiny::tags$li("Report subtype is restricted to html_report, client_report, or scientific_report."),
            shiny::tags$li("Output subtype is restricted to recognised registry object/output types."),
            shiny::tags$li("Paths are used by output, table, figure and data-source actions. Script and report wrappers use standard project locations derived from the object name."),
            shiny::tags$li("Leave Location as Auto unless the project has a specific path convention.")
          )
        )
      )
    )
  )
}

mod_add_object_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_subtype <- shiny::reactive({
      dashboard_object_subtype(input$object_type, input$object_subtype)
    })

    recommended_path <- shiny::reactive({
      dashboard_recommended_path(
        object_type = input$object_type,
        name = input$object_name,
        area = input$path_area,
        filename = input$path_filename,
        custom_path = input$object_path
      )
    })

    output$object_subtype_ui <- shiny::renderUI({
      object_type <- input$object_type %||% "script"
      choices <- dashboard_object_subtype_choices(object_type)
      if (length(choices) == 0L) {
        return(shiny::p(class = "projflow-help", "This object type does not require a subtype in the general creation form."))
      }
      shiny::selectInput(
        session$ns("object_subtype"),
        dashboard_object_subtype_label(object_type),
        choices = choices,
        selected = choices[[1]]
      )
    })

    output$object_guidance <- shiny::renderUI({
      object_type <- input$object_type %||% "script"
      text <- switch(
        object_type,
        script = "Creates and registers an R script. Select a valid script subtype such as data_preparation, statistical_analysis, model, visualisation or export.",
        report = "Creates and registers a Quarto report. Select one of the accepted report types: html_report, client_report or scientific_report.",
        output = "Registers an expected output file. Select the output/registry type, then use the path controls to choose the expected file location.",
        table = "Registers a tabular output. The recommended location is outputs/tables/.",
        figure = "Registers a figure output. The recommended location is outputs/figures/.",
        app = "Creates an application scaffold. Select shiny or quarto_dashboard.",
        task = "Creates a governance task. Use the Task board for priority, status, ownership and linked-object editing.",
        risk = "Creates a risk register item. Use the Risks tab for probability, impact, severity and mitigation plan.",
        milestone = "Creates a milestone. Use the Milestones tab for due dates and completion tracking.",
        decision = "Records a project decision. Use the Decisions tab for rationale, consequences and status edits.",
        data_source = "Registers an external data source. Use Custom path for absolute or external locations.",
        "Select an object type to see guidance."
      )
      shiny::p(class = "projflow-help", text)
    })

    output$path_preview <- shiny::renderUI({
      path <- recommended_path()
      object_type <- input$object_type %||% "script"
      if (!nzchar(path)) {
        return(shiny::p(class = "projflow-help", "No path will be passed for this action unless a custom path is supplied."))
      }
      note <- if (object_type %in% c("script", "report", "app")) {
        "Standard wrappers create these objects in their conventional project locations; the preview is advisory."
      } else {
        "This path will be passed to the action."
      }
      shiny::tagList(
        shiny::p(class = "projflow-help", "Recommended path: ", shiny::code(path)),
        shiny::p(class = "projflow-help", note)
      )
    })

    output$preview <- shiny::renderPrint({
      list(
        project_root = dashboard_root(state),
        object_type = input$object_type,
        name = dashboard_trim_text(input$object_name),
        subtype = selected_subtype(),
        recommended_path = recommended_path(),
        custom_path = dashboard_trim_text(input$object_path),
        dry_run = isTRUE(input$object_dry_run)
      )
    })

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$create_object, {
      dashboard_run_action(
        state = state,
        session = session,
        label = "Object creation",
        expr = {
          root <- dashboard_root(state)
          object_type <- dashboard_required_text(input$object_type, "Object type")
          object_name <- dashboard_trim_text(input$object_name)
          object_subtype <- selected_subtype()
          object_path <- recommended_path()
          dry_run <- isTRUE(input$object_dry_run)

          switch(
            object_type,
            script = new_script(
              name = dashboard_required_text(object_name, "Script name"),
              type = object_subtype,
              root = root,
              dry_run = dry_run,
              open = FALSE
            ),
            report = new_report(
              name = dashboard_required_text(object_name, "Report name"),
              type = object_subtype,
              root = root,
              dry_run = dry_run,
              open = FALSE
            ),
            output = new_output(
              name = dashboard_required_text(object_name, "Output name"),
              type = object_subtype,
              path = if (nzchar(object_path)) object_path else NULL,
              root = root,
              dry_run = dry_run
            ),
            table = new_table(
              name = dashboard_required_text(object_name, "Table name"),
              path = if (nzchar(object_path)) object_path else NULL,
              root = root,
              dry_run = dry_run
            ),
            figure = new_figure(
              name = dashboard_required_text(object_name, "Figure name"),
              path = if (nzchar(object_path)) object_path else NULL,
              root = root,
              dry_run = dry_run
            ),
            app = new_app(
              name = if (nzchar(object_name)) object_name else "app",
              type = object_subtype,
              root = root,
              dry_run = dry_run,
              open = FALSE
            ),
            task = add_project_task(
              title = dashboard_required_text(object_name, "Task title"),
              root = root
            ),
            risk = add_project_risk(
              title = dashboard_required_text(object_name, "Risk title"),
              root = root
            ),
            milestone = add_project_milestone(
              title = dashboard_required_text(object_name, "Milestone title"),
              root = root
            ),
            decision = record_project_decision(
              title = dashboard_required_text(object_name, "Decision title"),
              decision = object_name,
              root = root
            ),
            data_source = set_project_data_root(
              path = dashboard_required_text(object_path, "Data-source path"),
              name = if (nzchar(object_name)) object_name else "default",
              root = root
            ),
            rlang::abort(paste0("Unsupported object type: ", object_type))
          )
        }
      )
    })
  })
}
