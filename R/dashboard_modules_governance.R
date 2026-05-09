mod_risks_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dashboard_card(
      "Risks",
      subtitle = "Maintain a living risk register with probability, impact, mitigation, owner and status.",
      shiny::fluidRow(
        shiny::column(3, shiny::uiOutput(ns("open_box"))),
        shiny::column(3, shiny::uiOutput(ns("mitigating_box"))),
        shiny::column(3, shiny::uiOutput(ns("mitigated_box"))),
        shiny::column(3, shiny::uiOutput(ns("closed_box")))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        dashboard_form_card(
          "Add risk",
          subtitle = "Record risks early so mitigation can be assigned and reviewed.",
          shiny::textInput(ns("risk_title"), "Risk title"),
          shiny::textAreaInput(ns("risk_description"), "Description", rows = 3),
          shiny::selectInput(ns("risk_status"), "Status", choices = governance_status_levels("risk"), selected = "open"),
          shiny::selectInput(ns("risk_probability"), "Probability", choices = c("", "low", "medium", "high", "critical"), selected = ""),
          shiny::selectInput(ns("risk_impact"), "Impact", choices = c("", "low", "medium", "high", "critical"), selected = ""),
          shiny::textInput(ns("risk_mitigation"), "Mitigation plan"),
          shiny::textInput(ns("risk_owner"), "Owner"),
          shiny::dateInput(ns("risk_due_date"), "Review / mitigation due date", value = NULL),
          shiny::selectizeInput(ns("risk_linked_objects"), "Linked objects", choices = NULL, multiple = TRUE, options = list(placeholder = "Search project objects")),
          if (manage) shiny::actionButton(ns("add_risk"), "Add risk", class = "btn-primary") else dashboard_empty_state("Read-only diagnostic mode.")
        ),
        dashboard_card(
          "Risk charts",
          subtitle = "Distribution of risk status and severity.",
          shiny::plotOutput(ns("risk_status_plot"), height = "220px"),
          shiny::plotOutput(ns("risk_severity_plot"), height = "220px")
        )
      ),
      shiny::column(
        width = 8,
        dashboard_card(
          "Risk register",
          subtitle = "Select a row to edit risk metadata or close the risk.",
          dashboard_table_ui(ns("risks_table"))
        ),
        dashboard_form_card(
          "Selected risk editor",
          subtitle = "Update the selected risk without editing the governance YAML manually.",
          shiny::uiOutput(ns("selected_risk_label")),
          shiny::fluidRow(
            shiny::column(6, shiny::textInput(ns("edit_risk_title"), "Risk title")),
            shiny::column(3, shiny::selectInput(ns("edit_risk_status"), "Status", choices = governance_status_levels("risk"))),
            shiny::column(3, shiny::selectInput(ns("edit_risk_severity"), "Severity", choices = c("", "low", "medium", "high", "critical")))
          ),
          shiny::fluidRow(
            shiny::column(3, shiny::selectInput(ns("edit_risk_probability"), "Probability", choices = c("", "low", "medium", "high", "critical"))),
            shiny::column(3, shiny::selectInput(ns("edit_risk_impact"), "Impact", choices = c("", "low", "medium", "high", "critical"))),
            shiny::column(3, shiny::textInput(ns("edit_risk_owner"), "Owner")),
            shiny::column(3, shiny::dateInput(ns("edit_risk_due_date"), "Due date", value = NULL))
          ),
          shiny::selectizeInput(ns("edit_risk_linked_objects"), "Linked objects", choices = NULL, multiple = TRUE, options = list(placeholder = "Search project objects")),
          shiny::textAreaInput(ns("edit_risk_mitigation"), "Mitigation plan", rows = 2),
          shiny::textAreaInput(ns("edit_risk_description"), "Description", rows = 3),
          shiny::div(
            class = "projflow-actions",
            if (manage) shiny::actionButton(ns("update_risk"), "Save risk changes", class = "btn-primary"),
            if (manage) shiny::actionButton(ns("mitigate_risk"), "Mark mitigated", class = "btn-outline-success"),
            if (manage) shiny::actionButton(ns("close_risk"), "Close risk", class = "btn-outline-secondary"),
            if (manage) shiny::actionButton(ns("remove_risk"), "Remove risk", class = "btn-outline-danger") else dashboard_empty_state("Read-only diagnostic mode.")
          )
        )
      )
    )
  )
}

mod_risks_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    risk_data <- shiny::reactive(dashboard_safe_data_frame(dashboard_diagnostics(state)$governance$risks))
    linked_object_choices <- shiny::reactive(dashboard_linked_object_choices(state))
    shiny::observeEvent(linked_object_choices(), {
      choices <- linked_object_choices()
      dashboard_update_linked_objects(session, "risk_linked_objects", choices, shiny::isolate(input$risk_linked_objects))
      dashboard_update_linked_objects(session, "edit_risk_linked_objects", choices, shiny::isolate(input$edit_risk_linked_objects))
    }, ignoreInit = FALSE)
    dashboard_render_table(output, "risks_table", risk_data)

    risk_count <- function(status) {
      risks <- risk_data()
      if (!"status" %in% names(risks)) 0L else sum(risks$status == status, na.rm = TRUE)
    }
    output$open_box <- shiny::renderUI(dashboard_value_box("Open", risk_count("open"), if (risk_count("open") > 0L) "warning" else "success"))
    output$mitigating_box <- shiny::renderUI(dashboard_value_box("Mitigating", risk_count("mitigating"), "primary"))
    output$mitigated_box <- shiny::renderUI(dashboard_value_box("Mitigated", risk_count("mitigated"), "success"))
    output$closed_box <- shiny::renderUI(dashboard_value_box("Closed", risk_count("closed"), "secondary"))

    output$risk_status_plot <- shiny::renderPlot({
      dashboard_plot_categorical_counts(risk_data(), "status", title = "Risks by status")
    })
    output$risk_severity_plot <- shiny::renderPlot({
      dashboard_plot_categorical_counts(risk_data(), "severity", title = "Risks by severity")
    })

    selected_risk_row <- shiny::reactive({
      dashboard_selected_row(input, "risks_table", risk_data())
    })
    selected_risk <- shiny::reactive({
      risk <- selected_risk_row()
      if (is.null(risk) || !"id" %in% names(risk)) return(NULL)
      risk$id[[1]]
    })

    output$selected_risk_label <- shiny::renderUI({
      risk <- selected_risk_row()
      if (is.null(risk)) {
        return(dashboard_empty_state("Select a risk row to edit status, severity, mitigation and ownership."))
      }
      shiny::div(class = "projflow-selected-record", shiny::strong("Selected risk: "), risk$id[[1]], " - ", risk$title[[1]])
    })

    shiny::observeEvent(selected_risk_row(), {
      risk <- selected_risk_row()
      if (is.null(risk)) return(invisible(NULL))
      shiny::updateTextInput(session, "edit_risk_title", value = dashboard_cell(risk, "title"))
      shiny::updateSelectInput(session, "edit_risk_status", selected = dashboard_choice_or_default(dashboard_cell(risk, "status"), governance_status_levels("risk"), "open"))
      shiny::updateSelectInput(session, "edit_risk_probability", selected = dashboard_choice_or_default(dashboard_cell(risk, "probability"), c("", "low", "medium", "high", "critical"), ""))
      shiny::updateSelectInput(session, "edit_risk_impact", selected = dashboard_choice_or_default(dashboard_cell(risk, "impact"), c("", "low", "medium", "high", "critical"), ""))
      shiny::updateSelectInput(session, "edit_risk_severity", selected = dashboard_choice_or_default(dashboard_cell(risk, "severity"), c("", "low", "medium", "high", "critical"), ""))
      shiny::updateTextInput(session, "edit_risk_owner", value = dashboard_cell(risk, "owner"))
      shiny::updateDateInput(session, "edit_risk_due_date", value = dashboard_date_or_null(dashboard_cell(risk, "due_date")))
      dashboard_update_linked_objects(session, "edit_risk_linked_objects", linked_object_choices(), dashboard_cell(risk, "linked_objects"))
      shiny::updateTextAreaInput(session, "edit_risk_mitigation", value = dashboard_cell(risk, "mitigation"))
      shiny::updateTextAreaInput(session, "edit_risk_description", value = dashboard_cell(risk, "description"))
    }, ignoreInit = TRUE)

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$add_risk, {
      dashboard_run_action(
        state = state,
        session = session,
        label = "Risk creation",
        expr = add_project_risk(
          title = dashboard_required_text(input$risk_title, "Risk title"),
          root = dashboard_root(state),
          description = dashboard_optional_text(input$risk_description),
          probability = dashboard_optional_text(input$risk_probability),
          impact = dashboard_optional_text(input$risk_impact),
          mitigation = dashboard_optional_text(input$risk_mitigation),
          owner = dashboard_optional_text(input$risk_owner),
          due_date = dashboard_optional_date(input$risk_due_date),
          linked_objects = dashboard_split_values(input$risk_linked_objects),
          status = input$risk_status
        )
      )
    })

    shiny::observeEvent(input$update_risk, {
      shiny::req(selected_risk())
      dashboard_run_action(
        state,
        session,
        "Risk update",
        update_project_risk(
          risk = selected_risk(),
          root = dashboard_root(state),
          title = dashboard_required_text(input$edit_risk_title, "Risk title"),
          description = input$edit_risk_description %||% "",
          probability = input$edit_risk_probability %||% "",
          impact = input$edit_risk_impact %||% "",
          severity = input$edit_risk_severity %||% "",
          status = input$edit_risk_status,
          mitigation = input$edit_risk_mitigation %||% "",
          owner = input$edit_risk_owner %||% "",
          due_date = dashboard_date_update_value(input$edit_risk_due_date),
          linked_objects = dashboard_split_values(input$edit_risk_linked_objects)
        )
      )
    })

    shiny::observeEvent(input$mitigate_risk, {
      shiny::req(selected_risk())
      dashboard_run_action(state, session, "Risk update", mark_project_risk_mitigated(selected_risk(), root = dashboard_root(state)))
    })

    shiny::observeEvent(input$close_risk, {
      shiny::req(selected_risk())
      dashboard_run_action(state, session, "Risk closure", close_project_risk(selected_risk(), root = dashboard_root(state)))
    })

    shiny::observeEvent(input$remove_risk, {
      shiny::req(selected_risk())
      dashboard_run_action(state, session, "Risk removal", remove_project_risk(selected_risk(), root = dashboard_root(state)))
    })
  })
}

mod_milestones_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dashboard_card(
      "Milestones",
      subtitle = "Track planned delivery points and completion status.",
      shiny::fluidRow(
        shiny::column(3, shiny::uiOutput(ns("planned_box"))),
        shiny::column(3, shiny::uiOutput(ns("progress_box"))),
        shiny::column(3, shiny::uiOutput(ns("done_box"))),
        shiny::column(3, shiny::uiOutput(ns("delayed_box")))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        dashboard_form_card(
          "Add milestone",
          subtitle = "Record major delivery or review points.",
          shiny::textInput(ns("milestone_title"), "Milestone title"),
          shiny::textAreaInput(ns("milestone_description"), "Description", rows = 3),
          shiny::selectInput(ns("milestone_status"), "Status", choices = governance_status_levels("milestone"), selected = "planned"),
          shiny::dateInput(ns("milestone_due_date"), "Due date", value = NULL),
          shiny::selectizeInput(ns("milestone_linked_objects"), "Linked objects", choices = NULL, multiple = TRUE, options = list(placeholder = "Search project objects")),
          if (manage) shiny::actionButton(ns("add_milestone"), "Add milestone", class = "btn-primary") else dashboard_empty_state("Read-only diagnostic mode.")
        ),
        dashboard_card(
          "Milestone chart",
          subtitle = "Delivery status distribution.",
          shiny::plotOutput(ns("milestone_status_plot"), height = "250px")
        )
      ),
      shiny::column(
        width = 8,
        dashboard_card(
          "Milestone register",
          subtitle = "Select a row to edit delivery status, due date and linked objects.",
          dashboard_table_ui(ns("milestones_table"))
        ),
        dashboard_form_card(
          "Selected milestone editor",
          subtitle = "Update milestone metadata from the dashboard.",
          shiny::uiOutput(ns("selected_milestone_label")),
          shiny::fluidRow(
            shiny::column(6, shiny::textInput(ns("edit_milestone_title"), "Milestone title")),
            shiny::column(3, shiny::selectInput(ns("edit_milestone_status"), "Status", choices = governance_status_levels("milestone"))),
            shiny::column(3, shiny::dateInput(ns("edit_milestone_due_date"), "Due date", value = NULL))
          ),
          shiny::selectizeInput(ns("edit_milestone_linked_objects"), "Linked objects", choices = NULL, multiple = TRUE, options = list(placeholder = "Search project objects")),
          shiny::textAreaInput(ns("edit_milestone_description"), "Description", rows = 3),
          shiny::div(
            class = "projflow-actions",
            if (manage) shiny::actionButton(ns("update_milestone"), "Save milestone changes", class = "btn-primary"),
            if (manage) shiny::actionButton(ns("complete_milestone"), "Mark done", class = "btn-outline-success"),
            if (manage) shiny::actionButton(ns("remove_milestone"), "Remove milestone", class = "btn-outline-danger") else dashboard_empty_state("Read-only diagnostic mode.")
          )
        )
      )
    )
  )
}

mod_milestones_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    milestone_data <- shiny::reactive(dashboard_safe_data_frame(dashboard_diagnostics(state)$governance$milestones))
    linked_object_choices <- shiny::reactive(dashboard_linked_object_choices(state))
    shiny::observeEvent(linked_object_choices(), {
      choices <- linked_object_choices()
      dashboard_update_linked_objects(session, "milestone_linked_objects", choices, shiny::isolate(input$milestone_linked_objects))
      dashboard_update_linked_objects(session, "edit_milestone_linked_objects", choices, shiny::isolate(input$edit_milestone_linked_objects))
    }, ignoreInit = FALSE)
    dashboard_render_table(output, "milestones_table", milestone_data)

    milestone_count <- function(status) {
      milestones <- milestone_data()
      if (!"status" %in% names(milestones)) 0L else sum(milestones$status == status, na.rm = TRUE)
    }
    output$planned_box <- shiny::renderUI(dashboard_value_box("Planned", milestone_count("planned"), "secondary"))
    output$progress_box <- shiny::renderUI(dashboard_value_box("In progress", milestone_count("in_progress"), "primary"))
    output$done_box <- shiny::renderUI(dashboard_value_box("Done", milestone_count("done"), "success"))
    output$delayed_box <- shiny::renderUI(dashboard_value_box("Delayed", milestone_count("delayed"), if (milestone_count("delayed") > 0L) "warning" else "success"))

    output$milestone_status_plot <- shiny::renderPlot({
      dashboard_plot_categorical_counts(milestone_data(), "status", title = "Milestones by status")
    })

    selected_milestone_row <- shiny::reactive({
      dashboard_selected_row(input, "milestones_table", milestone_data())
    })
    selected_milestone <- shiny::reactive({
      milestone <- selected_milestone_row()
      if (is.null(milestone) || !"id" %in% names(milestone)) return(NULL)
      milestone$id[[1]]
    })

    output$selected_milestone_label <- shiny::renderUI({
      milestone <- selected_milestone_row()
      if (is.null(milestone)) {
        return(dashboard_empty_state("Select a milestone row to edit status, due date and description."))
      }
      shiny::div(class = "projflow-selected-record", shiny::strong("Selected milestone: "), milestone$id[[1]], " - ", milestone$title[[1]])
    })

    shiny::observeEvent(selected_milestone_row(), {
      milestone <- selected_milestone_row()
      if (is.null(milestone)) return(invisible(NULL))
      shiny::updateTextInput(session, "edit_milestone_title", value = dashboard_cell(milestone, "title"))
      shiny::updateSelectInput(session, "edit_milestone_status", selected = dashboard_choice_or_default(dashboard_cell(milestone, "status"), governance_status_levels("milestone"), "planned"))
      shiny::updateDateInput(session, "edit_milestone_due_date", value = dashboard_date_or_null(dashboard_cell(milestone, "due_date")))
      dashboard_update_linked_objects(session, "edit_milestone_linked_objects", linked_object_choices(), dashboard_cell(milestone, "linked_objects"))
      shiny::updateTextAreaInput(session, "edit_milestone_description", value = dashboard_cell(milestone, "description"))
    }, ignoreInit = TRUE)

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$add_milestone, {
      dashboard_run_action(
        state,
        session,
        "Milestone creation",
        add_project_milestone(
          title = dashboard_required_text(input$milestone_title, "Milestone title"),
          root = dashboard_root(state),
          description = dashboard_optional_text(input$milestone_description),
          status = input$milestone_status,
          due_date = dashboard_optional_date(input$milestone_due_date),
          linked_objects = dashboard_split_values(input$milestone_linked_objects)
        )
      )
    })

    shiny::observeEvent(input$update_milestone, {
      shiny::req(selected_milestone())
      dashboard_run_action(
        state,
        session,
        "Milestone update",
        update_project_milestone(
          milestone = selected_milestone(),
          root = dashboard_root(state),
          title = dashboard_required_text(input$edit_milestone_title, "Milestone title"),
          description = input$edit_milestone_description %||% "",
          status = input$edit_milestone_status,
          due_date = dashboard_date_update_value(input$edit_milestone_due_date),
          linked_objects = dashboard_split_values(input$edit_milestone_linked_objects)
        )
      )
    })

    shiny::observeEvent(input$complete_milestone, {
      shiny::req(selected_milestone())
      dashboard_run_action(state, session, "Milestone update", mark_project_milestone_done(selected_milestone(), root = dashboard_root(state)))
    })

    shiny::observeEvent(input$remove_milestone, {
      shiny::req(selected_milestone())
      dashboard_run_action(state, session, "Milestone removal", remove_project_milestone(selected_milestone(), root = dashboard_root(state)))
    })
  })
}

mod_decisions_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dashboard_card(
      "Decisions",
      subtitle = "Record analytical, data-management and reporting decisions with rationale and consequences.",
      shiny::fluidRow(
        shiny::column(4, shiny::uiOutput(ns("total_box"))),
        shiny::column(4, shiny::uiOutput(ns("active_box"))),
        shiny::column(4, shiny::uiOutput(ns("superseded_box")))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        dashboard_form_card(
          "Record decision",
          subtitle = "Document why the project changed, not only what changed.",
          shiny::textInput(ns("decision_title"), "Decision title"),
          shiny::textAreaInput(ns("decision_text"), "Decision", rows = 3),
          shiny::textAreaInput(ns("decision_rationale"), "Rationale", rows = 3),
          shiny::textAreaInput(ns("decision_consequences"), "Consequences", rows = 2),
          shiny::selectInput(ns("decision_status"), "Status", choices = governance_status_levels("decision"), selected = "active"),
          shiny::selectizeInput(ns("decision_linked_objects"), "Linked objects", choices = NULL, multiple = TRUE, options = list(placeholder = "Search project objects")),
          if (manage) shiny::actionButton(ns("add_decision"), "Record decision", class = "btn-primary") else dashboard_empty_state("Read-only diagnostic mode.")
        ),
        dashboard_card(
          "Decision chart",
          subtitle = "Decision status distribution.",
          shiny::plotOutput(ns("decision_status_plot"), height = "250px")
        )
      ),
      shiny::column(
        width = 8,
        dashboard_card(
          "Decision register",
          subtitle = "Select a row to edit status, rationale, consequences or linked objects.",
          dashboard_table_ui(ns("decisions_table"))
        ),
        dashboard_form_card(
          "Selected decision editor",
          subtitle = "Update decision metadata while preserving the audit trail.",
          shiny::uiOutput(ns("selected_decision_label")),
          shiny::fluidRow(
            shiny::column(8, shiny::textInput(ns("edit_decision_title"), "Decision title")),
            shiny::column(4, shiny::selectInput(ns("edit_decision_status"), "Status", choices = governance_status_levels("decision")))
          ),
          shiny::textAreaInput(ns("edit_decision_text"), "Decision", rows = 3),
          shiny::textAreaInput(ns("edit_decision_rationale"), "Rationale", rows = 3),
          shiny::textAreaInput(ns("edit_decision_consequences"), "Consequences", rows = 2),
          shiny::selectizeInput(ns("edit_decision_linked_objects"), "Linked objects", choices = NULL, multiple = TRUE, options = list(placeholder = "Search project objects")),
          shiny::div(
            class = "projflow-actions",
            if (manage) shiny::actionButton(ns("update_decision"), "Save decision changes", class = "btn-primary"),
            if (manage) shiny::actionButton(ns("remove_decision"), "Remove decision", class = "btn-outline-danger") else dashboard_empty_state("Read-only diagnostic mode.")
          )
        )
      )
    )
  )
}

mod_decisions_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    decision_data <- shiny::reactive(dashboard_safe_data_frame(dashboard_diagnostics(state)$governance$decisions))
    linked_object_choices <- shiny::reactive(dashboard_linked_object_choices(state))
    shiny::observeEvent(linked_object_choices(), {
      choices <- linked_object_choices()
      dashboard_update_linked_objects(session, "decision_linked_objects", choices, shiny::isolate(input$decision_linked_objects))
      dashboard_update_linked_objects(session, "edit_decision_linked_objects", choices, shiny::isolate(input$edit_decision_linked_objects))
    }, ignoreInit = FALSE)
    dashboard_render_table(output, "decisions_table", decision_data)

    decision_count <- function(status = NULL) {
      decisions <- decision_data()
      if (is.null(status)) return(nrow(decisions))
      if (!"status" %in% names(decisions)) 0L else sum(decisions$status == status, na.rm = TRUE)
    }
    output$total_box <- shiny::renderUI(dashboard_value_box("Recorded", decision_count(), "secondary"))
    output$active_box <- shiny::renderUI(dashboard_value_box("Active", decision_count("active"), "primary"))
    output$superseded_box <- shiny::renderUI(dashboard_value_box("Superseded", decision_count("superseded"), "secondary"))
    output$decision_status_plot <- shiny::renderPlot({
      dashboard_plot_categorical_counts(decision_data(), "status", title = "Decisions by status")
    })

    selected_decision_row <- shiny::reactive({
      dashboard_selected_row(input, "decisions_table", decision_data())
    })
    selected_decision <- shiny::reactive({
      decision <- selected_decision_row()
      if (is.null(decision) || !"id" %in% names(decision)) return(NULL)
      decision$id[[1]]
    })

    output$selected_decision_label <- shiny::renderUI({
      decision <- selected_decision_row()
      if (is.null(decision)) {
        return(dashboard_empty_state("Select a decision row to edit status, rationale and consequences."))
      }
      shiny::div(class = "projflow-selected-record", shiny::strong("Selected decision: "), decision$id[[1]], " - ", decision$title[[1]])
    })

    shiny::observeEvent(selected_decision_row(), {
      decision <- selected_decision_row()
      if (is.null(decision)) return(invisible(NULL))
      shiny::updateTextInput(session, "edit_decision_title", value = dashboard_cell(decision, "title"))
      shiny::updateTextAreaInput(session, "edit_decision_text", value = dashboard_cell(decision, "decision"))
      shiny::updateTextAreaInput(session, "edit_decision_rationale", value = dashboard_cell(decision, "rationale"))
      shiny::updateTextAreaInput(session, "edit_decision_consequences", value = dashboard_cell(decision, "consequences"))
      shiny::updateSelectInput(session, "edit_decision_status", selected = dashboard_choice_or_default(dashboard_cell(decision, "status"), governance_status_levels("decision"), "active"))
      dashboard_update_linked_objects(session, "edit_decision_linked_objects", linked_object_choices(), dashboard_cell(decision, "linked_objects"))
    }, ignoreInit = TRUE)

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$add_decision, {
      dashboard_run_action(
        state,
        session,
        "Decision creation",
        record_project_decision(
          title = dashboard_required_text(input$decision_title, "Decision title"),
          decision = dashboard_required_text(input$decision_text, "Decision"),
          rationale = dashboard_optional_text(input$decision_rationale),
          consequences = dashboard_optional_text(input$decision_consequences),
          linked_objects = dashboard_split_values(input$decision_linked_objects),
          status = input$decision_status,
          root = dashboard_root(state)
        )
      )
    })

    shiny::observeEvent(input$update_decision, {
      shiny::req(selected_decision())
      dashboard_run_action(
        state,
        session,
        "Decision update",
        update_project_decision(
          decision_id = selected_decision(),
          root = dashboard_root(state),
          title = dashboard_required_text(input$edit_decision_title, "Decision title"),
          decision = dashboard_required_text(input$edit_decision_text, "Decision"),
          rationale = input$edit_decision_rationale %||% "",
          consequences = input$edit_decision_consequences %||% "",
          linked_objects = dashboard_split_values(input$edit_decision_linked_objects),
          status = input$edit_decision_status
        )
      )
    })

    shiny::observeEvent(input$remove_decision, {
      shiny::req(selected_decision())
      dashboard_run_action(state, session, "Decision removal", remove_project_decision(selected_decision(), root = dashboard_root(state)))
    })
  })
}
