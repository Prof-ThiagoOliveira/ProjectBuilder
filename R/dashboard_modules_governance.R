mod_risks_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::textInput(ns("risk_title"), "Risk title"),
        shiny::textAreaInput(ns("risk_description"), "Description", rows = 4),
        shiny::selectInput(ns("risk_status"), "Status", choices = governance_status_levels("risk"), selected = "open"),
        shiny::textInput(ns("risk_probability"), "Probability"),
        shiny::textInput(ns("risk_impact"), "Impact"),
        shiny::textInput(ns("risk_owner"), "Owner"),
        if (manage) shiny::actionButton(ns("add_risk"), "Add risk"),
        if (manage) shiny::actionButton(ns("mitigate_risk"), "Mark selected risk mitigated"),
        if (manage) shiny::actionButton(ns("remove_risk"), "Remove selected risk")
      ),
      shiny::column(width = 8, dashboard_table_ui(ns("risks_table")))
    )
  )
}

mod_risks_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    risk_data <- shiny::reactive(state$diagnostics$governance$risks)
    dashboard_render_table(output, "risks_table", risk_data)

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$add_risk, {
      dashboard_run_action(
        state = state,
        session = session,
        label = "Risk creation",
        expr = add_project_risk(
          title = input$risk_title,
          root = state$root,
          description = input$risk_description,
          probability = input$risk_probability,
          impact = input$risk_impact,
          owner = input$risk_owner,
          status = input$risk_status
        )
      )
    })

    selected_risk <- shiny::reactive({
      sel <- input$risks_table_rows_selected
      risks <- risk_data()
      if (length(sel) != 1L || nrow(risks) < sel[[1]]) {
        return(NULL)
      }
      risks$id[[sel[[1]]]]
    })

    shiny::observeEvent(input$mitigate_risk, {
      shiny::req(selected_risk())
      dashboard_run_action(state, session, "Risk update", mark_project_risk_mitigated(selected_risk(), root = state$root))
    })

    shiny::observeEvent(input$remove_risk, {
      shiny::req(selected_risk())
      dashboard_run_action(state, session, "Risk removal", remove_project_risk(selected_risk(), root = state$root))
    })
  })
}

mod_milestones_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::textInput(ns("milestone_title"), "Milestone title"),
        shiny::textAreaInput(ns("milestone_description"), "Description", rows = 4),
        shiny::selectInput(ns("milestone_status"), "Status", choices = governance_status_levels("milestone"), selected = "planned"),
        shiny::dateInput(ns("milestone_due_date"), "Due date", value = NULL),
        if (manage) shiny::actionButton(ns("add_milestone"), "Add milestone"),
        if (manage) shiny::actionButton(ns("complete_milestone"), "Mark selected milestone done"),
        if (manage) shiny::actionButton(ns("remove_milestone"), "Remove selected milestone")
      ),
      shiny::column(width = 8, dashboard_table_ui(ns("milestones_table")))
    )
  )
}

mod_milestones_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    milestone_data <- shiny::reactive(state$diagnostics$governance$milestones)
    dashboard_render_table(output, "milestones_table", milestone_data)

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$add_milestone, {
      dashboard_run_action(
        state,
        session,
        "Milestone creation",
        add_project_milestone(
          title = input$milestone_title,
          root = state$root,
          description = input$milestone_description,
          status = input$milestone_status,
          due_date = if (is.null(input$milestone_due_date) || is.na(input$milestone_due_date)) NULL else as.character(input$milestone_due_date)
        )
      )
    })

    selected_milestone <- shiny::reactive({
      sel <- input$milestones_table_rows_selected
      milestones <- milestone_data()
      if (length(sel) != 1L || nrow(milestones) < sel[[1]]) {
        return(NULL)
      }
      milestones$id[[sel[[1]]]]
    })

    shiny::observeEvent(input$complete_milestone, {
      shiny::req(selected_milestone())
      dashboard_run_action(state, session, "Milestone update", mark_project_milestone_done(selected_milestone(), root = state$root))
    })

    shiny::observeEvent(input$remove_milestone, {
      shiny::req(selected_milestone())
      dashboard_run_action(state, session, "Milestone removal", remove_project_milestone(selected_milestone(), root = state$root))
    })
  })
}

mod_decisions_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::textInput(ns("decision_title"), "Decision title"),
        shiny::textAreaInput(ns("decision_text"), "Decision", rows = 4),
        shiny::textAreaInput(ns("decision_rationale"), "Rationale", rows = 3),
        if (manage) shiny::actionButton(ns("add_decision"), "Record decision"),
        if (manage) shiny::actionButton(ns("remove_decision"), "Remove selected decision")
      ),
      shiny::column(width = 8, dashboard_table_ui(ns("decisions_table")))
    )
  )
}

mod_decisions_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    decision_data <- shiny::reactive(state$diagnostics$governance$decisions)
    dashboard_render_table(output, "decisions_table", decision_data)

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$add_decision, {
      dashboard_run_action(
        state,
        session,
        "Decision creation",
        record_project_decision(
          title = input$decision_title,
          decision = input$decision_text,
          rationale = input$decision_rationale,
          root = state$root
        )
      )
    })

    selected_decision <- shiny::reactive({
      sel <- input$decisions_table_rows_selected
      decisions <- decision_data()
      if (length(sel) != 1L || nrow(decisions) < sel[[1]]) {
        return(NULL)
      }
      decisions$id[[sel[[1]]]]
    })

    shiny::observeEvent(input$remove_decision, {
      shiny::req(selected_decision())
      dashboard_run_action(state, session, "Decision removal", remove_project_decision(selected_decision(), root = state$root))
    })
  })
}
