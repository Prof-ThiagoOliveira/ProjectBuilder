mod_overview_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dashboard_card(
      "Executive overview",
      subtitle = "High-level project health, governance load, deliverable status and priority actions.",
      shiny::fluidRow(
        shiny::column(3, shiny::uiOutput(ns("health_box"))),
        shiny::column(3, shiny::uiOutput(ns("tasks_box"))),
        shiny::column(3, shiny::uiOutput(ns("outputs_box"))),
        shiny::column(3, shiny::uiOutput(ns("checks_box")))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 5,
        shiny::uiOutput(ns("project_profile")),
        shiny::uiOutput(ns("quick_actions"))
      ),
      shiny::column(
        width = 7,
        dashboard_card(
          "Priority attention list",
          subtitle = "Items that most affect project reliability, reproducibility or delivery.",
          dashboard_table_ui(ns("attention_table"))
        ),
        dashboard_card(
          "Recent activity",
          subtitle = "Latest governance and registry actions recorded for this project.",
          dashboard_table_ui(ns("recent_activity_table"))
        )
      )
    ),
    dashboard_card(
      "Operational coverage",
      subtitle = "Counts of major objects and controls currently registered in the project.",
      shiny::fluidRow(
        shiny::column(2, shiny::uiOutput(ns("scripts_box"))),
        shiny::column(2, shiny::uiOutput(ns("reports_box"))),
        shiny::column(2, shiny::uiOutput(ns("registered_outputs_box"))),
        shiny::column(2, shiny::uiOutput(ns("risks_box"))),
        shiny::column(2, shiny::uiOutput(ns("packages_box"))),
        shiny::column(2, shiny::uiOutput(ns("data_box")))
      )
    )
  )
}

mod_overview_server <- function(id, state, parent_session, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    diagnostics <- shiny::reactive(dashboard_diagnostics(state))

    summary_value <- function(name, default = 0L) {
      diagnostics_now <- diagnostics()
      dashboard_scalar(diagnostics_now$summary[[name]], default)
    }

    count_rows <- function(name) {
      nrow(dashboard_safe_data_frame(diagnostics()[[name]]))
    }

    output$health_box <- shiny::renderUI({
      status <- as.character(summary_value("overall_status", "Unknown"))
      dashboard_value_box(
        "Overall health",
        status,
        dashboard_status_theme(status),
        note = "Status from checks, registry and required project files."
      )
    })

    output$tasks_box <- shiny::renderUI({
      open <- summary_value("open_tasks", 0L)
      overdue <- summary_value("overdue_tasks", 0L)
      theme <- if (as.integer(overdue) > 0L) "warning" else "primary"
      dashboard_value_box("Open / overdue tasks", paste(open, "/", overdue), theme, note = "Workload requiring action.")
    })

    output$outputs_box <- shiny::renderUI({
      missing <- summary_value("missing_outputs", 0L)
      stale <- summary_value("stale_outputs", 0L)
      theme <- if (as.integer(missing) > 0L) "danger" else if (as.integer(stale) > 0L) "warning" else "success"
      dashboard_value_box("Missing / stale outputs", paste(missing, "/", stale), theme, note = "Reproducibility and build freshness.")
    })

    output$checks_box <- shiny::renderUI({
      counts <- dashboard_issue_counts(diagnostics())
      theme <- if (counts[["errors"]] > 0L) "danger" else if (counts[["warnings"]] > 0L) "warning" else "success"
      dashboard_value_box("Errors / warnings", paste(counts[["errors"]], "/", counts[["warnings"]]), theme, note = paste(counts[["suggestions"]], "suggestion(s)."))
    })

    output$scripts_box <- shiny::renderUI({
      dashboard_value_box("Scripts", count_rows("scripts"), "secondary", note = "Registered scripts.")
    })

    output$reports_box <- shiny::renderUI({
      dashboard_value_box("Reports", count_rows("reports"), "secondary", note = paste(summary_value("reports_needing_render", 0L), "need rendering."))
    })

    output$registered_outputs_box <- shiny::renderUI({
      dashboard_value_box("Outputs", count_rows("outputs"), "secondary", note = "Registered deliverable files.")
    })

    output$risks_box <- shiny::renderUI({
      open <- summary_value("open_risks", 0L)
      dashboard_value_box("Open risks", open, if (as.integer(open) > 0L) "warning" else "success", note = "Risk register status.")
    })

    output$packages_box <- shiny::renderUI({
      missing <- summary_value("missing_packages", 0L)
      dashboard_value_box("Missing packages", missing, if (as.integer(missing) > 0L) "warning" else "success", note = "Package availability check.")
    })

    output$data_box <- shiny::renderUI({
      unavailable <- summary_value("data_sources_unavailable", 0L)
      dashboard_value_box("Unavailable data", unavailable, if (as.integer(unavailable) > 0L) "danger" else "success", note = "External data source access.")
    })

    output$project_profile <- shiny::renderUI({
      project <- diagnostics()$project
      dashboard_card(
        "Project profile",
        subtitle = "Current scaffold metadata and enabled project capabilities.",
        dashboard_section_label("Components"),
        dashboard_pill_list(project$components %||% character(), empty = "No components recorded", status = "secondary"),
        dashboard_section_label("Deliverables"),
        dashboard_pill_list(project$deliverables %||% character(), empty = "No deliverables recorded", status = "secondary"),
        dashboard_section_label("Infrastructure"),
        dashboard_pill_list(project$infrastructure %||% character(), empty = "No infrastructure recorded", status = "secondary")
      )
    })

    output$quick_actions <- shiny::renderUI({
      if (!isTRUE(manage)) {
        return(dashboard_card(
          "Diagnostic mode",
          subtitle = "This dashboard is read-only.",
          dashboard_empty_state("Launch with mode = 'manage' to create objects, update governance items or repair project metadata.")
        ))
      }
      dashboard_card(
        "Quick navigation",
        subtitle = "Move directly to the operational area most likely to need attention.",
        shiny::div(
          class = "projflow-actions",
          shiny::actionButton(ns("go_tasks"), "Task board", class = "btn-outline-primary"),
          shiny::actionButton(ns("go_objects"), "Add object", class = "btn-outline-primary"),
          shiny::actionButton(ns("go_reports"), "Reports", class = "btn-outline-primary"),
          shiny::actionButton(ns("go_outputs"), "Outputs", class = "btn-outline-primary"),
          shiny::actionButton(ns("go_charts"), "Planning charts", class = "btn-outline-primary"),
          shiny::actionButton(ns("go_network"), "Network", class = "btn-outline-primary"),
          shiny::actionButton(ns("go_checks"), "Checks", class = "btn-outline-danger")
        )
      )
    })

    attention_data <- shiny::reactive(dashboard_attention_items(diagnostics()))
    dashboard_render_table(output, "attention_table", attention_data, selection = "none", page_length = 8)

    recent_activity <- shiny::reactive(dashboard_recent_activity(diagnostics(), n = 8L))
    dashboard_render_table(output, "recent_activity_table", recent_activity, selection = "none", page_length = 8)

    shiny::observeEvent(input$go_tasks, {
      dashboard_update_main_tab(parent_session, "Task board")
    })
    shiny::observeEvent(input$go_objects, {
      dashboard_update_main_tab(parent_session, "Add object")
    })
    shiny::observeEvent(input$go_reports, {
      dashboard_update_main_tab(parent_session, "Reports")
    })
    shiny::observeEvent(input$go_outputs, {
      dashboard_update_main_tab(parent_session, "Outputs")
    })
    shiny::observeEvent(input$go_charts, {
      dashboard_update_main_tab(parent_session, "Planning charts")
    })
    shiny::observeEvent(input$go_network, {
      dashboard_update_main_tab(parent_session, "Network")
    })
    shiny::observeEvent(input$go_checks, {
      dashboard_update_main_tab(parent_session, "Checks and fixes")
    })
  })
}
