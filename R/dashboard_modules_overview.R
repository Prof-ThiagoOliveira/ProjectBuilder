dashboard_overview_table_panel <- function(title, subtitle = NULL, ...) {
  shiny::div(
    class = "projflow-overview-table-panel",
    shiny::div(
      class = "projflow-overview-table-panel-header",
      shiny::h4(class = "projflow-overview-table-panel-title", title),
      if (!is.null(subtitle)) shiny::p(class = "projflow-overview-table-panel-subtitle", subtitle)
    ),
    ...
  )
}

mod_overview_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dashboard_card(
      "Project command summary",
      subtitle = "Immediate project health, delivery pressure and reproducibility status.",
      shiny::fluidRow(
        shiny::column(3, shiny::uiOutput(ns("health_box"))),
        shiny::column(3, shiny::uiOutput(ns("tasks_box"))),
        shiny::column(3, shiny::uiOutput(ns("outputs_box"))),
        shiny::column(3, shiny::uiOutput(ns("checks_box")))
      )
    ),
    dashboard_card(
      "Action centre",
      subtitle = "The two tables below are intentionally joined: first act on blockers, then verify the latest project activity.",
      class = "projflow-overview-command-card",
      shiny::div(
        class = "projflow-overview-table-grid",
        dashboard_overview_table_panel(
          "Priority attention",
          "Issues that most affect reliability, reproducibility or delivery.",
          dashboard_table_ui(ns("attention_table"))
        ),
        dashboard_overview_table_panel(
          "Recent activity",
          "Latest governance and registry actions.",
          dashboard_table_ui(ns("recent_activity_table"))
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 7,
        dashboard_card(
          "Delivery, governance and reproducibility control matrix",
          subtitle = "Compact operational summary across the main dashboard domains.",
          dashboard_table_ui(ns("control_matrix_table"))
        )
      ),
      shiny::column(
        width = 5,
        shiny::uiOutput(ns("project_profile")),
        shiny::uiOutput(ns("quick_actions"))
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

    output$project_profile <- shiny::renderUI({
      project <- diagnostics()$project
      dashboard_card(
        "Project profile",
        subtitle = "Scaffold metadata and enabled project capabilities.",
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
        subtitle = "Move directly to the area most likely to need intervention.",
        shiny::div(
          class = "projflow-actions",
          shiny::actionButton(ns("go_tasks"), "Tasks", class = "btn-outline-primary"),
          shiny::actionButton(ns("go_objects"), "Create object", class = "btn-outline-primary"),
          shiny::actionButton(ns("go_reports"), "Reports", class = "btn-outline-primary"),
          shiny::actionButton(ns("go_outputs"), "Outputs", class = "btn-outline-primary"),
          shiny::actionButton(ns("go_charts"), "Planning", class = "btn-outline-primary"),
          shiny::actionButton(ns("go_network"), "Network", class = "btn-outline-primary"),
          shiny::actionButton(ns("go_checks"), "Checks", class = "btn-outline-danger")
        )
      )
    })

    attention_data <- shiny::reactive(dashboard_attention_items(diagnostics()))
    dashboard_render_table(output, "attention_table", attention_data, selection = "none", page_length = 8)

    recent_activity <- shiny::reactive(dashboard_recent_activity(diagnostics(), n = 8L))
    dashboard_render_table(output, "recent_activity_table", recent_activity, selection = "none", page_length = 8)

    control_matrix_data <- shiny::reactive({
      diagnostics_now <- diagnostics()
      summary <- dashboard_safe_data_frame(diagnostics_now$summary)
      issue_counts <- dashboard_issue_counts(diagnostics_now)

      value <- function(name, default = 0L) {
        if (!name %in% names(summary) || nrow(summary) == 0L || is.na(summary[[name]][[1]])) {
          return(default)
        }
        summary[[name]][[1]]
      }
      n_rows <- function(x) nrow(dashboard_safe_data_frame(x))
      governance <- diagnostics_now$governance %||% list()

      rows <- list()
      add_row <- function(domain, indicator, value, interpretation) {
        rows[[length(rows) + 1L]] <<- data.frame(
          domain = domain,
          indicator = indicator,
          value = as.character(value),
          interpretation = interpretation,
          stringsAsFactors = FALSE
        )
      }

      add_row("Quality", "Errors / warnings / suggestions", paste(issue_counts[["errors"]], issue_counts[["warnings"]], issue_counts[["suggestions"]], sep = " / "), "Primary release and reproducibility gate.")
      add_row("Governance", "Open / overdue tasks", paste(value("open_tasks"), value("overdue_tasks"), sep = " / "), "Current workload and schedule pressure.")
      add_row("Governance", "Open risks", value("open_risks"), "Unresolved threats to delivery, quality or interpretation.")
      add_row("Delivery", "Missing / stale outputs", paste(value("missing_outputs"), value("stale_outputs"), sep = " / "), "Artefact availability and freshness.")
      add_row("Delivery", "Reports needing render", value("reports_needing_render"), "Reports with source files but no current rendered output.")
      add_row("Reproducibility", "Missing packages", value("missing_packages"), "R package dependencies not available locally.")
      add_row("Reproducibility", "Unavailable data sources", value("data_sources_unavailable"), "External data roots that are missing or unreadable.")
      add_row("Registry", "Scripts / reports / outputs", paste(n_rows(diagnostics_now$scripts), n_rows(diagnostics_now$reports), n_rows(diagnostics_now$outputs), sep = " / "), "Registered work products available for build and network views.")
      add_row("Governance", "Tasks / milestones / decisions", paste(n_rows(governance$tasks), n_rows(governance$milestones), n_rows(governance$decisions), sep = " / "), "Management records available for planning and audit.")
      add_row("Files", "Workflow files to review", value("orphan_files"), "Potential unregistered workflow files detected in project folders.")

      do.call(rbind, rows)
    })
    dashboard_render_table(output, "control_matrix_table", control_matrix_data, selection = "none", page_length = 10)

    shiny::observeEvent(input$go_tasks, {
      dashboard_update_main_tab(parent_session, "Tasks")
    })
    shiny::observeEvent(input$go_objects, {
      dashboard_update_main_tab(parent_session, "Create object")
    })
    shiny::observeEvent(input$go_reports, {
      dashboard_update_main_tab(parent_session, "Reports")
    })
    shiny::observeEvent(input$go_outputs, {
      dashboard_update_main_tab(parent_session, "Outputs")
    })
    shiny::observeEvent(input$go_charts, {
      dashboard_update_main_tab(parent_session, "Planning")
    })
    shiny::observeEvent(input$go_network, {
      dashboard_update_main_tab(parent_session, "Network")
    })
    shiny::observeEvent(input$go_checks, {
      dashboard_update_main_tab(parent_session, "Checks and fixes")
    })
  })
}
