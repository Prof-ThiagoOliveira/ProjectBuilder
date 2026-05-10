dashboard_nav_title <- function(root, manage = TRUE) {
  shiny::span(
    class = "projflow-brand",
    "projflow",
    shiny::span(
      class = "projflow-brand-badge",
      if (isTRUE(manage)) "manage" else "diagnose"
    )
  )
}

dashboard_page_container <- function(...) {
  shiny::div(class = "projflow-page-container", ...)
}

dashboard_sidebar_ui <- function(root, mode, manage = TRUE) {
  bslib::sidebar(
    title = "Project controls",
    width = 320,
    open = "desktop",
    shiny::div(
      class = "projflow-sidebar-section",
      shiny::div(class = "projflow-sidebar-heading", "Current project"),
      shiny::div(class = "projflow-sidebar-value", shiny::strong(safe_basename(root))),
      shiny::div(class = "projflow-sidebar-value", shiny::code(normalize_absolute_path(root))),
      shiny::div(class = "projflow-sidebar-value", paste("Mode:", if (isTRUE(manage)) "Manage" else "Diagnose"))
    ),
    shiny::div(
      class = "projflow-sidebar-section",
      shiny::div(class = "projflow-sidebar-heading", "Actions"),
      shiny::div(
        class = "projflow-actions",
        shiny::actionButton("refresh_dashboard", "Refresh diagnostics", class = "btn-primary"),
        if (isTRUE(manage)) {
          shiny::actionButton("show_backups", "Show backups", class = "btn-outline-secondary")
        }
      )
    ),
    shiny::div(
      class = "projflow-sidebar-section",
      shiny::div(class = "projflow-sidebar-heading", "Status"),
      shiny::uiOutput("dashboard_status_ui")
    ),
    shiny::div(
      class = "projflow-sidebar-section",
      shiny::div(class = "projflow-sidebar-heading", "Workflow map"),
      shiny::tags$ul(
        class = "projflow-sidebar-guide",
        shiny::tags$li(shiny::strong("Overview"), "health, blockers, missing outputs and next actions"),
        shiny::tags$li(shiny::strong("Planning"), "WBS, dependencies, Gantt and blocked work"),
        shiny::tags$li(shiny::strong("Tasks"), "task board, risks, milestones and decisions"),
        shiny::tags$li(shiny::strong("Outputs"), "reports, tables, figures, datasets and registry"),
        shiny::tags$li(shiny::strong("Diagnostics"), "checks, packages, files and network"),
        shiny::tags$li(shiny::strong("Settings"), "paths, data roots and dashboard preferences")
      )
    )
  )
}

dashboard_workflow_tasks_ui <- function(manage = TRUE) {
  bslib::navset_card_underline(
    id = "tasks_workflow_tabs",
    title = "Task, risk and governance management",
    full_screen = TRUE,
    bslib::nav_panel(
      title = "Task board",
      value = "task_board",
      mod_tasks_ui("tasks", manage = manage)
    ),
    bslib::nav_panel(
      title = "Risks",
      value = "risks",
      mod_risks_ui("risks", manage = manage)
    ),
    bslib::nav_panel(
      title = "Milestones",
      value = "milestones",
      mod_milestones_ui("milestones", manage = manage)
    ),
    bslib::nav_panel(
      title = "Decisions",
      value = "decisions",
      mod_decisions_ui("decisions", manage = manage)
    )
  )
}

dashboard_workflow_outputs_ui <- function(manage = TRUE) {
  bslib::navset_card_underline(
    id = "outputs_workflow_tabs",
    title = "Outputs and artefact control",
    full_screen = TRUE,
    bslib::nav_panel(
      title = "Outputs",
      value = "outputs_inventory",
      mod_outputs_ui("outputs", manage = manage)
    ),
    bslib::nav_panel(
      title = "Reports",
      value = "reports",
      mod_reports_ui("reports", manage = manage)
    ),
    bslib::nav_panel(
      title = "Create object",
      value = "add_object",
      mod_add_object_ui("add_object", manage = manage)
    ),
    bslib::nav_panel(
      title = "Registry",
      value = "registry",
      mod_registry_ui("registry", manage = manage)
    )
  )
}

dashboard_workflow_diagnostics_ui <- function(manage = TRUE) {
  bslib::navset_card_underline(
    id = "diagnostics_workflow_tabs",
    title = "Diagnostics and reproducibility checks",
    full_screen = TRUE,
    bslib::nav_panel(
      title = "Checks and fixes",
      value = "checks",
      mod_checks_ui("checks", manage = manage)
    ),
    bslib::nav_panel(
      title = "Packages and files",
      value = "dependencies",
      mod_dependencies_ui("dependencies")
    ),
    bslib::nav_panel(
      title = "Network",
      value = "network",
      mod_network_ui("network")
    ),
    bslib::nav_panel(
      title = "Activity log",
      value = "activity",
      mod_activity_log_ui("activity", manage = manage)
    )
  )
}

dashboard_workflow_settings_ui <- function(manage = TRUE) {
  bslib::navset_card_underline(
    id = "settings_workflow_tabs",
    title = "Project settings and data roots",
    full_screen = TRUE,
    bslib::nav_panel(
      title = "Project settings",
      value = "project_settings",
      mod_settings_ui("settings", manage = manage)
    ),
    bslib::nav_panel(
      title = "Data roots",
      value = "data_sources",
      mod_data_sources_ui("data_sources", manage = manage)
    )
  )
}
