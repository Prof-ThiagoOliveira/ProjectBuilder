#' Launch the projflow Project Manager
#'
#' @param root Existing project root to inspect and manage.
#' @param mode Launch mode. Use `"manage"` for the full control centre or
#'   `"diagnose"` for a read-only diagnostic view.
#' @param host Host interface passed to [shiny::runApp()].
#' @param port Optional port passed to [shiny::runApp()].
#' @param launch.browser Logical scalar indicating whether the dashboard should
#'   open in a browser automatically.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Invisibly returns the launched `shiny.appobj`.
#' @examples
#' \dontrun{
#' launch_project_manager()
#' launch_project_manager(mode = "diagnose")
#' }
#' @author Thiago de Paula Oliveira
#' @export
launch_project_manager <- function(
    root = ".",
    mode = c("manage", "diagnose"),
    host = "127.0.0.1",
    port = NULL,
    launch.browser = interactive(),
    ...) {
  mode <- match.arg(mode)
  root <- find_project_root(root)
  check_dashboard_dependencies(
    packages = c("shiny", "bslib", "DT", "htmltools"),
    require_network = FALSE
  )

  app <- project_manager_app(root = root, mode = mode)
  shiny::runApp(
    app,
    host = host,
    port = port,
    launch.browser = launch.browser,
    ...
  )

  invisible(app)
}

#' Open the projflow Project Control Centre
#'
#' @inheritParams launch_project_manager
#'
#' @return Invisibly returns the launched `shiny.appobj`.
#' @examples
#' \dontrun{
#' open_project_control_centre()
#' }
#' @author Thiago de Paula Oliveira
#' @export
open_project_control_centre <- function(...) {
  launch_project_manager(...)
}

#' Launch the read-only diagnostics dashboard
#'
#' @param root Existing project root to inspect.
#' @param launch Logical scalar indicating whether the Shiny application should
#'   be launched immediately.
#' @param ... Additional arguments passed to the launcher when `launch = TRUE`.
#'
#' @return A `shiny.appobj` when `launch = FALSE`, otherwise launches the app.
#' @examples
#' \dontrun{
#' project_diagnostics_app()
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_diagnostics_app <- function(root = ".", launch = interactive(), ...) {
  root <- find_project_root(root)
  app <- project_manager_app(root = root, mode = "diagnose")
  if (isTRUE(launch)) {
    launch_project_manager(root = root, mode = "diagnose", ...)
  } else {
    app
  }
}

project_manager_app <- function(root = ".", mode = c("manage", "diagnose")) {
  mode <- match.arg(mode)
  manage <- identical(mode, "manage")
  title <- paste0("projflow Project Manager: ", safe_basename(root))

  ui <- shiny::fluidPage(
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        ".projflow-root{font-size:0.95rem;color:#52606d;margin-bottom:1rem;}
         .projflow-actions .btn{margin-right:0.5rem;margin-bottom:0.5rem;}
         .projflow-panel{margin-top:1rem;}
         .projflow-muted{color:#6c757d;}"
      ))
    ),
    shiny::titlePanel(title),
    shiny::div(
      class = "projflow-root",
      shiny::strong("Project root: "),
      shiny::code(normalize_absolute_path(root)),
      shiny::span(" | "),
      shiny::strong("Mode: "),
      if (manage) "manage" else "diagnose"
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::div(
          class = "projflow-actions",
          shiny::actionButton("refresh_dashboard", "Refresh diagnostics"),
          if (manage) shiny::actionButton("show_backups", "Backups"),
          shiny::verbatimTextOutput("dashboard_status", placeholder = TRUE)
        )
      )
    ),
    shiny::tabsetPanel(
      id = "main_tabs",
      shiny::tabPanel("Overview", mod_overview_ui("overview", manage = manage)),
      shiny::tabPanel("Task board", mod_tasks_ui("tasks", manage = manage)),
      shiny::tabPanel("Registry", mod_registry_ui("registry", manage = manage)),
      shiny::tabPanel("Add object", mod_add_object_ui("add_object", manage = manage)),
      shiny::tabPanel("Outputs", mod_outputs_ui("outputs", manage = manage)),
      shiny::tabPanel("Reports", mod_reports_ui("reports", manage = manage)),
      shiny::tabPanel("Data sources", mod_data_sources_ui("data_sources", manage = manage)),
      shiny::tabPanel("Risks", mod_risks_ui("risks", manage = manage)),
      shiny::tabPanel("Milestones", mod_milestones_ui("milestones", manage = manage)),
      shiny::tabPanel("Decisions", mod_decisions_ui("decisions", manage = manage)),
      shiny::tabPanel("Network", mod_network_ui("network")),
      shiny::tabPanel("Checks and fixes", mod_checks_ui("checks", manage = manage)),
      shiny::tabPanel("Activity log", mod_activity_log_ui("activity", manage = manage)),
      shiny::tabPanel("Settings", mod_settings_ui("settings", manage = manage))
    )
  )

  server <- function(input, output, session) {
    state <- new_dashboard_state(root = root, mode = mode)
    refresh_dashboard_state(state)

    output$dashboard_status <- shiny::renderText({
      state$last_action %||% "Ready."
    })

    shiny::observeEvent(input$refresh_dashboard, {
      refresh_dashboard_state(state)
      state$last_action <- "Diagnostics refreshed."
      shiny::showNotification("Diagnostics refreshed.", type = "message")
    })

    shiny::observeEvent(input$show_backups, {
      backups <- list_project_backups(root)
      message <- if (nrow(backups) == 0L) {
        "No backups available."
      } else {
        paste("Available backups:", paste(backups$name, collapse = ", "))
      }
      shiny::showNotification(message, duration = 8)
    })

    mod_overview_server("overview", state)
    mod_tasks_server("tasks", state, manage = manage)
    mod_registry_server("registry", state, manage = manage)
    mod_add_object_server("add_object", state, manage = manage)
    mod_outputs_server("outputs", state, manage = manage)
    mod_reports_server("reports", state, manage = manage)
    mod_data_sources_server("data_sources", state, manage = manage)
    mod_risks_server("risks", state, manage = manage)
    mod_milestones_server("milestones", state, manage = manage)
    mod_decisions_server("decisions", state, manage = manage)
    mod_network_server("network", state)
    mod_checks_server("checks", state, manage = manage)
    mod_activity_log_server("activity", state, manage = manage)
    mod_settings_server("settings", state, manage = manage)
  }

  shiny::shinyApp(ui = ui, server = server)
}
