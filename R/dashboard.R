#' Open the projflow dashboard
#'
#' @param root Existing project root to inspect and manage.
#' @param mode Launch mode. Use \code{"manage"} for the full control centre or
#'   \code{"diagnose"} for a read-only diagnostic view.
#' @param host Host interface passed to \code{shiny::runApp()}.
#' @param port Optional port passed to \code{shiny::runApp()}. If \code{NULL}, a free
#'   local port is selected where possible.
#' @param launch.browser Logical scalar indicating whether the dashboard should
#'   open in a browser automatically.
#' @param background Logical scalar. If \code{TRUE}, the dashboard is started in a
#'   separate R process using \code{callr::r_bg()}, leaving the current R session
#'   available for scripts and project work. If \code{FALSE}, the dashboard is run in
#'   the current R session, which is useful for debugging.
#' @param ... Additional arguments passed to \code{shiny::runApp()} when
#'   \code{background = FALSE}.
#'
#' @return If \code{background = TRUE}, invisibly returns the \pkg{callr} background
#'   process handle. If \code{background = FALSE}, invisibly returns the launched
#'   \code{shiny.appobj} after the app stops.
#' @examples
#' \dontrun{
#' open_dashboard()
#' open_dashboard(mode = "diagnose")
#' open_dashboard(background = FALSE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
open_dashboard <- function(
    root = ".",
    mode = c("manage", "diagnose"),
    host = "127.0.0.1",
    port = NULL,
    launch.browser = interactive(),
    background = TRUE,
    ...) {
  mode <- match.arg(mode)
  validate_logical_scalar(launch.browser, "launch.browser")
  validate_logical_scalar(background, "background")
  root <- find_project_root(root)
  port <- dashboard_resolve_port(port)

  check_dashboard_dependencies(
    packages = c("shiny", "bslib", "htmltools"),
    require_network = FALSE
  )

  if (isTRUE(background)) {
    return(invisible(launch_project_manager_background(
      root = root,
      mode = mode,
      host = host,
      port = port,
      launch.browser = launch.browser
    )))
  }

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

#' Stop a background projflow dashboard
#'
#' @param root Existing project root.
#'
#' @return Invisibly returns \code{TRUE} if a recorded process was stopped, otherwise
#'   \code{FALSE}.
#' @examples
#' \dontrun{
#' stop_dashboard()
#' }
#' @author Thiago de Paula Oliveira
#' @export
stop_dashboard <- function(root = ".") {
  root <- find_project_root(root)
  metadata <- dashboard_process_metadata(root)
  if (length(metadata) == 0L || is.null(metadata$pid) || is.na(metadata$pid)) {
    cli::cli_alert_info("No background projflow dashboard is recorded for this project.")
    return(invisible(FALSE))
  }

  pid <- as.integer(metadata$pid)
  stopped <- dashboard_stop_pid(pid)
  dashboard_remove_process_metadata(root)

  if (isTRUE(stopped)) {
    cli::cli_alert_success("Stopped projflow dashboard process {pid}.")
  } else {
    cli::cli_alert_info("No running process was found for recorded dashboard pid {pid}.")
  }

  invisible(isTRUE(stopped))
}

#' Report background dashboard status
#'
#' @param root Existing project root.
#'
#' @return A data frame with the recorded dashboard process status.
#' @examples
#' \dontrun{
#' dashboard_status()
#' }
#' @author Thiago de Paula Oliveira
#' @export
dashboard_status <- function(root = ".") {
  root <- find_project_root(root)
  metadata <- dashboard_process_metadata(root)
  if (length(metadata) == 0L) {
    return(data.frame(
      running = FALSE,
      pid = NA_integer_,
      url = NA_character_,
      root = root,
      mode = NA_character_,
      started = as.POSIXct(NA),
      stringsAsFactors = FALSE
    ))
  }

  pid <- suppressWarnings(as.integer(metadata$pid %||% NA_integer_))
  data.frame(
    running = !is.na(pid) && dashboard_pid_running(pid),
    pid = pid,
    url = as.character(metadata$url %||% NA_character_),
    root = as.character(metadata$root %||% root),
    mode = as.character(metadata$mode %||% NA_character_),
    started = as.character(metadata$started %||% NA_character_),
    stringsAsFactors = FALSE
  )
}

#' Launch the read-only diagnostics dashboard
#'
#' @param root Existing project root to inspect.
#' @param launch Logical scalar indicating whether the Shiny application should
#'   be launched immediately.
#' @param ... Additional arguments passed to the launcher when \code{launch = TRUE}.
#'
#' @return A \code{shiny.appobj} when \code{launch = FALSE}, otherwise launches the app.
#' @examples
#' \dontrun{
#' project_diagnostics_app()
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_diagnostics_app <- function(root = ".", launch = interactive(), ...) {
  root <- find_project_root(root)
  if (isTRUE(launch)) {
    return(open_dashboard(root = root, mode = "diagnose", ...))
  }
  check_dashboard_dependencies(
    packages = c("shiny", "bslib", "htmltools"),
    require_network = FALSE
  )
  project_manager_app(root = root, mode = "diagnose")
}

project_manager_app <- function(root = ".", mode = c("manage", "diagnose")) {
  mode <- match.arg(mode)
  manage <- identical(mode, "manage")

  ui <- bslib::page_navbar(
    title = "projflow Project Manager",
    id = "main_tabs",
    selected = "Overview",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#235789"
    ),
    fillable = FALSE,
    sidebar = bslib::sidebar(
      title = "Project controls",
      width = 330,
      open = "desktop",
      shiny::div(
        class = "projflow-sidebar",
        shiny::div(class = "projflow-sidebar-title", safe_basename(root)),
        shiny::div(class = "projflow-sidebar-subtitle", normalize_absolute_path(root)),
        shiny::hr(),
        shiny::div(class = "projflow-sidebar-label", "Mode"),
        dashboard_pill(if (isTRUE(manage)) "Manage" else "Diagnose", status = if (isTRUE(manage)) "primary" else "secondary"),
        shiny::div(class = "projflow-sidebar-label", "Metadata"),
        shiny::code(project_metadata_relative_dir(root)),
        shiny::hr(),
        shiny::actionButton("refresh_dashboard", "Refresh diagnostics", class = "btn-primary w-100"),
        if (isTRUE(manage)) shiny::actionButton("show_backups", "Show backups", class = "btn-outline-secondary w-100 mt-2"),
        shiny::hr(),
        shiny::div(class = "projflow-sidebar-label", "Current status"),
        shiny::uiOutput("dashboard_status_ui"),
        shiny::hr(),
        shiny::div(class = "projflow-sidebar-label", "Workflow map"),
        shiny::tags$ul(
          class = "projflow-sidebar-map",
          shiny::tags$li("Overview: health, progress, blockers and next actions"),
          shiny::tags$li("Planning: WBS, dependencies, Gantt and blocked work"),
          shiny::tags$li("Tasks: task board, risks, milestones and decisions"),
          shiny::tags$li("Outputs: reports, tables, figures, datasets and registry"),
          shiny::tags$li("Diagnostics: checks, packages, files, network and audit log"),
          shiny::tags$li("Settings: project paths, data roots and preferences")
        )
      )
    ),
    header = shiny::tags$head(shiny::tags$style(dashboard_css())),
    bslib::nav_panel(
      "Overview",
      shiny::div(class = "projflow-page", mod_overview_ui("overview", manage = manage))
    ),
    bslib::nav_panel(
      "Planning",
      shiny::div(
        class = "projflow-page",
        dashboard_card(
          "Planning workspace",
          subtitle = "Work breakdown, dependency structure, Gantt timeline and blocked-task diagnostics.",
          mod_charts_ui("charts")
        )
      )
    ),
    bslib::nav_panel(
      "Tasks",
      shiny::div(
        class = "projflow-page",
        bslib::navset_card_underline(
          title = "Task management",
          bslib::nav_panel("Task table", mod_tasks_ui("tasks", manage = manage)),
          bslib::nav_panel("Risks", mod_risks_ui("risks", manage = manage)),
          bslib::nav_panel("Milestones", mod_milestones_ui("milestones", manage = manage)),
          bslib::nav_panel("Decisions", mod_decisions_ui("decisions", manage = manage))
        )
      )
    ),
    bslib::nav_panel(
      "Outputs",
      shiny::div(
        class = "projflow-page",
        bslib::navset_card_underline(
          title = "Outputs and artefacts",
          bslib::nav_panel("Outputs", mod_outputs_ui("outputs", manage = manage)),
          bslib::nav_panel("Reports", mod_reports_ui("reports", manage = manage)),
          bslib::nav_panel("Create object", mod_add_object_ui("add_object", manage = manage)),
          bslib::nav_panel("Registry", mod_registry_ui("registry", manage = manage))
        )
      )
    ),
    bslib::nav_panel(
      "Diagnostics",
      shiny::div(
        class = "projflow-page",
        bslib::navset_card_underline(
          title = "Diagnostics and audit",
          bslib::nav_panel("Checks and fixes", mod_checks_ui("checks", manage = manage)),
          bslib::nav_panel("Packages and files", mod_dependencies_ui("dependencies")),
          bslib::nav_panel("Network", mod_network_ui("network")),
          bslib::nav_panel("Activity log", mod_activity_log_ui("activity", manage = manage))
        )
      )
    ),
    bslib::nav_panel(
      "Settings",
      shiny::div(
        class = "projflow-page",
        bslib::navset_card_underline(
          title = "Project settings",
          bslib::nav_panel("Project settings", mod_settings_ui("settings", manage = manage)),
          bslib::nav_panel("Data roots", mod_data_sources_ui("data_sources", manage = manage))
        )
      )
    )
  )

  server <- function(input, output, session) {
    state <- new_dashboard_state(root = root, mode = mode)
    refresh_dashboard_state(state)

    output$dashboard_status_ui <- shiny::renderUI({
      message <- state$last_action %||% "Ready."
      type <- if (grepl("error|failed|required|does not", message, ignore.case = TRUE)) "danger" else "success"
      dashboard_alert(message, type = type)
    })

    shiny::observeEvent(input$refresh_dashboard, {
      refresh_dashboard_state(state)
      state$last_action <- "Diagnostics refreshed."
      shiny::showNotification("Diagnostics refreshed.", type = "message")
    })

    if (isTRUE(manage)) {
      shiny::observeEvent(input$show_backups, {
        backups <- list_project_backups(root)
        message <- if (nrow(backups) == 0L) {
          "No backups are available for this project yet."
        } else {
          paste("Available backups:", paste(backups$name, collapse = ", "))
        }
        shiny::showNotification(message, duration = 8)
      })
    }

    mod_overview_server("overview", state, parent_session = session, manage = manage)
    mod_tasks_server("tasks", state, manage = manage)
    mod_charts_server("charts", state)
    mod_add_object_server("add_object", state, manage = manage)
    mod_outputs_server("outputs", state, manage = manage)
    mod_reports_server("reports", state, manage = manage)
    mod_registry_server("registry", state, manage = manage)
    mod_network_server("network", state)
    mod_checks_server("checks", state, manage = manage)
    mod_dependencies_server("dependencies", state)
    mod_data_sources_server("data_sources", state, manage = manage)
    mod_risks_server("risks", state, manage = manage)
    mod_milestones_server("milestones", state, manage = manage)
    mod_decisions_server("decisions", state, manage = manage)
    mod_activity_log_server("activity", state, manage = manage)
    mod_settings_server("settings", state, manage = manage)
  }

  shiny::shinyApp(ui = ui, server = server)
}

launch_project_manager_background <- function(root,
                                              mode,
                                              host,
                                              port,
                                              launch.browser) {
  abort_missing_optional_packages(
    "callr",
    feature = "dashboard background launching",
    extra = "Alternatively, use background = FALSE."
  )

  url <- sprintf("http://%s:%s", host, port)
  process <- callr::r_bg(
    func = function(root, mode, host, port, launch_browser) {
      if (!requireNamespace("projflow", quietly = TRUE)) {
        stop("The `projflow` package must be installed in the background R process.", call. = FALSE)
      }
      projflow::open_dashboard(
        root = root,
        mode = mode,
        host = host,
        port = port,
        launch.browser = FALSE,
        background = FALSE
      )
    },
    args = list(
      root = root,
      mode = mode,
      host = host,
      port = port,
      launch_browser = launch.browser
    ),
    supervise = TRUE
  )

  dashboard_write_process_metadata(
    root = root,
    pid = process$get_pid(),
    url = url,
    mode = mode,
    host = host,
    port = port
  )

  if (isTRUE(launch.browser)) {
    utils::browseURL(url)
  }

  cli::cli_alert_success("projflow dashboard running at {.url {url}}.")
  cli::cli_alert_info("The dashboard is running in a separate R process; the current R session remains available.")
  cli::cli_alert_info("Stop it with {.code projflow::stop_dashboard()}.")

  invisible(process)
}

dashboard_resolve_port <- function(port = NULL) {
  if (!is.null(port)) {
    return(as.integer(port[[1]]))
  }
  if (requireNamespace("httpuv", quietly = TRUE) && "randomPort" %in% getNamespaceExports("httpuv")) {
    return(httpuv::randomPort())
  }
  sample(seq.int(3000L, 8999L), 1L)
}

dashboard_process_path <- function(root = ".") {
  project_metadata_path(find_project_root(root), "dashboard.yml", create_dir = TRUE, prefer_existing = FALSE)
}

dashboard_write_process_metadata <- function(root, pid, url, mode, host, port) {
  metadata <- list(
    pid = as.integer(pid),
    url = as.character(url),
    root = normalize_absolute_path(root),
    mode = as.character(mode),
    host = as.character(host),
    port = as.integer(port),
    started = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  write_yaml_file(dashboard_process_path(root), metadata, overwrite = TRUE)
  invisible(metadata)
}

dashboard_process_metadata <- function(root = ".") {
  path <- dashboard_process_path(root)
  read_yaml_if_exists(path, list())
}

dashboard_remove_process_metadata <- function(root = ".") {
  path <- dashboard_process_path(root)
  if (fs::file_exists(path)) {
    fs::file_delete(path)
  }
  invisible(path)
}

dashboard_pid_running <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (is.na(pid) || pid <= 0L) {
    return(FALSE)
  }

  if (identical(.Platform$OS.type, "windows")) {
    result <- tryCatch(
      suppressWarnings(system2("tasklist", c("/FI", paste0("PID eq ", pid)), stdout = TRUE, stderr = FALSE)),
      error = function(error) character(),
      warning = function(warning) character()
    )
    return(any(grepl(paste0("\\b", pid, "\\b"), result)))
  }

  status <- tryCatch(
    system2("kill", c("-0", as.character(pid)), stdout = FALSE, stderr = FALSE),
    error = function(error) 1L
  )
  identical(status, 0L)
}

dashboard_stop_pid <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (!dashboard_pid_running(pid)) {
    return(FALSE)
  }

  if (identical(.Platform$OS.type, "windows")) {
    status <- tryCatch(
      suppressWarnings(system2("taskkill", c("/PID", as.character(pid), "/T", "/F"), stdout = FALSE, stderr = FALSE)),
      error = function(error) 1L,
      warning = function(warning) 1L
    )
    return(identical(status, 0L) || !dashboard_pid_running(pid))
  }

  status <- tryCatch(
    system2("kill", as.character(pid), stdout = FALSE, stderr = FALSE),
    error = function(error) 1L
  )
  identical(status, 0L) || !dashboard_pid_running(pid)
}
