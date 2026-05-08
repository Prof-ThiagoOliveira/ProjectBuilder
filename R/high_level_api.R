#' Preview a project plan without writing files
#'
#' @param path Target project path. Defaults to `"."`.
#' @param title Optional project title.
#' @param components Project components to include.
#' @param deliverables Deliverables to prepare.
#' @param infrastructure Technical support features to enable.
#' @param preset Optional preset shortcut.
#' @param use_internal_data_dirs Should internal data directories be created?
#' @param include_example Should example files be created?
#'
#' @return A structured project plan.
#' @export
plan_project <- function(
    path = ".",
    title = NULL,
    components = c("statistical_analysis", "report"),
    deliverables = NULL,
    infrastructure = NULL,
    preset = NULL,
    use_internal_data_dirs = FALSE,
    include_example = TRUE) {
  build_project_plan(
    path = path,
    title = title,
    components = components,
    deliverables = deliverables,
    infrastructure = infrastructure,
    preset = preset,
    use_internal_data_dirs = use_internal_data_dirs,
    include_example = include_example
  )
}

#' Create a new project script
#'
#' @param name Script name.
#' @param type Script type.
#' @param root Project root.
#' @param open Included for API compatibility.
#'
#' @return Invisibly returns the created script path.
#' @export
new_script <- function(name, type = "analysis", root = ".", open = interactive()) {
  new_project_script(name = name, type = type, root = root, open = open)
}

#' Create a new report
#'
#' @param name Report name.
#' @param type Report type.
#' @param root Project root.
#' @param open Included for API compatibility.
#'
#' @return Invisibly returns the created report path.
#' @export
new_report <- function(
    name = "main_report",
    type = c("html_report", "client_report", "scientific_report"),
    root = ".",
    open = interactive()) {
  type <- match.arg(type)
  root <- find_project_root(root)

  if (identical(type, "client_report")) {
    add_project_deliverable("client_report", root = root, open = open)
    if (!identical(name, "client_report") &&
        !fs::file_exists(fs::path(root, "reports", paste0(name, ".qmd")))) {
      return(new_project_report(name = name, format = "qmd", root = root, open = open))
    }
    return(invisible(fs::path(root, "reports", "client_report.qmd")))
  }

  if (!"report" %in% project_components(root)) {
    add_project_component("report", root = root, open = FALSE)
  }

  if (identical(type, "html_report") && !"html_report" %in% project_deliverables(root)) {
    add_project_deliverable("html_report", root = root, open = FALSE)
  }

  if (identical(type, "scientific_report") && !"tables" %in% project_components(root)) {
    add_project_component("tables", root = root, open = FALSE)
  }

  if (identical(name, "main_report") && fs::file_exists(fs::path(root, "reports", "main_report.qmd"))) {
    return(invisible(fs::path(root, "reports", "main_report.qmd")))
  }

  new_project_report(name = name, format = "qmd", root = root, open = open)
}

#' Create a new app or dashboard
#'
#' @param name App or dashboard name.
#' @param type App type.
#' @param root Project root.
#' @param open Included for API compatibility.
#'
#' @return Invisibly returns the created app or dashboard path.
#' @export
new_app <- function(
    name = "app",
    type = c("shiny", "quarto_dashboard"),
    root = ".",
    open = interactive()) {
  type <- match.arg(type)
  root <- find_project_root(root)

  if (identical(type, "shiny")) {
    add_project_component("shiny_app", root = root, open = FALSE)
    return(invisible(fs::path(root, "app", "app.R")))
  }

  add_project_deliverable("dashboard", root = root, open = FALSE)
  if (!identical(name, "dashboard") &&
      !fs::file_exists(fs::path(root, "dashboard", paste0(name, ".qmd")))) {
    write_template_file(
      fs::path(root, "dashboard", paste0(name, ".qmd")),
      report_template_for_plan(list(name = name, path = fs::path("dashboard", paste0(name, ".qmd")), type = "dashboard")),
      overwrite = FALSE
    )
    return(invisible(fs::path(root, "dashboard", paste0(name, ".qmd"))))
  }

  invisible(fs::path(root, "dashboard", "dashboard.qmd"))
}

#' Add a component to an existing project
#'
#' @param component Component name.
#' @param root Project root.
#' @param open Included for API compatibility.
#'
#' @return A scaffold result object.
#' @export
new_component <- function(component, root = ".", open = interactive()) {
  add_project_component(component = component, root = root, open = open)
}

#' Build a project
#'
#' @param root Project root.
#' @param render_reports Should registered reports be rendered?
#' @param run_apps Should the app be launched after building?
#'
#' @return Structured build results.
#' @export
build_project <- function(root = ".", render_reports = TRUE, run_apps = FALSE) {
  validate_logical_scalar(render_reports, "render_reports")
  validate_logical_scalar(run_apps, "run_apps")

  root <- find_project_root(root)
  run_project(root = root)

  rendered <- NULL
  if (isTRUE(render_reports)) {
    rendered <- render_project_reports(root = root)
  }

  status <- NULL
  if ("project_management" %in% project_components(root)) {
    status <- project_status_report(root = root, output = "data")
  }

  if (isTRUE(run_apps)) {
    serve_project(root = root, watch = FALSE)
  }

  invisible(list(root = root, rendered = rendered, status = status))
}

#' Serve a project for interactive development
#'
#' @param root Project root.
#' @param watch Included for API compatibility.
#'
#' @return Invisibly returns the launched target or build result.
#' @export
serve_project <- function(root = ".", watch = interactive()) {
  validate_logical_scalar(watch, "watch")
  root <- find_project_root(root)

  if (fs::file_exists(fs::path(root, "app", "app.R"))) {
    if (!requireNamespace("shiny", quietly = TRUE)) {
      rlang::abort("The `shiny` package is required to serve this project app.")
    }

    shiny::runApp(appDir = fs::path(root, "app"), launch.browser = interactive())
    return(invisible(fs::path(root, "app", "app.R")))
  }

  if (isTRUE(watch)) {
    cli::cli_alert_info("Continuous watch mode is not implemented; rendering current reports once.")
  }

  invisible(render_project_reports(root = root))
}
