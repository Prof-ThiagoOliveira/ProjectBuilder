#' Preview a project plan without writing files
#'
#' @param path Target project path. No files are written; the path is used only
#'   to construct the proposed plan.
#' @param title Optional project title to include in the plan metadata.
#' @param components Character vector of components to include in the proposed
#'   scaffold.
#' @param deliverables Optional character vector of deliverables to prepare. If
#'   `NULL`, deliverables are inferred from the selected components.
#' @param infrastructure Optional character vector of technical features to
#'   enable, such as `git`, `quarto`, or `tests`.
#' @param preset Optional preset name used as a shorthand for a predefined
#'   scaffold configuration.
#' @param component_specs Optional custom component spec path, a single spec
#'   list, or a list of spec objects merged into the available component map for
#'   this plan.
#' @param use_internal_data_dirs Logical scalar indicating whether internal
#'   `data/raw/` and `data/processed/` directories should be included in the
#'   plan.
#' @param include_example Logical scalar indicating whether the built-in example
#'   script should be included in the plan.
#'
#' @return A structured project plan.
#' @examples
#' \dontrun{
#' plan_project(
#'   path = "demo-project",
#'   components = c("statistical_analysis", "report"),
#'   infrastructure = character()
#' )
#' }
#' @author Thiago de Paula Oliveira
#' @export
plan_project <- function(
    path = ".",
    title = NULL,
    components = c("statistical_analysis", "report"),
    deliverables = NULL,
    infrastructure = NULL,
    preset = NULL,
    component_specs = NULL,
    use_internal_data_dirs = FALSE,
    include_example = FALSE) {
  build_project_plan(
    path = path,
    title = title,
    components = components,
    deliverables = deliverables,
    infrastructure = infrastructure,
    preset = preset,
    component_specs = component_specs,
    use_internal_data_dirs = use_internal_data_dirs,
    include_example = include_example
  )
}

#' Create a new project script
#'
#' @param name Script name. The value is normalised to a safe snake_case object
#'   name before the file and registry entry are created.
#' @param type Script type recorded in the registry and used to infer default
#'   output locations when `outputs` are supplied.
#' @param root Existing project root where the script should be added.
#' @param order Optional numeric execution order. If omitted, the script is
#'   placed after the highest existing script order.
#' @param outputs Optional character vector of output names to register for the
#'   script. If `NULL`, the script is created without registered outputs.
#' @param output Optional explicit project-relative output path to register for
#'   the script.
#' @param template Template style used for the generated script. `"minimal"`
#'   creates a bare scaffold; `"example"` adds placeholder analysis code and
#'   save calls for any explicit outputs.
#' @param open Logical scalar kept for API compatibility. The script is created
#'   on disk but not opened automatically.
#' @param overwrite Logical scalar. If `TRUE`, replace an existing script file
#'   and registry entry with the same name.
#' @param repair Logical scalar. If `TRUE`, register an existing script file
#'   without overwriting it.
#' @param dry_run Logical scalar. If `TRUE`, return the planned change without
#'   modifying the project.
#'
#' @return Invisibly returns the created script path.
#' @examples
#' \dontrun{
#' new_script("clean_phenotypes", type = "data_cleaning", open = FALSE)
#'
#' new_script(
#'   "fit_model",
#'   type = "model",
#'   outputs = "heritability_model",
#'   template = "example",
#'   open = FALSE
#' )
#' }
#' @author Thiago de Paula Oliveira
#' @export
new_script <- function(name,
                       type = "analysis",
                       root = ".",
                       order = NULL,
                       outputs = NULL,
                       output = NULL,
                       template = c("minimal", "example"),
                       open = interactive(),
                       overwrite = FALSE,
                       repair = FALSE,
                       dry_run = FALSE) {
  new_project_script(
    name = name,
    type = type,
    root = root,
    order = order,
    outputs = outputs,
    output = output,
    template = template,
    open = open,
    overwrite = overwrite,
    repair = repair,
    dry_run = dry_run
  )
}

#' Create a new report
#'
#' @param name Report name. This is used as the filename stem unless the helper
#'   is returning an existing built-in report such as `main_report`.
#' @param type Report type shortcut. This determines which components or
#'   deliverables are added before the report file is created.
#' @param root Existing project root where the report should be added.
#' @param open Logical scalar kept for API compatibility. The report is written
#'   to disk but not opened automatically.
#' @param overwrite Logical scalar. If `TRUE`, replace an existing report file
#'   and registry entry with the same name.
#' @param repair Logical scalar. If `TRUE`, register an existing report file
#'   without overwriting it.
#' @param dry_run Logical scalar. If `TRUE`, return the planned change without
#'   modifying the project.
#'
#' @return Invisibly returns the created report path.
#' @examples
#' \dontrun{
#' new_report("main_report", type = "html_report", open = FALSE)
#' new_report("client_report", type = "client_report", open = FALSE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
new_report <- function(
    name = "main_report",
    type = c("html_report", "client_report", "scientific_report"),
    root = ".",
    open = interactive(),
    overwrite = FALSE,
    repair = FALSE,
    dry_run = FALSE) {
  type <- match.arg(type)
  root <- find_project_root(root)

  if (identical(type, "client_report")) {
    add_project_deliverable("client_report", root = root, open = open, overwrite = overwrite, dry_run = dry_run)
    if (!identical(name, "client_report") &&
        !fs::file_exists(fs::path(root, "reports", paste0(name, ".qmd")))) {
      return(new_project_report(name = name, format = "qmd", root = root, open = open, overwrite = overwrite, repair = repair, dry_run = dry_run))
    }
    return(invisible(fs::path(root, "reports", "client_report.qmd")))
  }

  if (!"report" %in% project_components(root)) {
    add_project_component("report", root = root, open = FALSE, overwrite = overwrite, dry_run = dry_run)
  }

  if (identical(type, "html_report") && !"html_report" %in% project_deliverables(root)) {
    add_project_deliverable("html_report", root = root, open = FALSE, overwrite = overwrite, dry_run = dry_run)
  }

  if (identical(type, "scientific_report") && !"tables" %in% project_components(root)) {
    add_project_component("tables", root = root, open = FALSE, overwrite = overwrite, dry_run = dry_run)
  }
  if (identical(type, "scientific_report") && !"figures" %in% project_components(root)) {
    add_project_component("figures", root = root, open = FALSE, overwrite = overwrite, dry_run = dry_run)
  }

  if (identical(name, "main_report") && fs::file_exists(fs::path(root, "reports", "main_report.qmd"))) {
    return(invisible(fs::path(root, "reports", "main_report.qmd")))
  }

  new_project_report(name = name, format = "qmd", root = root, open = open, overwrite = overwrite, repair = repair, dry_run = dry_run)
}

#' Create a new app or dashboard
#'
#' @param name App or dashboard name. For dashboards, non-default names are used
#'   as the `.qmd` filename stem.
#' @param type App type to create. `"shiny"` adds the Shiny app scaffold,
#'   whereas `"quarto_dashboard"` adds a Quarto dashboard deliverable.
#' @param root Existing project root where the app or dashboard should be added.
#' @param open Logical scalar kept for API compatibility. The created file is
#'   not opened automatically.
#' @param overwrite Logical scalar. If `TRUE`, replace an existing dashboard or
#'   app scaffold when supported.
#' @param dry_run Logical scalar. If `TRUE`, return the planned change without
#'   modifying the project.
#'
#' @return Invisibly returns the created app or dashboard path.
#' @examples
#' \dontrun{
#' new_app(type = "shiny", open = FALSE)
#' new_app(name = "operations_dashboard", type = "quarto_dashboard", open = FALSE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
new_app <- function(
    name = "app",
    type = c("shiny", "quarto_dashboard"),
    root = ".",
    open = interactive(),
    overwrite = FALSE,
    dry_run = FALSE) {
  type <- match.arg(type)
  root <- find_project_root(root)

  if (identical(type, "shiny")) {
    add_project_component("shiny_app", root = root, open = FALSE, overwrite = overwrite, dry_run = dry_run)
    return(invisible(fs::path(root, "app", "app.R")))
  }

  add_project_deliverable("dashboard", root = root, open = FALSE, overwrite = overwrite, dry_run = dry_run)
  if (!identical(name, "dashboard") &&
      !fs::file_exists(fs::path(root, "dashboard", paste0(name, ".qmd")))) {
    if (isTRUE(dry_run)) {
      return(new_registry_action("create_dashboard", "report", name, fs::path("dashboard", paste0(name, ".qmd")), root, TRUE))
    }
    write_template_file(
      fs::path(root, "dashboard", paste0(name, ".qmd")),
      report_template_for_plan(list(name = name, path = fs::path("dashboard", paste0(name, ".qmd")), type = "dashboard")),
      overwrite = overwrite
    )
    return(invisible(fs::path(root, "dashboard", paste0(name, ".qmd"))))
  }

  invisible(fs::path(root, "dashboard", "dashboard.qmd"))
}

#' Add a component to an existing project
#'
#' @param component Component name to add, such as `"tables"`,
#'   `"figures"`, or `"project_management"`.
#' @param root Existing project root to update.
#' @param open Logical scalar kept for API compatibility. Added files are not
#'   opened automatically.
#' @param overwrite Logical scalar. If `TRUE`, allow template files added by the
#'   component to be refreshed.
#' @param dry_run Logical scalar. If `TRUE`, return the planned amended project
#'   plan without writing files.
#'
#' @return A scaffold result object.
#' @examples
#' \dontrun{
#' new_component("tables", open = FALSE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
new_component <- function(component, root = ".", open = interactive(), overwrite = FALSE, dry_run = FALSE) {
  add_project_component(component = component, root = root, open = open, overwrite = overwrite, dry_run = dry_run)
}

#' Register a lightweight table output
#'
#' @inheritParams new_project_output
#'
#' @return Invisibly returns the registered output path, or a dry-run plan.
#' @examples
#' \dontrun{
#' new_table("summary_statistics")
#' }
#' @author Thiago de Paula Oliveira
#' @export
new_table <- function(name, path = NULL, root = ".", overwrite = FALSE, repair = FALSE, dry_run = FALSE) {
  new_project_output(name = name, type = "table", path = path, root = root, overwrite = overwrite, repair = repair, dry_run = dry_run)
}

#' Register a lightweight figure output
#'
#' @inheritParams new_project_output
#'
#' @return Invisibly returns the registered output path, or a dry-run plan.
#' @examples
#' \dontrun{
#' new_figure("heritability_plot")
#' }
#' @author Thiago de Paula Oliveira
#' @export
new_figure <- function(name, path = NULL, root = ".", overwrite = FALSE, repair = FALSE, dry_run = FALSE) {
  new_project_output(name = name, type = "figure", path = path, root = root, overwrite = overwrite, repair = repair, dry_run = dry_run)
}

#' Register a project output
#'
#' @inheritParams new_project_output
#'
#' @return Invisibly returns the registered output path, or a dry-run plan.
#' @examples
#' \dontrun{
#' new_output("model_fit", type = "model")
#' }
#' @author Thiago de Paula Oliveira
#' @export
new_output <- function(name, type = "output", path = NULL, root = ".", overwrite = FALSE, repair = FALSE, dry_run = FALSE) {
  new_project_output(name = name, type = type, path = path, root = root, overwrite = overwrite, repair = repair, dry_run = dry_run)
}

#' Build a project
#'
#' @param root Existing project root to build.
#' @param render_reports Logical scalar. If `TRUE`, render registered reports
#'   after running registered scripts.
#' @param run_apps Logical scalar. If `TRUE`, launch the project app or
#'   dashboard after the build completes.
#'
#' @return Structured build results.
#' @examples
#' \dontrun{
#' build_project(render_reports = FALSE)
#' build_project(render_reports = TRUE)
#' }
#' @author Thiago de Paula Oliveira
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
#' @param root Existing project root to preview or serve.
#' @param target Target to serve. `"auto"` chooses between reports, dashboard,
#'   Shiny app, or the full project based on the files present.
#' @param watch Logical scalar kept for API compatibility. Continuous watch mode
#'   is not currently implemented, so the function performs a one-shot preview.
#' @param render Logical scalar indicating whether reports or dashboards should
#'   be rendered as part of the preview step.
#'
#' @return Invisibly returns the launched target or build result.
#' @examples
#' \dontrun{
#' serve_project(target = "reports", render = TRUE)
#' serve_project(target = "shiny_app", render = FALSE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
serve_project <- function(
    root = ".",
    target = c("auto", "reports", "shiny_app", "dashboard", "project"),
    watch = interactive(),
    render = TRUE) {
  target <- match.arg(target)
  validate_logical_scalar(watch, "watch")
  validate_logical_scalar(render, "render")
  root <- find_project_root(root)
  has_shiny <- fs::file_exists(fs::path(root, "app", "app.R"))
  has_dashboard <- fs::file_exists(fs::path(root, "dashboard", "dashboard.qmd"))
  has_reports <- length(read_project_registry(root)$reports) > 0L || length(list.files(fs::path(root, "reports"), pattern = "\\.(qmd|Rmd)$")) > 0L

  resolved_target <- target
  if (identical(target, "auto")) {
    resolved_target <- if (has_shiny) {
      "shiny_app"
    } else if (has_dashboard) {
      "dashboard"
    } else if (has_reports) {
      "reports"
    } else {
      "project"
    }
  }

  if (identical(resolved_target, "shiny_app")) {
    if (!has_shiny) {
      rlang::abort("This project does not contain `app/app.R`.")
    }
    if (!requireNamespace("shiny", quietly = TRUE)) {
      rlang::abort("The `shiny` package is required to serve this project app.")
    }

    shiny::runApp(appDir = fs::path(root, "app"), launch.browser = interactive())
    return(invisible(fs::path(root, "app", "app.R")))
  }

  if (identical(resolved_target, "dashboard")) {
    if (!has_dashboard) {
      rlang::abort("This project does not contain a Quarto dashboard.")
    }
    if (isTRUE(watch)) {
      cli::cli_alert_info("Continuous dashboard watch mode is not implemented; previewing the current dashboard once.")
    }
    if (isTRUE(render)) {
      return(invisible(render_one_report(
        fs::path(root, "dashboard", "dashboard.qmd"),
        fs::path(root, "outputs", "reports", "dashboard.html")
      )))
    }
    return(invisible(fs::path(root, "dashboard", "dashboard.qmd")))
  }

  if (identical(resolved_target, "reports")) {
    if (isTRUE(watch)) {
      cli::cli_alert_info("Continuous report watch mode is not implemented; rendering current reports once.")
    }
    if (isTRUE(render)) {
      return(invisible(render_project_reports(root = root)))
    }
    return(invisible(project_status(root)))
  }

  if (isTRUE(watch)) {
    cli::cli_alert_info("Continuous project watch mode is not implemented; running a one-shot build.")
  }

  invisible(build_project(root = root, render_reports = render, run_apps = FALSE))
}
