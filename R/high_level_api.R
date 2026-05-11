#' Preview a project plan without creating files
#'
#' @description
#' \code{plan_project()} constructs the scaffold specification that can be
#' inspected, plotted with \code{\link{plot.project_plan}()}, and then passed
#' directly to \code{\link{new_project}()} through the \code{plan}
#' argument. It does not create, modify, or delete files. The function is
#' intended for inspection, validation, teaching, reproducibility review, and
#' interactive project design before committing a scaffold to disk.
#'
#' @details
#' A \pkg{projflow} project is assembled from controlled vocabularies rather
#' than free-text labels. This is deliberate: the registry, folder layout,
#' execution order, outputs, and package suggestions are easier to validate when
#' common project objects have standard names.
#'
#' The returned plan is the creation contract used by
#' \code{\link{new_project}()}. A typical workflow is
#' \code{plan <- plan_project(...)}, \code{plot(plan)}, and
#' \code{new_project(plan = plan)}. In particular, the plan describes:
#' \itemize{
#'   \item the target project path and project metadata;
#'   \item selected analytical components, after alias normalisation and
#'     dependency expansion;
#'   \item inferred or explicitly requested deliverables;
#'   \item requested technical infrastructure, such as Git, Quarto, renv,
#'     tests, GitHub Actions, or targets support;
#'   \item project-relative folders and files that would be created;
#'   \item analysis scripts, reports, dashboards, applications, and registered
#'     outputs that would be added to the project registry;
#'   \item R packages suggested by the selected components and infrastructure;
#'   \item checks, warnings, and dependency additions made during planning.
#' }
#'
#' Components are analytical work areas. Examples include
#' \code{"data_preparation"}, \code{"quality_control"},
#' \code{"exploratory_analysis"}, \code{"statistical_analysis"},
#' \code{"model_diagnostics"}, \code{"tables"}, \code{"figures"},
#' \code{"report"}, \code{"manuscript"}, \code{"dashboard"},
#' \code{"shiny_app"}, \code{"project_management"},
#' \code{"data_governance"}, \code{"communication"}, and
#' \code{"validation"}. The authoritative list should be obtained with
#' \code{available_project_components()} if that helper is exported in the
#' installed version of the package.
#'
#' Deliverables are user-facing or analysis-facing outputs. Examples may include
#' HTML reports, client reports, scientific reports, dashboards, Shiny
#' applications, tables, figures, and project documentation. If
#' \code{deliverables = NULL}, deliverables are inferred from the selected
#' components. For example, requesting \code{"report"} usually implies a report
#' deliverable, while requesting \code{"tables"} or \code{"figures"} may add
#' the corresponding output directories and registry entries.
#'
#' Infrastructure values describe technical project support. Typical values are
#' \code{"git"}, \code{"renv"}, \code{"quarto"},
#' \code{"github_actions"}, \code{"targets"}, and \code{"tests"}. Use
#' \code{NULL} to request the package default infrastructure and
#' \code{character()} to request no infrastructure.
#'
#' Common aliases are accepted and normalised before the plan is built. For
#' example, \code{"eda"} is interpreted as \code{"exploratory_analysis"},
#' \code{"stats"} as \code{"statistical_analysis"}, and \code{"shiny"} as
#' \code{"shiny_app"}. Unknown values are rejected. This protects the registry
#' from misspellings and unstandardised project-object names.
#'
#' @param path Character scalar. Target project directory. The directory is not
#'   created by \code{plan_project()}. The value is normalised and used to derive
#'   the proposed project name, root path, and project-relative paths shown in
#'   the returned plan.
#' @param title Optional character scalar. Human-readable project title stored in
#'   the proposed project metadata. If \code{NULL}, the title is inferred from
#'   \code{path} where possible.
#' @param components Character vector. Analytical components to include in the
#'   proposed scaffold. Values must be recognised component names or supported
#'   aliases. Components control which directories, scripts, reports, registry
#'   objects, outputs, package suggestions, and dependency checks are included.
#'   Use \code{available_project_components()} to inspect the accepted values in
#'   the installed package version.
#' @param deliverables Optional character vector. Deliverables to prepare. If
#'   \code{NULL}, deliverables are inferred from \code{components} and
#'   \code{preset}. If supplied, values must be recognised deliverable names or
#'   supported aliases. Use \code{available_project_deliverables()} to inspect
#'   the accepted values in the installed package version.
#' @param infrastructure Optional character vector. Technical infrastructure to
#'   enable. If \code{NULL}, package defaults are used. Use \code{character()}
#'   to request no infrastructure. Use
#'   \code{available_project_infrastructure()} to inspect the accepted values in
#'   the installed package version.
#' @param preset Optional character scalar. Name of a predefined scaffold
#'   configuration. A preset can contribute components, deliverables, and
#'   infrastructure before explicitly supplied values are normalised and merged.
#'   Explicit arguments may extend the preset. Use
#'   \code{available_project_presets()} to inspect the accepted preset names in
#'   the installed package version.
#' @param component_specs Optional custom component specification. This may be a
#'   YAML file path, a single component-specification list, or a list of
#'   component-specification objects. Custom specifications are merged into the
#'   available component map for the current plan, allowing package users to add
#'   project-specific components without changing the built-in component map.
#' @param use_internal_data_dirs Logical scalar. If \code{TRUE}, include
#'   internal \file{data/raw/} and \file{data/processed/} directories in the
#'   proposed folder layout. The default is \code{FALSE}, reflecting the package
#'   design preference for external data roots in reproducible analytical
#'   projects.
#' @param include_example Logical scalar. If \code{TRUE}, include the built-in
#'   example analysis script and any related placeholder outputs in the proposed
#'   scaffold.
#'
#' @return
#' An object of class \code{"project_plan"}. It is a structured list intended
#' for printing, inspection, and downstream use by \code{\link{new_project}()}.
#' The exact structure may evolve, but the object commonly contains:
#' \describe{
#'   \item{\code{path}}{Resolved target project path.}
#'   \item{\code{title}}{Project title used in the proposed metadata.}
#'   \item{\code{components}}{Normalised components after alias resolution and
#'     dependency expansion.}
#'   \item{\code{deliverables}}{Normalised deliverables, either supplied by the
#'     user or inferred from components and presets.}
#'   \item{\code{infrastructure}}{Normalised infrastructure values, including
#'     any infrastructure dependencies added during planning.}
#'   \item{\code{folders}}{Project-relative folders proposed for creation.}
#'   \item{\code{files}}{Project-relative files proposed for creation.}
#'   \item{\code{scripts}}{Script registry entries proposed for creation.}
#'   \item{\code{reports}}{Report, dashboard, or document registry entries
#'     proposed for creation.}
#'   \item{\code{outputs}}{Registered output objects proposed for creation.}
#'   \item{\code{packages}}{R packages suggested by the requested scaffold.}
#'   \item{\code{checks}}{Planning diagnostics, including automatically added
#'     components, deliverables, or infrastructure dependencies.}
#' }
#'
#' @seealso
#' \code{\link{new_project}()}, \code{\link{plot.project_plan}()},
#' \code{\link{project_plan_network_data}()}, \code{\link{new_component}()},
#' \code{\link{new_script}()}, \code{\link{new_report}()},
#' \code{\link{new_output}()}
#'
#' @examples
#' plan <- plan_project(
#'   path = file.path(tempdir(), "demo-project"),
#'   components = c("data_preparation", "statistical_analysis", "report"),
#'   infrastructure = character()
#' )
#'
#' plan$components
#' plan$deliverables
#' plan$files
#'
#' # Visualise the proposed plan before creating files.
#' plot(plan)
#'
#' # Create the inspected plan on disk.
#' # new_project(plan = plan, open = FALSE)
#'
#' # The function validates controlled vocabularies. This call would fail
#' # because "unknown_component" is not a recognised component name.
#' # plan_project(components = c("statistical_analysis", "unknown_component"))
#'
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

#' Create and register a new project script
#'
#' @description
#' `new_script()` adds a documented, non-executing R script to an existing
#' \pkg{projflow} project and records it in the project registry. The public
#' interface is intentionally small: the function creates one script and does
#' not register outputs, choose templates, repair registry state, or expose
#' execution-order internals.
#'
#' @details
#' Scripts are numbered automatically from 0 to N. The next script receives the
#' next available sequential number and is written under \file{analysis/}. The
#' generated file contains comments only; it does not execute code, save
#' \file{.rds} objects, write \file{.csv} files, or create any other analysis
#' artefact.
#'
#' Register outputs separately with \code{\link{new_output}()},
#' \code{\link{new_table}()}, or \code{\link{new_figure}()} after the user has
#' decided what the script should deliberately produce.
#'
#' @param name Character scalar. Human-readable script name. The value is
#'   normalised to a safe project-object name, typically a snake_case stem,
#'   before the file path and registry entry are created.
#' @param script_type Character scalar. Script type recorded in the registry.
#'   Values must be one of \code{project_script_types()}, such as
#'   \code{"data_preparation"}, \code{"statistical_analysis"},
#'   \code{"model_diagnostics"}, \code{"visualisation"}, or
#'   \code{"analysis"}.
#' @param root Character scalar. Path inside an existing \pkg{projflow} project.
#'   The project root is located before the script is added. Use \code{"."} for
#'   the current working directory.
#' @param open Logical scalar. If \code{TRUE}, an informational message is shown
#'   reminding the user to open the generated script manually.
#' @param overwrite Logical scalar. If \code{TRUE}, an existing script file and
#'   registry entry with the same normalised name may be replaced.
#'
#' @return
#' Invisibly returns the created script path.
#'
#' @seealso
#' \code{\link{plan_project}()}, \code{\link{new_component}()},
#' \code{\link{new_report}()}, \code{\link{new_output}()}
#'
#' @examples
#' \dontrun{
#' root <- tempfile("projflow-script-")
#' new_project(
#'   path = root,
#'   components = c("statistical_analysis"),
#'   infrastructure = character()
#' )
#'
#' new_script(
#'   name = "fit_model",
#'   script_type = "statistical_analysis",
#'   root = root,
#'   open = FALSE
#' )
#'
#' new_output(
#'   name = "model_fit",
#'   type = "model",
#'   path = "outputs/models/model_fit.rds",
#'   root = root
#' )
#' }
#'
#' @author Thiago de Paula Oliveira
#' @export
new_script <- function(name,
                       script_type = "analysis",
                       root = ".",
                       open = interactive(),
                       overwrite = FALSE) {
  new_project_script(
    name = name,
    script_type = script_type,
    root = root,
    open = open,
    overwrite = overwrite
  )
}

#' Create and register a new project report
#'
#' @description
#' \code{new_report()} adds a Quarto report to an existing \pkg{projflow}
#' project and records it in the project registry. It is the high-level report
#' creation helper for analysis reports, client-facing reports, and scientific
#' reports.
#'
#' @details
#' The function combines report-file creation with any registry updates required
#' by the selected report type.
#'
#' Supported report types are:
#' \itemize{
#'   \item \code{"html_report"}: creates or ensures an HTML-oriented report
#'     workflow. If the project does not already include the report component or
#'     HTML report deliverable, they are added.
#'   \item \code{"client_report"}: creates or ensures a client-report
#'     deliverable. The built-in \file{reports/client_report.qmd} file is used
#'     when the default name is requested.
#'   \item \code{"scientific_report"}: creates a scientific report scaffold and
#'     ensures that table and figure components are available, because scientific
#'     reports usually depend on structured tabular and graphical outputs.
#' }
#'
#' Reports are written as \file{.qmd} files. Rendering is handled separately by
#' \code{\link{build_project}()} or \code{\link{serve_project}()}, depending
#' on whether the user wants a full build or an interactive preview.
#'
#' @param name Character scalar. Report name. The value is used as the filename
#'   stem for non-default reports. For example, \code{name = "analysis_report"}
#'   creates \file{reports/analysis_report.qmd} unless a built-in report path is
#'   returned.
#' @param type Character scalar. Report type shortcut. One of
#'   \code{"html_report"}, \code{"client_report"}, or
#'   \code{"scientific_report"}. The type determines which components or
#'   deliverables are added before the report file is created.
#' @param root Character scalar. Path inside an existing \pkg{projflow} project.
#'   The project root is located before the report is added.
#' @param open Logical scalar. Retained for user-interface compatibility. In the
#'   current implementation, the report is written or registered but is not opened
#'   automatically by this wrapper.
#' @param overwrite Logical scalar. If \code{TRUE}, an existing report file and
#'   corresponding registry entry may be replaced where supported.
#' @param repair Logical scalar. If \code{TRUE}, register an existing report file
#'   without overwriting it. This is useful when a report exists on disk but is
#'   missing from the registry.
#' @param dry_run Logical scalar. If \code{TRUE}, return the planned change
#'   without writing files or modifying the registry.
#'
#' @return
#' Invisibly returns the path to the created, existing, or registered report file.
#' When \code{dry_run = TRUE}, returns the dry-run object produced by the
#' lower-level report or deliverable helper.
#'
#' @seealso
#' \code{\link{new_script}()}, \code{\link{new_app}()},
#' \code{\link{build_project}()}, \code{\link{serve_project}()}
#'
#' @examples
#' \dontrun{
#' root <- tempfile("projflow-report-")
#' new_project(
#'   path = root,
#'   components = c("statistical_analysis", "report"),
#'   infrastructure = character()
#' )
#'
#' new_report("main_report", type = "html_report", root = root, open = FALSE)
#' new_report("scientific_summary", type = "scientific_report", root = root)
#' }
#'
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

#' Create a project Shiny application or Quarto dashboard
#'
#' @description
#' \code{new_app()} adds an interactive project interface to an existing
#' \pkg{projflow} project. The interface can be a Shiny application or a Quarto
#' dashboard, depending on the requested \code{type}.
#'
#' @details
#' Interactive interfaces are useful for project diagnostics, operational
#' monitoring, reporting, and communication with collaborators. This function
#' creates only the scaffold and registry entries. It does not launch the
#' application or render the dashboard; use \code{\link{serve_project}()} for
#' previewing and \code{\link{build_project}()} for full project execution.
#'
#' Supported application types are:
#' \itemize{
#'   \item \code{"shiny"}: adds the \code{"shiny_app"} project component and
#'     returns the expected \file{app/app.R} path. The application can later be
#'     launched through \code{\link[shiny:runApp]{shiny::runApp}()} by
#'     \code{\link{serve_project}()}.
#'   \item \code{"quarto_dashboard"}: adds the dashboard deliverable and creates
#'     a \file{.qmd} dashboard file when a non-default dashboard name is
#'     requested.
#' }
#'
#' @param name Character scalar. Application or dashboard name. For Quarto
#'   dashboards, non-default names are used as the \file{.qmd} filename stem.
#'   For Shiny applications, the standard application path is \file{app/app.R}.
#' @param type Character scalar. Type of interactive interface to create. Use
#'   \code{"shiny"} for a Shiny application or \code{"quarto_dashboard"} for a
#'   Quarto dashboard.
#' @param root Character scalar. Path inside an existing \pkg{projflow} project.
#'   The project root is located before the application or dashboard is added.
#' @param open Logical scalar. Retained for user-interface compatibility. In the
#'   current implementation, the created file is not opened automatically by this
#'   wrapper.
#' @param overwrite Logical scalar. If \code{TRUE}, allow an existing dashboard
#'   or application scaffold to be replaced where supported.
#' @param dry_run Logical scalar. If \code{TRUE}, return the planned change
#'   without writing files or modifying the registry.
#'
#' @return
#' Invisibly returns the expected or created path for the Shiny application or
#' Quarto dashboard. With \code{dry_run = TRUE}, returns a dry-run registry
#' action or project-plan object where supported.
#'
#' @seealso
#' \code{\link{new_report}()}, \code{\link{new_component}()},
#' \code{\link{serve_project}()}, \code{\link{build_project}()}
#'
#' @examples
#' \dontrun{
#' root <- tempfile("projflow-app-")
#' new_project(
#'   path = root,
#'   components = c("statistical_analysis"),
#'   infrastructure = character()
#' )
#'
#' new_app(type = "shiny", root = root, open = FALSE)
#' new_app(name = "operations_dashboard", type = "quarto_dashboard", root = root)
#' }
#'
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
#' @description
#' \code{new_component()} extends an existing \pkg{projflow} project with one
#' additional analytical or organisational component.
#'
#' @details
#' A component is a standard project capability, such as tables, figures,
#' reporting, dashboards, Shiny applications, project management, data
#' governance, validation, or a specific analysis phase. Adding a component may
#' create folders, template scripts, reports, registry entries, expected outputs,
#' and package suggestions.
#'
#' Component dependencies are handled by the lower-level project planning logic.
#' For example, adding a report-related component may require report folders and
#' deliverables; adding table or figure components may require output folders.
#' Unknown components are rejected to avoid inconsistent project registries.
#'
#' This function modifies an existing project unless \code{dry_run = TRUE}.
#' Use \code{\link{plan_project}()} to preview a complete new project scaffold
#' before creating a project.
#'
#' @param component Character scalar. Component name to add. Examples include
#'   \code{"tables"}, \code{"figures"}, \code{"report"},
#'   \code{"dashboard"}, \code{"shiny_app"},
#'   \code{"project_management"}, \code{"data_governance"}, and
#'   \code{"validation"}. The exact accepted values are defined by the installed
#'   package version.
#' @param root Character scalar. Path inside an existing \pkg{projflow} project.
#'   The project root is located before the component is added.
#' @param open Logical scalar. Retained for user-interface compatibility. Added
#'   files are not opened automatically by this wrapper.
#' @param overwrite Logical scalar. If \code{TRUE}, allow template files added by
#'   the component to replace existing files where supported.
#' @param dry_run Logical scalar. If \code{TRUE}, return the planned project
#'   amendment without writing files or modifying the registry.
#'
#' @return
#' A scaffold result object produced by the component-addition helper. The object
#' records the files, folders, registry entries, and checks associated with the
#' requested component. With \code{dry_run = TRUE}, the returned object describes
#' the planned amendment without applying it.
#'
#' @seealso
#' \code{\link{plan_project}()}, \code{\link{new_script}()},
#' \code{\link{new_report}()}, \code{\link{new_app}()}
#'
#' @examples
#' \dontrun{
#' root <- tempfile("projflow-component-")
#' new_project(path = root, infrastructure = character())
#'
#' new_component("tables", root = root, open = FALSE)
#' new_component("project_management", root = root, dry_run = TRUE)
#' }
#'
#' @author Thiago de Paula Oliveira
#' @export
new_component <- function(component, root = ".", open = interactive(), overwrite = FALSE, dry_run = FALSE) {
  add_project_component(component = component, root = root, open = open, overwrite = overwrite, dry_run = dry_run)
}

#' Register a table output
#'
#' @description
#' \code{new_table()} registers a table output in an existing \pkg{projflow}
#' project. It is a convenience wrapper around \code{\link{new_output}()} with
#' \code{type = "table"}.
#'
#' @details
#' Registered table outputs make expected analysis products explicit. They can be
#' used by project diagnostics, project status summaries, reports, and build
#' workflows to distinguish expected tabular outputs from incidental files.
#'
#' If \code{path = NULL}, the package chooses the default table-output location
#' for the project. If \code{path} is supplied, it should usually be
#' project-relative so that the registry remains portable across machines.
#'
#' @param name Character scalar. Output name to store in the project registry.
#'   The value should be stable, descriptive, and suitable for use as a
#'   project-object identifier.
#' @param path Optional character scalar. Explicit project-relative output path.
#'   If \code{NULL}, a default path is inferred for a table output.
#' @param root Character scalar. Path inside an existing \pkg{projflow} project.
#'   The project root is located before the output is registered.
#' @param overwrite Logical scalar. If \code{TRUE}, allow an existing output
#'   registration with the same name to be replaced where supported.
#' @param repair Logical scalar. If \code{TRUE}, register an existing output path
#'   without attempting to create or overwrite the file.
#' @param dry_run Logical scalar. If \code{TRUE}, return the planned registry
#'   change without modifying the project.
#'
#' @return
#' Invisibly returns the registered table-output path, or returns a dry-run plan
#' when \code{dry_run = TRUE}.
#'
#' @seealso
#' \code{\link{new_figure}()}, \code{\link{new_output}()},
#' \code{\link{new_script}()}
#'
#' @examples
#' \dontrun{
#' root <- tempfile("projflow-table-")
#' new_project(path = root, components = "tables", infrastructure = character())
#'
#' new_table("summary_statistics", root = root)
#' new_table("model_coefficients", path = "outputs/tables/model_coefficients.csv", root = root)
#' }
#'
#' @author Thiago de Paula Oliveira
#' @export
new_table <- function(name, path = NULL, root = ".", overwrite = FALSE, repair = FALSE, dry_run = FALSE) {
  new_project_output(name = name, type = "table", path = path, root = root, overwrite = overwrite, repair = repair, dry_run = dry_run)
}

#' Register a figure output
#'
#' @description
#' \code{new_figure()} registers a figure output in an existing \pkg{projflow}
#' project. It is a convenience wrapper around \code{\link{new_output}()} with
#' \code{type = "figure"}.
#'
#' @details
#' Registered figure outputs make graphical products explicit in the project
#' registry. This is useful for report generation, project diagnostics, quality
#' control, and reproducibility review.
#'
#' If \code{path = NULL}, the package chooses the default figure-output
#' location for the project. If \code{path} is supplied, it should usually be
#' project-relative so that the registry remains portable across machines.
#'
#' @param name Character scalar. Output name to store in the project registry.
#'   The value should be stable, descriptive, and suitable for use as a
#'   project-object identifier.
#' @param path Optional character scalar. Explicit project-relative output path.
#'   If \code{NULL}, a default path is inferred for a figure output.
#' @param root Character scalar. Path inside an existing \pkg{projflow} project.
#'   The project root is located before the output is registered.
#' @param overwrite Logical scalar. If \code{TRUE}, allow an existing output
#'   registration with the same name to be replaced where supported.
#' @param repair Logical scalar. If \code{TRUE}, register an existing output path
#'   without attempting to create or overwrite the file.
#' @param dry_run Logical scalar. If \code{TRUE}, return the planned registry
#'   change without modifying the project.
#'
#' @return
#' Invisibly returns the registered figure-output path, or returns a dry-run plan
#' when \code{dry_run = TRUE}.
#'
#' @seealso
#' \code{\link{new_table}()}, \code{\link{new_output}()},
#' \code{\link{new_script}()}
#'
#' @examples
#' \dontrun{
#' root <- tempfile("projflow-figure-")
#' new_project(path = root, components = "figures", infrastructure = character())
#'
#' new_figure("heritability_plot", root = root)
#' new_figure("diagnostic_plot", path = "outputs/figures/diagnostic_plot.png", root = root)
#' }
#'
#' @author Thiago de Paula Oliveira
#' @export
new_figure <- function(name, path = NULL, root = ".", overwrite = FALSE, repair = FALSE, dry_run = FALSE) {
  new_project_output(name = name, type = "figure", path = path, root = root, overwrite = overwrite, repair = repair, dry_run = dry_run)
}

#' Register a project output
#'
#' @description
#' \code{new_output()} registers a named output object in an existing
#' \pkg{projflow} project. It is the general output-registration helper used by
#' \code{\link{new_table}()} and \code{\link{new_figure}()}.
#'
#' @details
#' Outputs are expected products of the analysis workflow. Registering them makes
#' the project structure explicit: the registry can record what should be
#' produced, where it should be located, and which object type it represents.
#'
#' The \code{type} argument should describe the output class. Common values
#' include \code{"output"}, \code{"table"}, \code{"figure"},
#' \code{"dataset"}, \code{"model"}, and \code{"report"}. Package-level
#' validation may restrict the accepted set.
#'
#' Prefer project-relative paths for \code{path}. Absolute paths make the
#' registry harder to share across operating systems and collaborators.
#'
#' @param name Character scalar. Output name to store in the project registry.
#'   The value should be stable, descriptive, and suitable for use as a
#'   project-object identifier.
#' @param type Character scalar. Output type recorded in the registry. The
#'   default, \code{"output"}, is generic. Use more specific values such as
#'   \code{"table"}, \code{"figure"}, \code{"dataset"}, or \code{"model"}
#'   when appropriate.
#' @param path Optional character scalar. Explicit project-relative output path.
#'   If \code{NULL}, a default path is inferred from \code{name} and
#'   \code{type}.
#' @param root Character scalar. Path inside an existing \pkg{projflow} project.
#'   The project root is located before the output is registered.
#' @param overwrite Logical scalar. If \code{TRUE}, allow an existing output
#'   registration with the same name to be replaced where supported.
#' @param repair Logical scalar. If \code{TRUE}, register an existing output path
#'   without attempting to create or overwrite the file.
#' @param dry_run Logical scalar. If \code{TRUE}, return the planned registry
#'   change without modifying the project.
#'
#' @return
#' Invisibly returns the registered output path, or returns a dry-run plan when
#' \code{dry_run = TRUE}.
#'
#' @seealso
#' \code{\link{new_table}()}, \code{\link{new_figure}()},
#' \code{\link{new_script}()}
#'
#' @examples
#' \dontrun{
#' root <- tempfile("projflow-output-")
#' new_project(path = root, components = "statistical_analysis", infrastructure = character())
#'
#' new_output("model_fit", type = "model", root = root)
#' new_output("clean_dataset", type = "dataset", path = "outputs/data/clean_dataset.rds", root = root)
#' }
#'
#' @author Thiago de Paula Oliveira
#' @export
new_output <- function(name, type = "output", path = NULL, root = ".", overwrite = FALSE, repair = FALSE, dry_run = FALSE) {
  new_project_output(name = name, type = type, path = path, root = root, overwrite = overwrite, repair = repair, dry_run = dry_run)
}

#' Build a project workflow
#'
#' @description
#' \code{build_project()} executes the registered project workflow and, if
#' requested, renders registered reports. It is the high-level build command for
#' an existing \pkg{projflow} project.
#'
#' @details
#' A build has up to three phases:
#' \itemize{
#'   \item registered scripts are run in the order stored in the project
#'     registry;
#'   \item registered reports are rendered when \code{render_reports = TRUE};
#'   \item an interactive app or dashboard is launched when
#'     \code{run_apps = TRUE}.
#' }
#'
#' If the project contains the \code{"project_management"} component, the
#' function also collects a structured status report after the workflow has run.
#'
#' This function is intended for reproducible project execution. It may run user
#' analysis code and may overwrite outputs created by those scripts. Review the
#' project registry and scripts before using it in important directories.
#'
#' @param root Character scalar. Path inside an existing \pkg{projflow} project.
#'   The project root is located before the workflow is run.
#' @param render_reports Logical scalar. If \code{TRUE}, render registered
#'   reports after registered scripts have been executed. If \code{FALSE}, only
#'   the registered scripts are run.
#' @param run_apps Logical scalar. If \code{TRUE}, call
#'   \code{\link{serve_project}()} after the build completes. This can launch a
#'   Shiny application or preview a dashboard, depending on the project contents.
#'
#' @return
#' Invisibly returns a list with build results:
#' \describe{
#'   \item{\code{root}}{Resolved project root.}
#'   \item{\code{rendered}}{Report-rendering results, or \code{NULL} when
#'     \code{render_reports = FALSE}.}
#'   \item{\code{status}}{Project-management status data, or \code{NULL} when
#'     the project does not contain the project-management component.}
#' }
#'
#' @seealso
#' \code{\link{serve_project}()}, \code{\link{new_script}()},
#' \code{\link{new_report}()}, \code{\link{new_app}()}
#'
#' @examples
#' \dontrun{
#' build_project(root = ".", render_reports = FALSE)
#' build_project(root = ".", render_reports = TRUE, run_apps = FALSE)
#' }
#'
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

#' Serve or preview a project interactively
#'
#' @description
#' \code{serve_project()} launches or previews the most appropriate interactive
#' target for an existing \pkg{projflow} project. Depending on \code{target}
#' and the files present in the project, it can launch a Shiny application,
#' render a Quarto dashboard, render reports, or run a one-shot project build.
#'
#' @details
#' The function resolves \code{target = "auto"} using the following priority:
#' \itemize{
#'   \item if \file{app/app.R} exists, serve the Shiny application;
#'   \item otherwise, if \file{dashboard/dashboard.qmd} exists, preview the
#'     dashboard;
#'   \item otherwise, if registered or discoverable report files exist, render or
#'     return the report target;
#'   \item otherwise, run a one-shot project build.
#' }
#'
#' Continuous watch mode is not currently implemented. If \code{watch = TRUE},
#' the function emits an informational message and performs a single preview or
#' build action.
#'
#' For Shiny applications, the \pkg{shiny} package must be installed because the
#' app is launched using \code{\link[shiny:runApp]{shiny::runApp}()}. For
#' Quarto dashboards and reports, rendering depends on the package's Quarto
#' rendering helpers and on a working Quarto installation where required by the
#' project.
#'
#' @param root Character scalar. Path inside an existing \pkg{projflow} project.
#'   The project root is located before any target is served or rendered.
#' @param target Character scalar. Target to serve or preview. Use
#'   \code{"auto"} to let the function choose from the project contents,
#'   \code{"reports"} to render or return report information,
#'   \code{"shiny_app"} to launch \file{app/app.R}, \code{"dashboard"} to
#'   render or return the dashboard path, or \code{"project"} to run a one-shot
#'   build.
#' @param watch Logical scalar. Retained for future interactive workflows.
#'   Continuous watching is not currently implemented; when \code{TRUE}, the
#'   function performs a single preview or build and reports that watch mode is
#'   unavailable.
#' @param render Logical scalar. If \code{TRUE}, reports or dashboards are
#'   rendered as part of the preview step. If \code{FALSE}, the function returns
#'   the relevant path or project status without rendering where supported.
#'
#' @return
#' Invisibly returns the launched target path, rendered report result, project
#' status object, or build result, depending on the resolved target:
#' \itemize{
#'   \item \code{"shiny_app"}: path to \file{app/app.R};
#'   \item \code{"dashboard"}: dashboard render result or dashboard path;
#'   \item \code{"reports"}: report-rendering result or project status;
#'   \item \code{"project"}: result from \code{\link{build_project}()}.
#' }
#'
#' @seealso
#' \code{\link{build_project}()}, \code{\link{new_app}()},
#' \code{\link{new_report}()},
#' \code{\link[shiny:runApp]{shiny::runApp}()}
#'
#' @examples
#' \dontrun{
#' serve_project(root = ".", target = "reports", render = TRUE)
#' serve_project(root = ".", target = "dashboard", render = FALSE)
#' serve_project(root = ".", target = "shiny_app", render = FALSE)
#' }
#'
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
