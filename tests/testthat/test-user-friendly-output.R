test_that("project_layers prints a compact user-facing summary", {
  layers <- project_layers()

  expect_s3_class(layers, "projflow_layers")
  expect_s3_class(layers, "data.frame")
  expect_named(layers, c("layer", "name", "responsibility", "primary_functions"))

  output <- capture.output(print(layers))
  expect_true(any(grepl("projflow architecture layers", output, fixed = TRUE)))
  expect_true(any(grepl("Layer 1: project_structure", output, fixed = TRUE)))
  expect_true(any(grepl("Primary functions:", output, fixed = TRUE)))
  expect_true(any(grepl("as.data.frame", output, fixed = TRUE)))
})

test_that("project_structure prints a compact checklist", {
  project_path <- make_project_path("structure-print")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  structure <- project_structure(project_path)

  expect_s3_class(structure, "projflow_structure")
  expect_s3_class(structure, "data.frame")
  expect_named(
    structure,
    c(
      "role", "path", "layer", "exists", "registered_scripts",
      "registered_reports", "registered_outputs"
    )
  )
  expect_true(all(structure$exists))

  output <- capture.output(print(structure))
  expect_true(any(grepl("projflow project structure", output, fixed = TRUE)))
  expect_true(any(grepl("Registered objects:", output, fixed = TRUE)))
  expect_true(any(grepl("Structure checklist:", output, fixed = TRUE)))
  expect_true(any(grepl("[ok", output, fixed = TRUE)))
  expect_true(any(grepl("as.data.frame", output, fixed = TRUE)))
})

test_that("setup_project prints a compact runtime summary", {
  project_path <- make_project_path("setup-print")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  status <- setup_project(project_path, install_missing = FALSE)

  expect_s3_class(status, "projflow_setup")
  expect_named(status, c("root", "config", "paths", "registry", "packages"))

  output <- capture.output(print(status))
  expect_true(any(grepl("projflow project setup", output, fixed = TRUE)))
  expect_true(any(grepl("Packages:", output, fixed = TRUE)))
  expect_true(any(grepl("Runtime settings:", output, fixed = TRUE)))
  expect_true(any(grepl("Working paths:", output, fixed = TRUE)))
  expect_true(any(grepl("project_structure", output, fixed = TRUE)))
})

test_that("start_project prints a guided setup summary with next action", {
  project_path <- make_project_path("start-project-print")

  result <- start_project(
    path = project_path,
    type = "analysis",
    data_location = "external",
    use_quarto = FALSE,
    use_renv = FALSE,
    use_git = TRUE,
    use_github_actions = FALSE,
    open = FALSE
  )

  output <- capture.output(print(result))
  expect_true(any(grepl("projflow started project", output, fixed = TRUE)))
  expect_true(any(grepl("Workflow: Analysis report", output, fixed = TRUE)))
  expect_true(any(grepl("Reports: R Markdown", output, fixed = TRUE)))
  expect_true(any(grepl("Starter files:", output, fixed = TRUE)))
  expect_true(any(grepl("Next action:", output, fixed = TRUE)))
  expect_true(any(grepl("set_project_data_root", output, fixed = TRUE)))
})

test_that("project package checks print a compact summary", {
  project_path <- make_project_path("package-print")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  status <- check_project_packages(project_path)

  expect_s3_class(status, "project_package_check")

  output <- capture.output(print(status))
  expect_true(any(grepl("projflow project packages", output, fixed = TRUE)))
  expect_true(any(grepl("Status:", output, fixed = TRUE)))
  expect_true(any(grepl("Declared:", output, fixed = TRUE)))
  expect_true(any(grepl("Missing:", output, fixed = TRUE)))
})

test_that("governance tables print compact summaries", {
  project_path <- make_project_path("governance-print")

  new_project(
    path = project_path,
    components = "project_management",
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  add_project_task("Review model diagnostics", root = project_path)
  add_project_risk("External data dictionary may change", root = project_path)
  add_project_decision("Use REML for the mixed model", root = project_path)
  add_project_milestone("Draft report complete", root = project_path)

  tasks <- project_tasks(project_path)
  risks <- project_risks(project_path)
  decisions <- project_decisions(project_path)
  milestones <- project_milestones(project_path)

  expect_s3_class(tasks, "projflow_tasks")
  expect_s3_class(risks, "projflow_risks")
  expect_s3_class(decisions, "projflow_decisions")
  expect_s3_class(milestones, "projflow_milestones")

  expect_true(any(grepl("projflow project tasks", capture.output(print(tasks)), fixed = TRUE)))
  expect_true(any(grepl("projflow project risks", capture.output(print(risks)), fixed = TRUE)))
  expect_true(any(grepl("projflow project decisions", capture.output(print(decisions)), fixed = TRUE)))
  expect_true(any(grepl("projflow project milestones", capture.output(print(milestones)), fixed = TRUE)))
  expect_true(any(grepl("Preview:", capture.output(print(tasks)), fixed = TRUE)))
  expect_true(any(grepl("as.data.frame", capture.output(print(risks)), fixed = TRUE)))
})

test_that("project_status_report prints a compact summary and preserves text modes", {
  project_path <- make_project_path("status-report-print")

  new_project(
    path = project_path,
    components = c("statistical_analysis", "project_management"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  add_project_task("Review model diagnostics", root = project_path)
  add_project_risk("External data dictionary may change", root = project_path)
  add_project_decision("Use REML for the mixed model", root = project_path)
  add_project_milestone("Draft report complete", root = project_path)

  markdown <- project_status_report(project_path)
  data <- project_status_report(project_path, output = "data")
  html <- project_status_report(project_path, output = "html")

  expect_s3_class(markdown, "projflow_status_report_markdown")
  expect_s3_class(data, "projflow_status_report")
  expect_s3_class(html, "projflow_status_report_html")
  expect_type(unclass(markdown), "character")
  expect_type(unclass(html), "character")

  markdown_output <- capture.output(print(markdown))
  data_output <- capture.output(print(data))

  expect_true(any(grepl("^# Project Status Report", markdown_output)))
  expect_true(any(grepl("## Overview", markdown_output, fixed = TRUE)))
  expect_true(any(grepl("projflow project status report", data_output, fixed = TRUE)))
  expect_true(any(grepl("Open tasks:", data_output, fixed = TRUE)))
  expect_true(any(grepl("Open risks:", data_output, fixed = TRUE)))
  expect_true(any(grepl("project_tasks()", data_output, fixed = TRUE)))
})

test_that("project diagnostics print a compact health summary", {
  project_path <- make_project_path("diagnostics-print")

  new_project(
    path = project_path,
    components = c("data_preparation", "statistical_analysis", "report", "project_management"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  add_project_task("Review model diagnostics", root = project_path)
  add_project_risk("External data dictionary may change", root = project_path)
  add_project_decision("Use REML for the mixed model", root = project_path)
  add_project_milestone("Draft report complete", root = project_path)
  new_output("model_summary", type = "table", path = "outputs/tables/model_summary.csv", root = project_path)

  diagnostics <- diagnose_project(project_path, output = "data")

  expect_s3_class(diagnostics, "projflow_diagnostics")

  output <- capture.output(print(diagnostics))
  expect_true(any(grepl("projflow diagnostics", output, fixed = TRUE)))
  expect_true(any(grepl("Health summary:", output, fixed = TRUE)))
  expect_true(any(grepl("Priority findings:", output, fixed = TRUE)))
  expect_true(any(grepl("Output drift:", output, fixed = TRUE)))
  expect_true(any(grepl("Recent activity:", output, fixed = TRUE)))
  expect_true(any(grepl("project_status_report()", output, fixed = TRUE)))
})

test_that("project_diagnostics_app checks dashboard dependencies before building the app", {
  project_path <- make_project_path("diagnostics-app-guard")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  checked <- FALSE
  local_mocked_bindings(
    check_dashboard_dependencies = function(...) {
      checked <<- TRUE
      invisible(TRUE)
    },
    project_manager_app = function(...) "app_object",
    .package = "projflow"
  )

  app <- project_diagnostics_app(project_path, launch = FALSE)
  expect_true(checked)
  expect_identical(app, "app_object")
})

test_that("serve_project reports missing shiny dependency with install guidance", {
  project_path <- make_project_path("serve-shiny-guard")

  new_project(
    path = project_path,
    components = "shiny_app",
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  local_mocked_bindings(
    missing_optional_packages = function(packages) {
      packages[packages == "shiny"]
    },
    .package = "projflow"
  )

  expect_error(
    serve_project(project_path, target = "shiny_app", render = FALSE),
    "Shiny app serving requires the following optional package"
  )
})
