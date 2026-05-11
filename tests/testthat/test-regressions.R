test_that("generated qmd report contains one YAML front matter block", {
  project_path <- make_project_path("report-template-yaml")

  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )

  report <- readLines(file.path(project_path, "reports", "main_report.qmd"), warn = FALSE)

  yaml_starts <- which(report == "---")
  expect_length(yaml_starts, 2)
  expect_identical(yaml_starts, c(1L, 4L))
  expect_false(any(grepl("^---$", report[-c(1L, 4L)])))
})

test_that("new_script does not register or create an output by default", {
  project_path <- make_project_path("new-script-no-output")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  new_script("secondary_analysis", root = project_path, open = FALSE)

  registry <- yaml::read_yaml(file.path(project_path, ".projflow", "project_registry.yml"))

  expect_true("secondary_analysis" %in% names(registry$scripts))
  expect_false("secondary_analysis" %in% names(registry$outputs))

  build_project(project_path, render_reports = FALSE)

  expect_false(file.exists(file.path(project_path, "outputs", "secondary_analysis.rds")))
})

test_that("new_script creation and output registration are separate", {
  project_path <- make_project_path("new-script-explicit-output")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  new_script(
    name = "model_fit",
    script_type = "model",
    root = project_path,
    open = FALSE
  )
  new_output(
    name = "heritability_model",
    type = "model",
    path = "outputs/models/heritability_model.rds",
    root = project_path
  )

  registry <- yaml::read_yaml(file.path(project_path, ".projflow", "project_registry.yml"))

  expect_true("model_fit" %in% names(registry$scripts))
  expect_length(registry$scripts$model_fit$outputs, 0L)
  expect_true("heritability_model" %in% names(registry$outputs))
})

test_that("component output registry entries are files, not directories", {
  plan <- plan_project(
    path = tempfile(),
    components = c("statistical_analysis", "tables", "figures"),
    infrastructure = character()
  )

  output_paths <- vapply(plan$registry$outputs, `[[`, character(1), "path")

  expect_true(all(grepl("\\.[A-Za-z0-9]+$", output_paths)))
  expect_false(any(output_paths %in% c("outputs/tables", "outputs/figures", "outputs/qc", "outputs/diagnostics")))
})

test_that("default project with report does not imply tables or figures", {
  plan <- plan_project(
    path = tempfile(),
    components = c("statistical_analysis", "report"),
    infrastructure = character()
  )

  expect_true("report" %in% plan$components)
  expect_true("html_report" %in% plan$deliverables)

  expect_false("tables" %in% plan$components)
  expect_false("figures" %in% plan$components)
  expect_false("tables" %in% plan$deliverables)
  expect_false("figures" %in% plan$deliverables)

  script_paths <- vapply(plan$scripts, `[[`, character(1), "path")
  expect_false("analysis/2_summarise_results.R" %in% script_paths)
})

test_that("component script template creates all registered placeholder outputs when examples are enabled", {
  plan <- plan_project(
    path = tempfile(),
    components = c("tables", "figures"),
    infrastructure = character(),
    include_example = TRUE
  )

  script <- plan$scripts[[which(vapply(plan$scripts, `[[`, character(1), "name") == "summarise_results")]]
  txt <- base_script_template(script)

  expect_true(grepl("summary_tables", txt, fixed = TRUE))
  expect_true(grepl("summary_figures", txt, fixed = TRUE))
})

test_that("report rendering tests use mocked Quarto", {
  local_mock_quarto_render()

  project_path <- make_project_path("mocked-quarto-render")

  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )

  expect_no_error(build_project(project_path, render_reports = TRUE))
  expect_true(file.exists(file.path(project_path, "outputs", "reports", "main_report", "main_report.html")))
})

test_that("local_mock_quarto_render keeps options active during the test", {
  local_mock_quarto_render()

  expect_true(is.function(getOption("projflow.quarto_available")))
  expect_true(is.function(getOption("projflow.quarto_render")))
})

test_that("project_status has the expected class", {
  project_path <- make_project_path("project-status-class")
  new_project(project_path, infrastructure = character(), open = FALSE)

  status <- project_status(project_path)

  expect_s3_class(status, "project_status")
  expect_s3_class(status, "project_check")
})

test_that("dependency messages distinguish deliverables from components", {
  plan <- plan_project(
    path = tempfile(),
    components = "report",
    deliverables = "tables",
    infrastructure = character()
  )

  expect_true(any(grepl("deliverable `tables`", plan$checks, fixed = TRUE)))
})

test_that("check_project does not render reports unless requested", {
  local_mock_quarto_render()

  project_path <- make_project_path("check-no-render")
  new_project(project_path, infrastructure = character(), open = FALSE)

  status <- check_project(project_path, deep = TRUE, render_reports = FALSE)

  expect_s3_class(status, "project_check")
  expect_false(file.exists(file.path(project_path, "outputs", "reports", "main_report", "main_report.html")))
})

test_that("check_project succeeds for a built default project", {
  project_path <- make_project_path("check-built-default")

  new_project(project_path, infrastructure = character(), open = FALSE)
  build_project(project_path, render_reports = FALSE)

  status <- check_project(project_path, deep = FALSE)

  expect_true(status$ok)
})

test_that("project management helpers are exported", {
  exports <- getNamespaceExports("projflow")

  expect_true("add_project_task" %in% exports)
  expect_true("project_status_report" %in% exports)
})

test_that("script creation public API remains minimal", {
  removed_args <- c(
    "order",
    "output_names",
    "output_path",
    "script_template",
    "template_path",
    "template_text",
    "repair",
    "dry_run"
  )

  expect_false(any(removed_args %in% names(formals(new_script))))
  expect_true(all(c("name", "script_type", "root", "open", "overwrite") %in% names(formals(new_script))))
})

test_that("add_project_decision accepts a single user-facing statement", {
  project_path <- make_project_path("decision-single-statement")

  new_project(
    path = project_path,
    components = c("project_management"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  decision_id <- add_project_decision("Use REML for the mixed model", root = project_path)
  decisions <- project_decisions(project_path)

  expect_true(decision_id %in% decisions$id)
  expect_equal(decisions$title[[1]], "Use REML for the mixed model")
  expect_equal(decisions$decision[[1]], "Use REML for the mixed model")
})

test_that("dashboard data frames flatten list columns before rendering", {
  data <- data.frame(
    id = "task_0001",
    title = "Review model diagnostics",
    stringsAsFactors = FALSE
  )
  data$linked_objects <- I(list(c("analysis_results", "diagnostic_plot")))

  out <- dashboard_safe_data_frame(data)

  expect_s3_class(out, "data.frame")
  expect_type(out$linked_objects, "character")
  expect_equal(out$linked_objects[[1]], "analysis_results; diagnostic_plot")
})
