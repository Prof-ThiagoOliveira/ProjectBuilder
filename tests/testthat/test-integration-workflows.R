test_that("project object lifecycle helpers keep registry and files consistent", {
  project_path <- make_project_path("object-lifecycle")

  new_project(
    path = project_path,
    preset = "basic_analysis",
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  expect_no_error(new_script("analysis_01", type = "statistical_analysis", root = project_path, open = FALSE))
  expect_no_error(new_report("supplementary_note", root = project_path, open = FALSE))
  expect_no_error(new_table("summary_statistics", root = project_path))
  expect_no_error(new_figure("heritability_plot", root = project_path))
  expect_no_error(new_app(root = project_path, type = "shiny", open = FALSE))
  expect_no_error(new_output("model_fit", type = "model", path = "outputs/models/model_fit.rds", root = project_path))

  expect_true(file.exists(file.path(project_path, "analysis", "analysis_01.R")))
  expect_true(file.exists(file.path(project_path, "reports", "supplementary_note.qmd")))
  expect_true(file.exists(file.path(project_path, "app", "app.R")))
  expect_false(file.exists(file.path(project_path, "outputs", "analysis_01.rds")))

  registry <- read_project_registry(project_path)
  expect_true(all(c("analysis_01", "summary_statistics", "heritability_plot", "model_fit") %in% c(names(registry$scripts), names(registry$outputs))))

  rename_plan <- rename_project_script("analysis_01", "analysis_02", root = project_path, dry_run = TRUE)
  expect_identical(rename_plan$new_name, "analysis_02")
  expect_true(file.exists(file.path(project_path, "analysis", "analysis_01.R")))

  expect_no_error(rename_project_script("analysis_01", "analysis_02", root = project_path))
  expect_false(file.exists(file.path(project_path, "analysis", "analysis_01.R")))
  expect_true(file.exists(file.path(project_path, "analysis", "analysis_02.R")))
  expect_true("analysis_02" %in% names(read_project_registry(project_path)$scripts))

  remove_plan <- remove_project_output("summary_statistics", root = project_path, dry_run = TRUE)
  expect_identical(remove_plan$name, "summary_statistics")
  expect_true("summary_statistics" %in% names(read_project_registry(project_path)$outputs))

  expect_no_error(remove_project_output("summary_statistics", root = project_path))
  expect_false("summary_statistics" %in% names(read_project_registry(project_path)$outputs))

  expect_no_error(remove_project_script("analysis_02", root = project_path, delete_files = TRUE, confirm = TRUE))
  expect_false(file.exists(file.path(project_path, "analysis", "analysis_02.R")))
  expect_false("analysis_02" %in% names(read_project_registry(project_path)$scripts))
})

test_that("explicit output registration and repair are supported", {
  project_path <- make_project_path("output-repair")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  dir.create(file.path(project_path, "outputs", "models"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data.frame(x = 1), file.path(project_path, "outputs", "models", "manual_model.rds"))

  expect_no_error(
    new_output(
      "manual_model",
      type = "model",
      path = "outputs/models/manual_model.rds",
      root = project_path,
      repair = TRUE
    )
  )

  registry <- read_project_registry(project_path)
  expect_identical(registry$outputs$manual_model$path, "outputs/models/manual_model.rds")
})

test_that("diagnostics data is available without launching Shiny", {
  project_path <- make_project_path("diagnostics-data")

  new_project(
    path = project_path,
    components = c("data_preparation", "report", "project_management"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  new_output("expected_model", type = "model", root = project_path)
  add_project_risk("Missing data", root = project_path, status = "open", linked_objects = "data_preparation")
  add_project_milestone("Draft report", root = project_path, linked_objects = "html_report")

  diagnostics <- diagnose_project(project_path, output = "data")

  expect_s3_class(diagnostics, "projflow_diagnostics")
  expect_true(all(c("project", "summary", "checks", "registry", "outputs", "network") %in% names(diagnostics)))
  expect_true("info" %in% diagnostics$checks$severity)
  expect_true("warning" %in% diagnostics$checks$severity)
  expect_true(any(diagnostics$network$edges$relationship == "risk_to_component"))
  expect_true(any(diagnostics$network$edges$relationship == "milestone_to_deliverable"))
})

test_that("status report HTML escapes user-provided governance text", {
  project_path <- make_project_path("status-report-html")

  new_project(
    path = project_path,
    components = c("statistical_analysis", "project_management"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  add_project_task("<script>alert('x')</script>", root = project_path)
  add_project_risk("<b>Open risk</b>", root = project_path, status = "open")

  html <- project_status_report(project_path, output = "html")

  expect_false(any(grepl("<script>", html, fixed = TRUE)))
  expect_false(any(grepl("<b>Open risk</b>", html, fixed = TRUE)))
  expect_true(any(grepl("&lt;script&gt;", html, fixed = TRUE)))
})
