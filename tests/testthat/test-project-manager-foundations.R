test_that("project_diagnostics_data returns structured read-only metadata", {
  project_path <- make_project_path("diagnostics-foundation")

  new_project(
    path = project_path,
    components = c("statistical_analysis", "project_management"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  before <- list.files(file.path(project_path, ".projflow"), recursive = TRUE)
  diagnostics <- project_diagnostics_data(project_path, include_network = TRUE)
  after <- list.files(file.path(project_path, ".projflow"), recursive = TRUE)

  expect_s3_class(diagnostics, "projflow_diagnostics")
  expect_true(all(c("project", "summary", "checks", "outputs", "network", "activity") %in% names(diagnostics)))
  expect_identical(before, after)
})

test_that("activity log entries are appended for governance writes", {
  project_path <- make_project_path("activity-log")

  new_project(
    path = project_path,
    components = c("project_management"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  task_id <- add_project_task("Review diagnostics", root = project_path)
  mark_project_task_done(task_id, root = project_path)

  activity <- list_project_activity(project_path)

  expect_true(nrow(activity) >= 2L)
  expect_true(any(activity$action == "add_task"))
  expect_true(any(activity$action == "update_task"))
})

test_that("registry backups are created before registry writes", {
  project_path <- make_project_path("registry-backups")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  new_output("model_fit", type = "model", root = project_path)
  backups <- list_project_backups(project_path)

  expect_true(any(grepl("^project_registry_", backups$name)))
})

test_that("diagnose_project can write a static HTML report", {
  project_path <- make_project_path("diagnostics-html")

  new_project(
    path = project_path,
    components = c("statistical_analysis", "project_management"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  add_project_task("<b>Escaped task</b>", root = project_path)
  output_file <- diagnose_project(project_path, output = "html")

  expect_true(file.exists(output_file))
  html <- readLines(output_file, warn = FALSE)
  expect_true(any(grepl("&lt;b&gt;Escaped task&lt;/b&gt;", html, fixed = TRUE)))
})

test_that("dashboard dependency checks report missing packages clearly", {
  expect_error(
    check_dashboard_dependencies(c("definitelyMissingProjflowTestPackage")),
    "projflow dashboard requires the following optional package"
  )
})

test_that("project manager app can be constructed without launching", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("bslib")

  project_path <- make_project_path("project-manager-app-construction")

  new_project(
    path = project_path,
    components = c("statistical_analysis", "project_management"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  app <- project_manager_app(project_path, mode = "manage")
  testthat::expect_s3_class(app, "shiny.appobj")
})

test_that("dashboard table helper tolerates NULL diagnostics tables", {
  testthat::expect_s3_class(dashboard_safe_data_frame(NULL), "data.frame")
  testthat::expect_equal(nrow(dashboard_safe_data_frame(NULL)), 0L)
})

test_that("dashboard date helpers tolerate list and mixed date values", {
  values <- list("2026-05-09", NA_character_, "")
  parsed <- dashboard_as_date_vector(values)
  expect_s3_class(parsed, "Date")
  expect_equal(sum(!is.na(parsed)), 1L)
  expect_equal(dashboard_count_with_dates(data.frame(due_date = I(values)), "due_date"), 1L)
})

test_that("support documentation files are not reported as registry candidates", {
  project_path <- make_project_path("support-files-not-orphans")

  new_project(
    path = project_path,
    components = c("statistical_analysis", "project_management"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  writeLines("support", file.path(project_path, "README.md"))
  dir.create(file.path(project_path, "docs"), showWarnings = FALSE)
  writeLines("support", file.path(project_path, "docs", "status.md"))

  diagnostics <- project_diagnostics_data(project_path)
  expect_false("README.md" %in% diagnostics$orphan_files$path)
  expect_false("docs/status.md" %in% diagnostics$orphan_files$path)
})


test_that("new project does not create synthetic plan tasks", {
  project_path <- make_project_path("no-synthetic-plan-tasks")

  new_project(
    path = project_path,
    components = c("data_preparation", "statistical_analysis", "report", "project_management"),
    deliverables = c("html_report", "manuscript", "status_report"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  tasks <- project_tasks(project_path)
  expect_equal(nrow(tasks), 0L)
})

test_that("dashboard object subtype choices prevent invalid creation arguments", {
  expect_equal(dashboard_object_subtype("report", NULL), "html_report")
  expect_equal(dashboard_object_subtype("report", "scientific_report"), "scientific_report")
  expect_error(dashboard_object_subtype("report", "report"), "Invalid Report type")

  expect_equal(dashboard_object_subtype("app", NULL), "shiny")
  expect_equal(dashboard_object_subtype("script", "statistical_analysis"), "statistical_analysis")
  expect_null(dashboard_object_subtype("task", NULL))
})

test_that("planning chart data are structured for hierarchical visualisation", {
  project_path <- make_project_path("planning-chart-data")

  new_project(
    path = project_path,
    components = c("data_preparation", "statistical_analysis", "report"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  diagnostics <- project_diagnostics_data(project_path, include_network = TRUE)
  wbs <- dashboard_wbs_data(diagnostics, max_items = 20L)
  pert <- dashboard_pert_data(diagnostics, network = diagnostics$network, max_items = 20L)

  expect_true(all(c("id", "label", "group", "level") %in% names(wbs$nodes)))
  expect_true(all(c("from", "to") %in% names(wbs$edges)))
  expect_s3_class(pert$nodes, "data.frame")
  expect_s3_class(pert$edges, "data.frame")
})

test_that("dashboard management helpers are exported", {
  exports <- getNamespaceExports("projflow")

  expect_true("open_dashboard" %in% exports)
  expect_true("stop_dashboard" %in% exports)
  expect_true("dashboard_status" %in% exports)
})

test_that("dashboard_status reports no recorded dashboard gracefully", {
  project_path <- make_project_path("dashboard-status-empty")
  new_project(project_path, infrastructure = character(), open = FALSE)

  status <- dashboard_status(project_path)

  expect_true(is.data.frame(status))
  expect_false(status$running[[1]])
  expect_true(is.na(status$pid[[1]]))
})
