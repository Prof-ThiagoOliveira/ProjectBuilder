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
    "optional packages that are not installed"
  )
})
