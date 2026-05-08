test_that("default scaffold is minimal and external-data oriented", {
  project_path <- make_project_path("simple-default")

  result <- new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )

  expect_s3_class(result, "analysis_project_scaffold")
  expect_true(result$scaffold_level %in% c("minimal", "simple"))

  expect_true(dir.exists(file.path(project_path, "analysis")))
  expect_true(dir.exists(file.path(project_path, "reports")))
  expect_true(dir.exists(file.path(project_path, "outputs")))
  expect_true(dir.exists(file.path(project_path, ".projectSetupR")))

  expect_false(dir.exists(file.path(project_path, "data")))
  expect_true(file.exists(file.path(project_path, "project.yml")))
  expect_true(file.exists(file.path(project_path, ".projectSetupR", "project_registry.yml")))
  expect_true(file.exists(file.path(project_path, ".projectSetupR", "local.yml")))
  expect_true(file.exists(file.path(project_path, "README.md")))
  expect_true(file.exists(file.path(project_path, "run_project.R")))
  expect_true(file.exists(file.path(project_path, ".gitignore")))
  expect_true(file.exists(file.path(project_path, "reports", "main_report.qmd")))
  expect_true(file.exists(file.path(project_path, "analysis", "example_analysis.R")))
  expect_true(file.exists(file.path(project_path, paste0(basename(project_path), ".Rproj"))))

  readme <- readLines(file.path(project_path, "README.md"), warn = FALSE)
  example_script <- readLines(file.path(project_path, "analysis", "example_analysis.R"), warn = FALSE)
  report <- readLines(file.path(project_path, "reports", "main_report.qmd"), warn = FALSE)
  gitignore <- readLines(file.path(project_path, ".gitignore"), warn = FALSE)

  expect_false(any(grepl("data/raw", readme, fixed = TRUE)))
  expect_false(any(grepl("data/raw", example_script, fixed = TRUE)))
  expect_false(any(grepl("data/raw", report, fixed = TRUE)))
  expect_true(".projectSetupR/local.yml" %in% gitignore)
})

test_that("internal data directories are created only when explicitly requested", {
  project_path <- make_project_path("internal-data")

  new_project(
    path = project_path,
    infrastructure = character(),
    use_internal_data_dirs = TRUE,
    open = FALSE
  )

  expect_true(dir.exists(file.path(project_path, "data", "raw")))
  expect_true(dir.exists(file.path(project_path, "data", "processed")))
})

test_that("example script runs without real data", {
  project_path <- make_project_path("example-script")

  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )

  withr::local_dir(project_path)
  expect_no_error(build_project(project_path, render_reports = FALSE))
  expect_true(file.exists(file.path(project_path, "outputs", "example_analysis.rds")))
})

test_that("default report renders without real data when Quarto is available", {
  skip_if_not_installed("quarto")
  skip_if_not(quarto::quarto_available())

  project_path <- make_project_path("render-report")

  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )

  expect_no_error(build_project(project_path, render_reports = TRUE))
  expect_true(file.exists(file.path(project_path, "outputs", "reports", "main_report.html")))
})

test_that("project management component creates governance files and task helpers work", {
  project_path <- make_project_path("project-management")

  new_project(
    path = project_path,
    components = c("statistical_analysis", "report", "project_management"),
    deliverables = c("html_report", "status_report"),
    infrastructure = character(),
    open = FALSE
  )

  expect_true(file.exists(file.path(project_path, "docs", "project_plan.md")))
  expect_true(file.exists(file.path(project_path, "docs", "assumptions.md")))
  expect_true(file.exists(file.path(project_path, "docs", "decisions.md")))
  expect_true(file.exists(file.path(project_path, "docs", "risks.md")))
  expect_true(file.exists(file.path(project_path, ".projectSetupR", "tasks.yml")))
  expect_true(file.exists(file.path(project_path, "reports", "status_report.qmd")))

  task_id <- add_project_task(
    "Review model outputs",
    root = project_path,
    status = "todo",
    priority = "high"
  )

  expect_true(task_id %in% project_tasks(project_path)$task)

  update_project_task(task_id, root = project_path, status = "blocked", notes = "Waiting for inputs")
  status <- project_status_report(project_path, output = "data")

  expect_true(any(status$tasks$status == "blocked"))
})

test_that("plan_project and high-level verbs grow the project", {
  project_path <- make_project_path("high-level-verbs")

  plan <- plan_project(
    path = project_path,
    components = c("statistical_analysis", "report"),
    deliverables = "html_report",
    infrastructure = character()
  )

  expect_true("report" %in% plan$components)
  expect_true("html_report" %in% plan$deliverables)
  expect_true("reports/main_report.qmd" %in% vapply(plan$reports, `[[`, character(1), "path"))

  new_project(
    path = project_path,
    components = c("statistical_analysis", "report"),
    infrastructure = character(),
    open = FALSE
  )

  expect_no_error(serve_project(project_path, watch = FALSE))

  withr::local_dir(project_path)
  expect_no_error(new_script("secondary_analysis", type = "analysis", open = FALSE))
  expect_no_error(new_report("client_report", type = "client_report", root = ".", open = FALSE))
  expect_no_error(new_component("shiny_app", root = ".", open = FALSE))
  expect_no_error(new_app(root = ".", type = "shiny", open = FALSE))

  expect_true(file.exists(file.path(project_path, "analysis", "secondary_analysis.R")))
  expect_true(file.exists(file.path(project_path, "reports", "client_report.qmd")))
  expect_true(file.exists(file.path(project_path, "app", "app.R")))
})
