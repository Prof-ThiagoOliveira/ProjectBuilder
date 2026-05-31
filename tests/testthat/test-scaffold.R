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
  expect_true(dir.exists(file.path(project_path, ".projflow")))

  expect_false(dir.exists(file.path(project_path, "data")))
  expect_true(file.exists(file.path(project_path, "project.yml")))
  expect_true(file.exists(file.path(project_path, ".projflow", "project_registry.yml")))
  expect_true(file.exists(file.path(project_path, ".projflow", "local.yml")))
  expect_true(file.exists(file.path(project_path, "README.md")))
  expect_true(file.exists(file.path(project_path, "run_project.R")))
  expect_true(file.exists(file.path(project_path, ".gitignore")))
  expect_true(file.exists(file.path(project_path, "reports", "main_report.qmd")))
  expect_false(file.exists(file.path(project_path, "analysis", "example_analysis.R")))
  expect_true(file.exists(file.path(project_path, paste0(basename(project_path), ".Rproj"))))

  readme <- readLines(file.path(project_path, "README.md"), warn = FALSE)
  report <- readLines(file.path(project_path, "reports", "main_report.qmd"), warn = FALSE)
  gitignore <- readLines(file.path(project_path, ".gitignore"), warn = FALSE)

  expect_false(any(grepl("data/raw", readme, fixed = TRUE)))
  expect_false(any(grepl("data/raw", report, fixed = TRUE)))
  expect_true(".projflow/local.yml" %in% gitignore)
})

test_that("infrastructure NULL infers defaults and character(0) disables them", {
  inferred_path <- make_project_path("inferred-infra")
  none_path <- make_project_path("no-infra")

  new_project(
    path = inferred_path,
    components = c("statistical_analysis", "report"),
    infrastructure = NULL,
    open = FALSE
  )
  new_project(
    path = none_path,
    components = c("statistical_analysis", "report"),
    infrastructure = character(),
    open = FALSE
  )

  expect_true("git" %in% project_infrastructure(inferred_path))
  expect_false("git" %in% project_infrastructure(none_path))
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

test_that("start_project creates a guided scaffold with user-selected options", {
  project_path <- make_project_path("guided-start")

  result <- start_project(
    path = project_path,
    type = "analysis",
    data_location = "internal",
    use_quarto = FALSE,
    use_renv = FALSE,
    use_git = TRUE,
    use_github_actions = FALSE,
    open = FALSE
  )

  config <- yaml::read_yaml(file.path(project_path, "project.yml"))
  registry <- yaml::read_yaml(file.path(project_path, ".projflow", "project_registry.yml"))

  expect_s3_class(result, "projflow_started_project")
  expect_s3_class(result, "analysis_project_scaffold")
  expect_true(dir.exists(file.path(project_path, "data", "raw")))
  expect_true(dir.exists(file.path(project_path, "data", "processed")))
  expect_true(file.exists(file.path(project_path, "reports", "main_report.Rmd")))
  expect_false(file.exists(file.path(project_path, "reports", "main_report.qmd")))
  expect_false(file.exists(file.path(project_path, "_quarto.yml")))
  expect_true(isFALSE(config$settings$use_quarto))
  expect_true(isTRUE(config$settings$use_git))
  expect_true(isFALSE(config$settings$use_renv))
  expect_identical(registry$reports$main_report$path, "reports/main_report.Rmd")
  config_infrastructure <- config$infrastructure
  if (is.null(config_infrastructure)) {
    config_infrastructure <- character()
  }
  expect_false("quarto" %in% config_infrastructure)
  expect_true("git" %in% config_infrastructure)
})

test_that("example script runs without real data", {
  project_path <- make_project_path("example-script")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = TRUE,
    open = FALSE
  )

  withr::local_dir(project_path)
  expect_no_error(build_project(project_path, render_reports = FALSE))
  expect_false(file.exists(file.path(project_path, "outputs", "analysis", "example_analysis.rds")))
  registry <- yaml::read_yaml(file.path(project_path, ".projflow", "project_registry.yml"))
  example_path <- registry$scripts$example_analysis$path
  expect_true(file.exists(file.path(project_path, example_path)))
  expect_false(any(grepl("save_project_object|saveRDS|write\\.csv", readLines(file.path(project_path, example_path), warn = FALSE))))
})

test_that("default report render workflow creates the registered output", {
  local_mock_quarto_render()
  
  project_path <- make_project_path("render-report")
  
  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )
  
  expect_no_error(build_project(project_path, render_reports = TRUE))
  expect_true(file.exists(file.path(project_path, "outputs", "reports", "main_report", "main_report.html")))
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
  expect_true(file.exists(file.path(project_path, ".projflow", "tasks.yml")))
  expect_true(file.exists(file.path(project_path, "reports", "status_report.qmd")))

  task_id <- add_project_task(
    "Review model outputs",
    root = project_path,
    status = "todo",
    priority = "high"
  )

  expect_true(task_id %in% project_tasks(project_path)$id)

  update_project_task(task_id, root = project_path, status = "blocked", description = "Waiting for inputs")
  status <- project_status_report(project_path, output = "data")

  expect_true(any(status$tasks$status == "blocked"))
})

test_that("custom component specs can be registered and used", {
  project_path <- make_project_path("custom-component")
  spec_path <- file.path(tempdir(), "genomic_component.yml")
  writeLines(
    c(
      "component: genomic_evaluation",
      "folders:",
      "  - outputs/ebv",
      "scripts:",
      "  - name: genomic_evaluation",
      "    path: analysis/08_genomic_evaluation.R",
      "    type: statistical_analysis",
      "    order: 80",
      "packages:",
      "  - stats"
    ),
    spec_path
  )

  expect_identical(read_project_component_spec(spec_path)$component, "genomic_evaluation")
  expect_identical(use_project_component_spec(spec_path), "genomic_evaluation")

  new_project(
    path = project_path,
    components = c("statistical_analysis", "genomic_evaluation", "report"),
    component_specs = spec_path,
    infrastructure = character(),
    open = FALSE
  )

  registry <- yaml::read_yaml(file.path(project_path, ".projflow", "project_registry.yml"))
  genomic_path <- registry$scripts$genomic_evaluation$path
  expect_true(file.exists(file.path(project_path, genomic_path)))
  expect_true(grepl("analysis/[0-9]+_genomic_evaluation\\.R$", genomic_path))
  expect_true(dir.exists(file.path(project_path, "outputs", "ebv")))

  registry <- yaml::read_yaml(file.path(project_path, ".projflow", "project_registry.yml"))
  expect_true("genomic_evaluation" %in% registry$components)
  expect_true("genomic_evaluation" %in% registry$custom_components)
  expect_true("genomic_evaluation" %in% names(registry$component_specs))
})

test_that("project options can be inspected before planning", {
  component <- inspect_project_component("quality_control")
  deliverable <- inspect_project_deliverable("client_report")
  infrastructure <- inspect_project_infrastructure("targets")
  preset <- inspect_project_preset("client_report")

  expect_s3_class(component, "projflow_component_spec")
  expect_s3_class(deliverable, "projflow_deliverable_spec")
  expect_s3_class(infrastructure, "projflow_infrastructure_spec")
  expect_s3_class(preset, "projflow_preset_spec")

  expect_identical(component$component, "quality_control")
  expect_identical(deliverable$deliverable, "client_report")
  expect_identical(infrastructure$infrastructure, "targets")
  expect_identical(preset$preset, "client_report")
})

test_that("custom deliverable specs can be registered and used", {
  project_path <- make_project_path("custom-deliverable")
  spec_path <- file.path(tempdir(), "executive_summary.yml")
  writeLines(
    c(
      "deliverable: executive_summary",
      "depends_on:",
      "  - report",
      "path: reports/executive_summary.qmd",
      "type: report",
      "packages:",
      "  - quarto"
    ),
    spec_path
  )

  expect_identical(use_project_deliverable_spec(spec_path), "executive_summary")
  expect_true("executive_summary" %in% available_project_deliverables())

  plan <- plan_project(
    path = project_path,
    components = "statistical_analysis",
    deliverables = "executive_summary",
    infrastructure = character()
  )

  report_paths <- vapply(plan$reports, `[[`, character(1), "path")
  expect_true("report" %in% plan$components)
  expect_true("executive_summary" %in% plan$deliverables)
  expect_true("reports/executive_summary.qmd" %in% report_paths)
})

test_that("custom preset specs can be registered and used", {
  project_path <- make_project_path("custom-preset")
  spec_path <- file.path(tempdir(), "rapid_review.yml")
  writeLines(
    c(
      "preset: rapid_review",
      "description: Compact preset for quick review projects",
      "components:",
      "  - data_preparation",
      "  - report",
      "deliverables:",
      "  - html_report",
      "infrastructure:",
      "  - git"
    ),
    spec_path
  )

  expect_identical(use_project_preset_spec(spec_path), "rapid_review")
  expect_true("rapid_review" %in% available_project_presets())

  plan <- plan_project(
    path = project_path,
    preset = "rapid_review",
    infrastructure = character()
  )

  expect_true(all(c("data_preparation", "report") %in% plan$components))
  expect_true("html_report" %in% plan$deliverables)
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

  local_mock_quarto_render()
  expect_no_error(serve_project(project_path))

  withr::local_dir(project_path)
  expect_no_error(new_script("secondary_analysis", script_type = "analysis", open = FALSE))
  expect_no_error(new_report("client_report", type = "client_report", root = ".", open = FALSE))
  expect_no_error(new_component("shiny_app", root = ".", open = FALSE))
  expect_no_error(new_app(root = ".", type = "shiny", open = FALSE))

  registry <- yaml::read_yaml(file.path(project_path, ".projflow", "project_registry.yml"))
  secondary_path <- registry$scripts$secondary_analysis$path
  expect_true(file.exists(file.path(project_path, secondary_path)))
  expect_true(file.exists(file.path(project_path, "reports", "client_report.qmd")))
  expect_true(file.exists(file.path(project_path, "app", "app.R")))
})

test_that("serve_project handles report, shiny and plain project targets", {
  report_path <- make_project_path("serve-report")
  plain_path <- make_project_path("serve-plain")
  shiny_path <- make_project_path("serve-shiny")
  dashboard_path <- make_project_path("serve-dashboard")

  new_project(
    path = report_path,
    components = c("statistical_analysis", "report"),
    infrastructure = character(),
    open = FALSE
  )
  new_project(
    path = plain_path,
    components = c("statistical_analysis"),
    infrastructure = character(),
    open = FALSE
  )
  new_project(
    path = shiny_path,
    components = c("statistical_analysis", "shiny_app"),
    infrastructure = character(),
    open = FALSE
  )
  new_project(
    path = dashboard_path,
    components = c("statistical_analysis"),
    infrastructure = character(),
    open = FALSE
  )
  new_app(
    name = "operations_dashboard",
    type = "quarto_dashboard",
    root = dashboard_path,
    open = FALSE
  )

  expect_no_error(serve_project(report_path, target = "reports", render = FALSE))
  expect_no_error(serve_project(plain_path, target = "project", render = FALSE))
  expect_no_error(serve_project(dashboard_path, target = "dashboard", render = FALSE))

  skip_if_not_installed("shiny")
  called <- FALSE
  local_mocked_bindings(
    runApp = function(appDir, launch.browser = TRUE) {
      called <<- TRUE
      invisible(appDir)
    },
    .package = "shiny"
  )
  expect_no_error(serve_project(shiny_path, target = "shiny_app", render = FALSE))
  expect_true(called)
})

test_that("default output paths use typed output subdirectories", {
  project_path <- make_project_path("typed-output-layout")

  new_project(
    path = project_path,
    components = c("data_preparation", "statistical_analysis", "tables", "figures", "report"),
    infrastructure = character(),
    open = FALSE
  )

  registry <- read_project_registry(project_path)
  expect_identical(registry$outputs$prepared_inputs$path, "outputs/data/prepared_inputs.rds")
  expect_identical(registry$outputs$analysis_results$path, "outputs/analysis/analysis_results.rds")
  expect_identical(registry$outputs$summary_tables$path, "outputs/tables/summary_tables.csv")
  expect_identical(registry$outputs$summary_figures$path, "outputs/figures/summary_figures.png")

  reports <- list_project_reports(project_path)
  expect_true("outputs/reports/main_report/main_report.html" %in% reports$output)
})

test_that("organise_project_outputs migrates old registered output paths", {
  project_path <- make_project_path("organise-output-layout")

  new_project(
    path = project_path,
    components = c("data_preparation", "statistical_analysis", "report"),
    infrastructure = character(),
    open = FALSE
  )

  registry <- read_project_registry(project_path)
  registry$outputs$prepared_inputs$path <- "outputs/prepared_inputs.rds"
  registry$outputs$analysis_results$path <- "outputs/analysis_results.rds"
  write_project_registry(registry, project_path, overwrite = TRUE)

  dir.create(file.path(project_path, "outputs"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data.frame(x = 1), file.path(project_path, "outputs", "prepared_inputs.rds"))
  saveRDS(data.frame(x = 2), file.path(project_path, "outputs", "analysis_results.rds"))

  actions <- organise_project_outputs(project_path)
  registry <- read_project_registry(project_path)

  expect_true(nrow(actions) >= 2L)
  expect_identical(registry$outputs$prepared_inputs$path, "outputs/data/prepared_inputs.rds")
  expect_identical(registry$outputs$analysis_results$path, "outputs/analysis/analysis_results.rds")
  expect_true(file.exists(file.path(project_path, "outputs", "data", "prepared_inputs.rds")))
  expect_true(file.exists(file.path(project_path, "outputs", "analysis", "analysis_results.rds")))
})
