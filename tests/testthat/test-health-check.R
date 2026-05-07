test_that("project object helpers create and register a new analysis object", {
  project_path <- make_project_path("object-helpers")

  create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  withr::local_dir(project_path)

  new_project_object("clean_trial_data", type = "data_cleaning")

  expect_true(file.exists("analysis/clean_trial_data.R"))
  expect_true(file.exists(".projectSetupR/project_registry.yml"))

  objects <- list_project_objects()
  expect_true("clean_trial_data" %in% objects$name)
  expect_true("data/processed/clean_trial_data.rds" %in% missing_project_outputs())
})

test_that("project object helpers can save and load standard outputs", {
  project_path <- make_project_path("object-save-load")

  create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  withr::local_dir(project_path)

  data <- mtcars[1:3, 1:2]
  save_project_object(data, name = "trial_data", type = "dataset")

  expect_true(file.exists("data/processed/trial_data.rds"))

  loaded <- load_project_object("trial_data")
  expect_identical(loaded, data)
})

test_that("project status and stale output helpers are actionable", {
  project_path <- make_project_path("project-status")

  create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  withr::local_dir(project_path)

  new_project_object("clean_trial_data", type = "data_cleaning")
  writeLines("raw", "data/raw/trial_data.csv")

  saveRDS(mtcars, "data/processed/clean_trial_data.rds")
  Sys.sleep(1)
  writeLines("# newer source", "analysis/clean_trial_data.R")

  status <- project_status()
  output <- capture.output(print(status))

  expect_s3_class(status, "project_status")
  expect_true("data/processed/clean_trial_data.rds" %in% stale_project_outputs())
  expect_true(any(grepl("Project status", output, fixed = TRUE)))
  expect_true(any(grepl("Needs attention", output, fixed = TRUE)))
})

test_that("run_project_step sources a registered analysis step", {
  project_path <- make_project_path("project-run-step")

  create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  withr::local_dir(project_path)

  new_project_object("export_summary", type = "export_step")
  writeLines(
    c(
      "summary_table <- data.frame(a = 1)",
      "projectSetupR::save_project_object(summary_table, name = 'export_summary', type = 'export_step')"
    ),
    "analysis/export_summary.R"
  )

  run_project_step("export_summary")

  expect_true(file.exists("outputs/tables/export_summary.csv"))
})
