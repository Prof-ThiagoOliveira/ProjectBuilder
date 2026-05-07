test_that("project health check is generated and returns the expected class", {
  project_path <- make_project_path("health-check")

  create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_true(file.exists(file.path(project_path, "R", "project_health_check.R")))

  environment <- new.env(parent = globalenv())
  sys.source(file.path(project_path, "R", "dependencies.R"), envir = environment)
  sys.source(file.path(project_path, "R", "project_health_check.R"), envir = environment)

  check <- withr::with_dir(project_path, environment$project_health_check())

  expect_s3_class(check, "project_health_check")
  expect_true("recommended_next_step" %in% names(check))
})

test_that("project health check detects missing directories", {
  project_path <- make_project_path("health-missing-dir")

  create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  unlink(file.path(project_path, "data", "raw"), recursive = TRUE, force = TRUE)

  environment <- new.env(parent = globalenv())
  sys.source(file.path(project_path, "R", "dependencies.R"), envir = environment)
  sys.source(file.path(project_path, "R", "project_health_check.R"), envir = environment)

  check <- withr::with_dir(project_path, environment$project_health_check())

  expect_true("data/raw" %in% check$missing_directories)
})

test_that("project health check detects missing packages and print method is actionable", {
  project_path <- make_project_path("health-missing-pkg")

  create_analysis_project(
    path = project_path,
    dependency_profile = "custom",
    packages = "DefinitelyMissingPackage",
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  environment <- new.env(parent = globalenv())
  sys.source(file.path(project_path, "R", "dependencies.R"), envir = environment)
  sys.source(file.path(project_path, "R", "project_health_check.R"), envir = environment)

  check <- withr::with_dir(project_path, environment$project_health_check())
  output <- capture.output(environment$`print.project_health_check`(check))

  expect_true("DefinitelyMissingPackage" %in% check$missing_packages)
  expect_true(any(grepl("Recommended next step", output, fixed = TRUE)))
  expect_true(any(grepl("install_packages.R", output, fixed = TRUE)))
})
