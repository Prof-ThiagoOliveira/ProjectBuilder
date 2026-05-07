test_that("simple mode keeps the scaffold minimal", {
  project_path <- make_project_path("options-simple")

  result <- create_analysis_project(
    path = project_path,
    preset = "analysis",
    mode = "simple",
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_identical(result$mode, "simple")
  expect_true(file.exists(file.path(project_path, "run_project.R")))
  expect_false(file.exists(file.path(project_path, "_quarto.yml")))
  expect_false(file.exists(file.path(project_path, "_targets.R")))
  expect_false(file.exists(file.path(project_path, "PROJECT_GUIDE.md")))
})

test_that("advanced package mode remains available explicitly", {
  project_path <- make_project_path("options-advanced-package")

  result <- create_analysis_project(
    path = project_path,
    preset = "package",
    mode = "advanced",
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_identical(result$mode, "advanced")
  expect_true(file.exists(file.path(project_path, "DESCRIPTION")))
  expect_true(file.exists(file.path(project_path, "NAMESPACE")))
  expect_true(dir.exists(file.path(project_path, "R")))
  expect_true(dir.exists(file.path(project_path, "tests", "testthat")))
})

test_that("pipeline requests enable advanced scaffold components", {
  project_path <- make_project_path("options-pipeline")

  result <- create_analysis_project(
    path = project_path,
    preset = "pipeline",
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_identical(result$mode, "advanced")
  expect_true(file.exists(file.path(project_path, "_targets.R")))
})

test_that("use_renv can be enabled without failing scaffold creation", {
  skip("renv initialisation is optional and slow in the current test environment.")

  project_path <- make_project_path("options-renv")

  expect_no_error(
    create_analysis_project(
      path = project_path,
      use_renv = TRUE,
      use_git = FALSE,
      open = FALSE
    )
  )
})
