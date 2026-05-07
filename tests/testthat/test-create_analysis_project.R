test_that("default scaffold is simple and analyst-facing", {
  project_path <- make_project_path("simple-default")

  result <- create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_s3_class(result, "analysis_project_scaffold")
  expect_identical(result$mode, "simple")
  expect_true(dir.exists(project_path))

  expect_true(dir.exists(file.path(project_path, "analysis")))
  expect_true(dir.exists(file.path(project_path, "data", "raw")))
  expect_true(dir.exists(file.path(project_path, "data", "processed")))
  expect_true(dir.exists(file.path(project_path, "reports")))
  expect_true(dir.exists(file.path(project_path, "outputs")))
  expect_true(dir.exists(file.path(project_path, ".projectSetupR")))

  expect_true(file.exists(file.path(project_path, "README.md")))
  expect_true(file.exists(file.path(project_path, "run_project.R")))
  expect_true(file.exists(file.path(project_path, "project.yml")))
  expect_true(file.exists(file.path(project_path, "reports", "main_report.qmd")))
  expect_true(file.exists(file.path(project_path, ".projectSetupR", "project_registry.yml")))
  expect_true(file.exists(file.path(project_path, paste0(basename(project_path), ".Rproj"))))

  expect_false(file.exists(file.path(project_path, "DESCRIPTION")))
  expect_false(file.exists(file.path(project_path, "NAMESPACE")))
  expect_false(dir.exists(file.path(project_path, "tests", "testthat")))
  expect_false(dir.exists(file.path(project_path, "R")))
  expect_false(file.exists(file.path(project_path, "PROJECT_GUIDE.md")))
  expect_false(dir.exists(file.path(project_path, "scripts")))
})

test_that("relative paths are resolved before scaffolding", {
  parent_dir <- file.path(
    tempdir(),
    paste0("relative-project-parent-", as.integer(stats::runif(1, 1, 1e9)))
  )
  dir.create(parent_dir, recursive = TRUE)
  withr::local_dir(parent_dir)

  result <- create_analysis_project(
    path = "./analysis project",
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expected_path <- normalizePath("./analysis project", winslash = "/", mustWork = FALSE)

  expect_identical(
    normalizePath(result$path, winslash = "/", mustWork = TRUE),
    expected_path
  )
  expect_true(dir.exists(expected_path))
  expect_true(file.exists(file.path(expected_path, "analysis_project.Rproj")))
})

test_that("tempfile-style path prefixes are flagged clearly", {
  raw_path <- tempfile("./../../R Packages/anal")
  resolved_path <- normalizePath(path.expand(raw_path), winslash = "/", mustWork = FALSE)
  warning_message <- detect_path_construction_warning(raw_path, resolved_path)

  expect_match(warning_message, "temporary-path prefix", fixed = TRUE)
  expect_match(warning_message, resolved_path, fixed = TRUE)
})
