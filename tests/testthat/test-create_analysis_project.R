test_that("a temporary project can be created", {
  project_path <- make_project_path()

  result <- create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_s3_class(result, "analysis_project_scaffold")
  expect_true(dir.exists(project_path))

  expect_true(dir.exists(file.path(project_path, "R")))
  expect_true(dir.exists(file.path(project_path, "scripts")))
  expect_true(dir.exists(file.path(project_path, "data", "raw")))
  expect_true(dir.exists(file.path(project_path, "outputs", "reports")))
  expect_true(dir.exists(file.path(project_path, "tests", "testthat")))
  expect_false(dir.exists(file.path(project_path, "data-raw")))
  expect_false(dir.exists(file.path(project_path, "man")))
  expect_false(dir.exists(file.path(project_path, "vignettes")))

  expect_true(file.exists(file.path(project_path, "README.md")))
  expect_true(file.exists(file.path(project_path, "DESCRIPTION")))
  expect_true(file.exists(file.path(project_path, "NAMESPACE")))
  expect_true(file.exists(file.path(project_path, ".here")))
  expect_true(file.exists(file.path(project_path, "PROJECT_GUIDE.md")))
  expect_true(file.exists(file.path(project_path, "config.yml")))
  expect_true(file.exists(file.path(project_path, "scripts", "_load_project.R")))
  expect_true(file.exists(file.path(project_path, "scripts", "00_start_here.R")))
  expect_true(file.exists(file.path(project_path, "scripts", "install_packages.R")))
  expect_true(file.exists(file.path(project_path, paste0(basename(project_path), ".Rproj"))))

  readme <- paste(readLines(file.path(project_path, "README.md"), warn = FALSE), collapse = "\n")
  guide <- paste(readLines(file.path(project_path, "PROJECT_GUIDE.md"), warn = FALSE), collapse = "\n")
  makefile <- paste(readLines(file.path(project_path, "Makefile"), warn = FALSE), collapse = "\n")

  expect_match(readme, "`R/` | Reusable project functions", fixed = TRUE)
  expect_false(grepl("R/` or `modules/", readme, fixed = TRUE))
  expect_match(guide, "Write reusable functions in `R/`.", fixed = TRUE)
  expect_false(grepl("For box loading", guide, fixed = TRUE))
  expect_match(makefile, "source('scripts/_load_project.R')", fixed = TRUE)
})
