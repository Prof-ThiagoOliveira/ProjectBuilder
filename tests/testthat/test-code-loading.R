test_that("package loading mode creates package-style loaders", {
  project_path <- make_project_path("code-loading-package")

  create_analysis_project(
    path = project_path,
    code_loading = "package",
    use_targets = TRUE,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_true(file.exists(file.path(project_path, "NAMESPACE")))
  expect_true(file.exists(file.path(project_path, "scripts", "_load_project.R")))

  loader <- readLines(file.path(project_path, "scripts", "_load_project.R"), warn = FALSE)
  targets_file <- readLines(file.path(project_path, "_targets.R"), warn = FALSE)
  report_file <- readLines(file.path(project_path, "reports", "index.qmd"), warn = FALSE)

  expect_true(any(grepl("pkgload::load_all", loader, fixed = TRUE)))
  expect_true(any(grepl("pkgload::load_all", targets_file, fixed = TRUE)))
  expect_true(any(grepl("pkgload::load_all", report_file, fixed = TRUE)))
})

test_that("box loading mode creates modules and box loader", {
  project_path <- make_project_path("code-loading-box")

  create_analysis_project(
    path = project_path,
    code_loading = "box",
    use_targets = TRUE,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_true(dir.exists(file.path(project_path, "modules")))
  expect_true(file.exists(file.path(project_path, "modules", "project_setup.R")))
  expect_true(file.exists(file.path(project_path, "scripts", "_load_project.R")))

  loader <- readLines(file.path(project_path, "scripts", "_load_project.R"), warn = FALSE)
  targets_file <- readLines(file.path(project_path, "_targets.R"), warn = FALSE)

  expect_true(any(grepl("box::use", loader, fixed = TRUE)))
  expect_true(any(grepl("box::use", targets_file, fixed = TRUE)))
})

test_that("source loading mode creates controlled source loader", {
  project_path <- make_project_path("code-loading-source")

  create_analysis_project(
    path = project_path,
    code_loading = "source",
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_true(file.exists(file.path(project_path, "R", "project_loader.R")))
  expect_true(file.exists(file.path(project_path, "scripts", "_load_project.R")))

  loader <- readLines(file.path(project_path, "R", "project_loader.R"), warn = FALSE)
  report_file <- readLines(file.path(project_path, "reports", "index.qmd"), warn = FALSE)

  expect_true(any(grepl("Missing project R files", loader, fixed = TRUE)))
  expect_true(any(grepl('source("scripts/_load_project.R")', report_file, fixed = TRUE)))
})

test_that("invalid code_loading errors clearly", {
  project_path <- make_project_path("code-loading-invalid")

  expect_error(
    create_analysis_project(
      path = project_path,
      code_loading = "invalid",
      use_renv = FALSE,
      use_git = FALSE,
      open = FALSE
    )
  )
})
