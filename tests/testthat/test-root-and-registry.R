test_that("simple projects are recognised from nested directories", {
  project_path <- make_project_path("simple-root")

  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )

  nested <- file.path(project_path, "analysis", "nested")
  dir.create(nested, recursive = TRUE)

  expect_identical(find_project_root(nested), normalizePath(project_path, winslash = "/", mustWork = TRUE))
})

test_that("projects with richer infrastructure are recognised from nested directories", {
  project_path <- make_project_path("advanced-root")

  new_project(
    path = project_path,
    infrastructure = c("github_actions", "tests"),
    open = FALSE
  )

  nested <- file.path(project_path, "tests", "testthat")
  expect_identical(find_project_root(nested), normalizePath(project_path, winslash = "/", mustWork = TRUE))
})

test_that("projects with only project.yml are recognised", {
  project_path <- make_project_path("project-yml-only")
  dir.create(project_path, recursive = TRUE)
  writeLines("project:\n  name: demo\n", file.path(project_path, "project.yml"))

  nested <- file.path(project_path, "child", "grandchild")
  dir.create(nested, recursive = TRUE)

  expect_identical(find_project_root(nested), normalizePath(project_path, winslash = "/", mustWork = TRUE))
})

test_that("projects with .projectSetupR registry are recognised", {
  project_path <- make_project_path("registry-only")
  dir.create(file.path(project_path, ".projectSetupR"), recursive = TRUE)
  yaml::write_yaml(default_project_registry("registry_only"), file.path(project_path, ".projectSetupR", "project_registry.yml"))

  nested <- file.path(project_path, "analysis", "subdir")
  dir.create(nested, recursive = TRUE)

  expect_identical(find_project_root(nested), normalizePath(project_path, winslash = "/", mustWork = TRUE))
})

test_that("new_project_script creates a parseable script and registers it", {
  project_path <- make_project_path("new-script")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  withr::local_dir(project_path)
  expect_no_error(new_project_script("Clean Phenotypes", type = "data_cleaning", open = FALSE))

  expect_true(file.exists("analysis/clean_phenotypes.R"))
  expect_no_error(parse(file = "analysis/clean_phenotypes.R"))

  registry <- yaml::read_yaml(".projectSetupR/project_registry.yml")
  expect_true("clean_phenotypes" %in% names(registry$scripts))
  expect_identical(registry$scripts$clean_phenotypes$type, "data_cleaning")
  expect_match(paste(readLines("analysis/clean_phenotypes.R", warn = FALSE), collapse = "\n"), 'type = "data_cleaning"', fixed = TRUE)
})
