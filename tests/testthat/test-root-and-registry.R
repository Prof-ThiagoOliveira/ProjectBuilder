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

test_that("new projects use .projflow metadata by default", {
  project_path <- make_project_path("new-metadata-dir")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  expect_true(dir.exists(file.path(project_path, ".projflow")))
  expect_true(file.exists(file.path(project_path, ".projflow", "project_registry.yml")))
  expect_true(file.exists(file.path(project_path, ".projflow", "local.yml")))
  expect_false(dir.exists(file.path(project_path, ".projectSetupR")))
})

test_that("new_script creates a parseable script and registers it", {
  project_path <- make_project_path("new-script")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  withr::local_dir(project_path)
  expect_no_error(new_script("Clean Phenotypes", script_type = "data_cleaning", open = FALSE))
  registry <- yaml::read_yaml(".projflow/project_registry.yml")
  script_path <- registry$scripts$clean_phenotypes$path

  expect_true(file.exists(script_path))
  expect_no_error(parse(file = script_path))

  expect_true("clean_phenotypes" %in% names(registry$scripts))
  expect_identical(registry$scripts$clean_phenotypes$type, "data_cleaning")
  expect_length(registry$scripts$clean_phenotypes$outputs, 0L)
  expect_false("clean_phenotypes" %in% names(registry$outputs))
  expect_false(any(grepl("save_project_object", readLines(script_path, warn = FALSE), fixed = TRUE)))
})
