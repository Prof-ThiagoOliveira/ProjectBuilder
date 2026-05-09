test_that("data source helpers store and read local external paths", {
  project_path <- make_project_path("data-sources")

  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )

  external_root <- file.path(project_path, "external-store")
  dir.create(external_root, recursive = TRUE)

  expect_no_error(set_project_data_root(external_root, root = project_path))
  expect_identical(
    project_data_root(root = project_path),
    normalizePath(external_root, winslash = "/", mustWork = FALSE)
  )
  expect_identical(
    project_data_path("phenotypes", "raw.csv", root = project_path),
    file.path(normalizePath(external_root, winslash = "/", mustWork = FALSE), "phenotypes", "raw.csv")
  )

  local_config <- yaml::read_yaml(file.path(project_path, ".projflow", "local.yml"))
  expect_identical(local_config$data_sources$default$path, normalizePath(external_root, winslash = "/", mustWork = FALSE))
})

test_that("multiple data sources can be configured and listed", {
  project_path <- make_project_path("multiple-sources")

  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )

  main_root <- file.path(project_path, "main-data")
  reference_root <- file.path(project_path, "reference-data")
  dir.create(main_root, recursive = TRUE)
  dir.create(reference_root, recursive = TRUE)

  set_project_data_root(main_root, name = "main", root = project_path)
  set_project_data_root(reference_root, name = "reference", root = project_path)

  sources <- list_project_data_sources(project_path)
  expect_true(all(c("main", "reference") %in% sources$name))
  expect_identical(
    project_data_path("phenotypes.csv", source = "main", root = project_path),
    file.path(normalizePath(main_root, winslash = "/", mustWork = FALSE), "phenotypes.csv")
  )
  expect_identical(
    project_data_path("genome_map.csv", source = "reference", root = project_path),
    file.path(normalizePath(reference_root, winslash = "/", mustWork = FALSE), "genome_map.csv")
  )
})

test_that("missing data source gives a clear error", {
  project_path <- make_project_path("missing-source")

  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )

  expect_error(
    project_data_root(root = project_path),
    "No external data root has been configured"
  )
})
