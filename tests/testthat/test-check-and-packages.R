test_that("check_project detects missing external data paths and internal data warnings", {
  project_path <- make_project_path("check-project")

  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )

  missing_root <- file.path(project_path, "does-not-exist")
  set_project_data_root(missing_root, root = project_path)
  dir.create(file.path(project_path, "data", "raw"), recursive = TRUE)

  check <- check_project(project_path, deep = FALSE)

  expect_false(check$ok)
  expect_true(any(grepl("does not exist", check$errors$message)))
  expect_true(any(grepl("internal data folders", check$warnings$message)))
})

test_that("check_project strict mode fails on critical inconsistencies", {
  project_path <- make_project_path("strict-check")

  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )

  unlink(file.path(project_path, ".projectSetupR", "project_registry.yml"))

  expect_error(
    check_project(project_path, deep = FALSE, strict = TRUE),
    "Project checks failed"
  )
})

test_that("setup_project reports missing packages without installing by default", {
  project_path <- make_project_path("package-check")

  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )
  add_project_package("packageThatShouldNotExistForTests", root = project_path)

  status <- setup_project(project_path, install_missing = FALSE)
  expect_true("packageThatShouldNotExistForTests" %in% status$packages$missing)
})

test_that("setup_project install_missing triggers installation helper", {
  project_path <- make_project_path("package-install")

  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )
  add_project_package("packageThatShouldNotExistForTests", root = project_path)

  called <- FALSE
  local_mocked_bindings(
    install_packages_for_project = function(packages, method = "auto", strict = FALSE) {
      called <<- TRUE
      NULL
    },
    .package = "projflow"
  )

  expect_no_error(setup_project(project_path, install_missing = TRUE))
  expect_true(called)
})
