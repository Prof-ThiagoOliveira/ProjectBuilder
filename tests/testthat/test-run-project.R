test_that("build_project works when called outside the project root", {
  project_path <- make_project_path("run-outside-root")
  call_path <- file.path(tempdir(), paste0("run-call-", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(call_path, recursive = TRUE)

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  withr::local_dir(project_path)
  new_script("record_wd", type = "analysis", open = FALSE)
  writeLines(
    c(
      "projflow::setup_project()",
      "result <- data.frame(wd = getwd(), stringsAsFactors = FALSE)",
      'projflow::save_project_object(result, name = "record_wd", type = "analysis")'
    ),
    "analysis/record_wd.R"
  )

  withr::local_dir(call_path)
  expect_no_error(build_project(project_path, render_reports = FALSE))

  saved <- readRDS(file.path(project_path, "outputs", "record_wd.rds"))
  expect_identical(saved$wd[[1]], normalizePath(project_path, winslash = "/", mustWork = TRUE))
})

test_that("build_project restores the original working directory", {
  project_path <- make_project_path("restore-wd")
  caller_dir <- file.path(tempdir(), paste0("caller-", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(caller_dir, recursive = TRUE)

  new_project(
    path = project_path,
    infrastructure = character(),
    open = FALSE
  )

  withr::local_dir(caller_dir)
  before <- getwd()
  expect_no_error(build_project(project_path, render_reports = FALSE))
  expect_identical(getwd(), before)
})

test_that("build_project follows registry order", {
  project_path <- make_project_path("run-order")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  withr::local_dir(project_path)
  new_script("second_step", order = 20, open = FALSE)
  new_script("first_step", order = 10, open = FALSE)

  writeLines(
    c(
      "projflow::setup_project()",
      "path <- file.path(projflow::setup_project()$root, 'outputs', 'order.txt')",
      "write('second', file = path, append = TRUE)"
    ),
    "analysis/second_step.R"
  )
  writeLines(
    c(
      "projflow::setup_project()",
      "path <- file.path(projflow::setup_project()$root, 'outputs', 'order.txt')",
      "write('first', file = path, append = TRUE)"
    ),
    "analysis/first_step.R"
  )

  expect_no_error(build_project(render_reports = FALSE))
  lines <- readLines(file.path(project_path, "outputs", "order.txt"), warn = FALSE)
  expect_identical(lines, c("first", "second"))
})

test_that("build_project reports failing scripts clearly", {
  project_path <- make_project_path("run-failure")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  withr::local_dir(project_path)
  new_script("broken_step", order = 10, open = FALSE)
  writeLines("stop('boom')", "analysis/broken_step.R")

  expect_error(
    build_project(render_reports = FALSE),
    "broken_step.*analysis/broken_step.R.*order: 10.*boom"
  )
})
