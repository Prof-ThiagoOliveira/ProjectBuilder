test_that("write_template_file skips existing files by default", {
  project_path <- make_project_path("overwrite-skip")
  dir.create(project_path, recursive = TRUE)
  target_file <- file.path(project_path, "example.txt")

  writeLines("original", target_file)

  result <- projectSetupR:::write_template_file(
    path = target_file,
    content = "replacement",
    overwrite = FALSE
  )

  expect_identical(result$status, "skipped")
  expect_identical(readLines(target_file, warn = FALSE), "original")
})

test_that("write_template_file overwrites existing files when requested", {
  project_path <- make_project_path("overwrite-replace")
  dir.create(project_path, recursive = TRUE)
  target_file <- file.path(project_path, "example.txt")

  writeLines("original", target_file)

  result <- projectSetupR:::write_template_file(
    path = target_file,
    content = "replacement",
    overwrite = TRUE
  )

  expect_identical(result$status, "overwritten")
  expect_identical(readLines(target_file, warn = FALSE), "replacement")
})

test_that("existing non-empty directories error unless overwrite is TRUE", {
  project_path <- make_project_path("overwrite-dir")
  dir.create(project_path, recursive = TRUE)
  writeLines("existing", file.path(project_path, "existing.txt"))

  expect_error(
    create_analysis_project(
      path = project_path,
      use_renv = FALSE,
      use_git = FALSE,
      open = FALSE
    ),
    "Refusing to use existing non-empty directory"
  )

  expect_no_error(
    create_analysis_project(
      path = project_path,
      use_renv = FALSE,
      use_git = FALSE,
      overwrite = TRUE,
      open = FALSE
    )
  )
})
