test_that("return object contains expected fields", {
  project_path <- make_project_path("return-object")

  result <- create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_named(
    result,
    c(
      "path",
      "project_name",
      "preset",
      "mode",
      "template",
      "dependency_profile",
      "code_loading",
      "packages",
      "directories_created",
      "files_created",
      "files_skipped",
      "warnings",
      "use_quarto",
      "use_rmarkdown",
      "use_targets",
      "use_renv",
      "use_git",
      "entrypoint",
      "guide"
    )
  )
})

test_that("print method reports the simple scaffold entry point", {
  project_path <- make_project_path("print-method-simple")

  result <- create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_output(print(result), "Project name:")
  expect_output(print(result), "Mode: simple")
  expect_output(print(result), "Start here:")
  expect_output(print(result), "README.md")
  expect_output(print(result), 'source\\("run_project.R"\\)')
})

test_that("print method reports the advanced scaffold entry point", {
  project_path <- make_project_path("print-method-advanced")

  result <- create_analysis_project(
    path = project_path,
    mode = "advanced",
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_output(print(result), "Mode: advanced")
  expect_output(print(result), "PROJECT_GUIDE.md")
  expect_output(print(result), 'source\\("scripts/00_start_here.R"\\)')
})
