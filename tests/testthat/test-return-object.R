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
      "use_git"
    )
  )
})

test_that("print method reports key scaffold information", {
  project_path <- make_project_path("print-method")

  result <- create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_output(print(result), "Project name:")
  expect_output(print(result), "Start here:")
  expect_output(print(result), "PROJECT_GUIDE.md")
  expect_output(print(result), 'source\\("scripts/00_start_here.R"\\)')
})
