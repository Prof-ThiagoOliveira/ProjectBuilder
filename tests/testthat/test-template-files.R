test_that("default scaffold generates only the minimal runnable files", {
  project_path <- make_project_path("template-simple")

  create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_no_error(parse(file.path(project_path, "run_project.R")))
  expect_no_error(yaml::read_yaml(file.path(project_path, "project.yml")))
  expect_no_error(yaml::read_yaml(file.path(project_path, ".projectSetupR", "project_registry.yml")))
  expect_true(file.exists(file.path(project_path, "reports", "main_report.qmd")))
  expect_false(file.exists(file.path(project_path, "config.yml")))
  expect_false(file.exists(file.path(project_path, "Makefile")))
})

test_that("advanced scaffold still writes parseable helper files", {
  project_path <- make_project_path("template-advanced")

  create_analysis_project(
    path = project_path,
    mode = "advanced",
    code_loading = "source",
    use_quarto = TRUE,
    use_rmarkdown = TRUE,
    use_targets = TRUE,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  r_files <- list.files(
    file.path(project_path, "R"),
    pattern = "\\.R$",
    full.names = TRUE
  )

  expect_true(length(r_files) > 0L)
  expect_no_error(lapply(r_files, parse))

  expect_no_error(yaml::read_yaml(file.path(project_path, "config.yml")))
  expect_no_error(yaml::read_yaml(file.path(project_path, "_quarto.yml")))
  expect_no_error(yaml::read_yaml(file.path(project_path, "reports", "templates", "report-format.yml")))

  expect_true(file.exists(file.path(project_path, "reports", "index.qmd")))
  expect_true(file.exists(file.path(project_path, "reports", "exploratory_analysis.qmd")))
  expect_true(file.exists(file.path(project_path, "reports", "final_report.qmd")))
  expect_true(file.exists(file.path(project_path, "reports", "exploratory_analysis.Rmd")))
  expect_true(file.exists(file.path(project_path, "reports", "final_report.Rmd")))
  expect_true(file.exists(file.path(project_path, "_targets.R")))
})
