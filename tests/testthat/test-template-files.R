test_that("generated R files parse and YAML files load", {
  project_path <- make_project_path("template-files")

  create_analysis_project(
    path = project_path,
    use_renv = FALSE,
    use_git = FALSE,
    use_rmarkdown = TRUE,
    use_targets = TRUE,
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

test_that("optional templates only appear when requested", {
  project_path <- make_project_path("template-options")

  create_analysis_project(
    path = project_path,
    use_quarto = FALSE,
    use_rmarkdown = FALSE,
    use_targets = FALSE,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_false(file.exists(file.path(project_path, "_quarto.yml")))
  expect_false(file.exists(file.path(project_path, "reports", "exploratory_analysis.Rmd")))
  expect_false(file.exists(file.path(project_path, "_targets.R")))
})
