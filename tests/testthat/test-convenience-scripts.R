test_that("advanced mode creates convenience scripts conditionally", {
  project_path <- make_project_path("convenience-advanced")

  create_analysis_project(
    path = project_path,
    mode = "advanced",
    use_quarto = TRUE,
    use_rmarkdown = TRUE,
    use_targets = TRUE,
    use_renv = TRUE,
    use_git = FALSE,
    use_lintr = TRUE,
    use_styler = TRUE,
    use_pkgdown = TRUE,
    open = FALSE
  )

  expect_true(file.exists(file.path(project_path, "scripts", "install_packages.R")))
  expect_true(file.exists(file.path(project_path, "scripts", "restore_environment.R")))
  expect_true(file.exists(file.path(project_path, "scripts", "render_reports.R")))
  expect_true(file.exists(file.path(project_path, "scripts", "render_rmarkdown_reports.R")))
  expect_true(file.exists(file.path(project_path, "scripts", "run_pipeline.R")))
  expect_true(file.exists(file.path(project_path, "scripts", "lint_project.R")))
  expect_true(file.exists(file.path(project_path, "scripts", "style_project.R")))
  expect_true(file.exists(file.path(project_path, "scripts", "build_site.R")))
})

test_that("simple mode does not generate the advanced convenience script set", {
  project_path <- make_project_path("convenience-simple")

  create_analysis_project(
    path = project_path,
    use_quarto = TRUE,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_false(dir.exists(file.path(project_path, "scripts")))
  expect_true(file.exists(file.path(project_path, "run_project.R")))
  expect_true(file.exists(file.path(project_path, "project.yml")))
})
