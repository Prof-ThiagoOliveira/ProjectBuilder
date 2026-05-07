test_that("convenience scripts are created conditionally", {
  project_path <- make_project_path("convenience-all")

  create_analysis_project(
    path = project_path,
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

test_that("optional scripts and config files are omitted when disabled", {
  project_path <- make_project_path("convenience-minimal")

  create_analysis_project(
    path = project_path,
    use_quarto = FALSE,
    use_rmarkdown = FALSE,
    use_targets = FALSE,
    use_renv = FALSE,
    use_git = FALSE,
    use_config = FALSE,
    use_lintr = FALSE,
    use_styler = FALSE,
    use_pkgdown = FALSE,
    open = FALSE
  )

  expect_false(file.exists(file.path(project_path, "scripts", "restore_environment.R")))
  expect_false(file.exists(file.path(project_path, "scripts", "render_reports.R")))
  expect_false(file.exists(file.path(project_path, "scripts", "render_rmarkdown_reports.R")))
  expect_false(file.exists(file.path(project_path, "scripts", "run_pipeline.R")))
  expect_false(file.exists(file.path(project_path, "scripts", "lint_project.R")))
  expect_false(file.exists(file.path(project_path, "scripts", "style_project.R")))
  expect_false(file.exists(file.path(project_path, "scripts", "build_site.R")))
  expect_false(file.exists(file.path(project_path, "config.yml")))
})
