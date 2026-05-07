test_that("option combinations create the expected optional files", {
  combinations <- expand.grid(
    use_quarto = c(TRUE, FALSE),
    use_rmarkdown = c(TRUE, FALSE),
    use_targets = c(TRUE, FALSE),
    use_git = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  for (index in seq_len(nrow(combinations))) {
    settings <- combinations[index, ]
    project_path <- make_project_path(paste0("options-", index))

    result <- create_analysis_project(
      path = project_path,
      use_quarto = settings$use_quarto,
      use_rmarkdown = settings$use_rmarkdown,
      use_renv = FALSE,
      use_targets = settings$use_targets,
      use_git = settings$use_git,
      open = FALSE
    )

    expect_s3_class(result, "analysis_project_scaffold")
    expect_identical(file.exists(file.path(project_path, "_quarto.yml")), settings$use_quarto)
    expect_identical(
      file.exists(file.path(project_path, "reports", "final_report.Rmd")),
      settings$use_rmarkdown
    )
    expect_identical(file.exists(file.path(project_path, "_targets.R")), settings$use_targets)
    expect_identical(file.exists(file.path(project_path, ".gitignore")), settings$use_git)
  }
})

test_that("use_renv can be enabled without failing scaffold creation", {
  project_path <- make_project_path("options-renv")

  expect_no_error(
    create_analysis_project(
      path = project_path,
      use_renv = TRUE,
      use_git = FALSE,
      open = FALSE
    )
  )
})

test_that("template presets enable their matching scaffold components", {
  targets_path <- make_project_path("template-targets")
  targets_result <- create_analysis_project(
    path = targets_path,
    template = "targets",
    use_targets = FALSE,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_true(file.exists(file.path(targets_path, "_targets.R")))
  expect_true(any(grepl("Template 'targets' enables targets scaffolding", targets_result$warnings, fixed = TRUE)))

  quarto_path <- make_project_path("template-quarto")
  quarto_result <- create_analysis_project(
    path = quarto_path,
    template = "quarto",
    use_quarto = FALSE,
    use_renv = FALSE,
    use_git = FALSE,
    open = FALSE
  )

  expect_true(file.exists(file.path(quarto_path, "_quarto.yml")))
  expect_true(any(grepl("Template 'quarto' enables Quarto report scaffolding", quarto_result$warnings, fixed = TRUE)))
})
