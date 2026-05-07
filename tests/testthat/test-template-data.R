test_that("template data helpers return expected formats", {
  package_vector <- projectSetupR:::format_package_vector(c("alpha", "beta"))
  imports <- projectSetupR:::format_description_imports(c("alpha", "beta"))

  expect_match(package_vector, '^c\\(')
  expect_match(package_vector, '"alpha"')
  expect_match(imports, "    alpha,\n    beta", fixed = TRUE)
  expect_identical(projectSetupR:::make_package_name("123 my-project!"), "myproject")
})

test_that("render_template supports multiple variables", {
  rendered <- projectSetupR:::render_template(
    "{{ greeting }}, {{ target }}!",
    data = list(greeting = "Hello", target = "world")
  )

  expect_identical(rendered, "Hello, world!")
})

test_that("write_registered_templates remains backwards compatible with project_name", {
  project_path <- make_project_path("template-backward")
  dir.create(project_path, recursive = TRUE)
  template_file <- file.path(projectSetupR:::template_root(), "tmp_backward_template.txt")
  writeLines("Project: {{ project_name }}", template_file)
  withr::defer(unlink(template_file), teardown_env())

  result <- projectSetupR:::write_registered_templates(
    path = project_path,
    project_name = "Example Project",
    registry = list(list(source = "tmp_backward_template.txt", target = "README.md")),
    overwrite = FALSE
  )

  expect_true(file.exists(file.path(project_path, "README.md")))
  expect_true(length(result$files_created) == 1L)
  expect_match(
    paste(readLines(file.path(project_path, "README.md"), warn = FALSE), collapse = "\n"),
    "Example Project"
  )
})

test_that("scaffold planning separates simple and advanced defaults", {
  simple_plan <- projectSetupR:::plan_project_scaffold(
    preset = "analysis",
    mode = "simple",
    use_quarto = TRUE,
    use_renv = FALSE,
    use_git = FALSE
  )

  advanced_plan <- projectSetupR:::plan_project_scaffold(
    preset = "package",
    mode = "advanced",
    use_quarto = TRUE,
    use_targets = TRUE,
    use_renv = FALSE,
    use_git = FALSE,
    use_config = TRUE,
    code_loading = "package",
    dependency_profile = "package-development"
  )

  expect_true("run_project.R" %in% simple_plan$user_files)
  expect_true("project.yml" %in% simple_plan$user_files)
  expect_true(".projectSetupR/project_registry.yml" %in% simple_plan$internal_files)
  expect_false("DESCRIPTION" %in% simple_plan$user_files)

  expect_true("README.md" %in% advanced_plan$user_files)
  expect_true("DESCRIPTION" %in% advanced_plan$optional_files)
  expect_true("NAMESPACE" %in% advanced_plan$optional_files)
})
