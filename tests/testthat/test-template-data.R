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
