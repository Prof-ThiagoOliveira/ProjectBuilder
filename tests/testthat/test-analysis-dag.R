test_that("project_analysis_dag builds an executable graph", {
  project_path <- make_project_path("analysis-dag")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  new_script("prepare_inputs", script_type = "data_preparation", root = project_path, open = FALSE)
  new_script("fit_model", script_type = "statistical_analysis", root = project_path, open = FALSE)
  new_output("prepared_inputs", type = "dataset", path = "outputs/data/prepared_inputs.rds", root = project_path)
  update_project_object("prepared_inputs", generated_by = "prepare_inputs", root = project_path)

  registry <- yaml::read_yaml(file.path(project_path, ".projflow", "project_registry.yml"))
  registry$scripts$fit_model$inputs <- "prepared_inputs"
  yaml::write_yaml(registry, file.path(project_path, ".projflow", "project_registry.yml"))

  dag <- project_analysis_dag(project_path)
  expect_s3_class(dag, "project_analysis_dag")
  expect_true(any(dag$edges$from == "script:prepare_inputs" & dag$edges$to == "output:prepared_inputs"))
  expect_true(any(dag$edges$from == "output:prepared_inputs" & dag$edges$to == "script:fit_model"))

  check <- validate_project_dag(dag = dag)
  expect_true(check$ok)

  order <- topological_project_order(project_path, type = "scripts")
  expect_true(match("prepare_inputs", order) < match("fit_model", order))
})

test_that("project_analysis_dag prints a compact summary", {
  project_path <- make_project_path("dag-print")

  new_project(
    path = project_path,
    components = c("data_preparation", "statistical_analysis", "report"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  dag <- project_analysis_dag(project_path)

  output <- capture.output(print(dag))
  expect_true(any(grepl("projflow analysis DAG", output, fixed = TRUE)))
  expect_true(any(grepl("Node types:", output, fixed = TRUE)))
  expect_true(any(grepl("Relationships:", output, fixed = TRUE)))
  expect_true(any(grepl("project_registered_files()", output, fixed = TRUE)))
  expect_true(any(grepl("topological_project_order()", output, fixed = TRUE)))
})

test_that("validate_project_dag prints a compact summary", {
  project_path <- make_project_path("dag-check-print")

  new_project(
    path = project_path,
    components = c("data_preparation", "statistical_analysis", "report"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  check <- validate_project_dag(project_path)
  expect_s3_class(check, "project_dag_check")

  output <- capture.output(print(check))
  expect_true(any(grepl("projflow DAG check", output, fixed = TRUE)))
  expect_true(any(grepl("Status:", output, fixed = TRUE)))
  expect_true(any(grepl("Issues:", output, fixed = TRUE)))
  expect_true(any(grepl("Information", output, fixed = TRUE)))
})

test_that("topological_project_order prints mixed node types without failing", {
  project_path <- make_project_path("topological-print")

  new_project(
    path = project_path,
    components = c("data_preparation", "statistical_analysis", "report"),
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  order <- topological_project_order(project_path, type = "all")

  output <- capture.output(print(order))
  expect_true(any(grepl("projflow topological execution order", output, fixed = TRUE)))
  expect_true(any(grepl("Ordered nodes:", output, fixed = TRUE)))
  expect_true(any(grepl("script:prepare_inputs", output, fixed = TRUE)))
})

test_that("validate_project_dag detects cycles", {
  dag <- structure(
    list(
      root = tempdir(),
      nodes = data.frame(
        id = c("script:a", "script:b"),
        label = c("a", "b"),
        type = c("script", "script"),
        path = NA_character_,
        status = NA_character_,
        object = c("a", "b"),
        order = c(1, 2),
        stringsAsFactors = FALSE
      ),
      edges = data.frame(
        from = c("script:a", "script:b"),
        to = c("script:b", "script:a"),
        relationship = c("script_to_script", "script_to_script"),
        stringsAsFactors = FALSE
      )
    ),
    class = "project_analysis_dag"
  )

  check <- validate_project_dag(dag = dag)
  expect_false(check$ok)
  expect_true("dag_cycle" %in% check$errors$check)
})
