test_that("project plans can be inspected, plotted and materialised", {
  project_path <- make_project_path("planned-project")

  plan <- plan_project(
    path = project_path,
    components = c("data_preparation", "statistical_analysis", "report"),
    infrastructure = character()
  )

  expect_s3_class(plan, "project_plan")
  expect_true("data_preparation" %in% plan$components)
  expect_true("statistical_analysis" %in% plan$components)

  network <- project_plan_network_data(plan)
  expect_true(all(c("nodes", "edges") %in% names(network)))
  expect_true(nrow(network$nodes) > 0L)
  expect_true(nrow(network$edges) > 0L)
  expect_true("project:root" %in% network$nodes$id)

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(plan))

  result <- new_project(plan = plan, open = FALSE)
  expect_s3_class(result, "analysis_project_scaffold")
  expect_true(dir.exists(project_path))
  expect_true(file.exists(file.path(project_path, "project.yml")))
})

test_that("new_project prevents plan and scaffold argument divergence", {
  project_path <- make_project_path("planned-project-conflict")

  plan <- plan_project(
    path = project_path,
    components = c("statistical_analysis", "report"),
    infrastructure = character()
  )

  expect_error(
    new_project(plan = plan, components = "data_preparation", open = FALSE),
    "When `plan` is supplied"
  )
})
