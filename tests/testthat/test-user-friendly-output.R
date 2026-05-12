test_that("project_layers prints a compact user-facing summary", {
  layers <- project_layers()

  expect_s3_class(layers, "projflow_layers")
  expect_s3_class(layers, "data.frame")
  expect_named(layers, c("layer", "name", "responsibility", "primary_functions"))

  output <- capture.output(print(layers))
  expect_true(any(grepl("projflow architecture layers", output, fixed = TRUE)))
  expect_true(any(grepl("Layer 1: project_structure", output, fixed = TRUE)))
  expect_true(any(grepl("Primary functions:", output, fixed = TRUE)))
  expect_true(any(grepl("as.data.frame", output, fixed = TRUE)))
})

test_that("project_structure prints a compact checklist", {
  project_path <- make_project_path("structure-print")

  new_project(
    path = project_path,
    infrastructure = character(),
    include_example = FALSE,
    open = FALSE
  )

  structure <- project_structure(project_path)

  expect_s3_class(structure, "projflow_structure")
  expect_s3_class(structure, "data.frame")
  expect_named(
    structure,
    c(
      "role", "path", "layer", "exists", "registered_scripts",
      "registered_reports", "registered_outputs"
    )
  )
  expect_true(all(structure$exists))

  output <- capture.output(print(structure))
  expect_true(any(grepl("projflow project structure", output, fixed = TRUE)))
  expect_true(any(grepl("Registered objects:", output, fixed = TRUE)))
  expect_true(any(grepl("Structure checklist:", output, fixed = TRUE)))
  expect_true(any(grepl("[ok", output, fixed = TRUE)))
  expect_true(any(grepl("as.data.frame", output, fixed = TRUE)))
})
