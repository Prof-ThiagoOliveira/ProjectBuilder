testthat::test_that("project_paths returns expected names", {
  paths <- project_paths(root = tempdir())

  testthat::expect_true(all(
    c(
      "root",
      "data_raw",
      "data_external",
      "data_interim",
      "data_processed",
      "data_metadata",
      "outputs",
      "tables",
      "figures",
      "models",
      "reports",
      "logs"
    ) %in% names(paths)
  ))
})
