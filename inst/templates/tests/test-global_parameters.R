testthat::test_that("global parameters include expected fields", {
  params <- get_global_parameters()

  testthat::expect_true(all(
    c(
      "project_name",
      "dependency_profile",
      "project_version",
      "project_owner",
      "report_date",
      "random_seed",
      "timezone",
      "default_output_format"
    ) %in% names(params)
  ))
})
