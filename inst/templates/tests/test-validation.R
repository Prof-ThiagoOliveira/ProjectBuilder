testthat::test_that("validation helpers detect expected problems", {
  data <- data.frame(id = c(1, 1), value = c("a", "b"))

  testthat::expect_error(
    check_required_columns(data, c("missing_column")),
    "Missing required columns"
  )

  testthat::expect_error(
    check_unique_key(data, "id"),
    "not unique"
  )
})
