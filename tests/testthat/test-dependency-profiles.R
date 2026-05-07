test_that("dependency profiles resolve expected package sets", {
  expect_identical(
    projectSetupR:::dependency_profiles()$minimal,
    c("data.table", "dplyr", "ggplot2", "readr", "readxl", "here", "fs", "glue", "yaml")
  )

  expect_true(all(c("tidyr", "purrr", "DT") %in% projectSetupR:::dependency_profiles()$analysis))
  expect_true(all(c("broom", "modelr", "lme4", "mgcv") %in% projectSetupR:::dependency_profiles()$modelling))
  expect_true(all(c("sf", "terra", "ncdf4") %in% projectSetupR:::dependency_profiles()$geospatial))
  expect_true(all(c("devtools", "pkgload", "pkgdown", "covr") %in%
    projectSetupR:::dependency_profiles()[["package-development"]]))
})

test_that("custom profile requires packages", {
  expect_error(
    projectSetupR:::resolve_dependency_profile("custom", packages = NULL),
    "`packages`"
  )
})

test_that("duplicate packages are removed and loading/tooling packages are added", {
  resolved <- projectSetupR:::resolve_dependency_profile(
    dependency_profile = "minimal",
    packages = c("fs", "custompkg", "custompkg"),
    code_loading = "package",
    use_targets = TRUE
  )

  expect_identical(sum(resolved == "custompkg"), 1L)
  expect_true(all(c("pkgload", "testthat", "targets") %in% resolved))
})

test_that("box mode adds box and preserves custom order", {
  resolved <- projectSetupR:::resolve_dependency_profile(
    dependency_profile = "custom",
    packages = c("zeta", "alpha"),
    code_loading = "box",
    use_config = FALSE,
    use_quarto = FALSE,
    include_core_packages = FALSE
  )

  expect_identical(resolved[1:2], c("zeta", "alpha"))
  expect_true("box" %in% resolved)
})
