dependency_profiles <- function() {
  list(
    minimal = c(
      "data.table",
      "dplyr",
      "ggplot2",
      "readr",
      "readxl",
      "here",
      "fs",
      "glue",
      "yaml"
    ),
    analysis = c(
      "data.table",
      "dplyr",
      "tidyr",
      "purrr",
      "ggplot2",
      "readr",
      "readxl",
      "stringr",
      "lubridate",
      "here",
      "fs",
      "glue",
      "yaml",
      "DT"
    ),
    modelling = c(
      "data.table",
      "dplyr",
      "tidyr",
      "purrr",
      "ggplot2",
      "broom",
      "modelr",
      "Matrix",
      "lme4",
      "nlme",
      "mgcv",
      "here",
      "fs",
      "glue",
      "yaml"
    ),
    geospatial = c(
      "data.table",
      "dplyr",
      "ggplot2",
      "sf",
      "terra",
      "lubridate",
      "ncdf4",
      "here",
      "fs",
      "glue",
      "yaml"
    ),
    `package-development` = c(
      "devtools",
      "pkgload",
      "usethis",
      "roxygen2",
      "testthat",
      "pkgdown",
      "lintr",
      "styler",
      "covr",
      "here",
      "fs",
      "glue",
      "yaml"
    )
  )
}

resolve_dependency_profile <- function(
    dependency_profile,
    packages = NULL,
    code_loading = "package",
    use_config = TRUE,
    use_quarto = TRUE,
    use_rmarkdown = FALSE,
    use_targets = FALSE,
    use_lintr = FALSE,
    use_styler = FALSE,
    use_pkgdown = FALSE,
    include_core_packages = TRUE,
    sort_packages = FALSE) {
  profiles <- dependency_profiles()

  if (dependency_profile == "custom") {
    packages <- validate_package_names(packages, allow_null = FALSE)
    resolved <- packages
  } else {
    resolved <- profiles[[dependency_profile]]
    packages <- validate_package_names(packages, allow_null = TRUE)
    resolved <- c(resolved, packages)
  }

  if (isTRUE(include_core_packages)) {
    resolved <- c(resolved, "here", "fs", "glue", "yaml")
  }

  if (identical(code_loading, "package")) {
    resolved <- c(resolved, "pkgload", "testthat")
  }

  if (identical(code_loading, "box")) {
    resolved <- c(resolved, "box")
  }

  if (isTRUE(use_config)) {
    resolved <- c(resolved, "config")
  }

  if (isTRUE(use_quarto)) {
    resolved <- c(resolved, "quarto")
  }

  if (isTRUE(use_rmarkdown)) {
    resolved <- c(resolved, "rmarkdown")
  }

  if (isTRUE(use_targets)) {
    resolved <- c(resolved, "targets")
  }

  if (isTRUE(use_lintr)) {
    resolved <- c(resolved, "lintr")
  }

  if (isTRUE(use_styler)) {
    resolved <- c(resolved, "styler")
  }

  if (isTRUE(use_pkgdown)) {
    resolved <- c(resolved, "pkgdown")
  }

  resolved <- unique(resolved)

  if (isTRUE(sort_packages)) {
    resolved <- sort(resolved)
  }

  resolved
}
