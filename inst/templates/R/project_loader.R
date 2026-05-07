#' Load project R files
#'
#' Controlled source loader for simple projects.
#'
#' @param root Project root.
#'
#' @return Invisibly returns TRUE.
load_project_files <- function(root = ".") {
  files <- file.path(
    root,
    c(
      "R/project_settings.R",
      "R/dependencies.R",
      "R/paths.R",
      "R/utils.R",
      "R/validation.R",
      "R/project_setup.R",
      "R/project_health_check.R"
    )
  )

  missing <- files[!file.exists(files)]

  if (length(missing) > 0L) {
    stop(
      "Missing project R files: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  for (file in files) {
    source(file, local = FALSE)
  }

  invisible(TRUE)
}
