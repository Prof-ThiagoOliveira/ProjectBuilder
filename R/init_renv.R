#' Initialise renv when available
#'
#' @param path Project root path.
#' @param snapshot Logical. Should `renv::snapshot()` be run?
#' @param strict Logical. Should failures error instead of warning?
#'
#' @return `NULL` on success or a warning message when renv was not initialised.
init_renv_project <- function(
    path,
    packages = character(),
    snapshot = TRUE,
    strict = FALSE) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    return("The `renv` package is not installed; skipped renv initialisation.")
  }

  result <- tryCatch(
    {
      renv::init(project = path, bare = TRUE)

      if (length(packages) > 0L) {
        renv::install(packages, project = path)
      }

      if (isTRUE(snapshot)) {
        renv::snapshot(project = path, prompt = FALSE)
      }

      NULL
    },
    error = function(error) conditionMessage(error)
  )

  if (!is.null(result) && isTRUE(strict)) {
    rlang::abort(result)
  }

  if (!is.null(result)) {
    return(paste0("renv initialisation failed: ", result))
  }

  NULL
}
