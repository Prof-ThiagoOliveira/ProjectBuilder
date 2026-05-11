#' Initialise renv when available
#'
#' @param path Project root path where \code{renv} should be initialised.
#' @param packages Character vector of package names to install into the project
#'   library immediately after \code{renv::init(project = path, bare = TRUE)}.
#' @param snapshot Logical scalar. If \code{TRUE}, run \code{renv::snapshot()} after the
#'   optional package installation step.
#' @param strict Logical scalar. If \code{TRUE}, initialisation failures raise an
#'   error; otherwise a warning-style message is returned.
#'
#' @return \code{NULL} on success or a warning message when renv was not initialised.
#' @examples
#' \dontrun{
#' tmp <- tempfile("projflow-renv-")
#' dir.create(tmp)
#' projflow:::init_renv_project(
#'   path = tmp,
#'   packages = c("fs", "yaml"),
#'   snapshot = FALSE
#' )
#' }
#' @author Thiago de Paula Oliveira
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
