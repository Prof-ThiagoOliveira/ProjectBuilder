#' Initialise a Git repository when available
#'
#' @param path Project root path where a Git repository should be initialised if
#'   one does not already exist.
#' @param strict Logical scalar. If \code{TRUE}, Git initialisation failures raise an
#'   error; otherwise a warning-style message is returned.
#'
#' @return \code{NULL} on success or a warning message when Git was not initialised.
#' @examples
#' \dontrun{
#' tmp <- tempfile("projflow-git-init-")
#' dir.create(tmp)
#' projflow:::init_git_repo(tmp)
#' }
#' @author Thiago de Paula Oliveira
init_git_repo <- function(path, strict = FALSE) {
  if (fs::dir_exists(fs::path(path, ".git"))) {
    return(NULL)
  }

  result <- tryCatch(
    {
      if (requireNamespace("gert", quietly = TRUE)) {
        gert::git_init(path)
      } else if (nzchar(Sys.which("git"))) {
        system2("git", c("-C", path, "init"), stdout = TRUE, stderr = TRUE)
      } else {
        return("Git is not available; skipped repository initialisation.")
      }

      NULL
    },
    error = function(error) conditionMessage(error)
  )

  if (!is.null(result) && isTRUE(strict)) {
    rlang::abort(result)
  }

  if (!is.null(result)) {
    return(paste0("Git initialisation failed: ", result))
  }

  NULL
}
