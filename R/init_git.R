#' Initialise a Git repository when available
#'
#' @param path Project root path.
#' @param strict Logical. Should failures error instead of warning?
#'
#' @return `NULL` on success or a warning message when Git was not initialised.
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
