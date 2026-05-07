install_packages_for_project <- function(
    packages,
    method = c("auto", "pak", "install.packages"),
    strict = FALSE) {
  method <- match.arg(method)

  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) == 0L) {
    return(NULL)
  }

  result <- tryCatch(
    {
      if (method == "auto") {
        method <- if (requireNamespace("pak", quietly = TRUE)) {
          "pak"
        } else {
          "install.packages"
        }
      }

      if (method == "pak") {
        pak::pak(missing)
      } else {
        utils::install.packages(missing)
      }

      NULL
    },
    error = function(error) conditionMessage(error)
  )

  if (!is.null(result) && isTRUE(strict)) {
    rlang::abort(result)
  }

  result
}
