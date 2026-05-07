#' Project package list
#'
#' Lists the R packages used by this project.
#'
#' @return Character vector of package names.
#' @export
project_packages <- function() {
  {{ package_vector }}
}

#' Identify missing project packages
#'
#' @param packages Character vector of package names.
#'
#' @return Character vector of missing packages.
#' @export
missing_project_packages <- function(packages = project_packages()) {
  packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
}

#' Check project packages
#'
#' @param packages Character vector of package names.
#'
#' @return Invisibly returns TRUE if all packages are available.
#' @export
check_project_packages <- function(packages = project_packages()) {
  missing <- missing_project_packages(packages)

  if (length(missing) > 0L) {
    stop(
      "Missing required packages: ",
      paste(missing, collapse = ", "),
      "\n\nInstall them with:\n",
      "install_project_packages()",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Install project packages
#'
#' @param packages Character vector of package names.
#' @param method Installation method.
#'
#' @return Invisibly returns TRUE.
#' @export
install_project_packages <- function(
    packages = project_packages(),
    method = c("auto", "pak", "install.packages")) {
  method <- match.arg(method)

  missing <- missing_project_packages(packages)

  if (length(missing) == 0L) {
    message("All project packages are already installed.")
    return(invisible(TRUE))
  }

  if (method == "auto") {
    method <- if (requireNamespace("pak", quietly = TRUE)) {
      "pak"
    } else {
      "install.packages"
    }
  }

  if (method == "pak") {
    if (!requireNamespace("pak", quietly = TRUE)) {
      stop(
        "Package 'pak' is not installed. Use method = 'install.packages' instead.",
        call. = FALSE
      )
    }

    pak::pak(missing)
  } else {
    install.packages(missing)
  }

  invisible(TRUE)
}

#' Load project packages
#'
#' @param packages Character vector of package names.
#' @param install_missing Logical. Should missing packages be installed?
#'
#' @return Invisibly returns TRUE.
#' @export
load_project_packages <- function(
    packages = project_packages(),
    install_missing = FALSE) {
  missing <- missing_project_packages(packages)

  if (length(missing) > 0L) {
    if (isTRUE(install_missing)) {
      install_project_packages(missing)
    } else {
      stop(
        "Missing required packages: ",
        paste(missing, collapse = ", "),
        "\n\nInstall them with:\n",
        "install_project_packages()",
        call. = FALSE
      )
    }
  }

  invisible(lapply(packages, library, character.only = TRUE))
  invisible(TRUE)
}
