project_packages <- function() {
  {{ package_vector }}
}

missing_project_packages <- function(packages = project_packages()) {
  packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
}

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

if ("box" %in% loadedNamespaces()) {
  box::export(
    project_packages,
    missing_project_packages,
    check_project_packages,
    install_project_packages,
    load_project_packages
  )
}
