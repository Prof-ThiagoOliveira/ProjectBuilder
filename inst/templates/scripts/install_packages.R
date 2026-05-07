# Install project packages -------------------------------------------------

if (file.exists("R/dependencies.R")) {
  source("R/dependencies.R")
} else if (file.exists("modules/dependencies.R")) {
  source("modules/dependencies.R")
} else {
  stop("Cannot find dependencies.R.", call. = FALSE)
}

install_project_packages()
