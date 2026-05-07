# Export outputs -----------------------------------------------------------

if (file.exists("scripts/_load_project.R")) {
  source("scripts/_load_project.R")
} else {
  stop("Missing scripts/_load_project.R", call. = FALSE)
}

project <- setup_project(
  install_missing = FALSE,
  check_paths = TRUE,
  set_seed = TRUE
)

params <- project$parameters
paths <- project$paths

# Use this script to export final tables, figures, model objects, and logs.

message("Export step not yet implemented.")
