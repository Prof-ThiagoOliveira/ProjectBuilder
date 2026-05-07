# Clean data ---------------------------------------------------------------

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

# Use this script to clean imported data and save analysis-ready outputs to:
# data/processed/

message("Cleaning step not yet implemented.")
