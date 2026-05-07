# Start here ---------------------------------------------------------------

source("scripts/_load_project.R")

project <- setup_project(
  install_missing = FALSE,
  check_paths = TRUE,
  set_seed = TRUE
)

params <- project$parameters
paths <- project$paths

print(project_health_check())

message("Project is ready.")
message("Next step: add raw data to data/raw/ or edit scripts/01_import_data.R.")
