library(targets)

{{ targets_loading_block }}

tar_option_set(
  packages = project_packages()
)

list(
  tar_target(parameters, get_global_parameters()),
  tar_target(paths, project_paths()),
  tar_target(example_result, summary(cars))
)
