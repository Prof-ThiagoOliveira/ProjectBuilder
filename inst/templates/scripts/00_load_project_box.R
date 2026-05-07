# Load project code --------------------------------------------------------

if (!requireNamespace("box", quietly = TRUE)) {
  stop(
    "Package 'box' is required for module-style project loading. ",
    "Install it with install.packages('box').",
    call. = FALSE
  )
}

box::use(project = ../modules/project_setup[setup_project])
box::use(health = ../modules/project_health_check[project_health_check, print.project_health_check])

setup_project <- project$setup_project
project_health_check <- health$project_health_check
print.project_health_check <- health$`print.project_health_check`

message("Project code loaded.")
