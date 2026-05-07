check_scaffold_integrity <- function(
    path,
    code_loading,
    use_quarto,
    use_rmarkdown,
    use_targets,
    use_config,
    use_git,
    use_renv) {
  required_dirs <- c(
    "scripts",
    "data/raw",
    "data/external",
    "data/interim",
    "data/processed",
    "data/metadata",
    "outputs/tables",
    "outputs/figures",
    "outputs/models",
    "outputs/reports",
    "outputs/logs",
    "reports",
    "tests/testthat"
  )

  if (identical(code_loading, "box")) {
    required_dirs <- c(required_dirs, "modules")
  } else {
    required_dirs <- c(required_dirs, "R")
  }

  if (isTRUE(use_quarto) || isTRUE(use_rmarkdown)) {
    required_dirs <- c(required_dirs, "reports/templates")
  }

  required_files <- c(
    "README.md",
    "PROJECT_GUIDE.md",
    "DESCRIPTION",
    ".here",
    fs::path("scripts", "00_start_here.R"),
    fs::path("scripts", "_load_project.R"),
    fs::path("scripts", "install_packages.R")
  )

  if (identical(code_loading, "package")) {
    required_files <- c(required_files, "NAMESPACE")
  }

  if (isTRUE(use_config)) {
    required_files <- c(required_files, "config.yml")
  }

  if (isTRUE(use_git)) {
    required_files <- c(required_files, ".gitignore", ".Rbuildignore", ".gitattributes")
  }

  if (isTRUE(use_quarto)) {
    required_files <- c(required_files, "_quarto.yml", fs::path("scripts", "render_reports.R"))
  }

  if (isTRUE(use_rmarkdown)) {
    required_files <- c(required_files, fs::path("scripts", "render_rmarkdown_reports.R"))
  }

  if (isTRUE(use_targets)) {
    required_files <- c(required_files, "_targets.R", fs::path("scripts", "run_pipeline.R"))
  }

  if (isTRUE(use_renv)) {
    required_files <- c(required_files, fs::path("scripts", "restore_environment.R"))
  }

  missing_dirs <- required_dirs[!fs::dir_exists(fs::path(path, required_dirs))]
  missing_files <- required_files[!fs::file_exists(fs::path(path, required_files))]

  c(
    if (length(missing_dirs) > 0L) {
      paste0("Missing directories after scaffold creation: ", paste(missing_dirs, collapse = ", "))
    },
    if (length(missing_files) > 0L) {
      paste0("Missing files after scaffold creation: ", paste(missing_files, collapse = ", "))
    }
  )
}
