box::use(pkgs = ./dependencies[missing_project_packages])

project_health_check <- function() {
  required_dirs <- c(
    "modules",
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
    "reports"
  )

  missing_packages <- pkgs$missing_project_packages()
  git_available <- requireNamespace("gert", quietly = TRUE) || nzchar(Sys.which("git"))
  has_git_repo <- dir.exists(".git")
  git_is_clean <- NA

  if (isTRUE(has_git_repo)) {
    if (requireNamespace("gert", quietly = TRUE)) {
      git_status <- tryCatch(
        gert::git_status(repo = "."),
        error = function(error) NULL
      )

      if (!is.null(git_status)) {
        git_is_clean <- nrow(git_status) == 0L
      }
    } else if (nzchar(Sys.which("git"))) {
      git_status <- tryCatch(
        system2("git", c("status", "--porcelain"), stdout = TRUE, stderr = FALSE),
        error = function(error) NULL
      )

      if (!is.null(git_status)) {
        git_is_clean <- length(git_status) == 0L
      }
    }
  }

  recommended_next_step <- if (length(missing_packages) > 0L) {
    'Run source("scripts/install_packages.R").'
  } else {
    "Add raw data to data/raw/ or edit scripts/01_import_data.R."
  }

  checks <- list(
    missing_directories = required_dirs[!dir.exists(required_dirs)],
    missing_packages = missing_packages,
    has_config = file.exists("config.yml"),
    has_gitignore = file.exists(".gitignore"),
    has_git_repo = has_git_repo,
    git_available = git_available,
    git_is_clean = git_is_clean,
    has_renv_lock = file.exists("renv.lock"),
    renv_available = requireNamespace("renv", quietly = TRUE),
    has_quarto = file.exists("_quarto.yml"),
    quarto_available = requireNamespace("quarto", quietly = TRUE),
    has_targets = file.exists("_targets.R"),
    targets_available = requireNamespace("targets", quietly = TRUE),
    recommended_next_step = recommended_next_step
  )

  class(checks) <- "project_health_check"
  checks
}

print.project_health_check <- function(x, ...) {
  cat("Project health check\n")
  cat("====================\n\n")

  if (length(x$missing_directories) == 0L) {
    cat("[OK] Required directories found\n")
  } else {
    cat("[WARN] Missing directories:\n")
    cat(paste0("  - ", x$missing_directories, collapse = "\n"), "\n")
  }

  if (length(x$missing_packages) == 0L) {
    cat("[OK] Required packages installed\n")
  } else {
    cat("[WARN] Missing packages:\n")
    cat(paste0("  - ", x$missing_packages, collapse = "\n"), "\n")
  }

  cat("\n")
  cat("[INFO] config.yml: ", x$has_config, "\n", sep = "")
  cat("[INFO] .gitignore: ", x$has_gitignore, "\n", sep = "")
  cat("[INFO] git available: ", x$git_available, "\n", sep = "")
  cat("[INFO] git repository: ", x$has_git_repo, "\n", sep = "")
  cat("[INFO] git clean: ", x$git_is_clean, "\n", sep = "")
  cat("[INFO] renv.lock: ", x$has_renv_lock, "\n", sep = "")
  cat("[INFO] renv available: ", x$renv_available, "\n", sep = "")
  cat("[INFO] _quarto.yml: ", x$has_quarto, "\n", sep = "")
  cat("[INFO] quarto available: ", x$quarto_available, "\n", sep = "")
  cat("[INFO] _targets.R: ", x$has_targets, "\n", sep = "")
  cat("[INFO] targets available: ", x$targets_available, "\n", sep = "")
  cat("\nRecommended next step: ", x$recommended_next_step, "\n", sep = "")

  invisible(x)
}

box::export(project_health_check, print.project_health_check)
