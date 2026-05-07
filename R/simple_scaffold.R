create_directories_from_plan <- function(path, directories) {
  created <- character()

  for (directory in unique(directories)) {
    full_path <- fs::path(path, directory)

    if (!fs::dir_exists(full_path)) {
      fs::dir_create(full_path, recurse = TRUE)
      created <- c(created, full_path)
    }
  }

  created
}

simple_gitkeep_paths <- function() {
  c(
    "analysis/.gitkeep",
    "data/raw/.gitkeep",
    "data/processed/.gitkeep",
    "outputs/.gitkeep"
  )
}

simple_template_data <- function(project_name, preset, packages) {
  list(
    project_name = project_name,
    preset = preset,
    package_vector = if (length(packages) == 0L) {
      "none yet"
    } else {
      paste(packages, collapse = ", ")
    }
  )
}

initial_project_registry <- function(use_quarto = TRUE) {
  objects <- list()

  if (isTRUE(use_quarto)) {
    objects$main_report <- list(
      type = "report",
      source = "reports/main_report.qmd",
      output = "outputs/reports/main_report.html",
      status = "active"
    )
  }

  list(objects = objects)
}

check_simple_scaffold_integrity <- function(path, plan) {
  required_paths <- unique(c(plan$user_files, plan$internal_files))
  missing <- required_paths[!vapply(
    required_paths,
    function(relative_path) {
      fs::file_exists(fs::path(path, relative_path)) ||
        fs::dir_exists(fs::path(path, relative_path))
    },
    logical(1)
  )]

  if (length(missing) == 0L) {
    return(character())
  }

  paste0("Expected scaffold file or directory is missing: ", missing)
}

create_simple_analysis_project <- function(
    path,
    project_name,
    preset,
    dependency_profile,
    selected_packages,
    use_quarto,
    use_renv,
    use_git,
    use_config,
    install_packages,
    snapshot_renv,
    run_health_check,
    strict,
    overwrite,
    open,
    inherited_warnings = character()) {
  plan <- plan_project_scaffold(
    preset = preset,
    mode = "simple",
    use_quarto = use_quarto,
    use_renv = use_renv,
    use_git = use_git,
    use_config = use_config,
    dependency_profile = dependency_profile
  )

  template_data <- simple_template_data(
    project_name = project_name,
    preset = preset,
    packages = selected_packages
  )

  warnings <- inherited_warnings
  files_created <- character()
  files_skipped <- character()

  fs::dir_create(path, recurse = TRUE)

  cli::cli_alert_info("Creating analyst-friendly project scaffold at {.path {path}}")

  directories_created <- create_directories_from_plan(path, plan$directories)

  simple_core <- write_registered_templates(
    path = path,
    project_name = project_name,
    registry = template_registry("simple_core", options = plan$options),
    overwrite = overwrite,
    template_data = template_data
  )
  files_created <- c(files_created, simple_core$files_created)
  files_skipped <- c(files_skipped, simple_core$files_skipped)

  simple_internal <- write_registered_templates(
    path = path,
    project_name = project_name,
    registry = template_registry("simple_internal", options = plan$options),
    overwrite = overwrite,
    template_data = template_data
  )
  files_created <- c(files_created, simple_internal$files_created)
  files_skipped <- c(files_skipped, simple_internal$files_skipped)

  if (isTRUE(use_quarto)) {
    simple_reports <- write_registered_templates(
      path = path,
      project_name = project_name,
      registry = template_registry("simple_quarto", options = plan$options),
      overwrite = overwrite,
      template_data = template_data
    )
    files_created <- c(files_created, simple_reports$files_created)
    files_skipped <- c(files_skipped, simple_reports$files_skipped)
  }

  if (isTRUE(use_config)) {
    config_result <- write_yaml_file(
      fs::path(path, "project.yml"),
      default_project_config(
        project_name = project_name,
        preset = preset,
        mode = "simple",
        packages = selected_packages,
        use_quarto = use_quarto
      ),
      overwrite = overwrite
    )

    if (config_result$status %in% c("created", "overwritten")) {
      files_created <- c(files_created, config_result$path)
    } else {
      files_skipped <- c(files_skipped, config_result$path)
    }
  }

  registry_result <- write_yaml_file(
    fs::path(path, ".projectSetupR", "project_registry.yml"),
    initial_project_registry(use_quarto = use_quarto),
    overwrite = overwrite
  )

  if (registry_result$status %in% c("created", "overwritten")) {
    files_created <- c(files_created, registry_result$path)
  } else {
    files_skipped <- c(files_skipped, registry_result$path)
  }

  rproj_result <- create_rproj_file(path, project_name, overwrite = overwrite)
  if (rproj_result$status %in% c("created", "overwritten")) {
    files_created <- c(files_created, rproj_result$path)
  } else {
    files_skipped <- c(files_skipped, rproj_result$path)
  }

  if (isTRUE(use_git)) {
    git_files <- create_git_files(
      path,
      overwrite = overwrite,
      mode = "simple",
      gitkeep_files = simple_gitkeep_paths()
    )
    files_created <- c(files_created, git_files$files_created)
    files_skipped <- c(files_skipped, git_files$files_skipped)
  }

  renv_available <- requireNamespace("renv", quietly = TRUE)

  if (isTRUE(use_renv)) {
    renv_warning <- init_renv_project(
      path,
      packages = if (isTRUE(install_packages) && renv_available) selected_packages else character(),
      snapshot = snapshot_renv,
      strict = strict
    )

    if (!is.null(renv_warning)) {
      warnings <- c(warnings, renv_warning)
      cli::cli_alert_warning(renv_warning)
    }
  }

  if (isTRUE(use_git)) {
    git_warning <- init_git_repo(path, strict = strict)

    if (!is.null(git_warning)) {
      warnings <- c(warnings, git_warning)
      cli::cli_alert_warning(git_warning)
    }
  }

  if (isTRUE(install_packages) && (!isTRUE(use_renv) || !renv_available)) {
    install_warning <- install_packages_for_project(selected_packages, strict = strict)

    if (!is.null(install_warning)) {
      warnings <- c(warnings, install_warning)
      cli::cli_alert_warning(install_warning)
    }
  }

  if (isTRUE(run_health_check)) {
    health_warnings <- check_simple_scaffold_integrity(path, plan)

    if (length(health_warnings) > 0L && isTRUE(strict)) {
      rlang::abort(health_warnings[[1]])
    }

    if (length(health_warnings) > 0L) {
      warnings <- c(warnings, health_warnings)

      for (warning in health_warnings) {
        cli::cli_alert_warning(warning)
      }
    }
  }

  if (isTRUE(open)) {
    warnings <- c(
      warnings,
      "Project opening is not automated by this package; open the .Rproj file manually if needed."
    )
  }

  result <- new_analysis_project_scaffold(
    path = path,
    project_name = project_name,
    template = "standard",
    dependency_profile = dependency_profile,
    code_loading = "source",
    packages = selected_packages,
    directories_created = unique(filesystem_normalize(directories_created)),
    files_created = unique(filesystem_normalize(files_created)),
    files_skipped = unique(filesystem_normalize(files_skipped)),
    warnings = warnings,
    use_quarto = use_quarto,
    use_rmarkdown = FALSE,
    use_targets = FALSE,
    use_renv = use_renv,
    use_git = use_git,
    preset = preset,
    mode = "simple",
    entrypoint = "run_project.R",
    guide = "README.md"
  )

  cli::cli_alert_success("Project created successfully.")
  cat("\nStart here:\n")
  cat("1. Open: ", paste0(project_name, ".Rproj"), "\n", sep = "")
  cat("2. Put raw data in: data/raw/\n")
  cat("3. Edit: run_project.R\n")
  cat('4. Run in R: source("run_project.R")', "\n", sep = "")

  if (isTRUE(use_quarto)) {
    cat("5. Write report text in: reports/main_report.qmd\n")
  }

  cat("\nUseful commands:\n")
  cat("- projectSetupR::project_status()\n")
  cat('- projectSetupR::new_project_object("clean_data", type = "data_cleaning")\n')
  cat("- projectSetupR::run_project()\n")

  invisible(result)
}
