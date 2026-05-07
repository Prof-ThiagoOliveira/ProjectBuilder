#' Create a reproducible R analysis project
#'
#' Creates a standard folder structure, helper R files, report templates,
#' Git-safe defaults, and optional reproducibility tooling for an R analysis
#' project.
#'
#' @param path Target project path.
#' @param project_name Project name. Defaults to the basename of `path`.
#' @param template Project template. One of `"standard"`, `"targets"`, or
#'   `"quarto"`.
#' @param use_quarto Logical. Should Quarto project files be created?
#' @param use_rmarkdown Logical. Should R Markdown report files be created?
#' @param use_renv Logical. Should renv be initialised if available?
#' @param use_targets Logical. Should a targets pipeline file be created?
#' @param use_git Logical. Should Git files be created and Git initialised if
#'   available?
#' @param overwrite Logical. Should existing files be overwritten?
#' @param open Logical. Should the project be opened after creation when
#'   possible?
#'
#' @return An object of class `"analysis_project_scaffold"`.
#'
#' @details Raw data, processed data, generated outputs, logs, and common secret
#'   files are ignored by Git by default when `use_git = TRUE`.
#'
#' @export
#'
#' @examples
#' tmp <- tempfile("analysis-project-")
#' create_analysis_project(tmp, use_renv = FALSE, use_git = FALSE, open = FALSE)
create_analysis_project <- function(
    path,
    project_name = basename(normalizePath(path, mustWork = FALSE)),
    template = c("standard", "targets", "quarto"),
    dependency_profile = c(
      "minimal",
      "analysis",
      "modelling",
      "geospatial",
      "package-development",
      "custom"
    ),
    packages = NULL,
    code_loading = c("package", "box", "source"),
    use_quarto = TRUE,
    use_rmarkdown = FALSE,
    use_renv = TRUE,
    use_targets = FALSE,
    use_git = TRUE,
    use_config = TRUE,
    use_lintr = FALSE,
    use_styler = FALSE,
    use_pkgdown = FALSE,
    install_packages = FALSE,
    snapshot_renv = TRUE,
    run_health_check = TRUE,
    strict = FALSE,
    overwrite = FALSE,
    open = interactive()) {
  validate_character_vector(path, "path")
  validate_character_vector(project_name, "project_name")
  validate_package_names(packages, allow_null = TRUE)
  validate_logical_scalar(use_quarto, "use_quarto")
  validate_logical_scalar(use_rmarkdown, "use_rmarkdown")
  validate_logical_scalar(use_renv, "use_renv")
  validate_logical_scalar(use_targets, "use_targets")
  validate_logical_scalar(use_git, "use_git")
  validate_logical_scalar(use_config, "use_config")
  validate_logical_scalar(use_lintr, "use_lintr")
  validate_logical_scalar(use_styler, "use_styler")
  validate_logical_scalar(use_pkgdown, "use_pkgdown")
  validate_logical_scalar(install_packages, "install_packages")
  validate_logical_scalar(snapshot_renv, "snapshot_renv")
  validate_logical_scalar(run_health_check, "run_health_check")
  validate_logical_scalar(strict, "strict")
  validate_logical_scalar(overwrite, "overwrite")
  validate_logical_scalar(open, "open")

  template <- match.arg(template)
  dependency_profile <- match.arg(dependency_profile)
  code_loading <- match.arg(code_loading)
  template_defaults <- apply_template_defaults(template, use_quarto, use_targets)
  use_quarto <- template_defaults$use_quarto
  use_targets <- template_defaults$use_targets
  validate_project_path(path, overwrite = overwrite)
  project_name <- validate_project_name(project_name)
  package_name <- make_package_name(project_name)

  selected_packages <- resolve_dependency_profile(
    dependency_profile = dependency_profile,
    packages = packages,
    code_loading = code_loading,
    use_config = use_config,
    use_quarto = use_quarto,
    use_rmarkdown = use_rmarkdown,
    use_targets = use_targets,
    use_lintr = use_lintr,
    use_styler = use_styler,
    use_pkgdown = use_pkgdown
  )

  template_data <- list(
    project_name = project_name,
    package_name = package_name,
    dependency_profile = dependency_profile,
    code_loading = code_loading,
    package_vector = format_package_vector(selected_packages),
    description_imports = format_description_imports(selected_packages),
    targets_loading_block = format_targets_loading_block(code_loading),
    report_setup_block = format_report_setup_block(code_loading),
    reusable_code_path = format_reusable_code_path(code_loading),
    hidden_support_files = format_hidden_support_files(code_loading),
    makefile_extra_targets = format_makefile_extra_targets(
      use_renv = use_renv,
      use_quarto = use_quarto,
      use_rmarkdown = use_rmarkdown,
      use_targets = use_targets,
      use_lintr = use_lintr,
      use_styler = use_styler,
      use_pkgdown = use_pkgdown
    ),
    readme_restore_section = format_readme_restore_section(use_renv),
    readme_render_section = format_readme_render_section(use_quarto, use_rmarkdown),
    readme_pipeline_section = format_readme_pipeline_section(use_targets),
    project_guide_render_section = format_project_guide_render_section(
      use_quarto,
      use_rmarkdown
    ),
    project_guide_pipeline_section = format_project_guide_pipeline_section(use_targets),
    use_quarto = use_quarto,
    use_rmarkdown = use_rmarkdown,
    use_targets = use_targets,
    use_config = use_config,
    use_lintr = use_lintr,
    use_styler = use_styler,
    use_pkgdown = use_pkgdown
  )

  scaffold_options <- list(
    dependency_profile = dependency_profile,
    code_loading = code_loading,
    use_quarto = use_quarto,
    use_rmarkdown = use_rmarkdown,
    use_targets = use_targets,
    use_renv = use_renv,
    use_git = use_git,
    use_config = use_config,
    use_lintr = use_lintr,
    use_styler = use_styler,
    use_pkgdown = use_pkgdown
  )

  warnings <- template_defaults$warnings
  files_created <- character()
  files_skipped <- character()

  fs::dir_create(path, recurse = TRUE)
  path <- fs::path_abs(path)

  cli::cli_alert_info("Creating analysis project scaffold at {.path {path}}")

  directories_created <- create_project_directories(
    path,
    code_loading = code_loading,
    dependency_profile = dependency_profile,
    use_pkgdown = use_pkgdown,
    use_quarto = use_quarto,
    use_rmarkdown = use_rmarkdown
  )

  core_templates <- write_registered_templates(
    path = path,
    project_name = project_name,
    registry = template_registry("core", options = scaffold_options),
    overwrite = overwrite,
    template_data = template_data
  )
  files_created <- c(files_created, core_templates$files_created)
  files_skipped <- c(files_skipped, core_templates$files_skipped)

  rproj_result <- create_rproj_file(path, project_name, overwrite = overwrite)
  if (rproj_result$status %in% c("created", "overwritten")) {
    files_created <- c(files_created, rproj_result$path)
  } else {
    files_skipped <- c(files_skipped, rproj_result$path)
  }

  code_loading_files <- scaffold_code_loading(
    path = path,
    project_name = project_name,
    code_loading = code_loading,
    template_data = template_data,
    overwrite = overwrite
  )
  files_created <- c(files_created, code_loading_files$files_created)
  files_skipped <- c(files_skipped, code_loading_files$files_skipped)

  convenience_scripts <- write_registered_templates(
    path = path,
    project_name = project_name,
    registry = template_registry("convenience_scripts", options = scaffold_options),
    overwrite = overwrite,
    template_data = template_data
  )
  files_created <- c(files_created, convenience_scripts$files_created)
  files_skipped <- c(files_skipped, convenience_scripts$files_skipped)

  if (isTRUE(use_git)) {
    git_files <- create_git_files(path, overwrite = overwrite)
    files_created <- c(files_created, git_files$files_created)
    files_skipped <- c(files_skipped, git_files$files_skipped)
  }

  if (isTRUE(use_quarto) || isTRUE(use_rmarkdown)) {
    report_style_files <- scaffold_report_styles(
      path = path,
      overwrite = overwrite,
      use_quarto = use_quarto,
      use_rmarkdown = use_rmarkdown,
      template_data = template_data
    )
    files_created <- c(files_created, report_style_files$files_created)
    files_skipped <- c(files_skipped, report_style_files$files_skipped)
  }

  if (isTRUE(use_quarto)) {
    quarto_files <- scaffold_quarto(
      path,
      project_name,
      template_data = template_data,
      overwrite = overwrite
    )
    files_created <- c(files_created, quarto_files$files_created)
    files_skipped <- c(files_skipped, quarto_files$files_skipped)
  }

  if (isTRUE(use_rmarkdown)) {
    rmarkdown_files <- scaffold_rmarkdown(
      path,
      project_name,
      template_data = template_data,
      overwrite = overwrite
    )
    files_created <- c(files_created, rmarkdown_files$files_created)
    files_skipped <- c(files_skipped, rmarkdown_files$files_skipped)
  }

  if (isTRUE(use_targets)) {
    targets_files <- scaffold_targets(
      path,
      project_name,
      template_data = template_data,
      overwrite = overwrite
    )
    files_created <- c(files_created, targets_files$files_created)
    files_skipped <- c(files_skipped, targets_files$files_skipped)
  }

  tooling_files <- write_registered_templates(
    path = path,
    project_name = project_name,
    registry = template_registry("tooling", options = scaffold_options),
    overwrite = overwrite,
    template_data = template_data
  )
  files_created <- c(files_created, tooling_files$files_created)
  files_skipped <- c(files_skipped, tooling_files$files_skipped)

  renv_available <- requireNamespace("renv", quietly = TRUE)

  if (isTRUE(use_renv)) {
    renv_warning <- init_renv_project(
      path,
      packages = if (isTRUE(install_packages) && renv_available) {
        selected_packages
      } else {
        character()
      },
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
    install_warning <- install_packages_for_project(
      selected_packages,
      strict = strict
    )
    if (!is.null(install_warning)) {
      warnings <- c(warnings, install_warning)
      cli::cli_alert_warning(install_warning)
    }
  }

  if (isTRUE(run_health_check)) {
    health_warnings <- check_scaffold_integrity(
      path = path,
      code_loading = code_loading,
      use_quarto = use_quarto,
      use_rmarkdown = use_rmarkdown,
      use_targets = use_targets,
      use_config = use_config,
      use_git = use_git,
      use_renv = use_renv
    )

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
    template = template,
    dependency_profile = dependency_profile,
    code_loading = code_loading,
    packages = selected_packages,
    directories_created = unique(filesystem_normalize(directories_created)),
    files_created = unique(filesystem_normalize(files_created)),
    files_skipped = unique(filesystem_normalize(files_skipped)),
    warnings = warnings,
    use_quarto = use_quarto,
    use_rmarkdown = use_rmarkdown,
    use_targets = use_targets,
    use_renv = use_renv,
    use_git = use_git
  )

  cli::cli_alert_success("Project created successfully.")
  cat("\nStart here:\n")
  cat("1. Open: ", paste0(project_name, ".Rproj"), "\n", sep = "")
  cat("2. Run in R: source(\"scripts/00_start_here.R\")\n")
  cat("3. Read: PROJECT_GUIDE.md\n")
  cat("4. Add raw data to: data/raw/\n")
  cat("5. Edit: scripts/01_import_data.R\n")

  if (isTRUE(use_quarto)) {
    cat("* Render reports with: Rscript scripts/render_reports.R\n")
  }

  if (isTRUE(use_targets)) {
    cat("* Run pipeline with: Rscript scripts/run_pipeline.R\n")
  }

  if (isTRUE(use_renv)) {
    cat("* Restore packages with: Rscript scripts/restore_environment.R\n")
  }

  invisible(result)
}

filesystem_normalize <- function(paths) {
  if (length(paths) == 0L) {
    return(character())
  }

  fs::path_norm(paths)
}
