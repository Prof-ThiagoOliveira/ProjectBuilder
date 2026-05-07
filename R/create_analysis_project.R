#' Create a reproducible R analysis project
#'
#' Creates a standard folder structure, helper R files, report templates,
#' Git-safe defaults, and optional reproducibility tooling for an R analysis
#' project. By default it creates a small analyst-facing scaffold with one
#' obvious entry point and keeps most setup machinery inside `projectSetupR`.
#'
#' @param path Target project path. Relative paths are resolved against the
#'   current working directory before scaffolding starts.
#' @param preset High-level project preset. Use `"analysis"` for a general
#'   analyst project, `"modelling"` for model-heavy work, `"geospatial"` for
#'   spatial analysis, `"pipeline"` for a targets-style workflow, and
#'   `"package"` for package-development scaffolding. When omitted, the preset
#'   is inferred from the detailed options.
#' @param mode Scaffold mode. Use `"simple"` for the minimal analyst-facing
#'   scaffold and `"advanced"` for the full package-style scaffold. When
#'   omitted, the mode is inferred from the requested options.
#' @param project_name Project name. Defaults to the basename of the resolved
#'   `path`.
#' @param template Project template. Use `"standard"` for a general
#'   script-and-report workflow, `"targets"` when the project should be driven
#'   by a [`targets`](https://docs.ropensci.org/targets/) pipeline, and
#'   `"quarto"` when Quarto reporting is a core part of the scaffold.
#' @param dependency_profile Dependency preset used to choose a default package
#'   set. Use `"minimal"` for a light scaffold, `"analysis"` for general data
#'   wrangling and reporting, `"modelling"` for statistical or predictive work,
#'   `"geospatial"` for spatial analysis, `"package-development"` when project
#'   code should behave like an internal package, and `"custom"` when you want
#'   to supply the package set yourself through `packages`.
#' @param packages Optional character vector of package names. Used when
#'   `dependency_profile = "custom"` and otherwise treated as additional
#'   packages to install and record on top of the selected profile.
#' @param code_loading Strategy for loading reusable project code. One of
#'   `"package"`, `"box"`, or `"source"`. Use `"package"` for the most
#'   structured workflow with package-style `R/`, tests, and `pkgload`;
#'   `"box"` for modular code without package conventions; and `"source"` for
#'   direct file sourcing. This mainly matters in advanced mode.
#' @param use_quarto Logical. Should Quarto project files and `.qmd` report
#'   templates be created? In simple mode this creates `reports/main_report.qmd`
#'   without exposing Quarto project infrastructure.
#' @param use_rmarkdown Logical. Should `.Rmd` report templates be created?
#'   Enable this when your team still relies on R Markdown or needs both report
#'   formats. This currently routes to the advanced scaffold.
#' @param use_renv Logical. Should `renv` be initialised if available? Enable
#'   this for reproducible environments, especially for team projects or work
#'   that will be rerun later.
#' @param use_targets Logical. Should a `targets` pipeline file and helper
#'   script be created? This is most useful for multi-step, expensive, or
#'   repeatedly rerun analyses and routes to the advanced scaffold.
#' @param use_git Logical. Should Git files be created and Git initialised if
#'   available? Leave this on for most projects unless version control is being
#'   managed elsewhere.
#' @param use_config Logical. Should an editable project configuration file be
#'   created? In simple mode this creates `project.yml`; in advanced mode it
#'   creates `config.yml`.
#' @param use_lintr Logical. Should linting support files be created? Enable
#'   this when code quality checks should be part of the workflow or CI.
#' @param use_styler Logical. Should code formatting support files be created?
#'   Enable this when you want a standardised code style from the start.
#' @param use_pkgdown Logical. Should pkgdown support files be created? This is
#'   mainly useful for package-like projects that need a browsable documentation
#'   site.
#' @param install_packages Logical. Should selected packages be installed during
#'   project creation when possible? Turn this on for a ready-to-run scaffold;
#'   leave it off if package installation is handled separately.
#' @param snapshot_renv Logical. Should `renv::snapshot()` be run after renv
#'   initialisation? Only relevant when `use_renv = TRUE`; this records the
#'   initial dependency state in `renv.lock`.
#' @param run_health_check Logical. Should the scaffold integrity checks run at
#'   the end of project creation? Keeping this enabled is recommended because it
#'   surfaces missing or skipped scaffold components immediately.
#' @param strict Logical. Should recoverable setup failures error instead of
#'   being returned as warnings? This is useful for automation, CI, or any
#'   workflow where partial scaffolds should be treated as failures.
#' @param overwrite Logical. Should existing files be overwritten? Leave this
#'   off unless you intentionally want to replace an existing scaffold.
#' @param open Logical. Should the project be opened after creation when
#'   possible? This flag is currently informational only; the package reports
#'   the `.Rproj` file to open but does not launch it automatically.
#'
#' @return An object of class `"analysis_project_scaffold"`.
#'
#' @details Raw data, processed data, generated outputs, logs, and common secret
#'   files are ignored by Git by default when `use_git = TRUE`.
#'
#'   The default call creates a small scaffold centred on:
#'
#'   - `README.md`
#'   - `run_project.R`
#'   - `project.yml`
#'   - `analysis/`
#'   - `reports/`
#'   - `data/raw/`
#'   - `data/processed/`
#'   - `outputs/`
#'
#'   Important argument interactions:
#'
#'   - `preset = "pipeline"` or `template = "targets"` enables advanced mode.
#'   - `preset = "package"` enables advanced mode.
#'   - `use_rmarkdown`, `use_targets`, `use_lintr`, `use_styler`, and
#'     `use_pkgdown` enable advanced mode when needed.
#'   - `template = "quarto"` implies `use_quarto = TRUE`.
#'   - `snapshot_renv` matters only when `use_renv = TRUE`.
#'   - `packages` is required when `dependency_profile = "custom"`.
#'
#' @section Recommended Starting Points:
#'   For a lightweight analysis:
#'   `create_analysis_project("my_project", preset = "analysis")`
#'
#'   For a modelling project:
#'   `create_analysis_project("my_project", preset = "modelling")`
#'
#'   For a package-style scaffold:
#'   `create_analysis_project("my_project", preset = "package", mode = "advanced")`
#'
#' @export
#'
#' @examples
#' tmp <- tempfile(pattern = "analysis-project-", tmpdir = tempdir())
#' create_analysis_project(tmp, use_renv = FALSE, use_git = FALSE, open = FALSE)
create_analysis_project <- function(
    path,
    preset = NULL,
    mode = NULL,
    project_name = NULL,
    template = c("standard", "targets", "quarto"),
    dependency_profile = c(
      "analysis",
      "minimal",
      "modelling",
      "geospatial",
      "package-development",
      "custom"
    ),
    packages = NULL,
    code_loading = c("source", "package", "box"),
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
  preset_supplied <- !missing(preset) && !is.null(preset)
  mode_supplied <- !missing(mode) && !is.null(mode)
  dependency_profile_supplied <- !missing(dependency_profile)
  code_loading_supplied <- !missing(code_loading)

  validate_character_vector(path, "path")
  if (preset_supplied) {
    validate_character_vector(preset, "preset")
  }
  if (mode_supplied) {
    validate_character_vector(mode, "mode")
  }
  validate_character_vector(project_name, "project_name", allow_null = TRUE)
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
  raw_path <- trimws(path)
  path <- resolve_project_path(path)
  template_defaults <- apply_template_defaults(template, use_quarto, use_targets)
  use_quarto <- template_defaults$use_quarto
  use_targets <- template_defaults$use_targets
  validate_project_path(path, overwrite = overwrite)
  if (is.null(project_name)) {
    project_name <- basename(path)
  }
  project_name <- validate_project_name(project_name)

  request <- resolve_scaffold_request(
    preset = if (preset_supplied) preset else NULL,
    mode = if (mode_supplied) mode else NULL,
    dependency_profile = dependency_profile,
    dependency_profile_supplied = dependency_profile_supplied,
    template = template,
    code_loading = code_loading,
    code_loading_supplied = code_loading_supplied,
    use_quarto = use_quarto,
    use_rmarkdown = use_rmarkdown,
    use_targets = use_targets,
    use_lintr = use_lintr,
    use_styler = use_styler,
    use_pkgdown = use_pkgdown
  )

  selected_packages <- resolve_dependency_profile(
    dependency_profile = request$dependency_profile,
    packages = packages,
    code_loading = request$code_loading,
    use_config = if (identical(request$mode, "advanced")) use_config else FALSE,
    use_quarto = request$use_quarto,
    use_rmarkdown = request$use_rmarkdown,
    use_targets = request$use_targets,
    use_lintr = request$use_lintr,
    use_styler = request$use_styler,
    use_pkgdown = request$use_pkgdown
  )

  path_warning <- detect_path_construction_warning(raw_path, path)
  warnings <- request$warnings
  if (!is.null(path_warning)) {
    warnings <- c(warnings, path_warning)
  }

  if (identical(request$mode, "simple")) {
    return(
      create_simple_analysis_project(
        path = path,
        project_name = project_name,
        preset = request$preset,
        dependency_profile = request$dependency_profile,
        selected_packages = selected_packages,
        use_quarto = request$use_quarto,
        use_renv = use_renv,
        use_git = use_git,
        use_config = use_config,
        install_packages = install_packages,
        snapshot_renv = snapshot_renv,
        run_health_check = run_health_check,
        strict = strict,
        overwrite = overwrite,
        open = open,
        inherited_warnings = warnings
      )
    )
  }

  create_advanced_analysis_project(
    path = path,
    project_name = project_name,
    preset = request$preset,
    template = request$template,
    dependency_profile = request$dependency_profile,
    packages = selected_packages,
    code_loading = request$code_loading,
    use_quarto = request$use_quarto,
    use_rmarkdown = request$use_rmarkdown,
    use_renv = use_renv,
    use_targets = request$use_targets,
    use_git = use_git,
    use_config = use_config,
    use_lintr = request$use_lintr,
    use_styler = request$use_styler,
    use_pkgdown = request$use_pkgdown,
    install_packages = install_packages,
    snapshot_renv = snapshot_renv,
    run_health_check = run_health_check,
    strict = strict,
    overwrite = overwrite,
    open = open,
    inherited_warnings = warnings
  )
}

create_advanced_analysis_project <- function(
    path,
    project_name,
    preset,
    template,
    dependency_profile,
    packages,
    code_loading,
    use_quarto,
    use_rmarkdown,
    use_renv,
    use_targets,
    use_git,
    use_config,
    use_lintr,
    use_styler,
    use_pkgdown,
    install_packages,
    snapshot_renv,
    run_health_check,
    strict,
    overwrite,
    open,
    inherited_warnings = character()) {
  package_name <- make_package_name(project_name)

  template_data <- list(
    project_name = project_name,
    package_name = package_name,
    dependency_profile = dependency_profile,
    code_loading = code_loading,
    package_vector = format_package_vector(packages),
    description_imports = format_description_imports(packages),
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

  warnings <- inherited_warnings
  files_created <- character()
  files_skipped <- character()

  fs::dir_create(path, recurse = TRUE)

  cli::cli_alert_info("Creating advanced analysis project scaffold at {.path {path}}")

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
    git_files <- create_git_files(path, overwrite = overwrite, mode = "advanced")
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
      packages = if (isTRUE(install_packages) && renv_available) packages else character(),
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
    install_warning <- install_packages_for_project(packages, strict = strict)
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
    packages = packages,
    directories_created = unique(filesystem_normalize(directories_created)),
    files_created = unique(filesystem_normalize(files_created)),
    files_skipped = unique(filesystem_normalize(files_skipped)),
    warnings = warnings,
    use_quarto = use_quarto,
    use_rmarkdown = use_rmarkdown,
    use_targets = use_targets,
    use_renv = use_renv,
    use_git = use_git,
    preset = preset,
    mode = "advanced",
    entrypoint = "scripts/00_start_here.R",
    guide = "PROJECT_GUIDE.md"
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
