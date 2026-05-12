project_readme_template <- function(plan) {
  component_line <- if (length(plan$components) == 0L) "none" else paste(plan$components, collapse = ", ")
  deliverable_line <- if (length(plan$deliverables) == 0L) "none" else paste(plan$deliverables, collapse = ", ")
  infrastructure_line <- if (length(plan$infrastructure) == 0L) "none" else paste(plan$infrastructure, collapse = ", ")

  paste(
    paste0("# ", safe_basename(plan$path)),
    "",
    "This repository stores project code, reports, and lightweight project metadata.",
    "Data are expected to live outside the repository.",
    "",
    "## Project plan",
    "",
    paste0("- Components: ", component_line),
    paste0("- Deliverables: ", deliverable_line),
    paste0("- Infrastructure: ", infrastructure_line),
    paste0("- Scaffold level: ", plan$scaffold_level),
    "",
    "## External data",
    "",
    "Configure the external data location:",
    "",
    "```r",
    'projflow::set_project_data_root("path/to/external/data")',
    "```",
    "",
    "Access data reproducibly:",
    "",
    "```r",
    'projflow::project_data_path("file.csv")',
    "```",
    "",
    "## Workflow",
    "",
    "```r",
    "projflow::check_project()",
    "projflow::build_project()",
    "```",
    "",
    "`outputs/` is intended for small generated artefacts and is organised by output type.",
    "Use `outputs/data/` for derived data, `outputs/analysis/` for analysis objects, `outputs/tables/` for tabular exports, `outputs/figures/` for plots, and `outputs/reports/<report-name>/` for rendered reports.",
    "Raw data, cleaned data, and large intermediate files should normally stay outside the repository.",
    sep = "\n"
  )
}

run_project_template <- function() {
  paste(
    "# Run project workflow",
    paste0("# Created: ", as.character(Sys.Date())),
    "#",
    "# This entrypoint is intentionally comment-only when generated.",
    "# Uncomment and run the commands below only after reviewing the project scripts.",
    "#",
    "# projflow::check_project(deep = FALSE)",
    "# projflow::build_project()",
    sep = "\n"
  )
}


base_script_template <- function(script) {
  template <- script$script_template %||% script$template %||% "documented"
  custom_template <- read_custom_script_template(
    template_path = script$template_path %||% NULL,
    template_text = script$template_text %||% NULL
  )
  if (!is.null(custom_template)) {
    return(substitute_script_template_placeholders(
      custom_template,
      name = script$name,
      script_type = script$type,
      outputs = script$outputs %||% character(),
      title = tools::file_path_sans_ext(safe_basename(script$path))
    ))
  }

  if (identical(template, "blank")) {
    return("")
  }

  outputs <- script$outputs %||% character()
  output_lines <- if (length(outputs) == 0L) {
    "# Expected outputs: none registered."
  } else {
    c(
      "# Expected outputs registered in .projflow/project_registry.yml:",
      paste0("# - ", outputs)
    )
  }

  paste(
    c(
      paste0("# ", tools::file_path_sans_ext(safe_basename(script$path))),
      paste0("# Script type: ", script$type),
      paste0("# Created: ", as.character(Sys.Date())),
      "#",
      "# Purpose:",
      "# - Describe the objective of this workflow step before adding code.",
      "#",
      "# Inputs:",
      "# - Document required external data, registry objects, or upstream scripts.",
      "# - External data should be accessed with projflow::project_data_path().",
      "#",
      output_lines,
      "#",
      "# Notes:",
      "# - This file is intentionally comment-only when generated.",
      "# - Add executable R code only when the analysis step is ready.",
      "# - Do not commit raw data or large generated artefacts to the project repository."
    ),
    collapse = "\n"
  )
}


report_template_for_plan <- function(report) {
  title <- switch(
    report$type,
    manuscript = "Manuscript",
    dashboard = "Dashboard",
    status_report = "Status Report",
    report = "Main Report",
    tools::toTitleCase(gsub("_", " ", report$name))
  )

  body <- if (identical(report$type, "status_report")) {
    c(
      "```{r}",
      "projflow::setup_project()",
      "projflow::project_status_report(output = 'data')",
      "```"
    )
  } else {
    c(
      "```{r}",
      "projflow::setup_project()",
      "data_available <- projflow::check_project_data_access()",
      "data_available",
      "```",
      "",
      "## Overview",
      "",
      "```{r}",
      "data.frame(message = 'Report template is ready. Add project outputs deliberately as they become available.')",
      "```"
    )
  }

  paste(
    c(
      "---",
      paste0('title: "', title, '"'),
      "format: html",
      "---",
      "",
      body
    ),
    collapse = "\n"
  )
}

shiny_app_template <- function() {
  paste(
    "library(shiny)",
    "",
    "ui <- fluidPage(",
    '  titlePanel("Project dashboard"),',
    "  tableOutput('preview')",
    ")",
    "",
    "server <- function(input, output, session) {",
    "  output$preview <- renderTable({",
    '    data.frame(message = "No project outputs are available yet. Add outputs deliberately from project scripts.")',
    "  })",
    "}",
    "",
    "shinyApp(ui, server)",
    sep = "\n"
  )
}


plain_file_template <- function(path, plan) {
  governance <- governance_file_templates()
  if (path %in% names(governance)) {
    return(governance[[path]])
  }

  switch(
    normalize_relative_path(path),
    ".here" = "",
    "config.yml" = paste0("project:\n  name: ", safe_basename(plan$path), "\n"),
    "_quarto.yml" = paste(
      "project:",
      "  type: default",
      "execute:",
      "  freeze: auto",
      sep = "\n"
    ),
    "tests/testthat.R" = "library(testthat)\nlibrary(projflow)\n\ntest_check('projflow')\n",
    ".lintr" = "linters: lintr::linters_with_defaults()\n",
    "_targets.R" = paste(
      "library(targets)",
      "",
      "list(",
      "  tar_target(project_check, projflow::check_project(deep = FALSE))",
      ")",
      sep = "\n"
    ),
    "Dockerfile" = "FROM rocker/r-ver:latest\n",
    "app/app.R" = shiny_app_template(),
    ""
  )
}

create_project_dirs <- function(path, folders) {
  created <- character()
  for (directory in unique(folders)) {
    full_path <- fs::path(path, directory)
    if (!fs::dir_exists(full_path)) {
      fs::dir_create(full_path, recurse = TRUE)
      created <- c(created, full_path)
    }
  }
  created
}

project_config_from_plan <- function(plan) {
  list(
    version = 1L,
    project = list(
      name = safe_basename(plan$path),
      title = if (is.null(plan$title)) safe_basename(plan$path) else plan$title,
      type = "analysis",
      scaffold_level = plan$scaffold_level,
      created = as.character(Sys.Date())
    ),
    components = plan$components,
    deliverables = plan$deliverables,
    infrastructure = plan$infrastructure,
    packages = plan$packages,
    settings = list(
      use_git = "git" %in% plan$infrastructure,
      use_github_actions = "github_actions" %in% plan$infrastructure,
      use_quarto = any(vapply(plan$reports, function(x) grepl("\\.qmd$", x$path), logical(1))),
      use_renv = "renv" %in% plan$infrastructure,
      use_internal_data_dirs = any(plan$folders %in% c("data/raw", "data/processed"))
    ),
    paths = list(
      analysis = "analysis",
      reports = "reports",
      outputs = "outputs"
    )
  )
}

write_plan_files <- function(path, plan, overwrite = FALSE) {
  files_created <- character()
  files_skipped <- character()

  track_result <- function(result) {
    if (result$status %in% c("created", "overwritten")) {
      files_created <<- c(files_created, result$path)
    } else {
      files_skipped <<- c(files_skipped, result$path)
    }
  }

  track_result(write_template_file(fs::path(path, "README.md"), project_readme_template(plan), overwrite = overwrite))
  track_result(write_template_file(fs::path(path, "run_project.R"), run_project_template(), overwrite = overwrite))
  track_result(write_yaml_file(fs::path(path, "project.yml"), project_config_from_plan(plan), overwrite = TRUE))
  track_result(write_yaml_file(fs::path(path, default_project_metadata_dir(), "project_registry.yml"), plan$registry, overwrite = TRUE))
  track_result(write_yaml_file(fs::path(path, default_project_metadata_dir(), "local.yml"), default_local_config(), overwrite = FALSE))

  if ("project_management" %in% plan$components) {
    track_result(write_yaml_file(fs::path(path, default_project_metadata_dir(), "tasks.yml"), plan$tasks, overwrite = FALSE))
  }

  for (script in plan$scripts) {
    track_result(write_template_file(fs::path(path, script$path), base_script_template(script), overwrite = overwrite))
  }

  for (report in plan$reports) {
    track_result(write_template_file(fs::path(path, report$path), report_template_for_plan(report), overwrite = overwrite))
  }

  extra_files <- setdiff(
    plan$files,
    project_core_files(safe_basename(plan$path))
  )

  for (file in extra_files) {
    if (file %in% vapply(plan$scripts, `[[`, character(1), "path") ||
        file %in% vapply(plan$reports, `[[`, character(1), "path")) {
      next
    }

    content <- plain_file_template(file, plan)
    track_result(write_template_file(fs::path(path, file), content, overwrite = overwrite))
  }

  list(files_created = unique(files_created), files_skipped = unique(files_skipped))
}

merge_existing_registry_into_plan <- function(plan, root) {
  root <- find_project_root(root)
  existing_registry <- read_project_registry(root)
  plan$registry <- utils::modifyList(existing_registry, plan$registry)
  plan
}

finalise_project_scaffold <- function(
    path,
    project_name,
    scaffold_level,
    packages,
    directories_created,
    files_created,
    files_skipped,
    infrastructure,
    warnings = character()) {
  new_analysis_project_scaffold(
    path = path,
    project_name = project_name,
    template = "component_plan",
    dependency_profile = "analysis",
    code_loading = scaffold_level,
    packages = packages,
    directories_created = unique(filesystem_normalize(directories_created)),
    files_created = unique(filesystem_normalize(files_created)),
    files_skipped = unique(filesystem_normalize(files_skipped)),
    warnings = warnings,
    use_quarto = TRUE,
    use_rmarkdown = FALSE,
    use_targets = "targets" %in% infrastructure,
    use_renv = "renv" %in% infrastructure,
    use_git = "git" %in% infrastructure,
    preset = "basic_analysis",
    scaffold_level = scaffold_level,
    entrypoint = "run_project.R",
    guide = "README.md"
  )
}

apply_project_plan <- function(plan, open = interactive(), overwrite = FALSE, dry_run = FALSE) {
  validate_logical_scalar(open, "open")
  validate_logical_scalar(overwrite, "overwrite")
  validate_logical_scalar(dry_run, "dry_run")

  if (isTRUE(dry_run)) {
    return(plan)
  }

  path <- plan$path
  if (!fs::dir_exists(path)) {
    validate_project_path(path, overwrite = overwrite)
  }
  fs::dir_create(path, recurse = TRUE)
  directories_created <- create_project_dirs(path, plan$folders)
  file_results <- write_plan_files(path, plan, overwrite = overwrite)
  rproj_result <- create_rproj_file(path, safe_basename(path), overwrite = overwrite)

  files_created <- file_results$files_created
  files_skipped <- file_results$files_skipped

  if (rproj_result$status %in% c("created", "overwritten")) {
    files_created <- c(files_created, rproj_result$path)
  } else {
    files_skipped <- c(files_skipped, rproj_result$path)
  }

  git_files <- create_git_files(
    path = path,
    overwrite = overwrite,
    use_internal_data_dirs = "data/raw" %in% plan$folders,
    deliverables = plan$deliverables
  )
  files_created <- c(files_created, git_files$files_created)
  files_skipped <- c(files_skipped, git_files$files_skipped)

  warnings <- character()
  for (note in plan$checks[grepl("^Adding required |^Moving `", plan$checks)]) {
    message(note)
  }

  if ("git" %in% plan$infrastructure) {
    git_warning <- init_git_repo(path, strict = FALSE)
    if (!is.null(git_warning)) {
      warnings <- c(warnings, git_warning)
    }
  }

  if ("renv" %in% plan$infrastructure) {
    renv_warning <- init_renv_project(path = path, packages = plan$packages, snapshot = TRUE, strict = FALSE)
    if (!is.null(renv_warning)) {
      warnings <- c(warnings, renv_warning)
    }
  }

  if ("github_actions" %in% plan$infrastructure) {
    workflow_path <- get("use_github_actions", mode = "function")(path, workflow = "check-project")
    files_created <- c(files_created, workflow_path)
  }

  ensure_gitignore_entries(path)
  if (isTRUE(open)) {
    warnings <- c(warnings, "Project opening is not automated by this package; open the .Rproj file manually if needed.")
  }

  finalise_project_scaffold(
    path = path,
    project_name = safe_basename(path),
    scaffold_level = plan$scaffold_level,
    packages = plan$packages,
    directories_created = directories_created,
    files_created = files_created,
    files_skipped = files_skipped,
    infrastructure = plan$infrastructure,
    warnings = warnings
  )
}

rebuild_project_plan <- function(root = ".") {
  root <- find_project_root(root)
  config <- read_project_config(root)
  registry <- read_project_registry(root)
  build_project_plan(
    path = root,
    title = config$project$title %||% config$project$name,
    components = project_components(root),
    deliverables = project_deliverables(root),
    infrastructure = project_infrastructure(root),
    component_specs = registry$component_specs %||% list(),
    use_internal_data_dirs = isTRUE(config$settings$use_internal_data_dirs),
    include_example = "example_analysis" %in% names(registry$scripts %||% list())
  )
}

add_project_component <- function(component, root = ".", open = interactive(), overwrite = FALSE, dry_run = FALSE) {
  plan <- rebuild_project_plan(root)
  plan <- build_project_plan(
    path = plan$path,
    title = plan$title,
    components = unique(c(plan$components, component)),
    deliverables = plan$deliverables,
    infrastructure = plan$infrastructure,
    use_internal_data_dirs = any(plan$folders %in% c("data/raw", "data/processed")),
    include_example = any(vapply(plan$scripts, function(script) identical(script$name, "example_analysis"), logical(1)))
  )
  plan <- merge_existing_registry_into_plan(plan, root)
  apply_project_plan(plan, open = open, overwrite = overwrite, dry_run = dry_run)
}

add_project_deliverable <- function(deliverable, root = ".", open = interactive(), overwrite = FALSE, dry_run = FALSE) {
  plan <- rebuild_project_plan(root)
  plan <- build_project_plan(
    path = plan$path,
    title = plan$title,
    components = plan$components,
    deliverables = unique(c(plan$deliverables, deliverable)),
    infrastructure = plan$infrastructure,
    use_internal_data_dirs = any(plan$folders %in% c("data/raw", "data/processed")),
    include_example = any(vapply(plan$scripts, function(script) identical(script$name, "example_analysis"), logical(1)))
  )
  plan <- merge_existing_registry_into_plan(plan, root)
  apply_project_plan(plan, open = open, overwrite = overwrite, dry_run = dry_run)
}
