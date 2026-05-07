project_type_specs <- function() {
  list(
    dataset = list(output_dir = "data/processed", output_ext = ".rds"),
    script = list(source_dir = "analysis", source_ext = ".R"),
    report = list(source_dir = "reports", source_ext = ".qmd", output_dir = "outputs/reports", output_ext = ".html"),
    table = list(output_dir = "outputs/tables", output_ext = ".csv"),
    figure = list(output_dir = "outputs/figures", output_ext = ".png"),
    model = list(source_dir = "analysis", source_ext = ".R", output_dir = "outputs/models", output_ext = ".rds", diagnostics_dir = "outputs/diagnostics/%s"),
    diagnostic = list(output_dir = "outputs/diagnostics/%s", output_ext = ""),
    parameter_file = list(output_dir = ".", output_ext = ".yml"),
    workflow_step = list(source_dir = "analysis", source_ext = ".R", output_dir = "outputs/logs", output_ext = ".txt"),
    data_cleaning = list(source_dir = "analysis", source_ext = ".R", output_dir = "data/processed", output_ext = ".rds"),
    analysis_step = list(source_dir = "analysis", source_ext = ".R", output_dir = "outputs", output_ext = ".rds"),
    export_step = list(source_dir = "analysis", source_ext = ".R", output_dir = "outputs/tables", output_ext = ".csv")
  )
}

validate_project_object_type <- function(type) {
  validate_choice(type, names(project_type_specs()), "type")
}

project_object_spec <- function(type, name) {
  type <- validate_project_object_type(type)
  spec <- project_type_specs()[[type]]

  if (!is.null(spec$diagnostics_dir)) {
    spec$diagnostics_dir <- sprintf(spec$diagnostics_dir, name)
  }

  if (!is.null(spec$output_dir) && grepl("%s", spec$output_dir, fixed = TRUE)) {
    spec$output_dir <- sprintf(spec$output_dir, name)
  }

  spec
}

find_project_root <- function(root = ".") {
  current <- resolve_project_path(root)

  repeat {
    if (fs::file_exists(fs::path(current, "project.yml")) ||
        fs::dir_exists(fs::path(current, ".projectSetupR"))) {
      return(current)
    }

    parent <- fs::path_dir(current)

    if (identical(parent, current)) {
      rlang::abort(
        "Could not find a project root. Run this inside a project created by `create_analysis_project()`."
      )
    }

    current <- parent
  }
}

default_project_config <- function(project_name = NULL, preset = "analysis", mode = "simple", packages = character(), use_quarto = TRUE) {
  list(
    project_name = project_name,
    preset = preset,
    mode = mode,
    report_engine = if (isTRUE(use_quarto)) "quarto" else "none",
    packages = packages,
    parameters = list(random_seed = 123),
    paths = list(
      raw_data = "data/raw",
      processed_data = "data/processed",
      analysis = "analysis",
      reports = "reports",
      outputs = "outputs"
    )
  )
}

default_project_registry <- function() {
  list(objects = list())
}

read_project_config <- function(root = ".") {
  config_path <- fs::path(find_project_root(root), "project.yml")

  if (!fs::file_exists(config_path)) {
    return(default_project_config())
  }

  yaml::read_yaml(config_path)
}

write_yaml_file <- function(path, data, overwrite = FALSE) {
  content <- yaml::as.yaml(data, indent.mapping.sequence = TRUE)
  write_template_file(path, content, overwrite = overwrite)
}

registry_path <- function(root = ".") {
  fs::path(find_project_root(root), ".projectSetupR", "project_registry.yml")
}

read_project_registry <- function(root = ".") {
  path <- registry_path(root)

  if (!fs::file_exists(path)) {
    return(default_project_registry())
  }

  registry <- yaml::read_yaml(path)

  if (is.null(registry$objects)) {
    registry$objects <- list()
  }

  registry
}

write_project_registry <- function(registry, root = ".", overwrite = TRUE) {
  write_yaml_file(registry_path(root), registry, overwrite = overwrite)
}

as_relative_project_path <- function(path, root = ".") {
  root <- find_project_root(root)
  normalized <- fs::path_norm(path)
  root_normalized <- fs::path_norm(root)

  if (startsWith(normalized, root_normalized)) {
    relative <- substring(normalized, nchar(root_normalized) + 2L)
    if (nzchar(relative)) {
      return(gsub("\\\\", "/", relative))
    }
  }

  gsub("\\\\", "/", path)
}

project_file_metadata <- function(path, root = ".") {
  if (is.null(path) || !nzchar(path)) {
    return(list(path = NA_character_, exists = FALSE, mtime = as.POSIXct(NA)))
  }

  full_path <- fs::path(find_project_root(root), path)

  list(
    path = full_path,
    exists = fs::file_exists(full_path) || fs::dir_exists(full_path),
    mtime = if (fs::file_exists(full_path) || fs::dir_exists(full_path)) file.info(full_path)$mtime else as.POSIXct(NA)
  )
}

registry_as_data_frame <- function(registry) {
  objects <- registry$objects

  if (length(objects) == 0L) {
    return(
      data.frame(
        name = character(),
        type = character(),
        source = character(),
        output = character(),
        status = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  do.call(
    rbind,
    lapply(
      names(objects),
      function(name) {
        entry <- objects[[name]]
        data.frame(
          name = name,
          type = if (!is.null(entry$type)) entry$type else NA_character_,
          source = if (!is.null(entry$source)) entry$source else NA_character_,
          output = if (!is.null(entry$output)) entry$output else NA_character_,
          status = if (!is.null(entry$status)) entry$status else "active",
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

default_source_template <- function(name, type) {
  switch(
    type,
    report = paste(
      "---",
      paste0("title: \"", name, "\""),
      "format: html",
      "---",
      "",
      "# Overview",
      "",
      "Describe the purpose of this report here.",
      "",
      "# Results",
      "",
      "Add narrative and outputs here.",
      sep = "\n"
    ),
    paste(
      "# Project step:", name,
      "",
      "# Use package helpers to save outputs in standard locations.",
      "# Example:",
      "# result <- head(mtcars)",
      paste0("# projectSetupR::save_project_object(result, name = \"", name, "\", type = \"dataset\")"),
      "",
      sep = "\n"
    )
  )
}

ensure_project_subdir <- function(root, relative_path) {
  directory <- fs::path(find_project_root(root), relative_path)
  fs::dir_create(directory, recurse = TRUE)
  directory
}

build_project_object_entry <- function(name, type, root = ".") {
  validate_character_vector(name, "name")
  spec <- project_object_spec(type, name)

  source <- NULL
  output <- NULL
  diagnostics <- NULL

  if (!is.null(spec$source_dir)) {
    source <- fs::path(spec$source_dir, paste0(name, spec$source_ext))
  }

  if (!is.null(spec$output_dir)) {
    if (nzchar(spec$output_ext)) {
      output <- fs::path(spec$output_dir, paste0(name, spec$output_ext))
    } else {
      output <- spec$output_dir
    }
  }

  if (!is.null(spec$diagnostics_dir)) {
    diagnostics <- spec$diagnostics_dir
  }

  entry <- list(
    type = type,
    source = if (!is.null(source)) gsub("\\\\", "/", source) else NULL,
    output = if (!is.null(output)) gsub("\\\\", "/", output) else NULL,
    status = "active"
  )

  if (!is.null(diagnostics)) {
    entry$diagnostics <- gsub("\\\\", "/", diagnostics)
  }

  entry
}

#' Return standard project paths
#'
#' @param root Project root. Defaults to the current project.
#'
#' @return A named list of absolute project paths.
#' @export
project_paths <- function(root = ".") {
  root <- find_project_root(root)
  config <- read_project_config(root)
  configured_paths <- config$paths

  defaults <- list(
    raw_data = "data/raw",
    processed_data = "data/processed",
    analysis = "analysis",
    reports = "reports",
    outputs = "outputs"
  )

  configured_paths <- utils::modifyList(defaults, configured_paths)

  c(
    list(root = root),
    lapply(configured_paths, function(path) fs::path(root, path))
  )
}

#' Set up a project for use
#'
#' @param root Project root. Defaults to the current project.
#' @param install_missing Logical. Included for compatibility; currently unused.
#' @param check_paths Logical. Should required directories be created if missing?
#' @param set_seed Logical. Should the configured random seed be set?
#'
#' @return A named list with project configuration, paths, and registry entries.
#' @export
setup_project <- function(
    root = ".",
    install_missing = FALSE,
    check_paths = TRUE,
    set_seed = TRUE) {
  validate_logical_scalar(install_missing, "install_missing")
  validate_logical_scalar(check_paths, "check_paths")
  validate_logical_scalar(set_seed, "set_seed")

  root <- find_project_root(root)
  config <- read_project_config(root)
  paths <- project_paths(root)

  if (isTRUE(check_paths)) {
    for (path in unname(paths[names(paths) != "root"])) {
      fs::dir_create(path, recurse = TRUE)
    }
  }

  if (isTRUE(set_seed) && !is.null(config$parameters$random_seed)) {
    set.seed(config$parameters$random_seed)
  }

  list(
    root = root,
    config = config,
    parameters = config$parameters,
    paths = paths,
    registry = list_project_objects(root)
  )
}

#' Register a project object
#'
#' @param name Object name.
#' @param type Object type.
#' @param source Optional source path relative to the project root.
#' @param output Optional output path relative to the project root.
#' @param root Project root. Defaults to the current project.
#' @param status Registry status string.
#'
#' @return Invisibly returns the registered object entry.
#' @export
register_project_object <- function(
    name,
    type,
    source = NULL,
    output = NULL,
    root = ".",
    status = "active") {
  validate_character_vector(name, "name")
  validate_character_vector(type, "type")
  validate_character_vector(status, "status")
  validate_project_object_type(type)

  root <- find_project_root(root)
  registry <- read_project_registry(root)
  entry <- build_project_object_entry(name, type, root = root)

  if (!is.null(source)) {
    entry$source <- gsub("\\\\", "/", source)
  }

  if (!is.null(output)) {
    entry$output <- gsub("\\\\", "/", output)
  }

  entry$status <- status
  registry$objects[[name]] <- entry
  write_project_registry(registry, root = root, overwrite = TRUE)

  invisible(entry)
}

#' Create and register a project object
#'
#' @param name Object name.
#' @param type Object type.
#' @param root Project root. Defaults to the current project.
#' @param overwrite Logical. Should an existing source file be overwritten?
#'
#' @return Invisibly returns the registered object entry.
#' @export
new_project_object <- function(name, type, root = ".", overwrite = FALSE) {
  validate_character_vector(name, "name")
  validate_logical_scalar(overwrite, "overwrite")
  type <- validate_project_object_type(type)
  root <- find_project_root(root)
  entry <- build_project_object_entry(name, type, root = root)

  if (!is.null(entry$source)) {
    source_path <- fs::path(root, entry$source)
    write_template_file(
      source_path,
      default_source_template(name, type),
      overwrite = overwrite
    )
  }

  if (!is.null(entry$output) && grepl("/$", entry$output)) {
    fs::dir_create(fs::path(root, entry$output), recurse = TRUE)
  }

  if (!is.null(entry$diagnostics)) {
    fs::dir_create(fs::path(root, entry$diagnostics), recurse = TRUE)
  }

  register_project_object(
    name = name,
    type = type,
    source = entry$source,
    output = entry$output,
    root = root,
    status = "active"
  )
}

#' List project objects
#'
#' @param root Project root. Defaults to the current project.
#'
#' @return A data frame describing registered project objects.
#' @export
list_project_objects <- function(root = ".") {
  registry_as_data_frame(read_project_registry(root))
}

output_path_for_object <- function(name, type, root = ".") {
  registry <- read_project_registry(root)
  entry <- registry$objects[[name]]

  if (is.null(entry)) {
    entry <- build_project_object_entry(name, type, root = root)
  }

  if (is.null(entry$output)) {
    rlang::abort(paste0("Object `", name, "` of type `", type, "` does not have a default output path."))
  }

  fs::path(find_project_root(root), entry$output)
}

save_object_to_path <- function(object, path) {
  extension <- tolower(fs::path_ext(path))
  fs::dir_create(fs::path_dir(path), recurse = TRUE)

  if (identical(extension, "rds")) {
    saveRDS(object, path)
    return(invisible(path))
  }

  if (identical(extension, "csv")) {
    if (!is.data.frame(object)) {
      rlang::abort("CSV outputs require a data frame.")
    }

    utils::write.csv(object, path, row.names = FALSE)
    return(invisible(path))
  }

  if (extension %in% c("yml", "yaml")) {
    write_yaml_file(path, object, overwrite = TRUE)
    return(invisible(path))
  }

  rlang::abort(paste0("Unsupported output type for `", path, "`."))
}

#' Save a project object using the standard project structure
#'
#' @param object Object to save.
#' @param name Object name.
#' @param type Object type.
#' @param root Project root. Defaults to the current project.
#'
#' @return Invisibly returns the saved output path.
#' @export
save_project_object <- function(object, name, type, root = ".") {
  validate_character_vector(name, "name")
  type <- validate_project_object_type(type)
  root <- find_project_root(root)
  path <- output_path_for_object(name, type, root = root)

  register_project_object(name = name, type = type, root = root)
  save_object_to_path(object, path)

  invisible(path)
}

load_object_from_path <- function(path) {
  extension <- tolower(fs::path_ext(path))

  if (identical(extension, "rds")) {
    return(readRDS(path))
  }

  if (identical(extension, "csv")) {
    return(utils::read.csv(path, stringsAsFactors = FALSE))
  }

  if (extension %in% c("yml", "yaml")) {
    return(yaml::read_yaml(path))
  }

  path
}

#' Load a saved project object
#'
#' @param name Object name.
#' @param root Project root. Defaults to the current project.
#'
#' @return The loaded object, or the source path when no saved output exists.
#' @export
load_project_object <- function(name, root = ".") {
  validate_character_vector(name, "name")
  registry <- read_project_registry(root)
  entry <- registry$objects[[name]]

  if (is.null(entry)) {
    rlang::abort(paste0("Object `", name, "` is not registered."))
  }

  if (!is.null(entry$output)) {
    output_path <- fs::path(find_project_root(root), entry$output)

    if (fs::file_exists(output_path)) {
      return(load_object_from_path(output_path))
    }
  }

  if (!is.null(entry$source)) {
    return(fs::path(find_project_root(root), entry$source))
  }

  rlang::abort(paste0("Object `", name, "` has no saved output or source file."))
}

run_script_path <- function(path, root = ".") {
  root <- find_project_root(root)
  environment <- new.env(parent = globalenv())
  sys.source(path, envir = environment)
  invisible(path)
}

render_one_report <- function(input_path, output_path) {
  extension <- tolower(fs::path_ext(input_path))
  fs::dir_create(fs::path_dir(output_path), recurse = TRUE)

  if (identical(extension, "qmd")) {
    if (!requireNamespace("quarto", quietly = TRUE)) {
      return("Quarto is not installed; skipped report rendering.")
    }

    quarto::quarto_render(
      input = input_path,
      output_file = fs::path_file(output_path),
      output_dir = fs::path_dir(output_path),
      quiet = TRUE
    )

    return(NULL)
  }

  if (identical(extension, "rmd")) {
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      return("rmarkdown is not installed; skipped report rendering.")
    }

    rmarkdown::render(
      input = input_path,
      output_file = fs::path_file(output_path),
      output_dir = fs::path_dir(output_path),
      quiet = TRUE
    )

    return(NULL)
  }

  paste0("Unsupported report type: ", input_path)
}

#' Render project reports
#'
#' @param root Project root. Defaults to the current project.
#'
#' @return Invisibly returns the rendered report paths.
#' @export
render_project_reports <- function(root = ".") {
  root <- find_project_root(root)
  objects <- list_project_objects(root)
  report_objects <- objects[objects$type == "report", , drop = FALSE]

  if (nrow(report_objects) == 0L) {
    report_files <- list.files(
      file.path(root, "reports"),
      pattern = "\\.(qmd|Rmd)$",
      full.names = TRUE
    )

    report_objects <- data.frame(
      name = tools::file_path_sans_ext(basename(report_files)),
      source = gsub("\\\\", "/", fs::path_rel(report_files, start = root)),
      output = file.path("outputs", "reports", paste0(tools::file_path_sans_ext(basename(report_files)), ".html")),
      stringsAsFactors = FALSE
    )
  }

  warnings <- character()
  rendered <- character()

  for (index in seq_len(nrow(report_objects))) {
    input_path <- fs::path(root, report_objects$source[[index]])
    output_path <- fs::path(root, report_objects$output[[index]])

    if (!fs::file_exists(input_path)) {
      warnings <- c(warnings, paste0("Missing report source: ", report_objects$source[[index]]))
      next
    }

    warning_message <- render_one_report(input_path, output_path)

    if (!is.null(warning_message)) {
      warnings <- c(warnings, warning_message)
    } else {
      rendered <- c(rendered, output_path)
    }
  }

  if (length(warnings) > 0L) {
    warning(paste(unique(warnings), collapse = "\n"), call. = FALSE)
  }

  invisible(rendered)
}

#' Run a registered project object
#'
#' @param name Object name.
#' @param root Project root. Defaults to the current project.
#'
#' @return Invisibly returns the source or output path used.
#' @export
run_project_object <- function(name, root = ".") {
  validate_character_vector(name, "name")
  root <- find_project_root(root)
  registry <- read_project_registry(root)
  entry <- registry$objects[[name]]

  if (is.null(entry)) {
    rlang::abort(paste0("Object `", name, "` is not registered."))
  }

  if (!is.null(entry$source)) {
    source_path <- fs::path(root, entry$source)

    if (!fs::file_exists(source_path)) {
      rlang::abort(paste0("Source file does not exist: ", entry$source))
    }

    if (tolower(fs::path_ext(source_path)) %in% c("qmd", "rmd")) {
      render_project_reports(root)
      return(invisible(source_path))
    }

    run_script_path(source_path, root = root)
    return(invisible(source_path))
  }

  if (!is.null(entry$output)) {
    return(invisible(fs::path(root, entry$output)))
  }

  invisible(NULL)
}

#' Run a project step
#'
#' @param name Step name.
#' @param root Project root. Defaults to the current project.
#'
#' @return Invisibly returns the source path used.
#' @export
run_project_step <- function(name, root = ".") {
  run_project_object(name, root = root)
}

#' Run the project workflow
#'
#' @param root Project root. Defaults to the current project.
#'
#' @return Invisibly returns the script paths that were run.
#' @export
run_project <- function(root = ".") {
  root <- find_project_root(root)
  objects <- list_project_objects(root)
  runnable_types <- c("script", "workflow_step", "data_cleaning", "analysis_step", "export_step", "model")
  runnable <- objects[objects$type %in% runnable_types & objects$status == "active", , drop = FALSE]

  scripts_run <- character()

  if (nrow(runnable) == 0L) {
    analysis_files <- list.files(
      file.path(root, "analysis"),
      pattern = "\\.R$",
      full.names = TRUE
    )

    for (script in analysis_files) {
      run_script_path(script, root = root)
      scripts_run <- c(scripts_run, script)
    }

    return(invisible(scripts_run))
  }

  for (index in seq_len(nrow(runnable))) {
    scripts_run <- c(
      scripts_run,
      fs::path(root, runnable$source[[index]])
    )
    run_project_object(runnable$name[[index]], root = root)
  }

  invisible(scripts_run)
}

#' List saved project outputs
#'
#' @param root Project root. Defaults to the current project.
#'
#' @return A data frame of registered outputs and whether they exist.
#' @export
list_project_outputs <- function(root = ".") {
  objects <- list_project_objects(root)

  if (nrow(objects) == 0L) {
    return(
      data.frame(
        name = character(),
        type = character(),
        output = character(),
        exists = logical(),
        stringsAsFactors = FALSE
      )
    )
  }

  outputs <- objects[!is.na(objects$output) & nzchar(objects$output), , drop = FALSE]

  if (nrow(outputs) == 0L) {
    return(
      data.frame(
        name = character(),
        type = character(),
        output = character(),
        exists = logical(),
        stringsAsFactors = FALSE
      )
    )
  }

  outputs$exists <- vapply(
    outputs$output,
    function(path) {
      full_path <- fs::path(find_project_root(root), path)
      fs::file_exists(full_path) || fs::dir_exists(full_path)
    },
    logical(1)
  )

  outputs[, c("name", "type", "output", "exists"), drop = FALSE]
}

#' Report missing project outputs
#'
#' @param root Project root. Defaults to the current project.
#'
#' @return A character vector of missing output paths.
#' @export
missing_project_outputs <- function(root = ".") {
  outputs <- list_project_outputs(root)

  if (nrow(outputs) == 0L) {
    return(character())
  }

  outputs$output[!outputs$exists]
}

#' Report stale project outputs
#'
#' @param root Project root. Defaults to the current project.
#'
#' @return A character vector of output paths older than their source files.
#' @export
stale_project_outputs <- function(root = ".") {
  objects <- list_project_objects(root)

  if (nrow(objects) == 0L) {
    return(character())
  }

  stale <- character()
  root <- find_project_root(root)

  for (index in seq_len(nrow(objects))) {
    source <- objects$source[[index]]
    output <- objects$output[[index]]

    if (is.na(source) || is.na(output) || !nzchar(source) || !nzchar(output)) {
      next
    }

    source_meta <- project_file_metadata(source, root = root)
    output_meta <- project_file_metadata(output, root = root)

    if (isTRUE(source_meta$exists) &&
        isTRUE(output_meta$exists) &&
        !is.na(source_meta$mtime) &&
        !is.na(output_meta$mtime) &&
        source_meta$mtime > output_meta$mtime) {
      stale <- c(stale, output)
    }
  }

  stale
}

build_project_status <- function(root = ".") {
  root <- find_project_root(root)
  ready <- character()
  needs_attention <- character()

  if (fs::file_exists(fs::path(root, "project.yml"))) {
    ready <- c(ready, "project.yml found")
  } else {
    needs_attention <- c(needs_attention, "project.yml is missing")
  }

  raw_files <- list.files(file.path(root, "data", "raw"), all.files = FALSE, full.names = TRUE)
  raw_files <- raw_files[basename(raw_files) != ".gitkeep"]

  if (length(raw_files) > 0L) {
    ready <- c(ready, paste0("data/raw/", basename(raw_files[[1]]), " found"))
  } else {
    needs_attention <- c(needs_attention, "No raw data files found in data/raw/")
  }

  report_files <- list.files(file.path(root, "reports"), pattern = "\\.(qmd|Rmd)$", full.names = FALSE)

  if (length(report_files) > 0L) {
    ready <- c(ready, paste0("reports/", report_files[[1]], " found"))
  } else {
    needs_attention <- c(needs_attention, "No report source files found in reports/")
  }

  objects <- list_project_objects(root)

  if (nrow(objects) == 0L) {
    needs_attention <- c(
      needs_attention,
      "No project objects are registered yet. Create one with `projectSetupR::new_project_object()`."
    )
  }

  missing <- missing_project_outputs(root)
  stale <- stale_project_outputs(root)

  if (length(missing) > 0L) {
    needs_attention <- c(
      needs_attention,
      paste0(missing, " is missing")
    )
  }

  if (length(stale) > 0L) {
    needs_attention <- c(
      needs_attention,
      paste0(stale, " is out of date")
    )
  }

  structure(
    list(
      root = root,
      ready = unique(ready),
      needs_attention = unique(needs_attention),
      objects = objects,
      missing_outputs = missing,
      stale_outputs = stale
    ),
    class = "project_status"
  )
}

#' Check project status
#'
#' @param root Project root. Defaults to the current project.
#'
#' @return An object of class `"project_status"`.
#' @export
project_status <- function(root = ".") {
  build_project_status(root)
}

#' Check whether a project is ready to run
#'
#' @param root Project root. Defaults to the current project.
#'
#' @return An object of class `"project_status"`.
#' @export
check_project <- function(root = ".") {
  project_status(root)
}

#' Print a project status summary
#'
#' @param x A project status object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
print.project_status <- function(x, ...) {
  cat("Project status\n\n")

  if (length(x$ready) > 0L) {
    cat("Ready:\n")
    cat(paste0("- ", x$ready, collapse = "\n"), "\n\n", sep = "")
  }

  if (length(x$needs_attention) > 0L) {
    cat("Needs attention:\n")
    cat(paste0("- ", x$needs_attention, collapse = "\n"), "\n", sep = "")
  }

  invisible(x)
}
