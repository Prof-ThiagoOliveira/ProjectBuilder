project_script_types <- function() {
  c(
    "import",
    "data_preparation",
    "data_cleaning",
    "quality_control",
    "exploratory_analysis",
    "exploration",
    "analysis",
    "statistical_analysis",
    "summary",
    "model_diagnostics",
    "model",
    "simulation",
    "forecasting",
    "optimisation",
    "causal_inference",
    "visualisation",
    "export",
    "dashboard",
    "manuscript"
  )
}

project_object_types <- function() {
  unique(c(
    project_script_types(),
    "dataset",
    "report",
    "table",
    "figure",
    "output"
  ))
}

empty_issue_table <- function() {
  data.frame(
    check = character(),
    message = character(),
    path = character(),
    fix = character(),
    stringsAsFactors = FALSE
  )
}

append_issue <- function(x, check, message, path = "", fix = "") {
  rbind(
    x,
    data.frame(
      check = check,
      message = message,
      path = path,
      fix = fix,
      stringsAsFactors = FALSE
    )
  )
}

is_absolute_path <- function(path) {
  grepl("^(?:[A-Za-z]:[\\\\/]|/|\\\\\\\\)", path)
}

safe_basename <- function(path) {
  gsub("\\\\", "/", basename(path))
}

normalize_relative_path <- function(path) {
  gsub("\\\\", "/", path)
}

normalize_absolute_path <- function(path) {
  normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
}

project_marker_path <- function(path) {
  registry <- fs::path(path, ".projectSetupR", "project_registry.yml")
  if (fs::file_exists(registry)) {
    return(registry)
  }

  for (marker in c("project.yml", "config.yml", ".here")) {
    marker_path <- fs::path(path, marker)
    if (fs::file_exists(marker_path)) {
      return(marker_path)
    }
  }

  rproj_files <- list.files(path, pattern = "\\.Rproj$", full.names = FALSE)
  if (length(rproj_files) == 1L) {
    return(fs::path(path, rproj_files[[1]]))
  }

  NULL
}

#' Find a project root directory
#'
#' @param root Starting path.
#'
#' @return Absolute project root path.
find_project_root <- function(root = ".") {
  current <- resolve_project_path(root)

  repeat {
    if (!is.null(project_marker_path(current))) {
      return(current)
    }

    parent <- fs::path_dir(current)
    if (identical(parent, current)) {
      rlang::abort(
        paste(
          "Could not find a project root.",
          "Expected one of:",
          "`.projectSetupR/project_registry.yml`, `project.yml`, `config.yml`, `.here`, or a single `.Rproj` file."
        )
      )
    }

    current <- parent
  }
}

default_project_config <- function(
    project_name = NULL,
    title = NULL,
    scaffold_level = "simple",
    packages = character(),
    use_git = TRUE,
    use_github_actions = FALSE,
    use_quarto = TRUE,
    use_renv = FALSE,
    use_internal_data_dirs = FALSE) {
  list(
    version = 1L,
    project = list(
      name = project_name,
      title = if (is.null(title)) project_name else title,
      type = "analysis",
      scaffold_level = scaffold_level,
      created = as.character(Sys.Date())
    ),
    packages = unique(packages),
    settings = list(
      use_git = isTRUE(use_git),
      use_github_actions = isTRUE(use_github_actions),
      use_quarto = isTRUE(use_quarto),
      use_renv = isTRUE(use_renv),
      use_internal_data_dirs = isTRUE(use_internal_data_dirs)
    ),
    execution = list(
      random_seed = 123
    ),
    paths = list(
      analysis = "analysis",
      reports = "reports",
      outputs = "outputs"
    )
  )
}

default_project_registry <- function(project_name = NULL) {
  list(
    version = 1L,
    project = list(
      name = project_name,
      created = as.character(Sys.Date())
    ),
    scripts = list(),
    reports = list(),
    outputs = list()
  )
}

default_local_config <- function() {
  list(
    data_sources = list()
  )
}

project_config_path <- function(root = ".") {
  fs::path(find_project_root(root), "project.yml")
}

registry_path <- function(root = ".") {
  fs::path(find_project_root(root), ".projectSetupR", "project_registry.yml")
}

local_config_path <- function(root = ".") {
  fs::path(find_project_root(root), ".projectSetupR", "local.yml")
}

write_yaml_file <- function(path, data, overwrite = FALSE) {
  content <- yaml::as.yaml(data, indent.mapping.sequence = TRUE)
  write_template_file(path, content, overwrite = overwrite)
}

normalize_registry <- function(registry, project_name = NULL) {
  normalize_keyword_field <- function(x) {
    if (is.null(x) || (is.list(x) && length(x) == 0L)) {
      return(character())
    }
    if (is.character(x)) {
      return(x)
    }
    unlist(x, use.names = FALSE)
  }

  if (is.null(registry)) {
    registry <- list()
  }

  if (is.null(registry$version)) {
    registry$version <- 1L
  }

  if (is.null(registry$project)) {
    registry$project <- list(
      name = project_name,
      created = as.character(Sys.Date())
    )
  }

  if (is.null(registry$components)) {
    registry$components <- character()
  }
  registry$components <- normalize_keyword_field(registry$components)

  if (is.null(registry$custom_components)) {
    registry$custom_components <- character()
  }
  registry$custom_components <- normalize_keyword_field(registry$custom_components)

  if (is.null(registry$component_specs)) {
    registry$component_specs <- list()
  }

  if (is.null(registry$deliverables)) {
    registry$deliverables <- character()
  }
  registry$deliverables <- normalize_keyword_field(registry$deliverables)

  if (is.null(registry$infrastructure)) {
    registry$infrastructure <- character()
  }
  registry$infrastructure <- normalize_keyword_field(registry$infrastructure)

  if (is.null(registry$scripts)) {
    registry$scripts <- list()
  }

  if (is.null(registry$reports)) {
    registry$reports <- list()
  }

  if (is.null(registry$outputs)) {
    registry$outputs <- list()
  }

  registry
}

read_project_config <- function(root = ".") {
  path <- project_config_path(root)
  if (!fs::file_exists(path)) {
    project_name <- safe_basename(find_project_root(root))
    return(default_project_config(project_name = project_name))
  }

  yaml::read_yaml(path)
}

write_project_config <- function(config, root = ".", overwrite = TRUE) {
  write_yaml_file(project_config_path(root), config, overwrite = overwrite)
}

read_project_registry <- function(root = ".") {
  path <- registry_path(root)
  project_name <- safe_basename(find_project_root(root))

  if (!fs::file_exists(path)) {
    return(default_project_registry(project_name = project_name))
  }

  normalize_registry(yaml::read_yaml(path), project_name = project_name)
}

write_project_registry <- function(registry, root = ".", overwrite = TRUE) {
  root <- find_project_root(root)
  registry <- normalize_registry(
    registry,
    project_name = safe_basename(root)
  )
  write_yaml_file(registry_path(root), registry, overwrite = overwrite)
}

read_project_local_config <- function(root = ".") {
  path <- local_config_path(root)
  if (!fs::file_exists(path)) {
    return(default_local_config())
  }

  config <- yaml::read_yaml(path)
  if (is.null(config$data_sources)) {
    config$data_sources <- list()
  }
  config
}

write_project_local_config <- function(config, root = ".", overwrite = TRUE) {
  if (is.null(config$data_sources)) {
    config$data_sources <- list()
  }
  write_yaml_file(local_config_path(root), config, overwrite = overwrite)
}

ensure_registry_file <- function(root = ".", overwrite = FALSE) {
  root <- resolve_project_path(root)
  fs::dir_create(fs::path(root, ".projectSetupR"), recurse = TRUE)

  registry_file <- fs::path(root, ".projectSetupR", "project_registry.yml")
  if (fs::file_exists(registry_file) && !isTRUE(overwrite)) {
    return(registry_file)
  }

  write_yaml_file(
    registry_file,
    default_project_registry(project_name = safe_basename(root)),
    overwrite = TRUE
  )

  registry_file
}

ensure_local_config_file <- function(root = ".", overwrite = FALSE) {
  root <- resolve_project_path(root)
  fs::dir_create(fs::path(root, ".projectSetupR"), recurse = TRUE)

  local_file <- fs::path(root, ".projectSetupR", "local.yml")
  if (fs::file_exists(local_file) && !isTRUE(overwrite)) {
    return(local_file)
  }

  write_yaml_file(
    local_file,
    default_local_config(),
    overwrite = TRUE
  )

  local_file
}

project_paths <- function(root = ".") {
  root <- find_project_root(root)
  config <- read_project_config(root)
  relative_paths <- config$paths

  defaults <- list(
    analysis = "analysis",
    reports = "reports",
    outputs = "outputs"
  )

  relative_paths <- utils::modifyList(defaults, relative_paths)
  paths <- c(
    list(root = root),
    lapply(relative_paths, function(path) fs::path(root, path)),
    list(
      project_config = project_config_path(root),
      registry = registry_path(root),
      local_config = local_config_path(root),
      registry_dir = fs::path(root, ".projectSetupR")
    )
  )

  if (isTRUE(config$settings$use_internal_data_dirs) ||
      fs::dir_exists(fs::path(root, "data"))) {
    paths$data_raw <- fs::path(root, "data", "raw")
    paths$data_processed <- fs::path(root, "data", "processed")
  }

  paths
}

project_packages <- function(root = ".") {
  packages <- read_project_config(root)$packages
  if (is.null(packages)) {
    return(character())
  }

  unique(unlist(packages, use.names = FALSE))
}

check_project_packages <- function(root = ".") {
  packages <- project_packages(root)
  installed <- packages[vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  missing <- setdiff(packages, installed)

  structure(
    list(
      ok = length(missing) == 0L,
      packages = packages,
      installed = installed,
      missing = missing
    ),
    class = "project_package_check"
  )
}

install_project_packages <- function(root = ".", confirm = interactive()) {
  validate_logical_scalar(confirm, "confirm")

  status <- check_project_packages(root)
  if (length(status$missing) == 0L) {
    return(status)
  }

  if (isTRUE(confirm) && interactive()) {
    answer <- tolower(trimws(readline(
      paste0(
        "Install missing project packages (",
        paste(status$missing, collapse = ", "),
        ")? [y/N] "
      )
    )))
    if (!answer %in% c("y", "yes")) {
      rlang::abort("Package installation cancelled.")
    }
  }

  warning_message <- install_packages_for_project(status$missing, strict = FALSE)
  status$warning <- warning_message
  status$ok <- is.null(warning_message)
  status
}

add_project_package <- function(package, root = ".") {
  validate_character_vector(package, "package")
  config <- read_project_config(root)
  config$packages <- sort(unique(c(project_packages(root), package)))
  write_project_config(config, root = root, overwrite = TRUE)
  invisible(config$packages)
}

remove_project_package <- function(package, root = ".") {
  validate_character_vector(package, "package")
  config <- read_project_config(root)
  config$packages <- setdiff(project_packages(root), package)
  write_project_config(config, root = root, overwrite = TRUE)
  invisible(config$packages)
}

#' Set up a project for use
#'
#' @param root Project root.
#' @param install_missing Should missing packages be installed?
#' @param check_paths Should key directories be created if absent?
#' @param set_seed Should the configured random seed be set?
#'
#' @return Project metadata, paths, registry, and package status.
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
    for (name in c("analysis", "reports", "outputs", "registry_dir")) {
      fs::dir_create(paths[[name]], recurse = TRUE)
    }

    if (isTRUE(config$settings$use_internal_data_dirs)) {
      fs::dir_create(paths$data_raw, recurse = TRUE)
      fs::dir_create(paths$data_processed, recurse = TRUE)
    }
  }

  package_status <- check_project_packages(root)
  if (length(package_status$missing) > 0L) {
    if (isTRUE(install_missing)) {
      package_status <- install_project_packages(root, confirm = FALSE)
    } else {
      cli::cli_alert_warning(
        paste(
          "Missing project packages:",
          paste(package_status$missing, collapse = ", ")
        )
      )
    }
  }

  if (isTRUE(set_seed) && !is.null(config$execution$random_seed)) {
    set.seed(config$execution$random_seed)
  }

  list(
    root = root,
    config = config,
    paths = paths,
    registry = read_project_registry(root),
    packages = package_status
  )
}

missing_data_root_message <- function() {
  paste(
    "No external data root has been configured for this project.",
    "",
    "Run:",
    '  projflow::set_project_data_root("path/to/external/data")',
    "",
    "Data should normally live outside the project repository.",
    sep = "\n"
  )
}

#' Configure a local external data root
#'
#' @param path External data root path.
#' @param name Data source name.
#' @param root Project root.
#'
#' @return Invisibly returns the stored absolute path.
#' @export
set_project_data_root <- function(path, name = "default", root = ".") {
  validate_character_vector(path, "path")
  name <- validate_project_object_name(name, repair = TRUE)
  root <- find_project_root(root)

  config <- read_project_local_config(root)
  config$data_sources[[name]] <- list(
    path = normalize_absolute_path(path)
  )
  write_project_local_config(config, root = root, overwrite = TRUE)

  invisible(config$data_sources[[name]]$path)
}

#' Read a configured external data root
#'
#' @param name Data source name.
#' @param root Project root.
#'
#' @return Absolute path.
#' @export
project_data_root <- function(name = "default", root = ".") {
  name <- validate_project_object_name(name, repair = TRUE)
  config <- read_project_local_config(root)
  source <- config$data_sources[[name]]

  if (is.null(source) || is.null(source$path) || !nzchar(source$path)) {
    rlang::abort(missing_data_root_message())
  }

  source$path
}

#' Build a path under a configured external data root
#'
#' @param ... Path components.
#' @param source Data source name.
#' @param root Project root.
#'
#' @return Absolute file path under the external data root.
#' @export
project_data_path <- function(..., source = "default", root = ".") {
  as.character(fs::path(project_data_root(name = source, root = root), ...))
}

#' List configured external data sources
#'
#' @param root Project root.
#'
#' @return Data frame of configured sources.
#' @export
list_project_data_sources <- function(root = ".") {
  config <- read_project_local_config(root)
  sources <- config$data_sources

  if (length(sources) == 0L) {
    return(
      data.frame(
        name = character(),
        path = character(),
        exists = logical(),
        readable = logical(),
        stringsAsFactors = FALSE
      )
    )
  }

  do.call(
    rbind,
    lapply(
      names(sources),
      function(name) {
        path <- sources[[name]]$path
        data.frame(
          name = name,
          path = path,
          exists = dir.exists(path) || file.exists(path),
          readable = file.access(path, 4L) == 0L,
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

#' Remove a configured external data source
#'
#' @param name Data source name.
#' @param root Project root.
#'
#' @return Invisibly returns remaining data sources.
#' @export
remove_project_data_source <- function(name = "default", root = ".") {
  name <- validate_project_object_name(name, repair = TRUE)
  config <- read_project_local_config(root)
  config$data_sources[[name]] <- NULL
  write_project_local_config(config, root = root, overwrite = TRUE)
  invisible(config$data_sources)
}

#' Check whether configured external data sources are accessible
#'
#' @param root Project root.
#'
#' @return Data frame describing configured data sources.
#' @export
check_project_data_access <- function(root = ".") {
  list_project_data_sources(root)
}

validate_project_object_name <- function(name, repair = FALSE) {
  validate_character_vector(name, "name")
  validate_logical_scalar(repair, "repair")

  original <- trimws(name[[1]])
  if (grepl("(/|\\\\|\\.\\.|~|:|\\*|\\?|\"|<|>|\\|)", original)) {
    rlang::abort("Project object names must not contain path separators or unsafe filesystem characters.")
  }

  if (grepl("^[a-z][a-z0-9_]*$", original)) {
    return(original)
  }

  if (!isTRUE(repair)) {
    rlang::abort("Project object names must match `^[a-z][a-z0-9_]*$`.")
  }

  repaired <- tolower(original)
  repaired <- gsub("[^a-z0-9]+", "_", repaired)
  repaired <- gsub("^_+|_+$", "", repaired)
  repaired <- gsub("_+", "_", repaired)

  if (!grepl("^[a-z][a-z0-9_]*$", repaired)) {
    rlang::abort("Could not convert `name` to a safe project object name.")
  }

  message("Converted `", original, "` to `", repaired, "`.")
  repaired
}

validate_project_object_type <- function(type) {
  validate_choice(type, project_object_types(), "type")
}

default_output_path <- function(name, type) {
  type <- validate_project_object_type(type)

  if (type %in% c("dataset", "import", "data_preparation", "data_cleaning", "quality_control", "exploration", "exploratory_analysis", "analysis", "statistical_analysis", "simulation", "forecasting", "optimisation", "causal_inference", "output")) {
    return(fs::path("outputs", paste0(name, ".rds")))
  }

  if (type %in% c("model", "model_diagnostics")) {
    return(fs::path("outputs", "models", paste0(name, ".rds")))
  }

  if (type %in% c("export", "table", "summary")) {
    return(fs::path("outputs", "tables", paste0(name, ".csv")))
  }

  if (type %in% c("figure", "visualisation", "dashboard")) {
    return(fs::path("outputs", "figures", paste0(name, ".png")))
  }

  if (type %in% c("report", "manuscript")) {
    return(fs::path("outputs", "reports", paste0(name, ".html")))
  }

  fs::path("outputs", paste0(name, ".rds"))
}

next_script_order <- function(registry) {
  orders <- vapply(
    registry$scripts,
    function(entry) {
      if (is.null(entry$order)) {
        return(NA_real_)
      }
      as.numeric(entry$order)
    },
    numeric(1)
  )
  orders <- orders[!is.na(orders)]

  if (length(orders) == 0L) {
    return(10)
  }

  max(orders) + 10
}

script_template <- function(name, type, title = NULL) {
  title <- if (is.null(title)) name else title
  paste(
    paste0("# ", title),
    paste0("# Created: ", as.character(Sys.Date())),
    "",
    "projflow::setup_project()",
    "",
    "# External data are configured outside the repository.",
    "# To configure the default data source, run once:",
    '# projflow::set_project_data_root("path/to/external/data")',
    "",
    "# Example:",
    '# input_file <- projflow::project_data_path("input_file.csv")',
    "# dat <- utils::read.csv(input_file)",
    "",
    "result <- data.frame(",
    '  message = "Replace this example with your analysis.",',
    "  created = Sys.time()",
    ")",
    "",
    paste0(
      'projflow::save_project_object(',
      "result, ",
      'name = "', name, '", ',
      'type = "', type, '"',
      ")"
    ),
    sep = "\n"
  )
}

report_template <- function(title) {
  paste(
    "---",
    paste0('title: "', title, '"'),
    "format: html",
    "---",
    "",
    "```{r}",
    "projflow::setup_project()",
    "data_available <- projflow::check_project_data_access()",
    "data_available",
    "```",
    "",
    "## Example",
    "",
    "```{r}",
    "if (nrow(data_available) == 0L) {",
    "  toy_data <- data.frame(",
    '    group = c("A", "B", "C"),',
    "    value = c(2, 5, 3)",
    "  )",
    "  toy_data",
    "} else {",
    '  data.frame(message = "External data sources are configured and readable.")',
    "}",
    "```",
    sep = "\n"
  )
}

registry_rows <- function(section, entries) {
  if (length(entries) == 0L) {
    return(
      data.frame(
        section = character(),
        name = character(),
        path = character(),
        type = character(),
        order = numeric(),
        generated_by = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  do.call(
    rbind,
    lapply(
      names(entries),
      function(name) {
        entry <- entries[[name]]
        data.frame(
          section = section,
          name = name,
          path = if (!is.null(entry$path)) entry$path else NA_character_,
          type = if (!is.null(entry$type)) entry$type else NA_character_,
          order = if (!is.null(entry$order)) as.numeric(entry$order) else NA_real_,
          generated_by = if (!is.null(entry$generated_by)) entry$generated_by else NA_character_,
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

#' List registered project entities
#'
#' @param root Project root.
#'
#' @return Data frame with scripts, reports, and outputs.
#' @export
list_project_objects <- function(root = ".") {
  registry <- read_project_registry(root)
  rbind(
    registry_rows("script", registry$scripts),
    registry_rows("report", registry$reports),
    registry_rows("output", registry$outputs)
  )
}

#' Register an output object in the project registry
#'
#' @param name Object name.
#' @param path Relative output path.
#' @param type Object type.
#' @param root Project root.
#' @param overwrite Should an existing registry entry be overwritten?
#'
#' @return Invisibly returns the registry entry.
#' @export
register_project_object <- function(name, path, type, root = ".", overwrite = FALSE) {
  name <- validate_project_object_name(name, repair = TRUE)
  validate_character_vector(path, "path")
  validate_logical_scalar(overwrite, "overwrite")
  type <- validate_project_object_type(type)

  if (is_absolute_path(path)) {
    rlang::abort("`path` must be relative to the project root.")
  }

  root <- find_project_root(root)
  registry <- read_project_registry(root)

  if (!is.null(registry$outputs[[name]]) && !isTRUE(overwrite)) {
    rlang::abort(paste0("Output `", name, "` is already registered."))
  }

  entry <- list(
    path = normalize_relative_path(path),
    type = type
  )
  registry$outputs[[name]] <- entry
  write_project_registry(registry, root = root, overwrite = TRUE)

  invisible(entry)
}

#' Remove an output object from the project registry
#'
#' @param name Object name.
#' @param root Project root.
#'
#' @return Invisibly returns the modified registry.
#' @export
unregister_project_object <- function(name, root = ".") {
  name <- validate_project_object_name(name, repair = TRUE)
  registry <- read_project_registry(root)
  registry$outputs[[name]] <- NULL
  write_project_registry(registry, root = root, overwrite = TRUE)
  invisible(registry)
}

#' Update an output object in the project registry
#'
#' @param name Object name.
#' @param ... Named fields to update.
#' @param root Project root.
#'
#' @return Invisibly returns the updated entry.
#' @export
update_project_object <- function(name, ..., root = ".") {
  name <- validate_project_object_name(name, repair = TRUE)
  updates <- list(...)
  registry <- read_project_registry(root)
  entry <- registry$outputs[[name]]

  if (is.null(entry)) {
    rlang::abort(paste0("Output `", name, "` is not registered."))
  }

  entry <- utils::modifyList(entry, updates)
  if (!is.null(entry$path) && is_absolute_path(entry$path)) {
    rlang::abort("Updated output paths must stay relative to the project root.")
  }

  registry$outputs[[name]] <- entry
  write_project_registry(registry, root = root, overwrite = TRUE)
  invisible(entry)
}

project_file_metadata <- function(path, root = ".") {
  if (is.null(path) || !nzchar(path)) {
    return(list(exists = FALSE, mtime = as.POSIXct(NA)))
  }

  full_path <- if (is_absolute_path(path)) {
    path
  } else {
    fs::path(find_project_root(root), path)
  }

  exists <- fs::file_exists(full_path) || fs::dir_exists(full_path)
  list(
    exists = exists,
    mtime = if (exists) file.info(full_path)$mtime else as.POSIXct(NA)
  )
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

  if (identical(extension, "png") && requireNamespace("ggplot2", quietly = TRUE) && inherits(object, "ggplot")) {
    ggplot2::ggsave(filename = path, plot = object)
    return(invisible(path))
  }

  rlang::abort(paste0("Unsupported output type for `", path, "`."))
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

#' Save a project object
#'
#' @param object Object to save.
#' @param name Object name.
#' @param type Object type.
#' @param root Project root.
#' @param location Save inside `outputs/` or under an external data source.
#' @param source External data source name when `location = "external"`.
#'
#' @return Invisibly returns the saved path.
#' @export
save_project_object <- function(
    object,
    name,
    type,
    root = ".",
    location = c("outputs", "external"),
    source = "default") {
  name <- validate_project_object_name(name, repair = TRUE)
  type <- validate_project_object_type(type)
  location <- match.arg(location)
  root <- find_project_root(root)

  relative_path <- default_output_path(name, type)
  output_path <- if (identical(location, "outputs")) {
    fs::path(root, relative_path)
  } else {
    fs::path(project_data_root(source, root = root), "outputs", safe_basename(relative_path))
  }

  if (identical(location, "outputs")) {
    registry <- read_project_registry(root)
    existing <- registry$outputs[[name]]
    registry$outputs[[name]] <- utils::modifyList(
      if (is.null(existing)) list() else existing,
      list(path = relative_path, type = type)
    )
    write_project_registry(registry, root = root, overwrite = TRUE)
  }

  save_object_to_path(object, output_path)
  invisible(output_path)
}

#' Save a project object to external storage
#'
#' @param object Object to save.
#' @param name Object name.
#' @param type Object type.
#' @param source External data source name.
#' @param root Project root.
#'
#' @return Invisibly returns the saved path.
#' @export
save_external_project_object <- function(object, name, type, source = "default", root = ".") {
  save_project_object(
    object = object,
    name = name,
    type = type,
    root = root,
    location = "external",
    source = source
  )
}

#' Load a saved project object
#'
#' @param name Object name.
#' @param root Project root.
#'
#' @return Loaded object.
#' @export
load_project_object <- function(name, root = ".") {
  name <- validate_project_object_name(name, repair = TRUE)
  root <- find_project_root(root)
  registry <- read_project_registry(root)
  entry <- registry$outputs[[name]]

  if (is.null(entry)) {
    rlang::abort(paste0("Output `", name, "` is not registered."))
  }

  output_path <- fs::path(root, entry$path)
  if (!fs::file_exists(output_path)) {
    rlang::abort(paste0("Registered output does not exist: ", entry$path))
  }

  load_object_from_path(output_path)
}

run_script_in_project <- function(script_path, root, name, order = NA_real_) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root)

  environment <- new.env(parent = globalenv())
  tryCatch(
    {
      sys.source(script_path, envir = environment)
      invisible(script_path)
    },
    error = function(error) {
      rlang::abort(
        paste0(
          "Failed while running project script `", name, "`",
          " (path: ", normalize_relative_path(fs::path_rel(script_path, start = root)),
          ", order: ", if (is.na(order)) "NA" else as.integer(order),
          "). Original error: ", conditionMessage(error)
        )
      )
    }
  )
}

projflow_quarto_available <- function() {
  override <- getOption("projflow.quarto_available", NULL)
  
  if (is.function(override)) {
    return(isTRUE(override()))
  }
  
  if (!requireNamespace("quarto", quietly = TRUE)) {
    return(FALSE)
  }
  
  if ("quarto_available" %in% getNamespaceExports("quarto")) {
    return(isTRUE(quarto::quarto_available()))
  }
  
  TRUE
}


projflow_quarto_render <- function(input,
                                   output_file = NULL,
                                   quiet = TRUE,
                                   execute_dir = NULL,
                                   ...) {
  override <- getOption("projflow.quarto_render", NULL)
  
  if (is.function(override)) {
    return(
      override(
        input = input,
        output_file = output_file,
        quiet = quiet,
        execute_dir = execute_dir,
        ...
      )
    )
  }
  
  if (!requireNamespace("quarto", quietly = TRUE)) {
    rlang::abort("Quarto is not installed.")
  }
  
  render_args <- list(
    input = input,
    output_file = output_file,
    quiet = quiet
  )
  
  if (!is.null(execute_dir) &&
      "execute_dir" %in% names(formals(quarto::quarto_render))) {
    render_args$execute_dir <- execute_dir
  }
  
  do.call(quarto::quarto_render, render_args)
}

render_one_report <- function(input_path, output_path) {
  extension <- tolower(fs::path_ext(input_path))
  input_path <- normalize_absolute_path(input_path)
  output_path <- normalize_absolute_path(output_path)
  
  fs::dir_create(fs::path_dir(output_path), recurse = TRUE)
  
  if (identical(extension, "qmd")) {
    if (!projflow_quarto_available()) {
      return("Quarto command-line tools are not available; skipped report rendering.")
    }
    
    render_root <- find_project_root(fs::path_dir(input_path))
    output_file <- fs::path_file(output_path)
    
    rendered <- tryCatch(
      projflow_quarto_render(
        input = input_path,
        output_file = output_file,
        quiet = TRUE,
        execute_dir = render_root
      ),
      error = function(error) {
        rlang::abort(
          paste0(
            "Failed to render Quarto report `",
            normalize_relative_path(input_path),
            "`. Check that Quarto is correctly installed and that the report runs interactively. ",
            "Original error: ",
            conditionMessage(error)
          ),
          parent = error
        )
      }
    )
    
    candidate_paths <- unique(c(
      output_path,
      if (is.character(rendered) && length(rendered) > 0L) rendered[[1]] else character(),
      fs::path(fs::path_dir(input_path), output_file),
      fs::path(render_root, output_file)
    ))
    
    candidate_paths <- candidate_paths[fs::file_exists(candidate_paths)]
    
    if (!fs::file_exists(output_path) && length(candidate_paths) > 0L) {
      fs::file_copy(candidate_paths[[1]], output_path, overwrite = TRUE)
    }
    
    if (!fs::file_exists(output_path)) {
      return(paste0("Quarto completed, but expected output was not found: ", output_path))
    }
    
    return(NULL)
  }
  
  if (identical(extension, "rmd")) {
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      return("rmarkdown is not installed; skipped report rendering.")
    }
    
    tryCatch(
      rmarkdown::render(
        input = input_path,
        output_file = fs::path_file(output_path),
        output_dir = fs::path_dir(output_path),
        quiet = TRUE
      ),
      error = function(error) {
        rlang::abort(
          paste0(
            "Failed to render R Markdown report `",
            normalize_relative_path(input_path),
            "`. Original error: ",
            conditionMessage(error)
          ),
          parent = error
        )
      }
    )
    
    return(NULL)
  }
  
  paste0("Unsupported report type: ", input_path)
}

#' Create a new project script
#'
#' @param name Script name.
#' @param type Script type.
#' @param root Project root.
#' @param order Execution order.
#' @param open Included for API compatibility. Opening is not automated.
#'
#' @return Invisibly returns the created script path.
#' @export
new_project_script <- function(name, type = "analysis", root = ".", order = NULL, open = interactive()) {
  validate_logical_scalar(open, "open")
  if (!is.null(order) && (!is.numeric(order) || length(order) != 1L || is.na(order))) {
    rlang::abort("`order` must be a single numeric value or `NULL`.")
  }

  name <- validate_project_object_name(name, repair = TRUE)
  type <- validate_choice(type, project_script_types(), "type")
  root <- find_project_root(root)
  registry <- read_project_registry(root)

  if (!is.null(registry$scripts[[name]])) {
    rlang::abort(paste0("Script `", name, "` is already registered."))
  }

  relative_path <- fs::path("analysis", paste0(name, ".R"))
  source_path <- fs::path(root, relative_path)

  write_template_file(
    source_path,
    script_template(name = name, type = type, title = name),
    overwrite = FALSE
  )

  if (is.null(order)) {
    order <- next_script_order(registry)
  }

  registry$scripts[[name]] <- list(
    path = normalize_relative_path(relative_path),
    type = type,
    order = order,
    outputs = list(name)
  )
  registry$outputs[[name]] <- list(
    path = normalize_relative_path(default_output_path(name, type)),
    type = type,
    generated_by = name
  )
  write_project_registry(registry, root = root, overwrite = TRUE)

  if (isTRUE(open)) {
    cli::cli_alert_info("Project opening is not automated; open the script manually if needed.")
  }

  invisible(source_path)
}

#' Create a new project report
#'
#' @param name Report name.
#' @param format Report source format.
#' @param root Project root.
#' @param open Included for API compatibility. Opening is not automated.
#'
#' @return Invisibly returns the created report path.
#' @export
new_project_report <- function(name, format = c("qmd", "Rmd"), root = ".", open = interactive()) {
  validate_logical_scalar(open, "open")
  name <- validate_project_object_name(name, repair = TRUE)
  format <- match.arg(format)
  root <- find_project_root(root)
  registry <- read_project_registry(root)

  relative_path <- fs::path("reports", paste0(name, ".", format))
  write_template_file(
    fs::path(root, relative_path),
    report_template(title = name),
    overwrite = FALSE
  )

  registry$reports[[name]] <- list(
    path = normalize_relative_path(relative_path),
    type = "report"
  )
  write_project_registry(registry, root = root, overwrite = TRUE)

  if (isTRUE(open)) {
    cli::cli_alert_info("Project opening is not automated; open the report manually if needed.")
  }

  invisible(fs::path(root, relative_path))
}

#' Create a new model script
#'
#' @param name Script name.
#' @param root Project root.
#' @param open Included for API compatibility.
#'
#' @return Invisibly returns the created script path.
#' @export
new_project_model <- function(name, root = ".", open = interactive()) {
  new_project_script(name = name, type = "model", root = root, open = open)
}

#' Create a new table-export script
#'
#' @param name Script name.
#' @param root Project root.
#' @param open Included for API compatibility.
#'
#' @return Invisibly returns the created script path.
#' @export
new_project_table <- function(name, root = ".", open = interactive()) {
  new_project_script(name = name, type = "export", root = root, open = open)
}

#' Create a new figure script
#'
#' @param name Script name.
#' @param root Project root.
#' @param open Included for API compatibility.
#'
#' @return Invisibly returns the created script path.
#' @export
new_project_figure <- function(name, root = ".", open = interactive()) {
  new_project_script(name = name, type = "visualisation", root = root, open = open)
}

#' Create a new project idea
#'
#' @param name Idea name.
#' @param type Script type to create when `create_script = TRUE`.
#' @param create_script Should a script be created immediately?
#' @param root Project root.
#' @param open Included for API compatibility.
#'
#' @return Invisibly returns the created script path or registry entry.
#' @export
new_project_idea <- function(name, type = "analysis", create_script = TRUE, root = ".", open = interactive()) {
  validate_logical_scalar(create_script, "create_script")
  if (isTRUE(create_script)) {
    return(new_project_script(name = name, type = type, root = root, open = open))
  }

  register_project_object(
    name = validate_project_object_name(name, repair = TRUE),
    path = default_output_path(validate_project_object_name(name, repair = TRUE), "output"),
    type = "output",
    root = root,
    overwrite = FALSE
  )
}

#' Create and register a project object
#'
#' @param name Object name.
#' @param type Object type.
#' @param root Project root.
#' @param overwrite Should an existing file be overwritten?
#'
#' @return Invisibly returns the created path.
#' @export
new_project_object <- function(name, type, root = ".", overwrite = FALSE) {
  validate_logical_scalar(overwrite, "overwrite")
  type <- validate_project_object_type(type)

  if (type %in% project_script_types()) {
    return(new_project_script(name = name, type = type, root = root, open = FALSE))
  }

  if (identical(type, "report")) {
    return(new_project_report(name = name, root = root, open = FALSE))
  }

  root <- find_project_root(root)
  name <- validate_project_object_name(name, repair = TRUE)
  relative_path <- default_output_path(name, type)

  if (fs::file_exists(fs::path(root, relative_path)) && !isTRUE(overwrite)) {
    rlang::abort(paste0("File already exists for `", name, "`: ", relative_path))
  }

  register_project_object(
    name = name,
    path = relative_path,
    type = type,
    root = root,
    overwrite = overwrite
  )
}

#' Render project reports
#'
#' @param root Project root.
#'
#' @return Invisibly returns rendered report paths.
#' @export
render_project_reports <- function(root = ".") {
  root <- find_project_root(root)
  registry <- read_project_registry(root)
  rendered <- character()
  warnings <- character()

  report_names <- names(registry$reports)
  if (length(report_names) == 0L) {
    report_files <- list.files(
      fs::path(root, "reports"),
      pattern = "\\.(qmd|Rmd)$",
      full.names = TRUE
    )
    report_names <- tools::file_path_sans_ext(basename(report_files))
    registry$reports <- stats::setNames(
      lapply(
        report_files,
        function(path) list(path = normalize_relative_path(fs::path_rel(path, start = root)), type = "report")
      ),
      report_names
    )
  }

  for (name in names(registry$reports)) {
    entry <- registry$reports[[name]]
    input_path <- fs::path(root, entry$path)
    output_path <- fs::path(root, default_output_path(name, "report"))

    if (!fs::file_exists(input_path)) {
      warnings <- c(warnings, paste0("Missing report source: ", entry$path))
      next
    }

    warning_message <- render_one_report(input_path, output_path)
    if (is.null(warning_message)) {
      rendered <- c(rendered, output_path)
    } else {
      warnings <- c(warnings, warning_message)
    }
  }

  if (length(warnings) > 0L) {
    warning(paste(unique(warnings), collapse = "\n"), call. = FALSE)
  }

  invisible(rendered)
}

run_registered_report <- function(name, root) {
  registry <- read_project_registry(root)
  entry <- registry$reports[[name]]

  if (is.null(entry)) {
    rlang::abort(paste0("Report `", name, "` is not registered."))
  }

  input_path <- fs::path(root, entry$path)
  output_path <- fs::path(root, default_output_path(name, "report"))
  warning_message <- render_one_report(input_path, output_path)

  if (!is.null(warning_message)) {
    rlang::abort(warning_message)
  }

  invisible(output_path)
}

#' Run a registered project object
#'
#' @param name Object name.
#' @param root Project root.
#'
#' @return Invisibly returns the executed path.
#' @export
run_project_object <- function(name, root = ".") {
  name <- validate_project_object_name(name, repair = TRUE)
  root <- find_project_root(root)
  registry <- read_project_registry(root)

  if (!is.null(registry$scripts[[name]])) {
    entry <- registry$scripts[[name]]
    return(run_script_in_project(
      script_path = fs::path(root, entry$path),
      root = root,
      name = name,
      order = entry$order
    ))
  }

  if (!is.null(registry$reports[[name]])) {
    return(run_registered_report(name, root))
  }

  if (!is.null(registry$outputs[[name]])) {
    return(invisible(fs::path(root, registry$outputs[[name]]$path)))
  }

  rlang::abort(paste0("Object `", name, "` is not registered."))
}

#' Run a registered project step
#'
#' @param name Step name.
#' @param root Project root.
#'
#' @return Invisibly returns the executed path.
#' @export
run_project_step <- function(name, root = ".") {
  run_project_object(name, root = root)
}

#' Run the project workflow
#'
#' @param root Project root.
#'
#' @return Invisibly returns executed script paths.
#' @export
run_project <- function(root = ".") {
  root <- find_project_root(root)
  registry <- read_project_registry(root)

  script_names <- names(registry$scripts)
  scripts_run <- character()

  if (length(script_names) == 0L) {
    analysis_files <- sort(list.files(
      fs::path(root, "analysis"),
      pattern = "\\.R$",
      full.names = TRUE
    ))

    for (script_path in analysis_files) {
      scripts_run <- c(scripts_run, script_path)
      run_script_in_project(
        script_path = script_path,
        root = root,
        name = tools::file_path_sans_ext(basename(script_path)),
        order = NA_real_
      )
    }

    return(invisible(scripts_run))
  }

  ordering <- order(vapply(registry$scripts, `[[`, numeric(1), "order"))
  for (name in script_names[ordering]) {
    entry <- registry$scripts[[name]]
    script_path <- fs::path(root, entry$path)
    scripts_run <- c(scripts_run, script_path)
    run_script_in_project(
      script_path = script_path,
      root = root,
      name = name,
      order = entry$order
    )
  }

  invisible(scripts_run)
}

#' List registered project outputs
#'
#' @param root Project root.
#'
#' @return Data frame of outputs and their existence status.
#' @export
list_project_outputs <- function(root = ".") {
  root <- find_project_root(root)
  registry <- read_project_registry(root)

  if (length(registry$outputs) == 0L) {
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

  do.call(
    rbind,
    lapply(
      names(registry$outputs),
      function(name) {
        entry <- registry$outputs[[name]]
        full_path <- fs::path(root, entry$path)
        data.frame(
          name = name,
          type = entry$type,
          output = entry$path,
          exists = fs::file_exists(full_path) || fs::dir_exists(full_path),
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

#' Report missing project outputs
#'
#' @param root Project root.
#'
#' @return Character vector of missing output paths.
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
#' @param root Project root.
#'
#' @return Character vector of stale output paths.
#' @export
stale_project_outputs <- function(root = ".") {
  root <- find_project_root(root)
  registry <- read_project_registry(root)
  stale <- character()

  for (name in names(registry$outputs)) {
    output_entry <- registry$outputs[[name]]
    generated_by <- output_entry$generated_by
    if (is.null(generated_by) || is.null(registry$scripts[[generated_by]])) {
      next
    }

    script_entry <- registry$scripts[[generated_by]]
    source_meta <- project_file_metadata(script_entry$path, root = root)
    output_meta <- project_file_metadata(output_entry$path, root = root)

    if (isTRUE(source_meta$exists) &&
        isTRUE(output_meta$exists) &&
        !is.na(source_meta$mtime) &&
        !is.na(output_meta$mtime) &&
        source_meta$mtime > output_meta$mtime) {
      stale <- c(stale, output_entry$path)
    }
  }

  stale
}

collect_scalar_strings <- function(x) {
  if (is.null(x)) {
    return(character())
  }

  if (is.list(x)) {
    return(unlist(lapply(x, collect_scalar_strings), use.names = FALSE))
  }

  if (is.character(x)) {
    return(x)
  }

  character()
}

validate_project_registry <- function(root = ".") {
  root <- find_project_root(root)
  errors <- empty_issue_table()
  warnings <- empty_issue_table()

  registry <- tryCatch(
    read_project_registry(root),
    error = function(error) error
  )

  if (inherits(registry, "error")) {
    errors <- append_issue(
      errors,
      "registry_yaml",
      conditionMessage(registry),
      ".projectSetupR/project_registry.yml",
      "Repair or recreate the registry YAML."
    )
    return(list(ok = FALSE, errors = errors, warnings = warnings))
  }

  if (!identical(registry$version, 1L)) {
    errors <- append_issue(
      errors,
      "registry_version",
      paste0("Unsupported registry version: ", registry$version),
      ".projectSetupR/project_registry.yml",
      "Use registry version 1."
    )
  }

  all_names <- c(names(registry$scripts), names(registry$reports), names(registry$outputs))
  if (anyDuplicated(all_names)) {
    errors <- append_issue(
      errors,
      "duplicate_names",
      "Duplicate names were found across scripts, reports, or outputs.",
      ".projectSetupR/project_registry.yml",
      "Use unique names across the registry."
    )
  }

  for (name in names(registry$scripts)) {
    entry <- registry$scripts[[name]]

    tryCatch(
      validate_project_object_name(name, repair = FALSE),
      error = function(error) {
        errors <<- append_issue(
          errors,
          "invalid_script_name",
          conditionMessage(error),
          entry$path,
          "Rename the script to a safe snake_case identifier."
        )
      }
    )

    if (is.null(entry$order) || !is.numeric(entry$order) || is.na(entry$order)) {
      errors <- append_issue(
        errors,
        "invalid_script_order",
        paste0("Script `", name, "` does not have a valid execution order."),
        entry$path,
        "Set a numeric `order` value in the registry."
      )
    }

    if (is_absolute_path(entry$path)) {
      errors <- append_issue(
        errors,
        "absolute_script_path",
        paste0("Script `", name, "` uses an absolute path."),
        entry$path,
        "Store script paths relative to the project root."
      )
    }
  }

  script_orders <- vapply(registry$scripts, `[[`, numeric(1), "order")
  if (length(script_orders) > 0L && anyDuplicated(script_orders)) {
    warnings <- append_issue(
      warnings,
      "duplicate_script_order",
      "Some scripts share the same execution order.",
      ".projectSetupR/project_registry.yml",
      "Use unique order values for deterministic execution."
    )
  }

  for (name in names(registry$outputs)) {
    entry <- registry$outputs[[name]]
    if (is_absolute_path(entry$path)) {
      errors <- append_issue(
        errors,
        "absolute_output_path",
        paste0("Output `", name, "` uses an absolute path."),
        entry$path,
        "Store output paths relative to the project root."
      )
    }

    if (!is.null(entry$generated_by) && is.null(registry$scripts[[entry$generated_by]])) {
      errors <- append_issue(
        errors,
        "missing_generator",
        paste0("Output `", name, "` references missing script `", entry$generated_by, "`."),
        entry$path,
        "Register the generating script or remove `generated_by`."
      )
    }
  }

  data_sources <- list_project_data_sources(root)
  if (nrow(data_sources) > 0L) {
    external_roots <- normalize_absolute_path(data_sources$path)
    for (name in names(registry$outputs)) {
      output_path <- normalize_absolute_path(fs::path(root, registry$outputs[[name]]$path))
      if (any(startsWith(output_path, external_roots))) {
        errors <- append_issue(
          errors,
          "output_in_external_root",
          paste0("Output `", name, "` points into a configured external data root."),
          registry$outputs[[name]]$path,
          "Keep project outputs in `outputs/` and raw data outside the repository."
        )
      }
    }
  }

  list(
    ok = nrow(errors) == 0L,
    errors = errors,
    warnings = warnings
  )
}

git_command <- function(root, args) {
  if (!nzchar(Sys.which("git"))) {
    return(character())
  }

  tryCatch(
    system2("git", c("-C", root, args), stdout = TRUE, stderr = FALSE),
    warning = function(warning) character(),
    error = function(error) character()
  )
}

git_exit_status <- function(root, args) {
  if (!nzchar(Sys.which("git"))) {
    return(1L)
  }

  status <- suppressWarnings(
    system2("git", c("-C", root, args), stdout = FALSE, stderr = FALSE)
  )
  as.integer(status)
}

required_gitignore_entries <- function() {
  c(
    ".projectSetupR/local.yml",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    ".Rproj.user/",
    "renv/library/",
    "*.RData",
    "*.rds",
    "*.qs",
    "*.parquet",
    "*.fst",
    "*.xlsx"
  )
}

ensure_gitignore_entries <- function(root = ".") {
  root <- find_project_root(root)
  path <- fs::path(root, ".gitignore")
  existing <- if (fs::file_exists(path)) readLines(path, warn = FALSE) else character()
  missing <- setdiff(required_gitignore_entries(), existing)

  if (length(missing) == 0L) {
    return(invisible(path))
  }

  lines <- unique(c(existing, missing))
  write_template_file(path, paste(lines, collapse = "\n"), overwrite = TRUE)
  invisible(path)
}

data_file_extensions <- c("rds", "qs", "parquet", "fst", "csv", "tsv", "xlsx")

detect_large_data_files <- function(root = ".", size_mb = 5) {
  root <- find_project_root(root)
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  if (length(files) == 0L) {
    return(character())
  }

  extensions <- tolower(fs::path_ext(files))
  keep <- extensions %in% data_file_extensions
  files <- files[keep]
  if (length(files) == 0L) {
    return(character())
  }

  info <- file.info(files)
  files[info$size > (size_mb * 1024^2)]
}

#' Check Git status for a project
#'
#' @param root Project root.
#'
#' @return Structured Git status information.
#' @export
check_git_status <- function(root = ".") {
  root <- find_project_root(root)
  git_initialized <- fs::dir_exists(fs::path(root, ".git"))
  remote <- FALSE
  local_config_ignored <- FALSE
  tracked_data_files <- character()

  if (git_initialized && nzchar(Sys.which("git"))) {
    remote <- length(git_command(root, c("remote"))) > 0L
    local_config_ignored <- git_exit_status(root, c("check-ignore", ".projectSetupR/local.yml")) == 0L
    tracked_data_files <- git_command(root, c("ls-files", "*.csv", "*.tsv", "*.xlsx", "*.rds", "*.parquet", "*.fst", "*.qs"))
  }

  list(
    git_initialized = git_initialized,
    has_remote = remote,
    local_config_ignored = local_config_ignored,
    tracked_data_files = tracked_data_files
  )
}

#' Check GitHub Actions workflow files
#'
#' @param root Project root.
#'
#' @return Data frame describing known workflows.
#' @export
check_github_actions <- function(root = ".") {
  root <- find_project_root(root)
  workflows <- c(
    "check-project" = fs::path(".github", "workflows", "check-project.yaml"),
    "render-reports" = fs::path(".github", "workflows", "render-reports.yaml")
  )

  do.call(
    rbind,
    lapply(
      names(workflows),
      function(name) {
        data.frame(
          workflow = name,
          path = workflows[[name]],
          exists = fs::file_exists(fs::path(root, workflows[[name]])),
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

github_workflow_contents <- function(workflow, use_renv = FALSE) {
  workflow <- match.arg(workflow, c("check-project", "render-reports"))

  dependency_step <- if (isTRUE(use_renv)) {
    "      - uses: r-lib/actions/setup-renv@v2"
  } else {
    "      - uses: r-lib/actions/setup-r-dependencies@v2"
  }

  run_step <- switch(
    workflow,
    "check-project" = c(
      "      - name: Check project",
      "        run: |",
      "          Rscript -e 'projflow::check_project(strict = TRUE)'"
    ),
    "render-reports" = c(
      "      - name: Render reports",
      "        run: |",
      "          Rscript -e 'projflow::build_project()'"
    )
  )

  paste(
    paste0("name: ", if (workflow == "check-project") "Check project" else "Render reports"),
    "",
    "on:",
    "  push:",
    "  pull_request:",
    "",
    "jobs:",
    paste0("  ", workflow, ":"),
    "    runs-on: ubuntu-latest",
    "",
    "    steps:",
    "      - uses: actions/checkout@v4",
    "      - uses: r-lib/actions/setup-r@v2",
    dependency_step,
    run_step,
    sep = "\n"
  )
}

#' Add a GitHub Actions workflow
#'
#' @param root Project root.
#' @param workflow Workflow name.
#'
#' @return Invisibly returns the created workflow path.
#' @export
use_github_actions <- function(root = ".", workflow = c("check-project", "render-reports")) {
  workflow <- match.arg(workflow)
  root <- find_project_root(root)
  config <- read_project_config(root)

  path <- fs::path(root, ".github", "workflows", paste0(workflow, ".yaml"))
  write_template_file(
    path,
    github_workflow_contents(
      workflow = workflow,
      use_renv = isTRUE(config$settings$use_renv) ||
        fs::file_exists(fs::path(root, "renv.lock"))
    ),
    overwrite = FALSE
  )

  config$settings$use_github_actions <- TRUE
  write_project_config(config, root = root, overwrite = TRUE)
  invisible(path)
}

check_project_impl <- function(root = ".", deep = TRUE, strict = FALSE, repair = FALSE) {
  validate_logical_scalar(deep, "deep")
  validate_logical_scalar(strict, "strict")
  validate_logical_scalar(repair, "repair")

  errors <- empty_issue_table()
  warnings <- empty_issue_table()
  suggestions <- empty_issue_table()

  root <- tryCatch(find_project_root(root), error = function(error) error)
  if (inherits(root, "error")) {
    errors <- append_issue(
      errors,
      "project_root",
      conditionMessage(root),
      "",
      "Create a project with `new_project()` or move into an existing project."
    )
    result <- structure(
      list(ok = FALSE, errors = errors, warnings = warnings, suggestions = suggestions),
      class = "project_check"
    )
    if (isTRUE(strict)) {
      rlang::abort(conditionMessage(root))
    }
    return(result)
  }

  if (isTRUE(repair)) {
    fs::dir_create(fs::path(root, ".projectSetupR"), recurse = TRUE)
    fs::dir_create(fs::path(root, "outputs"), recurse = TRUE)
    ensure_gitignore_entries(root)
    ensure_local_config_file(root)
    if (!fs::file_exists(fs::path(root, ".projectSetupR", "project_registry.yml"))) {
      ensure_registry_file(root, overwrite = TRUE)
    }
  }

  config <- tryCatch(read_project_config(root), error = function(error) error)
  config_ok <- !inherits(config, "error")
  config_settings <- if (config_ok) {
    config$settings
  } else {
    list(
      use_git = FALSE,
      use_github_actions = FALSE,
      use_internal_data_dirs = FALSE,
      use_renv = FALSE
    )
  }
  registry_check <- validate_project_registry(root)

  if (inherits(config, "error")) {
    errors <- append_issue(
      errors,
      "project_yaml",
      conditionMessage(config),
      "project.yml",
      "Fix the YAML syntax in `project.yml`."
    )
  }

  errors <- rbind(errors, registry_check$errors)
  warnings <- rbind(warnings, registry_check$warnings)

  required_files <- c("project.yml", fs::path(".projectSetupR", "project_registry.yml"))
  for (relative_path in required_files) {
    if (!fs::file_exists(fs::path(root, relative_path))) {
      errors <- append_issue(
        errors,
        "required_file",
        paste0("Missing required file: ", relative_path),
        relative_path,
        "Recreate the file or run `check_project(repair = TRUE)`."
      )
    }
  }

  required_dirs <- c("analysis", "reports", "outputs", ".projectSetupR")
  for (relative_path in required_dirs) {
    if (!fs::dir_exists(fs::path(root, relative_path))) {
      errors <- append_issue(
        errors,
        "required_directory",
        paste0("Missing required directory: ", relative_path),
        relative_path,
        "Create the directory or run `check_project(repair = TRUE)`."
      )
    }
  }

  gitignore_path <- fs::path(root, ".gitignore")
  if (!fs::file_exists(gitignore_path)) {
    warnings <- append_issue(
      warnings,
      "gitignore",
      "`.gitignore` is missing.",
      ".gitignore",
      "Create `.gitignore` or run `check_project(repair = TRUE)`."
    )
  } else {
    gitignore_lines <- readLines(gitignore_path, warn = FALSE)
    missing_entries <- setdiff(required_gitignore_entries(), gitignore_lines)
    if (length(missing_entries) > 0L) {
      warnings <- append_issue(
        warnings,
        "gitignore_entries",
        paste("Missing recommended `.gitignore` entries:", paste(missing_entries, collapse = ", ")),
        ".gitignore",
        "Add the missing entries or run `check_project(repair = TRUE)`."
      )
    }
  }

  git_status <- check_git_status(root)
  if (isTRUE(config_settings$use_git) && !git_status$git_initialized) {
    warnings <- append_issue(
      warnings,
      "git_repository",
      "Git is enabled in project settings, but the repository is not initialised.",
      ".git",
      "Run `git init` or add `git` to the project infrastructure."
    )
  }

  if (git_status$git_initialized && !git_status$local_config_ignored) {
    warnings <- append_issue(
      warnings,
      "local_config_ignored",
      "`.projectSetupR/local.yml` is not ignored by Git.",
      ".projectSetupR/local.yml",
      "Add `.projectSetupR/local.yml` to `.gitignore`."
    )
  }

  data_sources <- check_project_data_access(root)
  if (nrow(data_sources) > 0L) {
    for (index in seq_len(nrow(data_sources))) {
      if (!isTRUE(data_sources$exists[[index]])) {
        errors <- append_issue(
          errors,
          "missing_external_data_root",
          paste0("Configured external data root does not exist: ", data_sources$path[[index]]),
          ".projectSetupR/local.yml",
          "Update the local data root or create the directory."
        )
      } else if (!isTRUE(data_sources$readable[[index]])) {
        warnings <- append_issue(
          warnings,
          "unreadable_external_data_root",
          paste0("Configured external data root is not readable: ", data_sources$path[[index]]),
          ".projectSetupR/local.yml",
          "Check permissions for the configured directory."
        )
      }
    }
  }

  suspicious_dirs <- c("data/raw", "data/processed")
  for (relative_path in suspicious_dirs) {
        if (fs::dir_exists(fs::path(root, relative_path)) &&
        !isTRUE(config_settings$use_internal_data_dirs)) {
      warnings <- append_issue(
        warnings,
        "internal_data_dirs",
        "This project contains internal data folders. This is supported, but the recommended default is to keep data outside the repository.",
        relative_path,
        "Move raw and large data to an external location when practical."
      )
    }
  }

  registry <- read_project_registry(root)
  components_selected <- registry$components %||% character()
  deliverables_selected <- registry$deliverables %||% character()
  infrastructure_selected <- registry$infrastructure %||% character()
  reports_registry <- registry$reports %||% list()
  external_data_required <- any(c("data_preparation", "quality_control") %in% components_selected) ||
    "external_data_configured" %in% collect_scalar_strings(registry$checks) ||
    any(vapply(
      registry$scripts %||% list(),
      function(entry) {
        script_path <- fs::path(root, entry$path %||% "")
        if (!fs::file_exists(script_path)) {
          return(FALSE)
        }
        any(grepl("^\\s*[^#].*project_data_path\\(", readLines(script_path, warn = FALSE)))
      },
      logical(1)
    ))

  if (nrow(data_sources) == 0L && isTRUE(external_data_required)) {
    suggestions <- append_issue(
      suggestions,
      "external_data",
      "No external data root is configured for a project plan that expects external data.",
      ".projectSetupR/local.yml",
      'Run `projflow::set_project_data_root("path/to/external/data")`.'
    )
  }

  if (length(git_status$tracked_data_files) > 0L) {
    allowed_patterns <- character()
    if ("tables" %in% deliverables_selected) {
      allowed_patterns <- c(allowed_patterns, "^outputs/tables/.*\\.(csv|tsv)$")
    }
    if (any(c("html_report", "client_report", "internal_report") %in% deliverables_selected)) {
      allowed_patterns <- c(allowed_patterns, "^outputs/reports/.*\\.html$")
    }

    tracked_rel <- normalize_relative_path(git_status$tracked_data_files)
    allowed_tracked <- if (length(allowed_patterns) == 0L) {
      rep(FALSE, length(tracked_rel))
    } else {
      vapply(
        tracked_rel,
        function(path) any(vapply(allowed_patterns, grepl, logical(1), x = path)),
        logical(1)
      )
    }
    problematic_tracked <- tracked_rel[!allowed_tracked]

    if (length(problematic_tracked) > 0L) {
      warnings <- append_issue(
        warnings,
        "tracked_data_files",
        paste("Git is tracking data-like files:", paste(problematic_tracked, collapse = ", ")),
        "",
        "Keep raw and large data outside the repository."
      )
    }
  }

  component_required_files <- list(
    data_preparation = "analysis/01_prepare_inputs.R",
    quality_control = "analysis/02_quality_control.R",
    exploratory_analysis = "analysis/03_exploratory_analysis.R",
    statistical_analysis = "analysis/04_analysis.R",
    model_diagnostics = "analysis/05_model_diagnostics.R",
    report = "reports/main_report.qmd",
    manuscript = "manuscript/manuscript.qmd",
    shiny_app = "app/app.R"
  )

  for (component_name in intersect(names(component_required_files), components_selected)) {
    required_file <- component_required_files[[component_name]]
    if (!fs::file_exists(fs::path(root, required_file))) {
      errors <- append_issue(
        errors,
        paste0("component_", component_name),
        paste0("Component `", component_name, "` requires `", required_file, "`."),
        required_file,
        "Create the missing file or remove the component from the plan."
      )
    }
  }

  if ("data_preparation" %in% components_selected &&
      nrow(data_sources) == 0L &&
      !fs::file_exists(fs::path(root, "analysis", "example_analysis.R"))) {
    warnings <- append_issue(
      warnings,
      "external_data_configured",
      "The project includes `data_preparation`, but no external data root is configured and no example mode file is present.",
      ".projectSetupR/local.yml",
      'Run `projflow::set_project_data_root("path/to/external/data")`.'
    )
  }

  if ("quality_control" %in% components_selected && !fs::dir_exists(fs::path(root, "outputs", "qc"))) {
    warnings <- append_issue(
      warnings,
      "qc_outputs_registered",
      "The project includes `quality_control`, but `outputs/qc/` is missing.",
      "outputs/qc",
      "Create the QC output folder or rerun project creation."
    )
  }

  if ("model_diagnostics" %in% components_selected && !fs::dir_exists(fs::path(root, "outputs", "diagnostics"))) {
    warnings <- append_issue(
      warnings,
      "diagnostics_folder",
      "The project includes `model_diagnostics`, but `outputs/diagnostics/` is missing.",
      "outputs/diagnostics",
      "Create the diagnostics folder or rerun project creation."
    )
  }

  if ("shiny_app" %in% components_selected && fs::file_exists(fs::path(root, "app", "app.R"))) {
    app_lines <- readLines(fs::path(root, "app", "app.R"), warn = FALSE)
    app_parse <- tryCatch(parse(file = fs::path(root, "app", "app.R")), error = function(error) error)

    if (inherits(app_parse, "error")) {
      errors <- append_issue(
        errors,
        "shiny_app_parseable",
        conditionMessage(app_parse),
        "app/app.R",
        "Fix the Shiny app syntax."
      )
    }

    if (any(grepl("data/raw", app_lines, fixed = TRUE))) {
      errors <- append_issue(
        errors,
        "shiny_app_internal_data_reference",
        "The Shiny app references `data/raw`, which is not allowed by default.",
        "app/app.R",
        "Use `project_data_path()` or lightweight outputs from `outputs/`."
      )
    }
  }

  if ("tables" %in% deliverables_selected && !fs::dir_exists(fs::path(root, "outputs", "tables"))) {
    warnings <- append_issue(
      warnings,
      "tables_deliverable",
      "The project includes the `tables` deliverable, but `outputs/tables/` is missing.",
      "outputs/tables",
      "Create the folder or rerun project creation."
    )
  }

  if ("figures" %in% deliverables_selected && !fs::dir_exists(fs::path(root, "outputs", "figures"))) {
    warnings <- append_issue(
      warnings,
      "figures_deliverable",
      "The project includes the `figures` deliverable, but `outputs/figures/` is missing.",
      "outputs/figures",
      "Create the folder or rerun project creation."
    )
  }

  if ("dashboard" %in% deliverables_selected &&
      !fs::file_exists(fs::path(root, "dashboard", "dashboard.qmd")) &&
      !fs::file_exists(fs::path(root, "app", "app.R"))) {
    warnings <- append_issue(
      warnings,
      "dashboard_deliverable",
      "The project includes the `dashboard` deliverable, but no dashboard report or Shiny app was found.",
      "",
      "Create `dashboard/dashboard.qmd` or `app/app.R`."
    )
  }

  if ("project_management" %in% components_selected) {
    governance_files <- c(
      "docs/project_plan.md",
      "docs/assumptions.md",
      "docs/decisions.md",
      "docs/risks.md",
      ".projectSetupR/tasks.yml"
    )

    for (relative_path in governance_files) {
      if (!fs::file_exists(fs::path(root, relative_path))) {
        warnings <- append_issue(
          warnings,
          "project_management_file",
          paste0("Project management file is missing: ", relative_path),
          relative_path,
          "Create the missing governance file or rerun project creation."
        )
      }
    }

    tasks_data <- tryCatch(read_project_tasks_data(root), error = function(error) error)
    if (inherits(tasks_data, "error")) {
      warnings <- append_issue(
        warnings,
        "tasks_file_valid",
        conditionMessage(tasks_data),
        ".projectSetupR/tasks.yml",
        "Repair `.projectSetupR/tasks.yml`."
      )
    } else {
      valid_statuses <- c("todo", "in_progress", "blocked", "done", "cancelled")
      valid_priorities <- c("low", "medium", "high", "critical")

      for (task_name in names(tasks_data$tasks)) {
        task <- tasks_data$tasks[[task_name]]

        if (!task$status %in% valid_statuses) {
          warnings <- append_issue(
            warnings,
            "task_status",
            paste0("Task `", task_name, "` has an invalid status."),
            ".projectSetupR/tasks.yml",
            "Use one of: todo, in_progress, blocked, done, cancelled."
          )
        }

        if (!task$priority %in% valid_priorities) {
          warnings <- append_issue(
            warnings,
            "task_priority",
            paste0("Task `", task_name, "` has an invalid priority."),
            ".projectSetupR/tasks.yml",
            "Use one of: low, medium, high, critical."
          )
        }

        if (!is.null(task$due) && nzchar(as.character(task$due))) {
          due_ok <- !inherits(tryCatch(as.Date(task$due), error = function(error) error), "error")
          if (!isTRUE(due_ok)) {
            warnings <- append_issue(
              warnings,
              "task_due_date",
              paste0("Task `", task_name, "` has an invalid due date."),
              ".projectSetupR/tasks.yml",
              "Store due dates as valid ISO dates."
            )
          }
        }

        if (identical(task$status, "blocked")) {
          suggestions <- append_issue(
            suggestions,
            "blocked_task",
            paste0("Task `", task_name, "` is currently blocked."),
            ".projectSetupR/tasks.yml",
            "Update the blocker in the task notes or status report."
          )
        }

        if (!is.null(task$due) && nzchar(as.character(task$due)) &&
            !inherits(tryCatch(as.Date(task$due), error = function(error) error), "error") &&
            as.Date(task$due) < Sys.Date() &&
            !identical(task$status, "done")) {
          suggestions <- append_issue(
            suggestions,
            "overdue_task",
            paste0("Task `", task_name, "` is overdue."),
            ".projectSetupR/tasks.yml",
            "Review the due date or task status."
          )
        }
      }

      for (risk_name in names(tasks_data$risks)) {
        risk <- tasks_data$risks[[risk_name]]
        if (identical(risk$status %||% "open", "open") &&
            identical(risk$impact %||% NA_character_, "critical")) {
          suggestions <- append_issue(
            suggestions,
            "open_critical_risk",
            paste0("Risk `", risk_name, "` is open and marked critical."),
            ".projectSetupR/tasks.yml",
            "Review mitigation and ownership."
          )
        }
      }
    }
  }

  for (name in names(registry$scripts)) {
    entry <- registry$scripts[[name]]
    path <- fs::path(root, entry$path)

    if (!fs::file_exists(path)) {
      errors <- append_issue(
        errors,
        "missing_script",
        paste0("Registered script is missing: ", entry$path),
        entry$path,
        "Restore the script or remove it from the registry."
      )
      next
    }

    parse_result <- tryCatch(parse(file = path), error = function(error) error)
    if (inherits(parse_result, "error")) {
      errors <- append_issue(
        errors,
        "parseable_script",
        paste0("Script does not parse: ", conditionMessage(parse_result)),
        entry$path,
        "Fix the script syntax."
      )
    }
  }

  for (name in names(registry$reports)) {
    entry <- registry$reports[[name]]
    path <- fs::path(root, entry$path)
    if (!fs::file_exists(path)) {
      errors <- append_issue(
        errors,
        "missing_report",
        paste0("Registered report is missing: ", entry$path),
        entry$path,
        "Restore the report or remove it from the registry."
      )
    } else if (isTRUE(deep)) {
      warning_message <- tryCatch(
        render_one_report(path, fs::path(root, default_output_path(name, "report"))),
        error = function(error) conditionMessage(error)
      )
      if (!is.null(warning_message)) {
        warnings <- append_issue(
          warnings,
          "renderable_report",
          warning_message,
          entry$path,
          "Install the required reporting package or fix the report."
        )
      }
    }
  }

  missing_outputs <- missing_project_outputs(root)
  if (length(missing_outputs) > 0L) {
    warnings <- append_issue(
      warnings,
      "missing_outputs",
      paste("Registered outputs are missing:", paste(missing_outputs, collapse = ", ")),
      "outputs/",
      "Run the relevant scripts to regenerate these outputs."
    )
  }

  stale_outputs <- stale_project_outputs(root)
  if (length(stale_outputs) > 0L) {
    warnings <- append_issue(
      warnings,
      "stale_outputs",
      paste("Registered outputs are stale:", paste(stale_outputs, collapse = ", ")),
      "outputs/",
      "Re-run the scripts that generate these outputs."
    )
  }

  if (isTRUE(config_ok)) {
    package_status <- check_project_packages(root)
  } else {
    package_status <- list(missing = character())
  }
  if (length(package_status$missing) > 0L) {
    warnings <- append_issue(
      warnings,
      "missing_packages",
      paste("Missing project packages:", paste(package_status$missing, collapse = ", ")),
      "project.yml",
      "Run `projflow::install_project_packages(confirm = FALSE)`."
    )
  }

  config_strings <- if (!inherits(config, "error")) collect_scalar_strings(config) else character()
  if (any(is_absolute_path(config_strings))) {
    errors <- append_issue(
      errors,
      "absolute_paths_in_project_yml",
      "Absolute or machine-specific paths were found in `project.yml`.",
      "project.yml",
      "Keep machine-specific paths in `.projectSetupR/local.yml`."
    )
  }

  registry_strings <- collect_scalar_strings(registry)
  registry_paths <- registry_strings[grepl("(^[A-Za-z]:[\\\\/]|^/|^\\\\\\\\)", registry_strings)]
  if (length(registry_paths) > 0L) {
    errors <- append_issue(
      errors,
      "absolute_paths_in_registry",
      "Absolute or machine-specific paths were found in the project registry.",
      ".projectSetupR/project_registry.yml",
      "Store only project-relative paths in the registry."
    )
  }

  large_files <- detect_large_data_files(root)
  if (length(large_files) > 0L) {
    warnings <- append_issue(
      warnings,
      "large_data_files",
      paste("Large data-like files were found inside the repository:", paste(normalize_relative_path(fs::path_rel(large_files, start = root)), collapse = ", ")),
      "",
      "Keep large data outside the repository when possible."
    )
  }

  if (isTRUE(config_settings$use_github_actions) || "github_actions" %in% infrastructure_selected) {
    workflow_status <- check_github_actions(root)
    if (!all(workflow_status$exists)) {
      warnings <- append_issue(
        warnings,
        "github_actions",
        "GitHub Actions are enabled in project settings, but one or more workflow files are missing.",
        ".github/workflows/",
        "Run `projflow::use_github_actions()`."
      )
    }

    if (!git_status$has_remote) {
      warnings <- append_issue(
        warnings,
        "git_remote",
        "GitHub Actions are enabled, but no Git remote is configured.",
        "",
        "Add a Git remote before relying on GitHub automation."
      )
    }
  }

  if (isTRUE(config_ok) &&
      (isTRUE(config_settings$use_renv) || "renv" %in% infrastructure_selected) &&
      !fs::file_exists(fs::path(root, "renv.lock"))) {
    warnings <- append_issue(
      warnings,
      "renv_lock",
      "`renv` is enabled, but `renv.lock` is missing.",
      "renv.lock",
      "Initialise or snapshot renv."
    )
  }

  if ("targets" %in% infrastructure_selected && !fs::file_exists(fs::path(root, "_targets.R"))) {
    warnings <- append_issue(
      warnings,
      "targets_pipeline_valid",
      "The project includes `targets`, but `_targets.R` is missing.",
      "_targets.R",
      "Create `_targets.R` or remove the infrastructure entry."
    )
  }

  result <- structure(
    list(
      ok = nrow(errors) == 0L,
      errors = errors,
      warnings = warnings,
      suggestions = suggestions
    ),
    class = "project_check"
  )

  if (isTRUE(strict) && !isTRUE(result$ok)) {
    rlang::abort(
      paste(
        "Project checks failed:",
        paste(result$errors$message, collapse = " | ")
      )
    )
  }

  result
}

#' Check project health
#'
#' @param root Project root.
#' @param deep Should deeper checks such as report rendering run?
#' @param strict Should critical failures raise an error?
#' @param repair Should safe repairs be applied?
#'
#' @return Structured project-check result.
#' @export
check_project <- function(root = ".", deep = TRUE, strict = FALSE, repair = FALSE) {
  check_project_impl(root = root, deep = deep, strict = strict, repair = repair)
}

#' Project status alias
#'
#' @param root Project root.
#'
#' @return Structured project-check result.
#' @export
project_status <- function(root = ".") {
  check_project(root = root, deep = FALSE, strict = FALSE, repair = FALSE)
}

#' Print a project check summary
#'
#' @param x A project check result.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
print.project_check <- function(x, ...) {
  cat("Project check\n\n")
  cat("OK: ", if (isTRUE(x$ok)) "yes" else "no", "\n", sep = "")

  if (nrow(x$errors) > 0L) {
    cat("\nErrors:\n")
    cat(paste0("- ", x$errors$message), sep = "\n")
    cat("\n")
  }

  if (nrow(x$warnings) > 0L) {
    cat("\nWarnings:\n")
    cat(paste0("- ", x$warnings$message), sep = "\n")
    cat("\n")
  }

  if (nrow(x$suggestions) > 0L) {
    cat("\nSuggestions:\n")
    cat(paste0("- ", x$suggestions$message), sep = "\n")
    cat("\n")
  }

  invisible(x)
}

#' Print a project status summary
#'
#' @param x A project status object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
print.project_status <- function(x, ...) {
  print.project_check(x, ...)
}

migrate_internal_data_to_external <- function(root = ".", external_path) {
  validate_character_vector(external_path, "external_path")
  rlang::abort(
    paste(
      "`migrate_internal_data_to_external()` is not implemented yet.",
      "The intended behaviour is to create the external folder, confirm copy or move operations explicitly,",
      "and update `.projectSetupR/local.yml` without deleting user files automatically."
    )
  )
}
