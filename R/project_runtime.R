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

default_project_metadata_dir <- function() {
  ".projflow"
}

project_metadata_dir_names <- function() {
  default_project_metadata_dir()
}

project_metadata_dir_candidates <- function(root = ".") {
  root <- resolve_project_path(root)
  fs::path(root, project_metadata_dir_names())
}

existing_project_metadata_dir <- function(root = ".") {
  candidates <- project_metadata_dir_candidates(root)
  existing <- candidates[fs::dir_exists(candidates)]
  if (length(existing) == 0L) {
    return(NULL)
  }

  existing[[1]]
}

project_metadata_dir <- function(root = ".", create = FALSE, prefer_existing = TRUE) {
  root <- resolve_project_path(root)
  existing <- if (isTRUE(prefer_existing)) existing_project_metadata_dir(root) else NULL

  if (!is.null(existing)) {
    return(existing)
  }

  path <- fs::path(root, default_project_metadata_dir())
  if (isTRUE(create)) {
    fs::dir_create(path, recurse = TRUE)
  }

  path
}

project_metadata_relative_dir <- function(root = ".", prefer_existing = TRUE) {
  normalize_relative_path(fs::path_file(project_metadata_dir(root, prefer_existing = prefer_existing)))
}

project_metadata_path <- function(root = ".", ..., create_dir = FALSE, prefer_existing = TRUE) {
  fs::path(project_metadata_dir(root, create = create_dir, prefer_existing = prefer_existing), ...)
}

project_metadata_relative_path <- function(..., root = ".", prefer_existing = TRUE) {
  normalize_relative_path(fs::path(project_metadata_relative_dir(root, prefer_existing = prefer_existing), ...))
}

project_registry_relative_path <- function(root = ".", prefer_existing = TRUE) {
  project_metadata_relative_path("project_registry.yml", root = root, prefer_existing = prefer_existing)
}

project_local_config_relative_path <- function(root = ".", prefer_existing = TRUE) {
  project_metadata_relative_path("local.yml", root = root, prefer_existing = prefer_existing)
}

project_tasks_relative_path <- function(root = ".", prefer_existing = TRUE) {
  project_metadata_relative_path("tasks.yml", root = root, prefer_existing = prefer_existing)
}

project_marker_path <- function(path) {
  registry <- fs::path(path, default_project_metadata_dir(), "project_registry.yml")
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
#' @param root Starting path from which \code{projflow} should search upwards for a
#'   project marker such as \code{project.yml}, \code{.projflow/project_registry.yml},
#'   \code{.here}, or an \code{.Rproj} file.
#'
#' @return Absolute project root path.
#' @examples
#' \dontrun{
#' projflow:::find_project_root(".")
#' }
#' @author Thiago de Paula Oliveira
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
          "`.projflow/project_registry.yml`, `project.yml`, `config.yml`, `.here`, or a single `.Rproj` file."
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
  project_metadata_path(find_project_root(root), "project_registry.yml", create_dir = FALSE, prefer_existing = FALSE)
}

local_config_path <- function(root = ".") {
  project_metadata_path(find_project_root(root), "local.yml", create_dir = FALSE, prefer_existing = FALSE)
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
  metadata_dir <- project_metadata_dir(root, create = TRUE, prefer_existing = TRUE)

  registry_file <- fs::path(metadata_dir, "project_registry.yml")
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
  metadata_dir <- project_metadata_dir(root, create = TRUE, prefer_existing = TRUE)

  local_file <- fs::path(metadata_dir, "local.yml")
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
  metadata_dir <- project_metadata_dir(root, create = FALSE, prefer_existing = FALSE)

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
      registry_dir = metadata_dir
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
#' @param root Existing project root to initialise for the current R session.
#' @param install_missing Logical scalar. If \code{TRUE}, missing packages declared
#'   in the project configuration are installed automatically.
#' @param check_paths Logical scalar. If \code{TRUE}, key project directories are
#'   created if they are missing.
#' @param set_seed Logical scalar. If \code{TRUE}, set the configured project random
#'   seed when one is defined in \code{project.yml}.
#'
#' @return Project metadata, paths, registry, and package status.
#' @examples
#' \dontrun{
#' setup_project()
#' setup_project(install_missing = FALSE, check_paths = TRUE)
#' }
#' @author Thiago de Paula Oliveira
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
#' @param path Path to the external data directory for the current machine. The
#'   stored value is normalised to an absolute path.
#' @param name Data source name, such as \code{"default"} or \code{"reference"}.
#' @param root Existing project root whose \code{.projflow/local.yml} file should be
#'   updated.
#'
#' @return Invisibly returns the stored absolute path.
#' @examples
#' \dontrun{
#' set_project_data_root("/mnt/project_data", name = "default")
#' set_project_data_root("/mnt/reference_data", name = "reference")
#' }
#' @author Thiago de Paula Oliveira
#' @export
set_project_data_root <- function(path, name = "default", root = ".") {
  validate_character_vector(path, "path")
  name <- validate_project_object_name(name, repair = TRUE)
  root <- find_project_root(root)

  backup_project_local_config(root)
  config <- read_project_local_config(root)
  config$data_sources[[name]] <- list(
    path = normalize_absolute_path(path)
  )
  write_project_local_config(config, root = root, overwrite = TRUE)
  append_project_activity(
    action = "set_data_source",
    object_type = "data_source",
    object_id = name,
    object_name = name,
    details = list(path = config$data_sources[[name]]$path),
    root = root
  )

  invisible(config$data_sources[[name]]$path)
}

#' Read a configured external data root
#'
#' @param name Data source name to retrieve from \code{.projflow/local.yml}.
#' @param root Existing project root.
#'
#' @return Absolute path.
#' @examples
#' \dontrun{
#' project_data_root()
#' project_data_root("reference")
#' }
#' @author Thiago de Paula Oliveira
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
#' @param ... Path components appended below the selected external data root.
#' @param source Data source name to use when resolving the root directory.
#' @param root Existing project root.
#'
#' @return Absolute file path under the external data root.
#' @examples
#' \dontrun{
#' project_data_path("phenotypes.csv")
#' project_data_path("maps", "field_map.gpkg", source = "reference")
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_data_path <- function(..., source = "default", root = ".") {
  as.character(fs::path(project_data_root(name = source, root = root), ...))
}

#' List configured external data sources
#'
#' @param root Existing project root whose local data-source configuration should
#'   be listed.
#'
#' @return Data frame of configured sources.
#' @examples
#' \dontrun{
#' list_project_data_sources()
#' }
#' @author Thiago de Paula Oliveira
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
#' @param name Data source name to remove from \code{.projflow/local.yml}.
#' @param root Existing project root whose local data-source configuration
#'   should be updated.
#'
#' @return Invisibly returns remaining data sources.
#' @examples
#' \dontrun{
#' remove_project_data_source("reference")
#' }
#' @author Thiago de Paula Oliveira
#' @export
remove_project_data_source <- function(name = "default", root = ".") {
  name <- validate_project_object_name(name, repair = TRUE)
  root <- find_project_root(root)
  backup_project_local_config(root)
  config <- read_project_local_config(root)
  config$data_sources[[name]] <- NULL
  write_project_local_config(config, root = root, overwrite = TRUE)
  append_project_activity(
    action = "remove_data_source",
    object_type = "data_source",
    object_id = name,
    object_name = name,
    root = root
  )
  invisible(config$data_sources)
}

#' Check whether configured external data sources are accessible
#'
#' @param root Existing project root whose configured data sources should be
#'   checked for existence and readability.
#'
#' @return Data frame describing configured data sources.
#' @examples
#' \dontrun{
#' check_project_data_access()
#' }
#' @author Thiago de Paula Oliveira
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

infer_output_type <- function(output_name, script_type = NULL) {
  validate_character_vector(output_name, "output_name")
  output_name <- output_name[[1]]
  script_type <- script_type %||% NA_character_

  if (grepl("figure|plot|graph", output_name, ignore.case = TRUE) ||
      identical(script_type, "visualisation")) {
    return("figure")
  }

  if (grepl("table|summary", output_name, ignore.case = TRUE) ||
      identical(script_type, "summary")) {
    return("table")
  }

  if (grepl("model", output_name, ignore.case = TRUE) ||
      identical(script_type, "model")) {
    return("model")
  }

  if (grepl("diagnostic", output_name, ignore.case = TRUE) ||
      identical(script_type, "model_diagnostics")) {
    return("model_diagnostics")
  }

  if (grepl("qc|quality", output_name, ignore.case = TRUE) ||
      identical(script_type, "quality_control")) {
    return("quality_control")
  }

  if (script_type %in% c("import", "data_preparation", "data_cleaning") ||
      grepl("data|dataset|input|prepared|clean", output_name, ignore.case = TRUE)) {
    return("dataset")
  }

  "output"
}

canonical_output_type <- function(name, entry = list(), registry = list()) {
  type <- entry$type %||% "output"

  if (!identical(type, "output")) {
    return(type)
  }

  generated_by <- entry$generated_by %||% NA_character_
  script_type <- NA_character_
  if (!is.na(generated_by) && nzchar(generated_by) &&
      !is.null(registry$scripts[[generated_by]]$type)) {
    script_type <- registry$scripts[[generated_by]]$type
  }

  infer_output_type(name, script_type = script_type)
}

project_output_subdirs <- function() {
  c(
    "data",
    "analysis",
    "models",
    "diagnostics",
    "tables",
    "figures",
    "reports",
    "logs",
    "project_management",
    "deliverables"
  )
}

default_report_output_path <- function(name, extension = "html") {
  name <- validate_project_object_name(name, repair = TRUE)
  extension <- gsub("^\\.", "", extension)
  fs::path("outputs", "reports", name, paste0(name, ".", extension))
}

dashboard_report_entries <- function(registry) {
  reports <- registry$reports %||% list()
  if (length(reports) == 0L) {
    return(list())
  }

  keep <- vapply(
    reports,
    function(entry) {
      entry_type <- entry$type %||% ""
      entry_path <- normalize_relative_path(entry$path %||% "")
      identical(entry_type, "dashboard") || startsWith(entry_path, "dashboard/")
    },
    logical(1)
  )

  reports[keep]
}

default_output_path <- function(name, type) {
  type <- validate_project_object_type(type)

  if (type %in% c("dataset", "import", "data_preparation", "data_cleaning", "quality_control")) {
    return(fs::path("outputs", "data", paste0(name, ".rds")))
  }

  if (type %in% c("analysis", "statistical_analysis", "exploration", "exploratory_analysis", "simulation", "forecasting", "optimisation", "causal_inference", "output")) {
    return(fs::path("outputs", "analysis", paste0(name, ".rds")))
  }

  if (identical(type, "model")) {
    return(fs::path("outputs", "models", paste0(name, ".rds")))
  }

  if (identical(type, "model_diagnostics")) {
    return(fs::path("outputs", "diagnostics", paste0(name, ".rds")))
  }

  if (type %in% c("export", "table", "summary")) {
    return(fs::path("outputs", "tables", paste0(name, ".csv")))
  }

  if (type %in% c("figure", "visualisation", "dashboard")) {
    return(fs::path("outputs", "figures", paste0(name, ".png")))
  }

  if (type %in% c("report", "manuscript")) {
    return(default_report_output_path(name, "html"))
  }

  fs::path("outputs", "analysis", paste0(name, ".rds"))
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
    return(0)
  }

  max(orders) + 1
}


script_file_numbers <- function(registry) {
  paths <- vapply(registry$scripts %||% list(), function(entry) entry$path %||% "", character(1))
  stems <- tools::file_path_sans_ext(basename(paths))
  numbers <- suppressWarnings(as.integer(sub("^([0-9]+).*$", "\\1", stems)))
  numbers[grepl("^[0-9]+[_-]", stems) & !is.na(numbers)]
}

next_script_file_number <- function(registry) {
  numbers <- script_file_numbers(registry)
  if (length(numbers) == 0L) {
    return(0L)
  }
  max(numbers) + 1L
}

numbered_script_path <- function(name, number) {
  number <- suppressWarnings(as.integer(number))
  if (is.na(number)) {
    number <- 0L
  }
  fs::path("analysis", paste0(number, "_", name, ".R"))
}

substitute_script_template_placeholders <- function(text, name, script_type, outputs = NULL, title = NULL) {
  title <- if (is.null(title)) name else title
  output_text <- paste(outputs %||% character(), collapse = ", ")

  replacements <- c(
    "{{name}}" = name,
    "{{script_type}}" = script_type,
    "{{title}}" = title,
    "{{date}}" = as.character(Sys.Date()),
    "{{output_names}}" = output_text
  )

  for (placeholder in names(replacements)) {
    text <- gsub(placeholder, replacements[[placeholder]], text, fixed = TRUE)
  }

  text
}

read_custom_script_template <- function(template_path = NULL, template_text = NULL) {
  if (!is.null(template_path) && !is.null(template_text)) {
    rlang::abort("Use either `template_path` or `template_text`, not both.")
  }

  if (!is.null(template_path)) {
    validate_character_vector(template_path, "template_path")
    template_path <- template_path[[1]]
    if (!fs::file_exists(template_path)) {
      rlang::abort(paste0("`template_path` does not exist: ", template_path))
    }
    return(paste(readLines(template_path, warn = FALSE), collapse = "\n"))
  }

  if (!is.null(template_text)) {
    validate_character_vector(template_text, "template_text")
    return(paste(template_text, collapse = "\n"))
  }

  NULL
}

make_script_template <- function(name,
                            script_type,
                            outputs = NULL,
                            script_template = c("documented", "blank"),
                            title = NULL,
                            template_path = NULL,
                            template_text = NULL) {
  custom_template <- read_custom_script_template(template_path = template_path, template_text = template_text)
  title <- if (is.null(title)) name else title
  outputs <- outputs %||% character()

  if (!is.null(custom_template)) {
    return(substitute_script_template_placeholders(
      custom_template,
      name = name,
      script_type = script_type,
      outputs = outputs,
      title = title
    ))
  }

  script_template <- match.arg(script_template)
  if (identical(script_template, "blank")) {
    return("")
  }

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
      paste0("# ", title),
      paste0("# Script type: ", script_type),
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
#' @param root Existing project root whose registry should be listed.
#'
#' @return Data frame with scripts, reports, and outputs.
#' @examples
#' \dontrun{
#' list_project_objects()
#' }
#' @author Thiago de Paula Oliveira
#' @export
list_project_objects <- function(root = ".") {
  registry <- read_project_registry(root)
  rbind(
    registry_rows("script", registry$scripts),
    registry_rows("report", registry$reports),
    registry_rows("output", registry$outputs)
  )
}

#' Get one registered project object
#'
#' @param name Object name to retrieve.
#' @param root Existing project root.
#' @param section Optional section filter: \code{"script"}, \code{"report"}, or \code{"output"}.
#'
#' @return A named list describing the object.
#' @examples
#' \dontrun{
#' get_project_object("main_report", section = "report")
#' }
#' @author Thiago de Paula Oliveira
#' @export
get_project_object <- function(name, root = ".", section = c("script", "report", "output", "any")) {
  name <- validate_project_object_name(name, repair = TRUE)
  section <- match.arg(section)
  registry <- read_project_registry(root)

  if (!identical(section, "any")) {
    entry <- registry[[registry_section_for_type(section)]][[name]]
    if (is.null(entry)) {
      rlang::abort(paste0("No registered ", section, " named `", name, "`."))
    }
    return(entry)
  }

  for (candidate in c("scripts", "reports", "outputs")) {
    entry <- registry[[candidate]][[name]]
    if (!is.null(entry)) {
      return(entry)
    }
  }

  rlang::abort(paste0("No registered project object named `", name, "`."))
}

#' List registered project reports
#'
#' @param root Existing project root.
#'
#' @return Data frame of registered reports.
#' @examples
#' \dontrun{
#' list_project_reports()
#' }
#' @author Thiago de Paula Oliveira
#' @export
list_project_reports <- function(root = ".") {
  registry <- read_project_registry(root)
  if (length(registry$reports) == 0L) {
    return(data.frame(
      name = character(),
      source = character(),
      output = character(),
      type = character(),
      source_exists = logical(),
      output_exists = logical(),
      stringsAsFactors = FALSE
    ))
  }

  root <- find_project_root(root)
  do.call(
    rbind,
    lapply(names(registry$reports), function(name) {
      entry <- registry$reports[[name]]
      output_path <- default_output_path(name, "report")
      data.frame(
        name = name,
        source = entry$path,
        output = output_path,
        type = entry$type %||% "report",
        source_exists = file.exists(fs::path(root, entry$path)),
        output_exists = file.exists(fs::path(root, output_path)),
        stringsAsFactors = FALSE
      )
    })
  )
}

#' Register an output object in the project registry
#'
#' @param name Output object name to store in the registry.
#' @param path Project-relative output path, typically somewhere under
#'   \code{outputs/}.
#' @param type Output object type, such as \code{"table"}, \code{"figure"}, or
#'   \code{"analysis"}.
#' @param root Existing project root whose registry should be updated.
#' @param overwrite Logical scalar. If \code{TRUE}, replace an existing registry
#'   entry with the same name.
#'
#' @return Invisibly returns the registry entry.
#' @examples
#' \dontrun{
#' register_project_object(
#'   name = "weekly_summary",
#'   path = "outputs/tables/weekly_summary.csv",
#'   type = "table"
#' )
#' }
#' @author Thiago de Paula Oliveira
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
  backup_project_registry(root)
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
  append_project_activity(
    action = "register_output",
    object_type = "output",
    object_id = name,
    object_name = name,
    details = list(path = entry$path, type = entry$type),
    root = root
  )

  invisible(entry)
}

#' Remove an output object from the project registry
#'
#' @param name Output object name to remove from the registry.
#' @param root Existing project root whose registry should be updated.
#'
#' @return Invisibly returns the modified registry.
#' @examples
#' \dontrun{
#' unregister_project_object("weekly_summary")
#' }
#' @author Thiago de Paula Oliveira
#' @export
unregister_project_object <- function(name, root = ".") {
  name <- validate_project_object_name(name, repair = TRUE)
  root <- find_project_root(root)
  backup_project_registry(root)
  registry <- read_project_registry(root)
  registry$outputs[[name]] <- NULL
  write_project_registry(registry, root = root, overwrite = TRUE)
  append_project_activity(
    action = "unregister_output",
    object_type = "output",
    object_id = name,
    object_name = name,
    root = root
  )
  invisible(registry)
}

#' Update an output object in the project registry
#'
#' @param name Output object name to update.
#' @param ... Named fields to merge into the existing registry entry, such as
#'   \code{path}, \code{type}, or \code{generated_by}.
#' @param root Existing project root whose registry should be updated.
#'
#' @return Invisibly returns the updated entry.
#' @examples
#' \dontrun{
#' update_project_object("weekly_summary", generated_by = "summarise_weekly")
#' }
#' @author Thiago de Paula Oliveira
#' @export
update_project_object <- function(name, ..., root = ".") {
  name <- validate_project_object_name(name, repair = TRUE)
  updates <- list(...)
  root <- find_project_root(root)
  backup_project_registry(root)
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
  append_project_activity(
    action = "update_output",
    object_type = "output",
    object_id = name,
    object_name = name,
    details = updates,
    root = root
  )
  invisible(entry)
}

new_registry_action <- function(action, kind, name, path, root, dry_run, details = list()) {
  structure(
    utils::modifyList(
      list(
        action = action,
        kind = kind,
        name = name,
        path = normalize_relative_path(path),
        root = find_project_root(root),
        dry_run = dry_run
      ),
      details
    ),
    class = "projflow_registry_action"
  )
}

issue_table_with_severity <- function(x, severity) {
  if (nrow(x) == 0L) {
    x$severity <- character()
    return(x[, c("severity", "check", "message", "path", "fix")])
  }

  cbind(
    data.frame(severity = rep(severity, nrow(x)), stringsAsFactors = FALSE),
    x,
    stringsAsFactors = FALSE
  )
}

project_relative_path_exists <- function(root, path) {
  full_path <- fs::path(root, path)
  fs::file_exists(full_path) || fs::dir_exists(full_path)
}

registry_section_for_type <- function(type) {
  type <- match.arg(type, c("script", "report", "output"))
  switch(
    type,
    script = "scripts",
    report = "reports",
    output = "outputs"
  )
}

delete_project_relative_path <- function(root, path) {
  full_path <- normalize_absolute_path(fs::path(root, path))
  root <- normalize_absolute_path(root)

  if (!startsWith(full_path, paste0(root, "/")) && !identical(full_path, root)) {
    rlang::abort("Refusing to delete a path outside the project root.")
  }

  if (fs::file_exists(full_path)) {
    fs::file_delete(full_path)
  } else if (fs::dir_exists(full_path)) {
    fs::dir_delete(full_path)
  }

  invisible(full_path)
}

rename_project_relative_path <- function(root, from, to, overwrite = FALSE) {
  from_path <- fs::path(root, from)
  to_path <- fs::path(root, to)

  if (!project_relative_path_exists(root, from)) {
    rlang::abort(paste0("Source path does not exist: ", normalize_relative_path(from)))
  }

  if (project_relative_path_exists(root, to) && !isTRUE(overwrite)) {
    rlang::abort(paste0("Destination path already exists: ", normalize_relative_path(to)))
  }

  fs::dir_create(fs::path_dir(to_path), recurse = TRUE)
  if (project_relative_path_exists(root, to) && isTRUE(overwrite)) {
    delete_project_relative_path(root, to)
  }
  fs::file_move(from_path, to_path)
  invisible(to_path)
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
#' @param object R object to save. Supported output formats depend on \code{type} and
#'   the inferred file extension.
#' @param name Output object name recorded in the registry.
#' @param type Output object type, used to infer the default output file path.
#' @param root Existing project root where the output should be registered or
#'   resolved.
#' @param location Save either inside the project \code{outputs/} directory or under
#'   a configured external data source.
#' @param source External data source name to use when \code{location = "external"}.
#'
#' @return Invisibly returns the saved path.
#' @examples
#' \dontrun{
#' result <- data.frame(id = 1:3, value = c(4.2, 4.8, 5.1))
#' save_project_object(result, name = "analysis_results", type = "analysis")
#' }
#' @author Thiago de Paula Oliveira
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
#' @param object R object to save outside the repository.
#' @param name Output object name.
#' @param type Output object type used to infer the output filename.
#' @param source External data source name that should receive the output.
#' @param root Existing project root used to resolve the named external data
#'   source.
#'
#' @return Invisibly returns the saved path.
#' @examples
#' \dontrun{
#' result <- data.frame(id = 1:3, value = c(4.2, 4.8, 5.1))
#' save_external_project_object(
#'   result,
#'   name = "analysis_results_backup",
#'   type = "analysis",
#'   source = "default"
#' )
#' }
#' @author Thiago de Paula Oliveira
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
#' @param name Output object name to load from the registry.
#' @param root Existing project root whose registry should be consulted.
#'
#' @return Loaded object.
#' @examples
#' \dontrun{
#' load_project_object("analysis_results")
#' }
#' @author Thiago de Paula Oliveira
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
                                   output_dir = NULL,
                                   quiet = TRUE,
                                   execute_dir = NULL,
                                   ...) {
  override <- getOption("projflow.quarto_render", NULL)
  
  if (is.function(override)) {
    override_args <- list(
      input = input,
      output_file = output_file,
      quiet = quiet,
      execute_dir = execute_dir
    )
    override_formals <- names(formals(override))
    has_dots <- "..." %in% override_formals
    if (!is.null(output_dir) && (has_dots || "output_dir" %in% override_formals)) {
      override_args$output_dir <- output_dir
    }
    return(do.call(override, c(override_args, list(...))))
  }
  
  if (!requireNamespace("quarto", quietly = TRUE)) {
    rlang::abort("Quarto is not installed.")
  }
  
  render_args <- list(
    input = input,
    output_file = output_file,
    quiet = quiet
  )
  
  quarto_formals <- names(formals(quarto::quarto_render))
  
  if (!is.null(output_dir) && "output_dir" %in% quarto_formals) {
    render_args$output_dir <- output_dir
  }
  
  if (!is.null(execute_dir) && "execute_dir" %in% quarto_formals) {
    render_args$execute_dir <- execute_dir
  }
  
  do.call(quarto::quarto_render, render_args)
}

report_rendered_path_candidates <- function(input_path, output_path, rendered, output_file, render_root) {
  rendered_paths <- if (is.null(rendered)) {
    character()
  } else {
    unlist(rendered, recursive = TRUE, use.names = FALSE)
  }
  rendered_paths <- as.character(rendered_paths)
  rendered_paths <- rendered_paths[!is.na(rendered_paths) & nzchar(rendered_paths)]

  stem <- tools::file_path_sans_ext(output_file)
  expected_names <- unique(c(output_file, paste0(stem, ".html")))
  output_dir <- fs::path_dir(output_path)
  project_report_dir <- fs::path(render_root, "outputs", "reports")
  input_parent_name <- fs::path_file(fs::path_dir(input_path))

  search_dirs <- unique(normalize_absolute_path(c(
    output_dir,
    fs::path(output_dir, input_parent_name),
    fs::path_dir(input_path),
    project_report_dir,
    fs::path(project_report_dir, input_parent_name),
    render_root
  )))
  search_dirs <- search_dirs[fs::dir_exists(search_dirs)]

  discovered <- character()
  for (search_dir in search_dirs) {
    for (expected_name in expected_names) {
      discovered <- c(
        discovered,
        list.files(
          search_dir,
          pattern = utils::glob2rx(expected_name),
          full.names = TRUE,
          recursive = TRUE,
          ignore.case = FALSE,
          no.. = TRUE
        )
      )
    }
  }

  candidates <- unique(c(
    output_path,
    rendered_paths,
    fs::path(output_dir, output_file),
    fs::path(output_dir, input_parent_name, output_file),
    fs::path(project_report_dir, output_file),
    fs::path(project_report_dir, input_parent_name, output_file),
    fs::path(fs::path_dir(input_path), output_file),
    fs::path(render_root, output_file),
    discovered
  ))
  candidates <- normalize_absolute_path(candidates)
  candidates <- candidates[fs::file_exists(candidates)]

  if (length(candidates) == 0L) {
    return(character())
  }

  info <- file.info(candidates)
  candidates[order(info$mtime, decreasing = TRUE, na.last = TRUE)]
}

copy_report_companion_files <- function(from_html, to_html) {
  from_html <- normalize_absolute_path(from_html)
  to_html <- normalize_absolute_path(to_html)
  from_dir <- fs::path_dir(from_html)
  to_dir <- fs::path_dir(to_html)

  fs::dir_create(to_dir, recurse = TRUE)
  if (!identical(from_html, to_html)) {
    fs::file_copy(from_html, to_html, overwrite = TRUE)
  }

  from_stem <- tools::file_path_sans_ext(fs::path_file(from_html))
  to_stem <- tools::file_path_sans_ext(fs::path_file(to_html))
  companion_names <- unique(c(
    paste0(from_stem, "_files"),
    paste0(to_stem, "_files"),
    "site_libs",
    "libs"
  ))

  for (companion_name in companion_names) {
    source_dir <- fs::path(from_dir, companion_name)
    if (!fs::dir_exists(source_dir)) {
      next
    }

    target_name <- if (identical(companion_name, paste0(from_stem, "_files"))) {
      paste0(to_stem, "_files")
    } else {
      companion_name
    }
    target_dir <- fs::path(to_dir, target_name)

    if (fs::dir_exists(target_dir)) {
      fs::dir_delete(target_dir)
    }
    fs::dir_copy(source_dir, target_dir, overwrite = TRUE)
  }

  invisible(to_html)
}

cleanup_report_render_noise <- function(input_path, output_path, output_file, render_root) {
  output_path <- normalize_absolute_path(output_path)
  report_root <- normalize_absolute_path(fs::path(render_root, "outputs", "reports"))
  if (!fs::dir_exists(report_root)) {
    return(invisible(FALSE))
  }

  report_name <- tools::file_path_sans_ext(output_file)
  input_parent_name <- fs::path_file(fs::path_dir(input_path))
  noise_html <- unique(normalize_absolute_path(c(
    fs::path(report_root, output_file),
    fs::path(report_root, input_parent_name, output_file),
    fs::path(render_root, output_file),
    fs::path(fs::path_dir(input_path), output_file)
  )))
  noise_html <- setdiff(noise_html, output_path)

  for (path in noise_html) {
    if (fs::file_exists(path)) {
      fs::file_delete(path)
    }

    files_dir <- fs::path(fs::path_dir(path), paste0(report_name, "_files"))
    if (fs::dir_exists(files_dir)) {
      fs::dir_delete(files_dir)
    }
  }

  nested_dir <- fs::path(report_root, input_parent_name)
  if (!identical(normalize_absolute_path(nested_dir), fs::path_dir(output_path)) && fs::dir_exists(nested_dir)) {
    remaining <- list.files(nested_dir, all.files = TRUE, no.. = TRUE)
    if (length(remaining) == 0L) {
      fs::dir_delete(nested_dir)
    }
  }

  invisible(TRUE)
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
        output_dir = fs::path_dir(output_path),
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
    
    candidate_paths <- report_rendered_path_candidates(
      input_path = input_path,
      output_path = output_path,
      rendered = rendered,
      output_file = output_file,
      render_root = render_root
    )
    
    source_path <- character()
    if (length(candidate_paths) > 0L) {
      non_canonical <- setdiff(candidate_paths, output_path)
      source_path <- if (length(non_canonical) > 0L) non_canonical[[1]] else candidate_paths[[1]]
      copy_report_companion_files(source_path, output_path)
    }
    
    if (!fs::file_exists(output_path)) {
      searched <- paste(
        unique(normalize_absolute_path(c(
          fs::path_dir(output_path),
          fs::path(fs::path_dir(output_path), fs::path_file(fs::path_dir(input_path))),
          fs::path_dir(input_path),
          fs::path(render_root, "outputs", "reports"),
          render_root
        ))),
        collapse = "; "
      )
      return(paste0(
        "Quarto completed, but expected output was not found: ",
        output_path,
        ". Searched for `",
        output_file,
        "` under: ",
        searched
      ))
    }
    
    cleanup_report_render_noise(
      input_path = input_path,
      output_path = output_path,
      output_file = output_file,
      render_root = render_root
    )
    
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

# Internal script creation helper used by public wrappers and tests.
# This keeps advanced functionality out of the user-facing API while preserving
# a single implementation point for registry and filesystem updates.
create_project_script_internal <- function(name,
                               script_type = "analysis",
                               root = ".",
                               order = NULL,
                               output_names = NULL,
                               output_path = NULL,
                               script_template = c("documented", "blank"),
                               template_path = NULL,
                               template_text = NULL,
                               open = interactive(),
                               overwrite = FALSE,
                               repair = FALSE,
                               dry_run = FALSE) {
  validate_logical_scalar(open, "open")
  validate_logical_scalar(overwrite, "overwrite")
  validate_logical_scalar(repair, "repair")
  validate_logical_scalar(dry_run, "dry_run")
  if (!is.null(order) && (!is.numeric(order) || length(order) != 1L || is.na(order))) {
    rlang::abort("`order` must be a single numeric value or `NULL`.")
  }

  name <- validate_project_object_name(name, repair = TRUE)
  script_type <- validate_choice(script_type, project_script_types(), "script_type")
  if (is.null(template_path) && is.null(template_text)) {
    script_template <- match.arg(script_template)
  }
  if (!is.null(output_path) && !is.null(output_names)) {
    rlang::abort("Use either `output_names` or `output_path`, not both.")
  }

  output_names <- if (is.null(output_names)) character() else unique(vapply(output_names, validate_project_object_name, character(1), repair = TRUE))
  root <- find_project_root(root)
  backup_project_registry(root)
  registry <- read_project_registry(root)
  if (is.null(order)) {
    order <- next_script_order(registry)
  }
  existing_script <- registry$scripts[[name]]
  relative_path <- if (!is.null(existing_script) && isTRUE(overwrite) && !is.null(existing_script$path)) {
    existing_script$path
  } else {
    numbered_script_path(name, next_script_file_number(registry))
  }
  source_path <- fs::path(root, relative_path)

  if (!is.null(registry$scripts[[name]]) && !isTRUE(overwrite)) {
    rlang::abort(paste0("Script `", name, "` is already registered."))
  }

  if (project_relative_path_exists(root, relative_path) && !isTRUE(overwrite) && !isTRUE(repair)) {
    rlang::abort(paste0("Script file already exists: ", normalize_relative_path(relative_path)))
  }

  explicit_output <- NULL
  if (!is.null(output_path)) {
    validate_character_vector(output_path, "output_path")
    if (is_absolute_path(output_path[[1]])) {
      rlang::abort("`output_path` must be a project-relative path.")
    }
    explicit_output <- list(
      name = validate_project_object_name(tools::file_path_sans_ext(safe_basename(output_path[[1]])), repair = TRUE),
      path = normalize_relative_path(output_path[[1]]),
      type = infer_output_type(tools::file_path_sans_ext(safe_basename(output_path[[1]])), script_type)
    )
    output_names <- explicit_output$name
  }

  if (isTRUE(dry_run)) {
    return(new_registry_action(
      action = if (isTRUE(repair)) "repair_script" else "create_script",
      kind = "script",
      name = name,
      path = relative_path,
      root = root,
      dry_run = TRUE,
      details = list(
        script_type = script_type,
        order = if (is.null(order)) next_script_order(registry) else order,
        output_names = output_names,
        output_path = output_path
      )
    ))
  }

  if (!isTRUE(repair)) {
    write_template_file(
      source_path,
      make_script_template(
        name = name,
        script_type = script_type,
        outputs = output_names,
        script_template = script_template,
        title = name,
        template_path = template_path,
        template_text = template_text
      ),
      overwrite = overwrite
    )
  } else if (!project_relative_path_exists(root, relative_path)) {
    rlang::abort(paste0("Cannot repair script `", name, "` because the file does not exist: ", normalize_relative_path(relative_path)))
  }

  registry$scripts[[name]] <- list(
    path = normalize_relative_path(relative_path),
    type = script_type,
    order = order,
    outputs = output_names
  )

  if (length(output_names) > 0L) {
    for (output_name in output_names) {
      if (!is.null(registry$outputs[[output_name]]) && !isTRUE(overwrite)) {
        rlang::abort(paste0("Output `", output_name, "` is already registered."))
      }

      output_entry <- if (!is.null(explicit_output) && identical(output_name, explicit_output$name)) {
        explicit_output
      } else {
        list(
          path = normalize_relative_path(default_output_path(output_name, infer_output_type(output_name, script_type))),
          type = infer_output_type(output_name, script_type)
        )
      }

      registry$outputs[[output_name]] <- list(
        path = output_entry$path,
        type = output_entry$type,
        generated_by = name
      )
    }
  }

  write_project_registry(registry, root = root, overwrite = TRUE)
  append_project_activity(
    action = if (isTRUE(repair)) "repair_script" else "create_script",
    object_type = "script",
    object_id = name,
    object_name = name,
    details = list(path = normalize_relative_path(relative_path), script_type = script_type, output_names = output_names),
    root = root
  )

  if (isTRUE(open)) {
    cli::cli_alert_info("Project opening is not automated; open the script manually if needed.")
  }

  invisible(source_path)
}



#' Create a new project script
#'
#' @description
#' \code{new_project_script()} creates a documented, non-executing R script in
#' an existing \pkg{projflow} project and registers it as a workflow step. The
#' public interface is intentionally minimal and mirrors \code{\link{new_script}()}.
#'
#' @details
#' The script path is assigned automatically using the next sequential number
#' under \file{analysis/}. Generated scripts contain comments only and do not
#' create \file{.rds}, \file{.csv}, \file{.png}, \file{.html}, or other output
#' artefacts. Register expected outputs separately with
#' \code{\link{new_project_output}()}, \code{\link{new_output}()},
#' \code{\link{new_table}()}, or \code{\link{new_figure}()}.
#'
#' @param name Script name. The value is normalised to a safe snake_case name
#'   before creating the file and registry entry.
#' @param script_type Script type recorded in the registry.
#' @param root Existing project root where the script should be added.
#' @param open Logical scalar kept for API compatibility. The script is not
#'   opened automatically.
#' @param overwrite Logical scalar. If \code{TRUE}, replace an existing script
#'   file and registry entry with the same name.
#'
#' @return Invisibly returns the created script path.
#' @examples
#' \dontrun{
#' new_project_script("clean_phenotypes", script_type = "data_cleaning", open = FALSE)
#' new_project_output("cleaned_phenotypes", type = "dataset")
#' }
#' @keywords internal
#' @noRd
new_project_script <- function(name,
                               script_type = "analysis",
                               root = ".",
                               open = interactive(),
                               overwrite = FALSE) {
  create_project_script_internal(
    name = name,
    script_type = script_type,
    root = root,
    open = open,
    overwrite = overwrite
  )
}

#' Create a new project report
#'
#' @param name Report name used as the filename stem and registry key.
#' @param format Source format to create. Supported values are \code{"qmd"} and
#'   \code{"Rmd"}.
#' @param root Existing project root where the report should be added.
#' @param open Logical scalar kept for API compatibility. The report file is not
#'   opened automatically.
#' @param overwrite Logical scalar. If \code{TRUE}, replace an existing report file
#'   and registry entry with the same name.
#' @param repair Logical scalar. If \code{TRUE}, register an existing report file
#'   without overwriting it.
#' @param dry_run Logical scalar. If \code{TRUE}, return the planned change without
#'   writing files.
#'
#' @return Invisibly returns the created report path.
#' @examples
#' \dontrun{
#' new_project_report("supplementary_note", format = "qmd", open = FALSE)
#' }
#' @keywords internal
#' @noRd
new_project_report <- function(name,
                               format = c("qmd", "Rmd"),
                               root = ".",
                               open = interactive(),
                               overwrite = FALSE,
                               repair = FALSE,
                               dry_run = FALSE) {
  validate_logical_scalar(open, "open")
  validate_logical_scalar(overwrite, "overwrite")
  validate_logical_scalar(repair, "repair")
  validate_logical_scalar(dry_run, "dry_run")
  name <- validate_project_object_name(name, repair = TRUE)
  format <- match.arg(format)
  root <- find_project_root(root)
  backup_project_registry(root)
  registry <- read_project_registry(root)

  relative_path <- fs::path("reports", paste0(name, ".", format))
  if (!is.null(registry$reports[[name]]) && !isTRUE(overwrite)) {
    rlang::abort(paste0("Report `", name, "` is already registered."))
  }
  if (project_relative_path_exists(root, relative_path) && !isTRUE(overwrite) && !isTRUE(repair)) {
    rlang::abort(paste0("Report file already exists: ", normalize_relative_path(relative_path)))
  }

  if (isTRUE(dry_run)) {
    return(new_registry_action("create_report", "report", name, relative_path, root, TRUE, list(format = format)))
  }

  if (!isTRUE(repair)) {
    write_template_file(
      fs::path(root, relative_path),
      report_template(title = name),
      overwrite = overwrite
    )
  } else if (!project_relative_path_exists(root, relative_path)) {
    rlang::abort(paste0("Cannot repair report `", name, "` because the file does not exist: ", normalize_relative_path(relative_path)))
  }

  registry$reports[[name]] <- list(
    path = normalize_relative_path(relative_path),
    type = "report"
  )
  write_project_registry(registry, root = root, overwrite = TRUE)
  append_project_activity(
    action = if (isTRUE(repair)) "repair_report" else "create_report",
    object_type = "report",
    object_id = name,
    object_name = name,
    details = list(path = normalize_relative_path(relative_path), format = format),
    root = root
  )

  if (isTRUE(open)) {
    cli::cli_alert_info("Project opening is not automated; open the report manually if needed.")
  }

  invisible(fs::path(root, relative_path))
}

#' Create a new project idea
#'
#' @param name Idea name used either as a script name or as an output registry
#'   key depending on \code{create_script}.
#' @param script_type Script type to create when \code{create_script = TRUE}.
#' @param create_script Logical scalar. If \code{TRUE}, create a script immediately;
#'   otherwise register a placeholder output object.
#' @param root Existing project root to update.
#' @param open Logical scalar kept for API compatibility.
#'
#' @return Invisibly returns the created script path or registry entry.
#' @examples
#' \dontrun{
#' new_project_idea("future_analysis", create_script = TRUE, open = FALSE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
new_project_idea <- function(name, script_type = "analysis", create_script = TRUE, root = ".", open = interactive()) {
  validate_logical_scalar(create_script, "create_script")
  if (isTRUE(create_script)) {
    return(new_project_script(name = name, script_type = script_type, root = root, open = open))
  }

  new_project_output(
    name = name,
    type = "output",
    root = root,
    overwrite = FALSE,
    repair = FALSE,
    dry_run = FALSE
  )
}

#' Create or register a project output
#'
#' @param name Output object name used as the registry key and default filename
#'   stem.
#' @param type Output type such as \code{"table"}, \code{"figure"}, \code{"model"}, or
#'   \code{"output"}.
#' @param path Optional explicit project-relative path for the output. When
#'   omitted, \code{projflow} uses the default location for the selected \code{type}.
#' @param root Existing project root to update.
#' @param overwrite Logical scalar. If \code{TRUE}, replace an existing registry
#'   entry for the same output name.
#' @param repair Logical scalar. If \code{TRUE}, register an existing output file
#'   without overwriting it.
#' @param dry_run Logical scalar. If \code{TRUE}, return the planned change without
#'   modifying the project.
#'
#' @return Invisibly returns the registered output path, or a dry-run plan.
#' @examples
#' \dontrun{
#' new_project_output("model_fit", type = "model")
#' new_project_output("weekly_summary", type = "table", path = "outputs/tables/weekly_summary.csv")
#' }
#' @keywords internal
#' @noRd
new_project_output <- function(name,
                               type = "output",
                               path = NULL,
                               root = ".",
                               overwrite = FALSE,
                               repair = FALSE,
                               dry_run = FALSE) {
  validate_logical_scalar(overwrite, "overwrite")
  validate_logical_scalar(repair, "repair")
  validate_logical_scalar(dry_run, "dry_run")

  name <- validate_project_object_name(name, repair = TRUE)
  type <- validate_project_object_type(type)
  root <- find_project_root(root)
  registry <- read_project_registry(root)
  relative_path <- if (is.null(path)) {
    default_output_path(name, type)
  } else {
    validate_character_vector(path, "path")
    if (is_absolute_path(path[[1]])) {
      rlang::abort("`path` must be relative to the project root.")
    }
    normalize_relative_path(path[[1]])
  }

  if (!is.null(registry$outputs[[name]]) && !isTRUE(overwrite)) {
    rlang::abort(paste0("Output `", name, "` is already registered."))
  }

  if (isTRUE(dry_run)) {
    return(new_registry_action("create_output", "output", name, relative_path, root, TRUE, list(type = type)))
  }

  if (isTRUE(repair) && !project_relative_path_exists(root, relative_path)) {
    rlang::abort(paste0("Cannot repair output `", name, "` because the file does not exist: ", relative_path))
  }

  register_project_object(
    name = name,
    path = relative_path,
    type = type,
    root = root,
    overwrite = overwrite
  )
}

remove_registry_entry <- function(section, name, root = ".", delete_files = FALSE, confirm = FALSE, dry_run = FALSE) {
  section <- match.arg(section, c("script", "report", "output"))
  validate_logical_scalar(delete_files, "delete_files")
  validate_logical_scalar(confirm, "confirm")
  validate_logical_scalar(dry_run, "dry_run")

  root <- find_project_root(root)
  name <- validate_project_object_name(name, repair = TRUE)
  backup_project_registry(root)
  registry <- read_project_registry(root)
  section_name <- registry_section_for_type(section)
  entry <- registry[[section_name]][[name]]

  if (is.null(entry)) {
    rlang::abort(paste0(tools::toTitleCase(section), " `", name, "` is not registered."))
  }

  if (isTRUE(delete_files) && !isTRUE(confirm) && !isTRUE(dry_run)) {
    rlang::abort("Set `confirm = TRUE` to delete project files.")
  }

  if (isTRUE(dry_run)) {
    return(new_registry_action("remove", section, name, entry$path, root, TRUE, list(delete_files = delete_files)))
  }

  registry[[section_name]][[name]] <- NULL

  if (identical(section, "script")) {
    generated <- names(registry$outputs)[vapply(
      registry$outputs,
      function(output) identical(output$generated_by %||% "", name),
      logical(1)
    )]
    if (length(generated) > 0L) {
      for (output_name in generated) {
        registry$outputs[[output_name]]$generated_by <- NULL
      }
    }
  }

  write_project_registry(registry, root = root, overwrite = TRUE)

  if (isTRUE(delete_files) && project_relative_path_exists(root, entry$path)) {
    delete_project_relative_path(root, entry$path)
  }
  append_project_activity(
    action = paste0("remove_", section),
    object_type = section,
    object_id = name,
    object_name = name,
    details = list(path = entry$path, delete_files = delete_files),
    root = root
  )

  invisible(entry)
}

rename_registry_entry <- function(section, from, to, root = ".", overwrite = FALSE, dry_run = FALSE) {
  section <- match.arg(section, c("script", "report", "output"))
  validate_logical_scalar(overwrite, "overwrite")
  validate_logical_scalar(dry_run, "dry_run")

  root <- find_project_root(root)
  from <- validate_project_object_name(from, repair = TRUE)
  to <- validate_project_object_name(to, repair = TRUE)
  backup_project_registry(root)
  registry <- read_project_registry(root)
  section_name <- registry_section_for_type(section)
  entry <- registry[[section_name]][[from]]

  if (is.null(entry)) {
    rlang::abort(paste0(tools::toTitleCase(section), " `", from, "` is not registered."))
  }
  if (!is.null(registry[[section_name]][[to]]) && !isTRUE(overwrite)) {
    rlang::abort(paste0(tools::toTitleCase(section), " `", to, "` is already registered."))
  }

  extension <- fs::path_ext(entry$path)
  new_relative_path <- switch(
    section,
    script = {
      directory <- fs::path_dir(entry$path %||% "analysis")
      if (identical(directory, ".")) {
        directory <- "analysis"
      }
      stem <- tools::file_path_sans_ext(safe_basename(entry$path))
      prefix <- sub("^([0-9]+)[_-].*$", "\\1", stem)
      basename <- if (grepl("^[0-9]+[_-]", stem)) {
        paste0(prefix, "_", to, ".", extension)
      } else {
        paste0(to, ".", extension)
      }
      normalize_relative_path(fs::path(directory, basename))
    },
    report = normalize_relative_path(fs::path("reports", paste0(to, ".", extension))),
    output = normalize_relative_path(default_output_path(to, entry$type))
  )

  if (isTRUE(dry_run)) {
    return(new_registry_action("rename", section, from, entry$path, root, TRUE, list(new_name = to, new_path = new_relative_path)))
  }

  if (project_relative_path_exists(root, entry$path)) {
    rename_project_relative_path(root, entry$path, new_relative_path, overwrite = overwrite)
  }

  registry[[section_name]][[from]] <- NULL
  entry$path <- new_relative_path
  registry[[section_name]][[to]] <- entry

  if (identical(section, "script")) {
    for (output_name in names(registry$outputs)) {
      if (identical(registry$outputs[[output_name]]$generated_by %||% "", from)) {
        registry$outputs[[output_name]]$generated_by <- to
      }
    }
  }

  write_project_registry(registry, root = root, overwrite = TRUE)
  append_project_activity(
    action = paste0("rename_", section),
    object_type = section,
    object_id = to,
    object_name = to,
    details = list(previous_name = from, path = new_relative_path),
    root = root
  )
  invisible(entry)
}

#' Remove a project object
#'
#' @param name Object name to remove.
#' @param root Existing project root to update.
#' @param section Object section to remove: \code{"script"}, \code{"report"}, or
#'   \code{"output"}.
#' @param delete_files Logical scalar. If \code{TRUE}, delete the corresponding file
#'   after validating the path and requiring confirmation.
#' @param confirm Logical scalar confirming file deletion when
#'   \code{delete_files = TRUE}.
#' @param dry_run Logical scalar. If \code{TRUE}, return the planned change without
#'   modifying the project.
#'
#' @return Invisibly returns the removed entry, or a dry-run plan.
#' @examples
#' \dontrun{
#' remove_project_object("analysis_results", section = "output")
#' }
#' @author Thiago de Paula Oliveira
#' @export
remove_project_object <- function(name,
                                  root = ".",
                                  section = c("script", "report", "output"),
                                  delete_files = FALSE,
                                  confirm = FALSE,
                                  dry_run = FALSE) {
  section <- match.arg(section)
  remove_registry_entry(section, name, root = root, delete_files = delete_files, confirm = confirm, dry_run = dry_run)
}

#' Remove a project script
#'
#' @inheritParams remove_project_object
#'
#' @return Invisibly returns the removed script entry, or a dry-run plan.
#' @examples
#' \dontrun{
#' remove_project_script("analysis_01", delete_files = FALSE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
remove_project_script <- function(name, root = ".", delete_files = FALSE, confirm = FALSE, dry_run = FALSE) {
  remove_registry_entry("script", name, root = root, delete_files = delete_files, confirm = confirm, dry_run = dry_run)
}

#' Remove a project report
#'
#' @inheritParams remove_project_object
#'
#' @return Invisibly returns the removed report entry, or a dry-run plan.
#' @examples
#' \dontrun{
#' remove_project_report("main_report", delete_files = FALSE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
remove_project_report <- function(name, root = ".", delete_files = FALSE, confirm = FALSE, dry_run = FALSE) {
  remove_registry_entry("report", name, root = root, delete_files = delete_files, confirm = confirm, dry_run = dry_run)
}

#' Remove a project output
#'
#' @inheritParams remove_project_object
#'
#' @return Invisibly returns the removed output entry, or a dry-run plan.
#' @examples
#' \dontrun{
#' remove_project_output("analysis_results", delete_files = FALSE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
remove_project_output <- function(name, root = ".", delete_files = FALSE, confirm = FALSE, dry_run = FALSE) {
  remove_registry_entry("output", name, root = root, delete_files = delete_files, confirm = confirm, dry_run = dry_run)
}

#' Rename a project object
#'
#' @param from Existing object name.
#' @param to Replacement object name.
#' @param root Existing project root to update.
#' @param section Object section to rename: \code{"script"}, \code{"report"}, or
#'   \code{"output"}.
#' @param overwrite Logical scalar. If \code{TRUE}, allow an existing destination
#'   registry entry or file path to be replaced.
#' @param dry_run Logical scalar. If \code{TRUE}, return the planned change without
#'   modifying the project.
#'
#' @return Invisibly returns the updated entry, or a dry-run plan.
#' @examples
#' \dontrun{
#' rename_project_object("analysis_results", "analysis_results_v2", section = "output")
#' }
#' @author Thiago de Paula Oliveira
#' @export
rename_project_object <- function(from,
                                  to,
                                  root = ".",
                                  section = c("script", "report", "output"),
                                  overwrite = FALSE,
                                  dry_run = FALSE) {
  section <- match.arg(section)
  rename_registry_entry(section, from, to, root = root, overwrite = overwrite, dry_run = dry_run)
}

#' Rename a project script
#'
#' @inheritParams rename_project_object
#'
#' @return Invisibly returns the updated script entry, or a dry-run plan.
#' @examples
#' \dontrun{
#' rename_project_script("analysis_01", "analysis_02")
#' }
#' @author Thiago de Paula Oliveira
#' @export
rename_project_script <- function(from, to, root = ".", overwrite = FALSE, dry_run = FALSE) {
  rename_registry_entry("script", from, to, root = root, overwrite = overwrite, dry_run = dry_run)
}

#' Rename a project report
#'
#' @inheritParams rename_project_object
#'
#' @return Invisibly returns the updated report entry, or a dry-run plan.
#' @examples
#' \dontrun{
#' rename_project_report("main_report", "final_report")
#' }
#' @author Thiago de Paula Oliveira
#' @export
rename_project_report <- function(from, to, root = ".", overwrite = FALSE, dry_run = FALSE) {
  rename_registry_entry("report", from, to, root = root, overwrite = overwrite, dry_run = dry_run)
}

#' Rename a project output
#'
#' @inheritParams rename_project_object
#'
#' @return Invisibly returns the updated output entry, or a dry-run plan.
#' @examples
#' \dontrun{
#' rename_project_output("analysis_results", "analysis_results_v2")
#' }
#' @author Thiago de Paula Oliveira
#' @export
rename_project_output <- function(from, to, root = ".", overwrite = FALSE, dry_run = FALSE) {
  rename_registry_entry("output", from, to, root = root, overwrite = overwrite, dry_run = dry_run)
}

#' Update a registered project output
#'
#' @param name Output name.
#' @param ... Fields to update.
#' @param root Existing project root.
#'
#' @return Invisibly returns the updated output entry.
#' @examples
#' \dontrun{
#' update_project_output("analysis_results", generated_by = "fit_model")
#' }
#' @author Thiago de Paula Oliveira
#' @export
update_project_output <- function(name, ..., root = ".") {
  update_project_object(name, ..., root = root)
}

#' Update a registered project report
#'
#' @param name Report name.
#' @param root Existing project root.
#' @param overwrite Logical scalar. If \code{TRUE}, allow file-path changes to
#'   replace an existing destination.
#' @param path Optional replacement report source path.
#' @param type Optional replacement report type.
#'
#' @return Invisibly returns the updated report entry.
#' @examples
#' \dontrun{
#' update_project_report("main_report", path = "reports/final_report.qmd")
#' }
#' @author Thiago de Paula Oliveira
#' @export
update_project_report <- function(name, root = ".", overwrite = FALSE, path = NULL, type = NULL) {
  name <- validate_project_object_name(name, repair = TRUE)
  validate_logical_scalar(overwrite, "overwrite")
  root <- find_project_root(root)
  backup_project_registry(root)
  registry <- read_project_registry(root)
  entry <- registry$reports[[name]]
  if (is.null(entry)) {
    rlang::abort(paste0("Report `", name, "` is not registered."))
  }

  if (!is.null(path)) {
    validate_character_vector(path, "path")
    if (is_absolute_path(path[[1]])) {
      rlang::abort("Report paths must remain project-relative.")
    }
    new_path <- normalize_relative_path(path[[1]])
    if (!identical(entry$path, new_path) && project_relative_path_exists(root, new_path) && !isTRUE(overwrite)) {
      rlang::abort(paste0("Destination report path already exists: ", new_path))
    }
    if (project_relative_path_exists(root, entry$path) && !identical(entry$path, new_path)) {
      rename_project_relative_path(root, entry$path, new_path, overwrite = overwrite)
    }
    entry$path <- new_path
  }
  if (!is.null(type)) {
    entry$type <- validate_optional_text(type, "type")
  }

  registry$reports[[name]] <- entry
  write_project_registry(registry, root = root, overwrite = TRUE)
  append_project_activity(
    action = "update_report",
    object_type = "report",
    object_id = name,
    object_name = name,
    details = list(path = entry$path, type = entry$type),
    root = root
  )
  invisible(entry)
}

#' Render project reports
#'
#' @param root Existing project root whose registered reports should be
#'   rendered.
#'
#' @return Invisibly returns rendered report paths.
#' @examples
#' \dontrun{
#' render_project_reports()
#' }
#' @author Thiago de Paula Oliveira
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
#' @param name Registered script, report, or output name to resolve and run.
#' @param root Existing project root whose registry should be consulted.
#'
#' @return Invisibly returns the executed path.
#' @examples
#' \dontrun{
#' run_project_object("analysis_core")
#' }
#' @author Thiago de Paula Oliveira
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
#' @param name Registered step name. This is an alias for
#'   \code{run_project_object()} kept for workflow readability.
#' @param root Existing project root whose registry should be consulted.
#'
#' @return Invisibly returns the executed path.
#' @examples
#' \dontrun{
#' run_project_step("analysis_core")
#' }
#' @author Thiago de Paula Oliveira
#' @export
run_project_step <- function(name, root = ".") {
  run_project_object(name, root = root)
}

#' Run the project workflow
#'
#' @description
#' \code{run_project()} executes registered analysis scripts in topological DAG order.
#' If the registry does not contain enough dependency information to build a
#' valid DAG, the function falls back to the explicit registry order.
#'
#' @param root Existing project root whose registered scripts should be run.
#'
#' @return Invisibly returns executed script paths.
#' @examples
#' \dontrun{
#' run_project()
#' }
#' @author Thiago de Paula Oliveira
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

  dag_check <- validate_project_dag(root = root, strict = FALSE)
  if (isTRUE(dag_check$ok)) {
    ordered_names <- topological_project_order(root = root, type = "scripts")
    script_names <- ordered_names[ordered_names %in% names(registry$scripts)]
  } else {
    warning(
      "The analysis DAG is invalid; falling back to registry script order. Run `validate_project_dag()` for details.",
      call. = FALSE
    )
    script_names <- registry_script_names_by_order(registry)
  }

  for (name in script_names) {
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
#' @param root Existing project root whose registered outputs should be listed.
#'
#' @return Data frame of outputs and their existence status.
#' @examples
#' \dontrun{
#' list_project_outputs()
#' }
#' @author Thiago de Paula Oliveira
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
          exists = fs::file_exists(full_path),
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

#' Report missing project outputs
#'
#' @param root Existing project root whose registered outputs should be checked.
#'
#' @return Character vector of missing output paths.
#' @examples
#' \dontrun{
#' missing_project_outputs()
#' }
#' @author Thiago de Paula Oliveira
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
#' @param root Existing project root whose registered outputs should be compared
#'   against their generating scripts.
#'
#' @return Character vector of stale output paths.
#' @examples
#' \dontrun{
#' stale_project_outputs()
#' }
#' @author Thiago de Paula Oliveira
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

validate_report_source <- function(path) {
  extension <- tolower(fs::path_ext(path))
  if (!extension %in% c("qmd", "rmd")) {
    return(NULL)
  }

  lines <- readLines(path, warn = FALSE)
  yaml_delimiters <- which(trimws(lines) == "---")

  if (length(yaml_delimiters) < 2L || yaml_delimiters[[1]] != 1L) {
    return("Report source is missing a valid YAML front matter block at the top of the file.")
  }

  if (length(yaml_delimiters) > 2L) {
    return("Report source contains repeated YAML delimiters; keep a single front matter block at the top of the file.")
  }

  NULL
}

validate_project_registry <- function(root = ".", render_reports = FALSE) {
  root <- find_project_root(root)
  validate_logical_scalar(render_reports, "render_reports")
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
      project_registry_relative_path(root),
      "Repair or recreate the registry YAML."
    )
    return(list(ok = FALSE, errors = errors, warnings = warnings))
  }

  if (!identical(registry$version, 1L)) {
    errors <- append_issue(
      errors,
      "registry_version",
      paste0("Unsupported registry version: ", registry$version),
      project_registry_relative_path(root),
      "Use registry version 1."
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
      project_registry_relative_path(root),
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
    ".projflow/local.yml",
    ".projflow/activity_log.yml",
    ".projflow/backups/",
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
#' @param root Existing project root whose Git state should be inspected.
#'
#' @return Structured Git status information.
#' @examples
#' \dontrun{
#' check_git_status()
#' }
#' @author Thiago de Paula Oliveira
#' @export
check_git_status <- function(root = ".") {
  root <- find_project_root(root)
  git_initialized <- fs::dir_exists(fs::path(root, ".git"))
  remote <- FALSE
  local_config_ignored <- FALSE
  tracked_data_files <- character()

  if (git_initialized && nzchar(Sys.which("git"))) {
    remote <- length(git_command(root, c("remote"))) > 0L
    local_config_ignored <- git_exit_status(root, c("check-ignore", ".projflow/local.yml")) == 0L
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
#' @param root Existing project root whose GitHub Actions workflow files should
#'   be inspected.
#'
#' @return Data frame describing known workflows.
#' @examples
#' \dontrun{
#' check_github_actions()
#' }
#' @author Thiago de Paula Oliveira
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
#' @param root Existing project root where the workflow file should be created.
#' @param workflow Workflow name to create. Supported values are
#'   \code{"check-project"} and \code{"render-reports"}.
#'
#' @return Invisibly returns the created workflow path.
#' @examples
#' \dontrun{
#' use_github_actions(workflow = "check-project")
#' }
#' @author Thiago de Paula Oliveira
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

check_project_impl <- function(root = ".", deep = FALSE, render_reports = FALSE, strict = FALSE, repair = FALSE) {
  validate_logical_scalar(deep, "deep")
  validate_logical_scalar(render_reports, "render_reports")
  validate_logical_scalar(strict, "strict")
  validate_logical_scalar(repair, "repair")

  errors <- empty_issue_table()
  warnings <- empty_issue_table()
  suggestions <- empty_issue_table()
  info <- empty_issue_table()

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
      list(
        ok = FALSE,
        root = "",
        errors = errors,
        warnings = warnings,
        suggestions = suggestions,
        info = info,
        registered_files = NULL,
        issues = rbind(
          issue_table_with_severity(errors, "error"),
          issue_table_with_severity(warnings, "warning"),
          issue_table_with_severity(suggestions, "suggestion"),
          issue_table_with_severity(info, "info")
        )
      ),
      class = "project_check"
    )
    if (isTRUE(strict)) {
      rlang::abort(conditionMessage(root))
    }
    return(result)
  }

  if (isTRUE(repair)) {
    fs::dir_create(project_metadata_dir(root, create = TRUE, prefer_existing = TRUE), recurse = TRUE)
    fs::dir_create(fs::path(root, "outputs"), recurse = TRUE)
    ensure_gitignore_entries(root)
    ensure_local_config_file(root)
    if (!fs::file_exists(registry_path(root))) {
      ensure_registry_file(root, overwrite = TRUE)
    }
  }

  info <- append_issue(
    info,
    "project_root_detected",
    paste0("Project root detected at ", root),
    "",
    ""
  )
  info <- append_issue(
    info,
    "metadata_dir",
    paste0("Using project metadata directory `", project_metadata_relative_dir(root), "`."),
    project_metadata_relative_dir(root),
    ""
  )

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
  registry_check <- validate_project_registry(root, render_reports = render_reports)

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

  required_files <- c("project.yml", project_registry_relative_path(root))
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

  required_dirs <- c(
    "analysis",
    "reports",
    "outputs",
    fs::path("outputs", project_output_subdirs()),
    project_metadata_relative_dir(root)
  )
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
      paste0("`", project_local_config_relative_path(root), "` is not ignored by Git."),
      project_local_config_relative_path(root),
      paste0("Add `", project_local_config_relative_path(root), "` to `.gitignore`.")
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
          project_local_config_relative_path(root),
          "Update the local data root or create the directory."
        )
      } else if (!isTRUE(data_sources$readable[[index]])) {
        warnings <- append_issue(
          warnings,
          "unreadable_external_data_root",
          paste0("Configured external data root is not readable: ", data_sources$path[[index]]),
          project_local_config_relative_path(root),
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
      project_local_config_relative_path(root),
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

  script_components <- c(
    "data_preparation",
    "quality_control",
    "exploratory_analysis",
    "statistical_analysis",
    "model_diagnostics"
  )
  registered_scripts <- registry$scripts %||% list()
  registered_script_types <- vapply(
    registered_scripts,
    function(entry) entry$type %||% NA_character_,
    character(1)
  )

  for (component_name in intersect(script_components, components_selected)) {
    script_names <- names(registered_scripts)[registered_script_types == component_name]
    script_paths <- vapply(
      registered_scripts[script_names],
      function(entry) entry$path %||% NA_character_,
      character(1)
    )
    script_paths <- script_paths[!is.na(script_paths)]
    existing_paths <- script_paths[fs::file_exists(fs::path(root, script_paths))]

    if (length(existing_paths) == 0L) {
      errors <- append_issue(
        errors,
        paste0("component_", component_name),
        paste0("Component `", component_name, "` requires at least one registered script file."),
        paste(script_paths, collapse = ", "),
        "Create the missing script, add the component again, or remove the component from the plan."
      )
    }
  }

  report_required_files <- list(
    report = "reports/main_report.qmd",
    manuscript = "manuscript/manuscript.qmd",
    shiny_app = "app/app.R"
  )

  for (component_name in intersect(names(report_required_files), components_selected)) {
    required_file <- report_required_files[[component_name]]
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

  if ("data_preparation" %in% components_selected && nrow(data_sources) == 0L) {
    warnings <- append_issue(
      warnings,
      "external_data_configured",
      "The project includes `data_preparation`, but no external data root is configured.",
      project_local_config_relative_path(root),
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
      length(dashboard_report_entries(registry)) == 0L &&
      !fs::file_exists(fs::path(root, "app", "app.R"))) {
    warnings <- append_issue(
      warnings,
      "dashboard_deliverable",
      "The project includes the `dashboard` deliverable, but no dashboard report or Shiny app was found.",
      "",
      "Create a registered dashboard source under `dashboard/` or add `app/app.R`."
    )
  }

  if ("project_management" %in% components_selected) {
    governance_files <- c(
      "docs/project_plan.md",
      "docs/assumptions.md",
      "docs/decisions.md",
      "docs/risks.md",
      project_tasks_relative_path(root)
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
        project_tasks_relative_path(root),
        paste0("Repair `", project_tasks_relative_path(root), "`.")
      )
    } else {
      valid_statuses <- c("backlog", "todo", "in_progress", "blocked", "done", "cancelled")
      valid_priorities <- c("low", "medium", "high", "critical")

      for (task in tasks_data$tasks) {
        task_name <- task$title %||% task$id %||% "task"

        if (!task$status %in% valid_statuses) {
          warnings <- append_issue(
            warnings,
            "task_status",
            paste0("Task `", task_name, "` has an invalid status."),
            project_tasks_relative_path(root),
            "Use one of: backlog, todo, in_progress, blocked, done, cancelled."
          )
        }

        if (!is.null(task$priority) && !task$priority %in% valid_priorities) {
          warnings <- append_issue(
            warnings,
            "task_priority",
            paste0("Task `", task_name, "` has an invalid priority."),
            project_tasks_relative_path(root),
            "Use one of: low, medium, high, critical."
          )
        }

        if (!is.null(task$due_date) && nzchar(as.character(task$due_date))) {
          due_ok <- !inherits(tryCatch(as.Date(task$due_date), error = function(error) error), "error")
          if (!isTRUE(due_ok)) {
            warnings <- append_issue(
              warnings,
              "task_due_date",
              paste0("Task `", task_name, "` has an invalid due date."),
              project_tasks_relative_path(root),
              "Store due dates as valid ISO dates."
            )
          }
        }

        if (identical(task$status, "blocked")) {
          suggestions <- append_issue(
            suggestions,
            "blocked_task",
            paste0("Task `", task_name, "` is currently blocked."),
            project_tasks_relative_path(root),
            "Update the blocker in the task notes or status report."
          )
        }

        if (!is.null(task$due_date) && nzchar(as.character(task$due_date)) &&
            !inherits(tryCatch(as.Date(task$due_date), error = function(error) error), "error") &&
            as.Date(task$due_date) < Sys.Date() &&
            !task$status %in% c("done", "cancelled")) {
          suggestions <- append_issue(
            suggestions,
            "overdue_task",
            paste0("Task `", task_name, "` is overdue."),
            project_tasks_relative_path(root),
            "Review the due date or task status."
          )
        }
      }

      for (risk in tasks_data$risks) {
        risk_name <- risk$title %||% risk$id %||% "risk"
        if (identical(risk$status %||% "open", "open") &&
            identical(risk$impact %||% NA_character_, "critical")) {
          suggestions <- append_issue(
            suggestions,
            "open_critical_risk",
            paste0("Risk `", risk_name, "` is open and marked critical."),
            project_tasks_relative_path(root),
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
    } else {
      if (isTRUE(deep)) {
        source_warning <- tryCatch(
          validate_report_source(path),
          error = function(error) conditionMessage(error)
        )
        if (!is.null(source_warning)) {
          warnings <- append_issue(
            warnings,
            "report_source",
            source_warning,
            entry$path,
            "Keep a single valid YAML front matter block at the top of the report."
          )
        }
      }

      if (!isTRUE(render_reports)) {
        next
      }

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
      paste0("Keep machine-specific paths in `", project_local_config_relative_path(root), "`.")
    )
  }

  registry_strings <- collect_scalar_strings(registry)
  registry_paths <- registry_strings[grepl("(^[A-Za-z]:[\\\\/]|^/|^\\\\\\\\)", registry_strings)]
  if (length(registry_paths) > 0L) {
    errors <- append_issue(
      errors,
      "absolute_paths_in_registry",
      "Absolute or machine-specific paths were found in the project registry.",
      project_registry_relative_path(root),
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

  dag_check <- validate_project_dag(root = root, strict = FALSE)
  errors <- rbind(errors, dag_check$errors)
  warnings <- rbind(warnings, dag_check$warnings)
  info <- rbind(info, dag_check$info)

  registered_files <- tryCatch(
    project_registered_files(root = root),
    error = function(error) NULL
  )

  result <- structure(
    list(
      ok = nrow(errors) == 0L,
      root = normalizePath(root, winslash = "/", mustWork = FALSE),
      errors = errors,
      warnings = warnings,
      suggestions = suggestions,
      info = info,
      registered_files = registered_files,
      issues = rbind(
        issue_table_with_severity(errors, "error"),
        issue_table_with_severity(warnings, "warning"),
        issue_table_with_severity(suggestions, "suggestion"),
        issue_table_with_severity(info, "info")
      )
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
#' @param root Existing project root to validate.
#' @param deep Logical scalar. If \code{TRUE}, run deeper static checks such as
#'   validating report source structure in addition to the default checks.
#' @param render_reports Logical scalar. If \code{TRUE}, render registered reports as
#'   part of the health check.
#' @param strict Logical scalar. If \code{TRUE}, abort when critical errors are
#'   found; otherwise return a structured result object.
#' @param repair Logical scalar. If \code{TRUE}, apply safe repairs such as creating
#'   missing project metadata directories and default config files.
#'
#' @return Structured project-check result.
#' @examples
#' \dontrun{
#' check_project()
#' check_project(deep = TRUE, render_reports = FALSE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
check_project <- function(root = ".", deep = FALSE, render_reports = FALSE, strict = FALSE, repair = FALSE) {
  check_project_impl(
    root = root,
    deep = deep,
    render_reports = render_reports,
    strict = strict,
    repair = repair
  )
}

#' Project status alias
#'
#' @param root Existing project root to inspect.
#'
#' @return Structured project-check result.
#' @examples
#' \dontrun{
#' project_status()
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_status <- function(root = ".") {
  structure(
    check_project(root = root, deep = FALSE, render_reports = FALSE, strict = FALSE, repair = FALSE),
    class = c("project_status", "project_check")
  )
}

issue_section_title <- function(severity) {
  switch(
    severity,
    error = "Errors",
    warning = "Warnings",
    suggestion = "Suggestions",
    info = "Information",
    tools::toTitleCase(severity)
  )
}

severity_order_value <- function(severity) {
  order <- c(error = 1L, warning = 2L, suggestion = 3L, info = 4L)
  unname(order[severity] %||% 99L)
}

print_issue_sections <- function(issues, include_info = TRUE) {
  if (is.null(issues) || nrow(issues) == 0L) {
    cat("\nNo check items.\n")
    return(invisible(NULL))
  }

  if (!isTRUE(include_info)) {
    issues <- issues[issues$severity != "info", , drop = FALSE]
  }

  if (nrow(issues) == 0L) {
    return(invisible(NULL))
  }

  severities <- unique(as.character(issues$severity[order(vapply(issues$severity, severity_order_value, integer(1))) ]))
  for (severity in severities) {
    rows <- issues[issues$severity == severity, , drop = FALSE]
    if (nrow(rows) == 0L) {
      next
    }
    cat("\n", issue_section_title(severity), " (", nrow(rows), ")\n", sep = "")
    for (i in seq_len(nrow(rows))) {
      check <- rows$check[[i]] %||% ""
      message <- rows$message[[i]] %||% ""
      path <- rows$path[[i]] %||% ""
      fix <- rows$fix[[i]] %||% ""
      cat("  - [", check, "] ", message, "\n", sep = "")
      if (nzchar(path)) {
        cat("      path: ", path, "\n", sep = "")
      }
      if (nzchar(fix)) {
        cat("      fix:  ", fix, "\n", sep = "")
      }
    }
  }
  invisible(NULL)
}

#' Print a project check summary
#'
#' @param x A \code{"project_check"} object, usually created by \code{check_project()}.
#' @param ... Additional arguments accepted for S3 compatibility but ignored by
#'   this method.
#'
#' @return \code{x}, invisibly.
#' @examples
#' \dontrun{
#' x <- check_project()
#' print(x)
#' }
#' @author Thiago de Paula Oliveira
#' @export
print.project_check <- function(x, ...) {
  cat("projflow project check\n")
  cat("----------------------\n")
  if (!is.null(x$root) && nzchar(x$root)) {
    cat("Root: ", x$root, "\n", sep = "")
  }
  cat("Status: ", if (isTRUE(x$ok)) "OK" else "Needs attention", "\n", sep = "")
  cat(
    "Issues: ",
    nrow(x$errors %||% empty_issue_table()), " error(s); ",
    nrow(x$warnings %||% empty_issue_table()), " warning(s); ",
    nrow(x$suggestions %||% empty_issue_table()), " suggestion(s); ",
    nrow(x$info %||% empty_issue_table()), " info item(s)\n",
    sep = ""
  )

  if (inherits(x$registered_files, "projflow_registered_files")) {
    cat("\nRegistered execution graph\n")
    cat("--------------------------\n")
    visible_files <- x$registered_files[x$registered_files$type %in% c("script", "output", "table", "figure", "report", "deliverable"), , drop = FALSE]
    if (nrow(visible_files) > 0L) {
      for (kind in unique(as.character(visible_files$type[order(vapply(visible_files$type, node_kind_order, integer(1))) ]))) {
        rows <- visible_files[visible_files$type == kind, , drop = FALSE]
        cat(node_kind_label(kind), ": ", nrow(rows), "
", sep = "")
      }
    } else {
      cat("No registered executable files found.\n")
    }
  }

  print_issue_sections(x$issues, include_info = FALSE)

  if (nrow(x$info %||% empty_issue_table()) > 0L) {
    cat("\nUse project_check_items() for the full issue table, including information items.\n")
  }

  invisible(x)
}

#' Print a project status summary
#'
#' @param x A \code{"project_status"} object, usually created by \code{project_status()}.
#' @param ... Additional arguments accepted for S3 compatibility but ignored by
#'   this method.
#'
#' @return \code{x}, invisibly.
#' @examples
#' \dontrun{
#' x <- project_status()
#' print(x)
#' }
#' @author Thiago de Paula Oliveira
#' @export
print.project_status <- function(x, ...) {
  print.project_check(x, ...)
}
