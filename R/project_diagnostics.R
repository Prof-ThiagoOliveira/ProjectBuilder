project_file_hashes <- function(paths) {
  if (length(paths) == 0L) {
    return(character())
  }
  files <- paths[file.exists(paths)]
  hashes <- rep(NA_character_, length(paths))
  if (length(files) > 0L) {
    idx <- match(files, paths)
    hashes[idx] <- unname(tools::md5sum(files))
  }
  hashes
}

escape_html_text <- function(text) {
  if (requireNamespace("htmltools", quietly = TRUE)) {
    return(as.character(htmltools::htmlEscape(as.character(text))))
  }

  text <- as.character(text)
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text
}

project_registered_files <- function(root = ".", registry = read_project_registry(root)) {
  root <- find_project_root(root)
  object_paths <- unique(c(
    vapply(registry$scripts %||% list(), `[[`, character(1), "path"),
    vapply(registry$reports %||% list(), `[[`, character(1), "path"),
    vapply(registry$outputs %||% list(), `[[`, character(1), "path"),
    "project.yml",
    project_registry_relative_path(root),
    project_local_config_relative_path(root),
    if (fs::file_exists(tasks_path(root))) project_tasks_relative_path(root) else character()
  ))
  normalize_relative_path(object_paths)
}

project_files_data <- function(root = ".", include_file_hashes = FALSE, registry = read_project_registry(root)) {
  root <- find_project_root(root)
  files <- project_registered_files(root, registry)
  full_paths <- fs::path(root, files)
  exists <- file.exists(full_paths)
  info <- file.info(full_paths)
  data <- data.frame(
    path = files,
    exists = exists,
    size = ifelse(exists, info$size, NA_real_),
    modified = ifelse(exists, as.character(info$mtime), NA_character_),
    stringsAsFactors = FALSE
  )
  if (isTRUE(include_file_hashes)) {
    data$hash <- project_file_hashes(full_paths)
  }
  data
}

orphan_project_files <- function(root = ".", registry = read_project_registry(root)) {
  root <- find_project_root(root)
  registered <- project_registered_files(root, registry)
  all_files <- list.files(root, recursive = TRUE, all.files = FALSE)
  all_files <- normalize_relative_path(all_files)
  ignored_prefixes <- c(".git/", ".Rproj.user/", paste0(default_project_metadata_dir(), "/backups/"))
  all_files <- all_files[!vapply(all_files, function(path) any(startsWith(path, ignored_prefixes)), logical(1))]
  setdiff(all_files, registered)
}

#' Construct project network data
#'
#' @param root Existing project root.
#'
#' @return A list with `nodes` and `edges` data frames.
#' @examples
#' \dontrun{
#' network <- project_network_data()
#' names(network)
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_network_data <- function(root = ".") {
  root <- find_project_root(root)
  registry <- read_project_registry(root)
  tasks <- project_tasks(root)
  risks <- project_risks(root)
  milestones <- project_milestones(root)
  decisions <- project_decisions(root)
  data_sources <- list_project_data_sources(root)

  nodes <- list()
  edges <- list()
  linked_values <- function(data, index) {
    if (!"linked_objects" %in% names(data) || length(data$linked_objects) < index) {
      return(character())
    }
    linked_raw <- data$linked_objects[[index]]
    if (length(linked_raw) == 0L || all(is.na(linked_raw))) {
      return(character())
    }
    linked <- strsplit(as.character(linked_raw), ";\\s*")[[1]]
    linked[nzchar(linked)]
  }

  add_node <- function(id, label, type, status = NA_character_, path = NA_character_) {
    nodes[[id]] <<- data.frame(
      id = id,
      label = label,
      type = type,
      status = status,
      path = path,
      stringsAsFactors = FALSE
    )
  }

  add_edge <- function(from, to, relationship) {
    edges[[length(edges) + 1L]] <<- data.frame(
      from = from,
      to = to,
      relationship = relationship,
      stringsAsFactors = FALSE
    )
  }

  for (component in registry$components %||% character()) {
    add_node(paste0("component:", component), component, "component")
  }
  for (deliverable in registry$deliverables %||% character()) {
    add_node(paste0("deliverable:", deliverable), deliverable, "deliverable")
  }
  if (nrow(data_sources) > 0L) {
    for (i in seq_len(nrow(data_sources))) {
      add_node(paste0("data_source:", data_sources$name[[i]]), data_sources$name[[i]], "data_source", if (isTRUE(data_sources$exists[[i]]) && isTRUE(data_sources$readable[[i]])) "available" else "unavailable", data_sources$path[[i]])
    }
  }

  for (name in names(registry$scripts %||% list())) {
    entry <- registry$scripts[[name]]
    add_node(paste0("script:", name), name, "script", entry$type %||% NA_character_, entry$path %||% NA_character_)
    for (component in registry$components %||% character()) {
      if (identical(entry$type %||% "", component)) {
        add_edge(paste0("component:", component), paste0("script:", name), "component_to_script")
      }
    }
    for (output_name in entry$outputs %||% character()) {
      add_edge(paste0("script:", name), paste0("output:", output_name), "script_to_output")
    }
  }

  for (name in names(registry$reports %||% list())) {
    entry <- registry$reports[[name]]
    add_node(paste0("report:", name), name, "report", entry$type %||% NA_character_, entry$path %||% NA_character_)
    for (component in registry$components %||% character()) {
      if (identical(entry$type %||% "", component)) {
        add_edge(paste0("component:", component), paste0("report:", name), "component_to_report")
      }
    }
    deliverable <- entry$deliverable %||% NA_character_
    if (!is.na(deliverable) && nzchar(deliverable)) {
      add_edge(paste0("report:", name), paste0("deliverable:", deliverable), "report_to_deliverable")
    }
    for (input_name in entry$inputs %||% character()) {
      add_edge(paste0("output:", input_name), paste0("report:", name), "output_to_report")
    }
  }

  for (name in names(registry$outputs %||% list())) {
    entry <- registry$outputs[[name]]
    add_node(paste0("output:", name), name, entry$type %||% "output", if (file.exists(fs::path(root, entry$path %||% ""))) "exists" else "missing", entry$path %||% NA_character_)
    if (!is.null(entry$generated_by)) {
      add_edge(paste0("script:", entry$generated_by), paste0("output:", name), "script_to_output")
    }
  }

  if (nrow(tasks) > 0L) {
    for (i in seq_len(nrow(tasks))) {
      task_id <- tasks$id[[i]]
      add_node(paste0("task:", task_id), tasks$title[[i]], "task", tasks$status[[i]])
      linked <- linked_values(tasks, i)
      for (object_name in linked) {
        if (object_name %in% names(registry$scripts %||% list())) add_edge(paste0("task:", task_id), paste0("script:", object_name), "task_to_script")
        if (object_name %in% names(registry$reports %||% list())) add_edge(paste0("task:", task_id), paste0("report:", object_name), "task_to_report")
        if (object_name %in% names(registry$outputs %||% list())) add_edge(paste0("task:", task_id), paste0("output:", object_name), "task_to_output")
      }
    }
  }

  if (nrow(risks) > 0L) {
    for (i in seq_len(nrow(risks))) {
      risk_id <- risks$id[[i]]
      add_node(paste0("risk:", risk_id), risks$title[[i]], "risk", risks$status[[i]])
      linked <- linked_values(risks, i)
      for (object_name in linked) {
        if (object_name %in% registry$components %||% character()) add_edge(paste0("risk:", risk_id), paste0("component:", object_name), "risk_to_component")
      }
    }
  }

  if (nrow(milestones) > 0L) {
    for (i in seq_len(nrow(milestones))) {
      milestone_id <- milestones$id[[i]]
      add_node(paste0("milestone:", milestone_id), milestones$title[[i]], "milestone", milestones$status[[i]])
      linked <- linked_values(milestones, i)
      for (object_name in linked) {
        if (object_name %in% registry$deliverables %||% character()) add_edge(paste0("milestone:", milestone_id), paste0("deliverable:", object_name), "milestone_to_deliverable")
        if (object_name %in% names(registry$reports %||% list())) add_edge(paste0("milestone:", milestone_id), paste0("report:", object_name), "milestone_to_report")
      }
    }
  }

  if (nrow(decisions) > 0L) {
    for (i in seq_len(nrow(decisions))) {
      decision_id <- decisions$id[[i]]
      add_node(paste0("decision:", decision_id), decisions$title[[i]], "decision", decisions$status[[i]])
      linked <- linked_values(decisions, i)
      for (object_name in linked) {
        if (object_name %in% registry$components %||% character()) add_edge(paste0("decision:", decision_id), paste0("component:", object_name), "decision_to_component")
      }
    }
  }

  nodes_df <- if (length(nodes) == 0L) {
    data.frame(id = character(), label = character(), type = character(), status = character(), path = character(), stringsAsFactors = FALSE)
  } else {
    unique(do.call(rbind, unname(nodes)))
  }
  edges_df <- if (length(edges) == 0L) {
    data.frame(from = character(), to = character(), relationship = character(), stringsAsFactors = FALSE)
  } else {
    unique(do.call(rbind, edges))
  }

  list(nodes = nodes_df, edges = edges_df)
}

check_dashboard_dependencies <- function(
    packages = c("shiny", "bslib", "DT", "htmltools"),
    require_network = FALSE) {
  validate_character_vector(packages, "packages")
  validate_logical_scalar(require_network, "require_network")
  needed <- unique(c(packages, if (isTRUE(require_network)) "visNetwork" else character()))
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    rlang::abort(
      paste0(
        "The projflow Project Manager requires optional packages that are not installed:\n- ",
        paste(missing, collapse = "\n- "),
        "\n\nInstall them with:\ninstall.packages(c(",
        paste(sprintf('\"%s\"', missing), collapse = ", "),
        "))"
      )
    )
  }
  invisible(TRUE)
}

diagnostics_summary_frame <- function(diagnostics) {
  data.frame(
    metric = c(
      "errors",
      "warnings",
      "suggestions",
      "open_tasks",
      "overdue_tasks",
      "open_risks",
      "missing_outputs",
      "stale_outputs",
      "missing_packages",
      "unavailable_data_sources"
    ),
    value = c(
      nrow(diagnostics$checks[diagnostics$checks$severity == "error", , drop = FALSE]),
      nrow(diagnostics$checks[diagnostics$checks$severity == "warning", , drop = FALSE]),
      nrow(diagnostics$checks[diagnostics$checks$severity == "suggestion", , drop = FALSE]),
      diagnostics$summary$open_tasks[[1]],
      diagnostics$summary$overdue_tasks[[1]],
      diagnostics$summary$open_risks[[1]],
      diagnostics$summary$missing_outputs[[1]],
      diagnostics$summary$stale_outputs[[1]],
      diagnostics$summary$missing_packages[[1]],
      diagnostics$summary$data_sources_unavailable[[1]]
    ),
    stringsAsFactors = FALSE
  )
}

#' Collect structured project diagnostics data
#'
#' @param root Existing project root to inspect.
#' @param include_file_hashes Logical scalar. If `TRUE`, include MD5 hashes for
#'   registered files that exist locally.
#' @param include_git Logical scalar. If `TRUE`, include Git status data.
#' @param include_packages Logical scalar. If `TRUE`, include package status
#'   data.
#' @param include_network Logical scalar. If `TRUE`, include project network
#'   data.
#'
#' @return An object of class `"projflow_diagnostics"`.
#' @examples
#' \dontrun{
#' diagnostics <- project_diagnostics_data()
#' names(diagnostics)
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_diagnostics_data <- function(
    root = ".",
    include_file_hashes = FALSE,
    include_git = TRUE,
    include_packages = TRUE,
    include_network = TRUE) {
  validate_logical_scalar(include_file_hashes, "include_file_hashes")
  validate_logical_scalar(include_git, "include_git")
  validate_logical_scalar(include_packages, "include_packages")
  validate_logical_scalar(include_network, "include_network")

  root <- find_project_root(root)
  registry <- read_project_registry(root)
  tasks <- project_tasks(root)
  risks <- project_risks(root)
  milestones <- project_milestones(root)
  decisions <- project_decisions(root)
  outputs <- list_project_outputs(root)
  objects <- list_project_objects(root)
  reports <- if (length(registry$reports) == 0L) {
    data.frame(name = character(), path = character(), type = character(), output_path = character(), source_exists = logical(), output_exists = logical(), stringsAsFactors = FALSE)
  } else {
    data.frame(
      name = names(registry$reports),
      path = vapply(registry$reports, `[[`, character(1), "path"),
      type = vapply(registry$reports, `[[`, character(1), "type"),
      output_path = normalize_relative_path(vapply(names(registry$reports), default_output_path, character(1), type = "report")),
      source_exists = vapply(registry$reports, function(entry) file.exists(fs::path(root, entry$path)), logical(1)),
      output_exists = vapply(names(registry$reports), function(name) file.exists(fs::path(root, default_output_path(name, "report"))), logical(1)),
      stringsAsFactors = FALSE
    )
  }
  scripts <- if (length(registry$scripts) == 0L) {
    data.frame(name = character(), path = character(), type = character(), order = numeric(), exists = logical(), stringsAsFactors = FALSE)
  } else {
    data.frame(
      name = names(registry$scripts),
      path = vapply(registry$scripts, `[[`, character(1), "path"),
      type = vapply(registry$scripts, `[[`, character(1), "type"),
      order = vapply(registry$scripts, `[[`, numeric(1), "order"),
      exists = vapply(registry$scripts, function(entry) file.exists(fs::path(root, entry$path)), logical(1)),
      stringsAsFactors = FALSE
    )
  }
  data_sources <- list_project_data_sources(root)
  package_data <- if (isTRUE(include_packages)) check_project_packages(root) else list(packages = character(), installed = character(), missing = character())
  package_df <- if (!isTRUE(include_packages) || length(package_data$packages) == 0L) {
    data.frame(package = character(), installed = logical(), stringsAsFactors = FALSE)
  } else {
    data.frame(
      package = package_data$packages,
      installed = package_data$packages %in% package_data$installed,
      stringsAsFactors = FALSE
    )
  }
  check_result <- check_project(root = root, deep = FALSE, render_reports = FALSE, strict = FALSE, repair = FALSE)
  files <- project_files_data(root, include_file_hashes = include_file_hashes, registry = registry)
  network <- if (isTRUE(include_network)) project_network_data(root) else list(nodes = data.frame(), edges = data.frame())
  orphan_files <- orphan_project_files(root, registry)
  activity <- list_project_activity(root)
  status_summary <- project_status_report(root, output = "data")

  diagnostics <- list(
    project = list(
      name = registry$project$name %||% safe_basename(root),
      root = root,
      metadata_dir = project_metadata_relative_dir(root),
      components = registry$components %||% character(),
      deliverables = registry$deliverables %||% character(),
      infrastructure = registry$infrastructure %||% character()
    ),
    summary = data.frame(
      overall_status = if (nrow(check_result$errors) > 0L) "Broken" else if (nrow(check_result$warnings) > 0L) "Needs attention" else "Healthy",
      open_tasks = status_summary$counts$open_tasks,
      overdue_tasks = status_summary$counts$overdue_tasks,
      open_risks = status_summary$counts$open_risks,
      missing_outputs = length(missing_project_outputs(root)),
      stale_outputs = length(stale_project_outputs(root)),
      reports_needing_render = sum(reports$source_exists & !reports$output_exists),
      missing_packages = sum(!package_df$installed),
      data_sources_unavailable = sum(!data_sources$exists | !data_sources$readable),
      orphan_files = length(orphan_files),
      stringsAsFactors = FALSE
    ),
    checks = check_result$issues,
    registry = registry,
    objects = objects,
    scripts = scripts,
    reports = reports,
    outputs = outputs,
    data_sources = data_sources,
    packages = package_df,
    governance = list(
      tasks = tasks,
      risks = risks,
      milestones = milestones,
      decisions = decisions
    ),
    files = files,
    network = network,
    activity = activity,
    orphan_files = data.frame(path = orphan_files, stringsAsFactors = FALSE)
  )

  class(diagnostics) <- "projflow_diagnostics"
  diagnostics
}

render_diagnostics_console <- function(diagnostics) {
  summary <- diagnostics$summary
  lines <- c(
    "projflow diagnostics",
    "",
    paste0("Project: ", diagnostics$project$name),
    paste0("Root: ", diagnostics$project$root),
    paste0("Metadata directory: ", diagnostics$project$metadata_dir),
    "",
    paste0("Overall status: ", summary$overall_status[[1]]),
    paste0("Errors: ", nrow(diagnostics$checks[diagnostics$checks$severity == "error", , drop = FALSE])),
    paste0("Warnings: ", nrow(diagnostics$checks[diagnostics$checks$severity == "warning", , drop = FALSE])),
    paste0("Suggestions: ", nrow(diagnostics$checks[diagnostics$checks$severity == "suggestion", , drop = FALSE])),
    paste0("Open tasks: ", summary$open_tasks[[1]]),
    paste0("Overdue tasks: ", summary$overdue_tasks[[1]]),
    paste0("Open risks: ", summary$open_risks[[1]]),
    paste0("Missing outputs: ", summary$missing_outputs[[1]]),
    paste0("Stale outputs: ", summary$stale_outputs[[1]])
  )
  cat(paste(lines, collapse = "\n"), "\n")
  invisible(diagnostics)
}

render_diagnostics_html <- function(diagnostics, file = NULL) {
  root <- diagnostics$project$root
  output_file <- if (is.null(file)) {
    fs::path(root, "outputs", "reports", "project_diagnostics.html")
  } else {
    validate_character_vector(file, "file")
    if (is_absolute_path(file[[1]])) file[[1]] else fs::path(root, file[[1]])
  }

  fs::dir_create(fs::path_dir(output_file), recurse = TRUE)

  render_table_html <- function(df) {
    if (nrow(df) == 0L) {
      return("<p>No records.</p>")
    }
    header <- paste(sprintf("<th>%s</th>", escape_html_text(names(df))), collapse = "")
    rows <- apply(df, 1, function(row) {
      paste0("<tr>", paste(sprintf("<td>%s</td>", escape_html_text(as.character(row))), collapse = ""), "</tr>")
    })
    paste0("<table><thead><tr>", header, "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>")
  }

  html <- paste0(
    "<html><head><title>projflow diagnostics</title><style>",
    "body{font-family:Segoe UI,Arial,sans-serif;margin:2rem;line-height:1.4;}h1,h2{margin-top:1.5rem;}table{border-collapse:collapse;width:100%;margin:1rem 0;}th,td{border:1px solid #d0d0d0;padding:0.4rem;text-align:left;}th{background:#f5f5f5;}pre{background:#f7f7f7;padding:1rem;}</style></head><body>",
    "<h1>projflow diagnostics</h1>",
    "<p><strong>Project:</strong> ", escape_html_text(diagnostics$project$name), "<br/>",
    "<strong>Root:</strong> ", escape_html_text(diagnostics$project$root), "</p>",
    "<h2>Health summary</h2>",
    render_table_html(diagnostics$summary),
    "<h2>Checks</h2>",
    render_table_html(diagnostics$checks),
    "<h2>Registry objects</h2>",
    render_table_html(diagnostics$objects),
    "<h2>Outputs</h2>",
    render_table_html(diagnostics$outputs),
    "<h2>Reports</h2>",
    render_table_html(diagnostics$reports),
    "<h2>Data sources</h2>",
    render_table_html(diagnostics$data_sources),
    "<h2>Packages</h2>",
    render_table_html(diagnostics$packages),
    "<h2>Governance summary</h2>",
    render_table_html(diagnostics$governance$tasks),
    "<h2>Network summary</h2>",
    render_table_html(diagnostics$network$edges),
    "<h2>Suggested fixes</h2>",
    render_table_html(diagnostics$checks[diagnostics$checks$severity %in% c("error", "warning", "suggestion"), c("severity", "check", "message", "fix"), drop = FALSE]),
    "<h2>Activity log</h2>",
    render_table_html(diagnostics$activity),
    "</body></html>"
  )

  writeLines(enc2utf8(html), output_file, useBytes = TRUE)
  invisible(normalize_absolute_path(output_file))
}

#' Diagnose a project
#'
#' @param root Existing project root to inspect.
#' @param output Diagnostic output mode.
#' @param file Optional output file used when `output = "html"`.
#' @param ... Reserved for future extensions.
#'
#' @return Diagnostics in the requested format.
#' @examples
#' \dontrun{
#' diagnose_project(output = "data")
#' diagnose_project(output = "console")
#' diagnose_project(output = "html")
#' }
#' @author Thiago de Paula Oliveira
#' @export
diagnose_project <- function(
    root = ".",
    output = c("data", "console", "html", "app"),
    file = NULL,
    ...) {
  output <- match.arg(output)
  diagnostics <- project_diagnostics_data(root = root, ...)

  if (identical(output, "data")) {
    return(diagnostics)
  }
  if (identical(output, "console")) {
    return(render_diagnostics_console(diagnostics))
  }
  if (identical(output, "html")) {
    return(render_diagnostics_html(diagnostics, file = file))
  }

  launch_project_manager(root = root, mode = "diagnose", ...)
}
