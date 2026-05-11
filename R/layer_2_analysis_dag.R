# Layer 2: analysis Directed Acyclic Graph -----------------------------------
#
# This layer is the execution backbone. It is intentionally narrower than the
# management/network graph: only data inputs, scripts, outputs, reports and
# deliverables belong here.

dag_empty_nodes <- function() {
  data.frame(
    id = character(),
    label = character(),
    type = character(),
    path = character(),
    status = character(),
    object = character(),
    order = numeric(),
    stringsAsFactors = FALSE
  )
}

dag_empty_edges <- function() {
  data.frame(
    from = character(),
    to = character(),
    relationship = character(),
    stringsAsFactors = FALSE
  )
}

dag_path_status <- function(root, path) {
  if (is.null(path) || length(path) == 0L || is.na(path[[1]]) || !nzchar(path[[1]])) {
    return(NA_character_)
  }
  full_path <- if (is_absolute_path(path[[1]])) path[[1]] else fs::path(root, path[[1]])
  if (fs::file_exists(full_path) || fs::dir_exists(full_path)) "exists" else "missing"
}

registry_script_names_by_order <- function(registry) {
  script_names <- names(registry$scripts %||% list())
  if (length(script_names) == 0L) {
    return(character())
  }

  orders <- vapply(
    registry$scripts[script_names],
    function(entry) {
      value <- suppressWarnings(as.numeric(entry$order %||% NA_real_))
      if (is.na(value)) Inf else value
    },
    numeric(1)
  )

  script_names[order(orders, script_names, na.last = TRUE)]
}

dag_make_node <- function(id, label, type, path = NA_character_, status = NA_character_, object = NA_character_, order = NA_real_) {
  data.frame(
    id = as.character(id),
    label = as.character(label),
    type = as.character(type),
    path = as.character(path %||% NA_character_),
    status = as.character(status %||% NA_character_),
    object = as.character(object %||% NA_character_),
    order = as.numeric(order %||% NA_real_),
    stringsAsFactors = FALSE
  )
}

dag_make_edge <- function(from, to, relationship) {
  data.frame(
    from = as.character(from),
    to = as.character(to),
    relationship = as.character(relationship),
    stringsAsFactors = FALSE
  )
}

#' Build the executable analysis DAG
#'
#' @description
#' \code{project_analysis_dag()} builds the strict execution graph for an existing
#' projflow project. Unlike \code{project_network_data()}, which also includes tasks,
#' risks, decisions and milestones, this function only includes nodes that can
#' affect analysis execution or deliverable production.
#'
#' @details
#' The graph direction follows analytical causality: external data and declared
#' inputs point to scripts; scripts point to outputs; outputs point to reports;
#' reports point to deliverables. The returned graph is designed for validation,
#' topological ordering and \code{targets} generation.
#'
#' @param root Path inside an existing projflow project.
#' @param include_deliverables Logical scalar. If \code{TRUE}, include registered
#'   deliverables as terminal DAG nodes.
#'
#' @return A list with \code{nodes} and \code{edges} data frames and class
#'   \code{"project_analysis_dag"}.
#' @examples
#' \dontrun{
#' dag <- project_analysis_dag()
#' validate_project_dag(dag = dag)
#' }
#' @author Thiago de Paula Oliveira
#' @export
project_analysis_dag <- function(root = ".", include_deliverables = TRUE) {
  validate_logical_scalar(include_deliverables, "include_deliverables")
  root <- find_project_root(root)
  registry <- read_project_registry(root)
  data_sources <- list_project_data_sources(root)

  nodes <- list()
  edges <- list()

  add_node <- function(id, label, type, path = NA_character_, status = NA_character_, object = NA_character_, order = NA_real_) {
    if (is.null(nodes[[id]])) {
      nodes[[id]] <<- dag_make_node(id, label, type, path, status, object, order)
    } else {
      current <- nodes[[id]]
      if ((is.na(current$path[[1]]) || !nzchar(current$path[[1]])) && !is.na(path[[1]]) && nzchar(path[[1]])) {
        current$path <- as.character(path[[1]])
      }
      if ((is.na(current$status[[1]]) || !nzchar(current$status[[1]])) && !is.na(status[[1]]) && nzchar(status[[1]])) {
        current$status <- as.character(status[[1]])
      }
      nodes[[id]] <<- current
    }
    invisible(id)
  }

  add_edge <- function(from, to, relationship) {
    edges[[length(edges) + 1L]] <<- dag_make_edge(from, to, relationship)
    invisible(NULL)
  }

  if (nrow(data_sources) > 0L) {
    for (i in seq_len(nrow(data_sources))) {
      status <- if (isTRUE(data_sources$exists[[i]]) && isTRUE(data_sources$readable[[i]])) "available" else "unavailable"
      add_node(
        paste0("data_source:", data_sources$name[[i]]),
        data_sources$name[[i]],
        "data_source",
        path = data_sources$path[[i]],
        status = status,
        object = data_sources$name[[i]]
      )
    }
  }

  output_names <- names(registry$outputs %||% list())
  for (name in output_names) {
    entry <- registry$outputs[[name]]
    add_node(
      paste0("output:", name),
      name,
      entry$type %||% "output",
      path = entry$path %||% NA_character_,
      status = dag_path_status(root, entry$path %||% NA_character_),
      object = name
    )
  }

  script_names <- registry_script_names_by_order(registry)
  for (name in script_names) {
    entry <- registry$scripts[[name]]
    add_node(
      paste0("script:", name),
      name,
      "script",
      path = entry$path %||% NA_character_,
      status = dag_path_status(root, entry$path %||% NA_character_),
      object = name,
      order = as.numeric(entry$order %||% NA_real_)
    )

    for (dependency in entry$depends_on %||% character()) {
      dependency <- validate_project_object_name(dependency, repair = TRUE)
      add_node(paste0("script:", dependency), dependency, "script", object = dependency)
      add_edge(paste0("script:", dependency), paste0("script:", name), "script_to_script")
    }

    script_inputs <- entry$inputs %||% character()
    for (input_name in script_inputs) {
      input_name <- as.character(input_name)
      if (input_name %in% output_names) {
        add_edge(paste0("output:", input_name), paste0("script:", name), "output_to_script")
      } else if (nrow(data_sources) > 0L && input_name %in% data_sources$name) {
        add_edge(paste0("data_source:", input_name), paste0("script:", name), "data_source_to_script")
      } else {
        add_node(paste0("input:", input_name), input_name, "input", object = input_name)
        add_edge(paste0("input:", input_name), paste0("script:", name), "input_to_script")
      }
    }

    if (length(script_inputs) == 0L && entry$type %in% c("import", "data_preparation", "data_cleaning", "quality_control") && nrow(data_sources) > 0L) {
      add_edge(paste0("data_source:", data_sources$name[[1]]), paste0("script:", name), "data_source_to_script")
    }

    for (output_name in entry$outputs %||% character()) {
      if (is.null(nodes[[paste0("output:", output_name)]])) {
        add_node(paste0("output:", output_name), output_name, "output", object = output_name)
      }
      add_edge(paste0("script:", name), paste0("output:", output_name), "script_to_output")
    }
  }

  for (name in output_names) {
    entry <- registry$outputs[[name]]
    if (!is.null(entry$generated_by) && nzchar(entry$generated_by)) {
      add_node(paste0("script:", entry$generated_by), entry$generated_by, "script", object = entry$generated_by)
      add_edge(paste0("script:", entry$generated_by), paste0("output:", name), "script_to_output")
    }
  }

  for (name in names(registry$reports %||% list())) {
    entry <- registry$reports[[name]]
    add_node(
      paste0("report:", name),
      name,
      "report",
      path = entry$path %||% NA_character_,
      status = dag_path_status(root, entry$path %||% NA_character_),
      object = name
    )

    for (input_name in entry$inputs %||% character()) {
      input_name <- as.character(input_name)
      if (input_name %in% output_names || !is.null(nodes[[paste0("output:", input_name)]])) {
        add_node(paste0("output:", input_name), input_name, "output", object = input_name)
        add_edge(paste0("output:", input_name), paste0("report:", name), "output_to_report")
      } else {
        add_node(paste0("input:", input_name), input_name, "input", object = input_name)
        add_edge(paste0("input:", input_name), paste0("report:", name), "input_to_report")
      }
    }

    deliverable <- entry$deliverable %||% NA_character_
    if (isTRUE(include_deliverables) && !is.na(deliverable) && nzchar(deliverable)) {
      add_node(paste0("deliverable:", deliverable), deliverable, "deliverable", object = deliverable)
      add_edge(paste0("report:", name), paste0("deliverable:", deliverable), "report_to_deliverable")
    }
  }

  if (isTRUE(include_deliverables)) {
    for (deliverable in registry$deliverables %||% character()) {
      add_node(paste0("deliverable:", deliverable), deliverable, "deliverable", object = deliverable)
    }
  }

  nodes_df <- if (length(nodes) == 0L) dag_empty_nodes() else unique(do.call(rbind, unname(nodes)))
  edges_df <- if (length(edges) == 0L) dag_empty_edges() else unique(do.call(rbind, edges))

  structure(
    list(root = root, nodes = nodes_df, edges = edges_df),
    class = "project_analysis_dag"
  )
}

dag_topological_sort <- function(nodes, edges) {
  node_ids <- unique(as.character(nodes$id %||% character()))
  edge_ids <- unique(c(as.character(edges$from %||% character()), as.character(edges$to %||% character())))
  ids <- unique(c(node_ids, edge_ids))

  if (length(ids) == 0L) {
    return(list(order = character(), cycle_nodes = character()))
  }

  incoming <- stats::setNames(rep(0L, length(ids)), ids)
  outgoing <- stats::setNames(vector("list", length(ids)), ids)

  if (nrow(edges) > 0L) {
    for (i in seq_len(nrow(edges))) {
      from <- as.character(edges$from[[i]])
      to <- as.character(edges$to[[i]])
      if (!from %in% ids || !to %in% ids) {
        next
      }
      outgoing[[from]] <- unique(c(outgoing[[from]], to))
      incoming[[to]] <- incoming[[to]] + 1L
    }
  }

  rank <- seq_along(ids)
  names(rank) <- ids
  if ("order" %in% names(nodes)) {
    node_order <- nodes$order
    node_order[is.na(node_order)] <- Inf
    rank[nodes$id] <- order(order(node_order, seq_along(node_order), na.last = TRUE))
  }

  sort_queue <- function(x) {
    x[order(rank[x], x, na.last = TRUE)]
  }

  queue <- sort_queue(names(incoming)[incoming == 0L])
  result <- character()

  while (length(queue) > 0L) {
    current <- queue[[1]]
    queue <- queue[-1]
    result <- c(result, current)

    for (to in outgoing[[current]] %||% character()) {
      incoming[[to]] <- incoming[[to]] - 1L
      if (incoming[[to]] == 0L) {
        queue <- sort_queue(unique(c(queue, to)))
      }
    }
  }

  list(
    order = result,
    cycle_nodes = setdiff(ids, result)
  )
}

#' Validate the executable analysis DAG
#'
#' @description
#' \code{validate_project_dag()} checks that the executable project graph is valid:
#' every edge endpoint must be present, outputs should not have multiple
#' producers, and the execution graph must be acyclic.
#'
#' @param root Path inside an existing projflow project. Ignored when \code{dag} is
#'   supplied.
#' @param dag Optional object returned by \code{project_analysis_dag()}.
#' @param strict Logical scalar. If \code{TRUE}, abort when DAG errors are found.
#'
#' @return A structured object with \code{ok}, \code{errors}, \code{warnings}, \code{info}, and
#'   \code{issues} fields.
#' @examples
#' \dontrun{
#' validate_project_dag()
#' }
#' @author Thiago de Paula Oliveira
#' @export
validate_project_dag <- function(root = ".", dag = NULL, strict = FALSE) {
  validate_logical_scalar(strict, "strict")
  if (is.null(dag)) {
    dag <- project_analysis_dag(root = root)
  }

  if (!inherits(dag, "project_analysis_dag")) {
    rlang::abort("`dag` must be an object created by `project_analysis_dag()`.")
  }

  errors <- empty_issue_table()
  warnings <- empty_issue_table()
  info <- empty_issue_table()

  nodes <- dag$nodes
  edges <- dag$edges

  duplicated_nodes <- unique(nodes$id[duplicated(nodes$id)])
  if (length(duplicated_nodes) > 0L) {
    errors <- append_issue(errors, "dag_duplicate_nodes", paste("Duplicate DAG nodes:", paste(duplicated_nodes, collapse = ", ")), "", "Ensure every registered object has one unique name.")
  }

  unknown_from <- setdiff(edges$from, nodes$id)
  unknown_to <- setdiff(edges$to, nodes$id)
  if (length(unknown_from) > 0L) {
    errors <- append_issue(errors, "dag_missing_source_nodes", paste("DAG edges reference missing source nodes:", paste(unknown_from, collapse = ", ")), "", "Register missing input, script or output nodes.")
  }
  if (length(unknown_to) > 0L) {
    errors <- append_issue(errors, "dag_missing_target_nodes", paste("DAG edges reference missing target nodes:", paste(unknown_to, collapse = ", ")), "", "Register missing script, output, report or deliverable nodes.")
  }

  self_edges <- edges$from == edges$to
  if (any(self_edges)) {
    errors <- append_issue(errors, "dag_self_dependency", paste("DAG contains self-dependencies:", paste(edges$from[self_edges], collapse = ", ")), "", "Remove dependencies from an object to itself.")
  }

  producer_edges <- edges[edges$relationship == "script_to_output", , drop = FALSE]
  if (nrow(producer_edges) > 0L) {
    producer_count <- table(producer_edges$to)
    multiple <- names(producer_count)[producer_count > 1L]
    if (length(multiple) > 0L) {
      warnings <- append_issue(warnings, "dag_multiple_output_producers", paste("Outputs have multiple registered producers:", paste(multiple, collapse = ", ")), "", "Prefer exactly one script as the producer of each reproducible output.")
    }
  }

  ordered <- dag_topological_sort(nodes, edges)
  if (length(ordered$cycle_nodes) > 0L) {
    errors <- append_issue(errors, "dag_cycle", paste("The analysis DAG contains a cycle involving:", paste(ordered$cycle_nodes, collapse = ", ")), "", "Remove backward dependencies so the workflow flows from inputs to outputs.")
  }

  missing_files <- nodes[nodes$type %in% c("script", "report") & nodes$status %in% "missing", , drop = FALSE]
  if (nrow(missing_files) > 0L) {
    warnings <- append_issue(warnings, "dag_missing_registered_files", paste("Registered DAG files are missing:", paste(missing_files$path, collapse = ", ")), "", "Restore the files or remove stale registry entries.")
  }

  info <- append_issue(info, "dag_summary", paste0("Analysis DAG contains ", nrow(nodes), " node(s) and ", nrow(edges), " edge(s)."), "", "")

  result <- structure(
    list(
      ok = nrow(errors) == 0L,
      errors = errors,
      warnings = warnings,
      info = info,
      issues = rbind(
        issue_table_with_severity(errors, "error"),
        issue_table_with_severity(warnings, "warning"),
        issue_table_with_severity(info, "info")
      )
    ),
    class = "project_dag_check"
  )

  if (isTRUE(strict) && !isTRUE(result$ok)) {
    rlang::abort(paste(result$errors$message, collapse = " | "))
  }

  result
}

#' Return the project topological execution order
#'
#' @description
#' \code{topological_project_order()} returns the DAG order for scripts, reports or
#' all executable nodes. Scripts with explicit dependencies or input/output links
#' are ordered by graph topology rather than by filename alone.
#'
#' @param root Path inside an existing projflow project.
#' @param type Character scalar. Return all DAG nodes, only scripts, or only
#'   reports.
#'
#' @return Character vector of ordered node IDs or object names.
#' @examples
#' \dontrun{
#' topological_project_order(type = "scripts")
#' }
#' @author Thiago de Paula Oliveira
#' @export
topological_project_order <- function(root = ".", type = c("all", "scripts", "reports")) {
  type <- match.arg(type)
  dag <- project_analysis_dag(root = root)
  validate_project_dag(dag = dag, strict = TRUE)
  ordered <- dag_topological_sort(dag$nodes, dag$edges)$order

  if (identical(type, "scripts")) {
    return(sub("^script:", "", ordered[grepl("^script:", ordered)]))
  }
  if (identical(type, "reports")) {
    return(sub("^report:", "", ordered[grepl("^report:", ordered)]))
  }

  ordered
}

#' Plot or return the executable analysis DAG
#'
#' @description
#' \code{plot_project_dag()} gives a lightweight visual entry point to the analysis
#' DAG. If \code{visNetwork} is installed, an interactive network is returned;
#' otherwise the function prints the ordered edge table and invisibly returns the
#' DAG object.
#'
#' @param root Path inside an existing projflow project.
#' @param ... Reserved for future plotting options.
#'
#' @return Invisibly returns the DAG object, or a \code{visNetwork} object when that
#' optional package is available.
#' @examples
#' \dontrun{
#' plot_project_dag()
#' }
#' @author Thiago de Paula Oliveira
#' @export
plot_project_dag <- function(root = ".", ...) {
  dag <- project_analysis_dag(root = root)

  if (requireNamespace("visNetwork", quietly = TRUE)) {
    nodes <- dag$nodes
    edges <- dag$edges
    names(nodes)[names(nodes) == "type"] <- "group"
    return(visNetwork::visNetwork(nodes, edges))
  }

  print(dag$edges)
  invisible(dag)
}

target_name <- function(prefix, name) {
  paste0(prefix, "_", gsub("[^A-Za-z0-9_]", "_", name))
}

target_command_for_script <- function(name, dependencies) {
  dependency_code <- if (length(dependencies) == 0L) "" else paste0(paste(dependencies, collapse = ";\n  "), ";\n  ")
  paste0("{\n  ", dependency_code, "projflow::run_project_object(\"", name, "\", root = \".\")\n}")
}

script_target_dependencies <- function(script_name, registry) {
  entry <- registry$scripts[[script_name]]
  dependencies <- character()

  for (dependency in entry$depends_on %||% character()) {
    if (dependency %in% names(registry$scripts %||% list())) {
      dependencies <- c(dependencies, target_name("step", dependency))
    }
  }

  for (input_name in entry$inputs %||% character()) {
    output <- registry$outputs[[input_name]]
    producer <- output$generated_by %||% NA_character_
    if (!is.na(producer) && nzchar(producer) && producer %in% names(registry$scripts %||% list())) {
      dependencies <- c(dependencies, target_name("step", producer))
    }
  }

  unique(dependencies)
}

#' Write a targets pipeline from the project DAG
#'
#' @description
#' \code{write_targets_pipeline()} converts the registered analysis DAG into a
#' minimal \code{_targets.R} file. The generated file delegates execution to
#' \code{projflow::run_project_object()} while preserving graph dependencies among
#' scripts where the registry declares \code{depends_on}, \code{inputs}, \code{outputs}, or
#' \code{generated_by} fields.
#'
#' @param root Path inside an existing projflow project.
#' @param path Output path for the targets pipeline, relative to \code{root} unless
#'   an absolute path is supplied.
#' @param overwrite Logical scalar. If \code{TRUE}, replace an existing file.
#'
#' @return Invisibly returns the written file path.
#' @examples
#' \dontrun{
#' write_targets_pipeline(overwrite = TRUE)
#' }
#' @author Thiago de Paula Oliveira
#' @export
write_targets_pipeline <- function(root = ".", path = "_targets.R", overwrite = FALSE) {
  validate_character_vector(path, "path")
  validate_logical_scalar(overwrite, "overwrite")
  root <- find_project_root(root)
  validate_project_dag(root = root, strict = TRUE)
  registry <- read_project_registry(root)
  script_names <- topological_project_order(root = root, type = "scripts")

  target_lines <- character()
  for (script_name in script_names) {
    if (is.null(registry$scripts[[script_name]])) {
      next
    }
    deps <- script_target_dependencies(script_name, registry)
    target_lines <- c(
      target_lines,
      paste0(
        "  targets::tar_target(\n",
        "    name = ", target_name("step", script_name), ",\n",
        "    command = ", target_command_for_script(script_name, deps), "\n",
        "  )"
      )
    )
  }

  body <- paste(
    "# Generated by projflow::write_targets_pipeline().",
    "# Edit the projflow registry, then regenerate this file when dependencies change.",
    "",
    "library(targets)",
    "",
    "list(",
    paste(target_lines, collapse = ",\n"),
    ")",
    sep = "\n"
  )

  output_path <- if (is_absolute_path(path)) path else fs::path(root, path)
  write_template_file(output_path, body, overwrite = overwrite)
  invisible(output_path)
}
