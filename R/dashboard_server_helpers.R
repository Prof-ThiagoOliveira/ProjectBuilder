with_project_action_source <- function(source = "dashboard", expr) {
  old <- getOption("projflow.action_source", "cli")
  options(projflow.action_source = source)
  on.exit(options(projflow.action_source = old), add = TRUE)
  force(expr)
}

new_dashboard_state <- function(root, mode) {
  shiny::reactiveValues(
    root = root,
    mode = mode,
    diagnostics = NULL,
    network = NULL,
    network_token = -1L,
    last_action = "Ready.",
    refresh_token = 0L,
    selected_object = NULL
  )
}

dashboard_root <- function(state) {
  shiny::isolate(state$root)
}

dashboard_diagnostics <- function(state) {
  diagnostics <- state$diagnostics
  shiny::validate(
    shiny::need(!is.null(diagnostics), "Diagnostics are not available yet. Click Refresh diagnostics.")
  )
  diagnostics
}

dashboard_summary <- function(state) {
  dashboard_diagnostics(state)$summary
}

dashboard_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(default)
  }
  x[[1]]
}

dashboard_summary_value <- function(state, name, default = 0L) {
  summary <- dashboard_summary(state)
  dashboard_scalar(summary[[name]], default)
}

dashboard_trim_text <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return("")
  }
  trimws(as.character(x[[1]]))
}

dashboard_optional_text <- function(x) {
  value <- dashboard_trim_text(x)
  if (nzchar(value)) value else NULL
}

dashboard_required_text <- function(x, label) {
  value <- dashboard_trim_text(x)
  if (!nzchar(value)) {
    rlang::abort(paste0(label, " is required."))
  }
  value
}

dashboard_optional_date <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return(NULL)
  }
  as.character(x[[1]])
}

dashboard_split_values <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(character())
  }
  if (is.list(x) && !is.data.frame(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  values <- as.character(x)
  values <- values[!is.na(values)]
  if (length(values) == 0L) {
    return(character())
  }
  values <- unlist(strsplit(values, "[,;]", perl = TRUE), use.names = FALSE)
  values <- trimws(values)
  unique(values[nzchar(values)])
}


dashboard_empty_data_frame <- function() {
  data.frame(stringsAsFactors = FALSE)
}

dashboard_flatten_table_cell <- function(value, collapse = "; ") {
  if (is.null(value) || length(value) == 0L) {
    return(NA_character_)
  }

  if (is.list(value) && !is.data.frame(value)) {
    value <- unlist(value, use.names = FALSE, recursive = TRUE)
  }

  if (length(value) == 0L) {
    return(NA_character_)
  }

  value <- as.character(value)
  value <- value[!is.na(value)]
  if (length(value) == 0L) {
    return(NA_character_)
  }

  paste(value, collapse = collapse)
}

dashboard_normalise_table_columns <- function(data) {
  if (!is.data.frame(data)) {
    return(data)
  }

  for (column in names(data)) {
    values <- data[[column]]
    if (is.list(values) && !is.data.frame(values)) {
      data[[column]] <- vapply(values, dashboard_flatten_table_cell, character(1))
    }
  }

  data
}

dashboard_safe_data_frame <- function(x) {
  if (is.null(x)) {
    return(dashboard_empty_data_frame())
  }
  if (is.data.frame(x)) {
    return(dashboard_normalise_table_columns(x))
  }
  if (is.list(x)) {
    out <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(error) NULL)
    if (is.data.frame(out)) {
      return(dashboard_normalise_table_columns(out))
    }
  }
  data.frame(value = dashboard_flatten_table_cell(x), stringsAsFactors = FALSE)
}

dashboard_selected_row <- function(input, table_id, data) {
  selected <- input[[paste0(table_id, "_rows_selected")]]
  data <- dashboard_safe_data_frame(data)
  if (length(selected) != 1L || nrow(data) < selected[[1]]) {
    return(NULL)
  }
  data[selected[[1]], , drop = FALSE]
}

dashboard_nav_route <- function(selected, subnav = NULL, subview = NULL) {
  selected <- as.character(selected %||% "overview")

  route <- switch(
    selected,
    "Overview" = list(tab = "overview"),
    "overview" = list(tab = "overview"),
    "Planning charts" = list(tab = "planning"),
    "Planning" = list(tab = "planning"),
    "planning" = list(tab = "planning"),
    "Task board" = list(tab = "tasks", subnav = "tasks_workflow_tabs", subview = "task_board"),
    "Tasks" = list(tab = "tasks", subnav = "tasks_workflow_tabs", subview = "task_board"),
    "tasks" = list(tab = "tasks"),
    "Risks" = list(tab = "tasks", subnav = "tasks_workflow_tabs", subview = "risks"),
    "Milestones" = list(tab = "tasks", subnav = "tasks_workflow_tabs", subview = "milestones"),
    "Decisions" = list(tab = "tasks", subnav = "tasks_workflow_tabs", subview = "decisions"),
    "Add object" = list(tab = "outputs", subnav = "outputs_workflow_tabs", subview = "add_object"),
    "Create object" = list(tab = "outputs", subnav = "outputs_workflow_tabs", subview = "add_object"),
    "Reports" = list(tab = "outputs", subnav = "outputs_workflow_tabs", subview = "reports"),
    "Outputs" = list(tab = "outputs", subnav = "outputs_workflow_tabs", subview = "outputs_inventory"),
    "outputs" = list(tab = "outputs"),
    "Registry" = list(tab = "outputs", subnav = "outputs_workflow_tabs", subview = "registry"),
    "Network" = list(tab = "diagnostics", subnav = "diagnostics_workflow_tabs", subview = "network"),
    "Checks and fixes" = list(tab = "diagnostics", subnav = "diagnostics_workflow_tabs", subview = "checks"),
    "Checks" = list(tab = "diagnostics", subnav = "diagnostics_workflow_tabs", subview = "checks"),
    "Packages and files" = list(tab = "diagnostics", subnav = "diagnostics_workflow_tabs", subview = "dependencies"),
    "Diagnostics" = list(tab = "diagnostics", subnav = "diagnostics_workflow_tabs", subview = "checks"),
    "diagnostics" = list(tab = "diagnostics"),
    "Activity log" = list(tab = "diagnostics", subnav = "diagnostics_workflow_tabs", subview = "activity"),
    "Settings" = list(tab = "settings", subnav = "settings_workflow_tabs", subview = "project_settings"),
    "settings" = list(tab = "settings"),
    "Data sources" = list(tab = "settings", subnav = "settings_workflow_tabs", subview = "data_sources"),
    list(tab = selected)
  )

  if (!is.null(subnav)) {
    route$subnav <- subnav
  }
  if (!is.null(subview)) {
    route$subview <- subview
  }
  route
}

dashboard_select_nav <- function(session, input_id, selected) {
  if ("nav_select" %in% getNamespaceExports("bslib")) {
    bslib::nav_select(input_id, selected = selected, session = session)
  } else {
    shiny::updateTabsetPanel(session = session, inputId = input_id, selected = selected)
  }
  invisible(selected)
}

dashboard_update_main_tab <- function(parent_session, selected, subnav = NULL, subview = NULL) {
  if (is.null(parent_session) || !inherits(parent_session, "ShinySession")) {
    rlang::abort("A valid parent Shiny session is required to update the main dashboard tab.")
  }
  route <- dashboard_nav_route(selected, subnav = subnav, subview = subview)
  dashboard_select_nav(parent_session, "main_tabs", route$tab)
  if (!is.null(route$subnav) && !is.null(route$subview)) {
    parent_session$onFlushed(function() {
      dashboard_select_nav(parent_session, route$subnav, route$subview)
    }, once = TRUE)
  }
  invisible(route)
}

refresh_dashboard_state <- function(state) {
  root <- dashboard_root(state)
  diagnostics <- project_diagnostics_data(root, include_network = FALSE)

  state$diagnostics <- diagnostics
  state$network <- NULL
  state$network_token <- -1L
  state$refresh_token <- shiny::isolate(state$refresh_token %||% 0L) + 1L

  invisible(diagnostics)
}

# Lazily compute the network only when the Network or Planning charts tabs need
# it. Large projects can contain many objects and dependency edges; building the
# graph during every dashboard refresh makes the whole app feel slow even when
# the user only wants to edit tasks or review outputs.
dashboard_network_data <- function(state) {
  token <- state$refresh_token %||% 0L
  network <- state$network
  if (is.null(network) || !identical(state$network_token, token)) {
    network <- project_network_data(dashboard_root(state))
    state$network <- network
    state$network_token <- token
  }
  network
}

dashboard_run_action <- function(state, session, label, expr) {
  tryCatch(
    {
      result <- with_project_action_source("dashboard", expr)
      refresh_dashboard_state(state)
      state$last_action <- paste0(label, " completed.")
      shiny::showNotification(state$last_action, type = "message")
      result
    },
    error = function(error) {
      state$last_action <- conditionMessage(error)
      shiny::showNotification(conditionMessage(error), type = "error", duration = 8)
      NULL
    }
  )
}

dashboard_issue_counts <- function(diagnostics) {
  checks <- dashboard_safe_data_frame(diagnostics$checks)
  if (!"severity" %in% names(checks)) {
    return(c(errors = 0L, warnings = 0L, suggestions = 0L))
  }
  c(
    errors = sum(checks$severity == "error", na.rm = TRUE),
    warnings = sum(checks$severity == "warning", na.rm = TRUE),
    suggestions = sum(checks$severity == "suggestion", na.rm = TRUE)
  )
}

dashboard_attention_items <- function(diagnostics) {
  summary <- dashboard_safe_data_frame(diagnostics$summary)
  value <- function(name) {
    if (!name %in% names(summary) || nrow(summary) == 0L || is.na(summary[[name]][[1]])) {
      return(0L)
    }
    suppressWarnings(as.integer(summary[[name]][[1]]))
  }
  issue_counts <- dashboard_issue_counts(diagnostics)
  rows <- list()
  add_row <- function(area, severity, item, recommended_action) {
    rows[[length(rows) + 1L]] <<- data.frame(
      area = area,
      severity = severity,
      item = item,
      recommended_action = recommended_action,
      stringsAsFactors = FALSE
    )
  }

  if (issue_counts[["errors"]] > 0L) {
    add_row("Checks", "critical", paste(issue_counts[["errors"]], "error(s)"), "Open Checks and fixes, then repair or address the failing item.")
  }
  if (issue_counts[["warnings"]] > 0L) {
    add_row("Checks", "warning", paste(issue_counts[["warnings"]], "warning(s)"), "Review warnings before rendering reports or publishing results.")
  }
  if (value("overdue_tasks") > 0L) {
    add_row("Tasks", "warning", paste(value("overdue_tasks"), "overdue task(s)"), "Open the Task board and update ownership, due dates or status.")
  }
  if (value("open_risks") > 0L) {
    add_row("Risks", "warning", paste(value("open_risks"), "open risk(s)"), "Review the risk register and record mitigation decisions.")
  }
  if (value("missing_outputs") > 0L) {
    add_row("Outputs", "critical", paste(value("missing_outputs"), "missing output(s)"), "Run or repair the upstream script, then refresh diagnostics.")
  }
  if (value("stale_outputs") > 0L) {
    add_row("Outputs", "warning", paste(value("stale_outputs"), "stale output(s)"), "Re-run affected scripts or rebuild the project.")
  }
  if (value("reports_needing_render") > 0L) {
    add_row("Reports", "warning", paste(value("reports_needing_render"), "report(s) need rendering"), "Open Reports and render selected or all reports.")
  }
  if (value("missing_packages") > 0L) {
    add_row("Packages", "warning", paste(value("missing_packages"), "missing package(s)"), "Install required packages or update project package metadata.")
  }
  if (value("data_sources_unavailable") > 0L) {
    add_row("Data", "critical", paste(value("data_sources_unavailable"), "unavailable data source(s)"), "Open Data sources and correct paths or permissions.")
  }
  if (value("orphan_files") > 0L) {
    add_row("Files", "info", paste(value("orphan_files"), "workflow file(s) to review"), "Open Packages and files; register only files that are part of the reproducible workflow.")
  }

  if (length(rows) == 0L) {
    return(data.frame(
      area = "Project",
      severity = "ok",
      item = "No priority items detected",
      recommended_action = "Continue with analysis, reporting or routine governance review.",
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}

dashboard_recent_activity <- function(diagnostics, n = 6L) {
  activity <- dashboard_safe_data_frame(diagnostics$activity)
  if (nrow(activity) == 0L) {
    return(data.frame(
      when = character(),
      action = character(),
      object_type = character(),
      object_name = character(),
      stringsAsFactors = FALSE
    ))
  }
  if ("entry" %in% names(activity)) {
    when <- if ("timestamp" %in% names(activity)) {
      activity$timestamp
    } else if ("created_at" %in% names(activity)) {
      activity$created_at
    } else {
      rep(NA_character_, nrow(activity))
    }
    time <- suppressWarnings(as.POSIXct(when, tz = "UTC"))
    activity <- activity[order(time, activity$entry, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  }
  keep <- intersect(c("timestamp", "created_at", "action", "object_type", "object_name", "status", "source", "summary"), names(activity))
  out <- activity[, keep, drop = FALSE]
  if ("timestamp" %in% names(out)) names(out)[names(out) == "timestamp"] <- "when"
  if ("created_at" %in% names(out)) names(out)[names(out) == "created_at"] <- "when"
  utils::head(out, n)
}

dashboard_filter_data <- function(data, column, selected) {
  data <- dashboard_safe_data_frame(data)
  selected <- dashboard_trim_text(selected)
  if (!nzchar(selected) || identical(selected, "All") || !column %in% names(data)) {
    return(data)
  }
  values <- vapply(data[[column]], dashboard_flatten_table_cell, character(1))
  data[values == selected, , drop = FALSE]
}

dashboard_cell <- function(row, column, default = "") {
  if (is.null(row) || !is.data.frame(row) || !column %in% names(row) || nrow(row) < 1L) {
    return(default)
  }
  value <- dashboard_flatten_table_cell(row[[column]][[1]])
  if (is.na(value) || !nzchar(value)) {
    return(default)
  }
  value
}

dashboard_choice_or_default <- function(value, choices, default) {
  value <- dashboard_trim_text(value)
  if (nzchar(value) && value %in% choices) {
    return(value)
  }
  default
}

dashboard_as_date_vector <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(as.Date(character()))
  }
  if (inherits(x, "Date")) {
    return(x)
  }
  if (is.data.frame(x)) {
    return(as.Date(character()))
  }
  if (is.list(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    out <- suppressWarnings(as.Date(x, origin = "1970-01-01"))
    out[is.na(x)] <- NA
    return(out)
  }
  values <- trimws(as.character(x))
  values[values %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  suppressWarnings(as.Date(values))
}

dashboard_date_or_null <- function(value) {
  parsed <- dashboard_as_date_vector(value)
  parsed <- parsed[!is.na(parsed)]
  if (length(parsed) == 0L) {
    return(NULL)
  }
  parsed[[1]]
}

dashboard_date_update_value <- function(value) {
  parsed <- dashboard_date_or_null(value)
  if (is.null(parsed)) {
    return("")
  }
  as.character(parsed)
}


dashboard_plot_empty <- function(message = "No data available") {
  graphics::plot.new()
  graphics::text(0.5, 0.5, message, cex = 1)
  invisible(NULL)
}

dashboard_plot_categorical_counts <- function(data, column, title = NULL) {
  data <- dashboard_safe_data_frame(data)
  if (nrow(data) == 0L || !column %in% names(data)) {
    return(dashboard_plot_empty("No records available"))
  }
  values <- as.character(data[[column]])
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0L) {
    return(dashboard_plot_empty("No values recorded"))
  }
  counts <- sort(table(values), decreasing = TRUE)
  labels <- names(counts)
  old_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = old_mar), add = TRUE)
  graphics::par(mar = c(3.8, 8, 3, 1))
  mids <- graphics::barplot(
    counts,
    horiz = TRUE,
    las = 1,
    main = title %||% column,
    xlab = "Number of records",
    names.arg = labels,
    border = NA,
    xlim = c(0, max(counts) * 1.18)
  )
  graphics::text(as.numeric(counts), mids, labels = as.integer(counts), pos = 4, cex = 0.85)
  graphics::box(bty = "l")
  invisible(counts)
}


dashboard_linked_object_choices <- function(state, include_governance = TRUE) {
  diagnostics <- dashboard_diagnostics(state)
  choices <- character()

  cell_text <- function(value) {
    if (is.null(value) || length(value) == 0L) {
      return(NA_character_)
    }
    if (is.list(value)) {
      value <- unlist(value, use.names = FALSE)
    }
    if (length(value) == 0L) {
      return(NA_character_)
    }
    value <- as.character(value)
    value <- value[!is.na(value) & nzchar(value)]
    if (length(value) == 0L) {
      return(NA_character_)
    }
    paste(value, collapse = ", ")
  }

  column_text <- function(data, column) {
    vapply(data[[column]], cell_text, character(1))
  }

  add_choices <- function(data, type, name_col = "name", value_col = "name") {
    data <- dashboard_safe_data_frame(data)
    if (nrow(data) == 0L || !value_col %in% names(data)) {
      return(invisible(NULL))
    }

    values <- column_text(data, value_col)
    labels <- values
    if (name_col %in% names(data)) {
      labels <- column_text(data, name_col)
    }

    keep <- !is.na(values) & nzchar(values)
    if (!any(keep)) {
      return(invisible(NULL))
    }

    values <- values[keep]
    labels <- labels[keep]
    labels[is.na(labels) | !nzchar(labels)] <- values[is.na(labels) | !nzchar(labels)]

    names(values) <- paste0(type, ": ", labels)
    choices <<- c(choices, values)
    invisible(NULL)
  }

  add_choices(diagnostics$scripts, "script")
  add_choices(diagnostics$reports, "report")
  add_choices(diagnostics$outputs, "output")
  add_choices(diagnostics$data_sources, "data source", name_col = "name", value_col = "name")

  if (isTRUE(include_governance)) {
    governance <- diagnostics$governance
    add_choices(governance$tasks, "task", name_col = "title", value_col = "id")
    add_choices(governance$risks, "risk", name_col = "title", value_col = "id")
    add_choices(governance$milestones, "milestone", name_col = "title", value_col = "id")
    add_choices(governance$decisions, "decision", name_col = "title", value_col = "id")
  }

  choices <- choices[!duplicated(unname(choices))]
  if (length(choices) == 0L) {
    return(character())
  }
  choices[order(names(choices))]
}

dashboard_update_linked_objects <- function(session, input_id, choices, selected = character()) {
  selected <- dashboard_split_values(selected)
  if (length(selected) > 0L) {
    missing_selected <- setdiff(selected, unname(choices))
    if (length(missing_selected) > 0L) {
      names(missing_selected) <- paste0("existing: ", missing_selected)
      choices <- c(choices, missing_selected)
    }
  }
  shiny::updateSelectizeInput(
    session = session,
    inputId = input_id,
    choices = choices,
    selected = selected,
    server = TRUE
  )
}

dashboard_sanitise_stem <- function(x, fallback = "object") {
  x <- dashboard_trim_text(x)
  if (!nzchar(x)) {
    x <- fallback
  }
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) fallback else x
}

dashboard_recommended_path <- function(object_type, name, area = "Auto", filename = "", custom_path = "") {
  custom_path <- dashboard_trim_text(custom_path)
  if (nzchar(custom_path)) {
    return(normalize_relative_path(custom_path))
  }

  object_type <- dashboard_trim_text(object_type)
  area <- dashboard_trim_text(area)
  filename <- dashboard_trim_text(filename)
  stem <- dashboard_sanitise_stem(name, fallback = object_type %||% "object")

  if (!nzchar(filename)) {
    filename <- switch(
      object_type,
      script = paste0(stem, ".R"),
      report = paste0(stem, ".qmd"),
      table = paste0(stem, ".csv"),
      figure = paste0(stem, ".png"),
      output = paste0(stem, ".rds"),
      app = "app.R",
      data_source = "",
      paste0(stem, ".txt")
    )
  }

  if (!nzchar(filename)) {
    return("")
  }

  if (identical(area, "Auto") || !nzchar(area)) {
    area <- switch(
      object_type,
      script = "analysis",
      report = "reports",
      table = "outputs/tables",
      figure = "outputs/figures",
      output = "outputs",
      app = file.path("app", stem),
      data_source = "",
      "outputs"
    )
  }

  if (identical(area, "Project root")) {
    return(normalize_relative_path(filename))
  }
  if (identical(area, "Custom")) {
    return(normalize_relative_path(filename))
  }
  normalize_relative_path(file.path(area, filename))
}

dashboard_object_subtype_choices <- function(object_type) {
  object_type <- dashboard_trim_text(object_type)
  switch(
    object_type,
    script = project_script_types(),
    report = c("html_report", "client_report", "scientific_report"),
    output = unique(c("output", "dataset", "table", "figure", "model", "model_diagnostics", "report", project_object_types())),
    app = c("shiny", "quarto_dashboard"),
    character()
  )
}

dashboard_object_subtype_label <- function(object_type) {
  object_type <- dashboard_trim_text(object_type)
  switch(
    object_type,
    script = "Script type",
    report = "Report type",
    output = "Output type",
    app = "Application type",
    "Subtype"
  )
}

dashboard_object_subtype_default <- function(object_type) {
  object_type <- dashboard_trim_text(object_type)
  switch(
    object_type,
    script = "analysis",
    report = "html_report",
    output = "output",
    app = "shiny",
    ""
  )
}

dashboard_object_subtype <- function(object_type, subtype = NULL) {
  choices <- dashboard_object_subtype_choices(object_type)
  if (length(choices) == 0L) {
    return(NULL)
  }
  value <- dashboard_trim_text(subtype)
  if (!nzchar(value)) {
    value <- dashboard_object_subtype_default(object_type)
  }
  if (!value %in% choices) {
    rlang::abort(paste0(
      "Invalid ", dashboard_object_subtype_label(object_type), ": ", value,
      ". Choose one of: ", paste(choices, collapse = ", "), "."
    ))
  }
  value
}
