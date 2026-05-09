mod_charts_ui <- function(id) {
  ns <- shiny::NS(id)
  gantt_output <- if (dashboard_has_timevis()) {
    shiny::tagList(
      timevis::timevisOutput(ns("gantt_timeline"), height = "560px"),
      shiny::uiOutput(ns("gantt_selection"))
    )
  } else {
    shiny::tagList(
      dashboard_alert("Install the optional timevis package to use the interactive Gantt timeline. The static fallback is shown below.", type = "info"),
      shiny::plotOutput(ns("gantt_plot"), height = "560px")
    )
  }

  wbs_output <- if (dashboard_has_visnetwork()) {
    visNetwork::visNetworkOutput(ns("wbs_network"), height = "520px")
  } else {
    shiny::plotOutput(ns("wbs_plot"), height = "520px")
  }

  pert_output <- if (dashboard_has_visnetwork()) {
    visNetwork::visNetworkOutput(ns("pert_network"), height = "520px")
  } else {
    shiny::plotOutput(ns("pert_plot"), height = "520px")
  }

  shiny::tagList(
    dashboard_card(
      "Planning charts",
      subtitle = "Management views for schedule control, work breakdown and dependency review. These charts are derived from task, milestone and registry metadata.",
      shiny::fluidRow(
        shiny::column(3, shiny::uiOutput(ns("scheduled_box"))),
        shiny::column(3, shiny::uiOutput(ns("dependency_box"))),
        shiny::column(3, shiny::uiOutput(ns("governance_box"))),
        shiny::column(3, shiny::uiOutput(ns("coverage_box")))
      ),
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput(ns("chart_scope"), "Chart scope", choices = c("Active work", "All records"), selected = "Active work")),
        shiny::column(3, shiny::numericInput(ns("max_items"), "Maximum items per chart", value = 20, min = 6, max = 80, step = 1)),
        shiny::column(3, shiny::uiOutput(ns("chart_branch_ui"))),
        shiny::column(3, shiny::tagList(
          shiny::checkboxInput(ns("show_completed"), "Show completed work", value = FALSE),
          shiny::checkboxInput(ns("show_external_dependencies"), "Show external dependencies", value = FALSE)
        ))
      )
    ),
    dashboard_card(
      "Interactive Gantt chart",
      subtitle = if (dashboard_has_timevis()) {
        "Interactive schedule view of dated tasks and milestones. Drag the timeline to move across dates; use the mouse wheel or zoom buttons to change scale."
      } else {
        "Schedule view of dated tasks and milestones. Install timevis for an interactive drag-and-zoom timeline."
      },
      actions = shiny::actionButton(ns("expand_gantt"), "Expand", class = "btn-outline-primary btn-sm"),
      gantt_output
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        dashboard_card(
          "Work Breakdown Structure",
          subtitle = "High-level WBS by management area and registered work products.",
          actions = shiny::actionButton(ns("expand_wbs"), "Expand", class = "btn-outline-primary btn-sm"),
          wbs_output
        )
      ),
      shiny::column(
        width = 6,
        dashboard_card(
          "Dependency map",
          subtitle = "Registry-derived precedence view. This is a dependency map, not a probabilistic PERT duration model.",
          actions = shiny::actionButton(ns("expand_pert"), "Expand", class = "btn-outline-primary btn-sm"),
          pert_output
        )
      )
    )
  )
}

mod_charts_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    diagnostics <- shiny::reactive(dashboard_diagnostics(state))

    output$scheduled_box <- shiny::renderUI({
      governance <- diagnostics()$governance
      tasks <- dashboard_safe_data_frame(governance$tasks)
      milestones <- dashboard_safe_data_frame(governance$milestones)
      scheduled <- dashboard_count_with_dates(tasks, "due_date") + dashboard_count_with_dates(milestones, "due_date")
      dashboard_value_box("Scheduled items", scheduled, if (scheduled > 0L) "primary" else "secondary", note = "Tasks and milestones with valid due dates.")
    })

    output$dependency_box <- shiny::renderUI({
      edges <- dashboard_safe_data_frame(dashboard_network_data(state)$edges)
      dashboard_value_box("Dependencies", nrow(edges), if (nrow(edges) > 0L) "primary" else "secondary", note = "Edges in the project network.")
    })

    output$governance_box <- shiny::renderUI({
      governance <- diagnostics()$governance
      total <- nrow(dashboard_safe_data_frame(governance$tasks)) +
        nrow(dashboard_safe_data_frame(governance$milestones)) +
        nrow(dashboard_safe_data_frame(governance$risks)) +
        nrow(dashboard_safe_data_frame(governance$decisions))
      dashboard_value_box("Governance records", total, if (total > 0L) "primary" else "secondary", note = "Tasks, milestones, risks and decisions.")
    })

    output$coverage_box <- shiny::renderUI({
      objects <- dashboard_safe_data_frame(diagnostics()$objects)
      dashboard_value_box("Registered objects", nrow(objects), "secondary", note = "Objects available for WBS and dependency views.")
    })

    chart_options <- shiny::reactive({
      list(
        max_items = as.integer(input$max_items %||% 20L),
        active_only = identical(input$chart_scope %||% "Active work", "Active work"),
        show_completed = isTRUE(input$show_completed)
      )
    })

    gantt_records <- shiny::reactive({
      opts <- chart_options()
      dashboard_gantt_records(
        diagnostics(),
        max_items = opts$max_items,
        active_only = opts$active_only,
        show_completed = opts$show_completed
      )
    })

    if (dashboard_has_timevis()) {
      output$gantt_timeline <- timevis::renderTimevis({
        dashboard_timevis_gantt(gantt_records(), height = "560px")
      })
      output$gantt_timeline_full <- timevis::renderTimevis({
        dashboard_timevis_gantt(gantt_records(), height = "75vh")
      })
      output$gantt_selection <- shiny::renderUI({
        selected <- input$gantt_timeline_selected
        records <- gantt_records()
        if (is.null(selected) || length(selected) == 0L || nrow(records) == 0L) {
          return(shiny::div(class = "projflow-chart-note", "Select an item on the timeline to inspect its status and dates."))
        }
        selected <- as.character(selected[[1]])
        row <- records[records$timevis_id == selected, , drop = FALSE]
        if (nrow(row) == 0L) {
          return(shiny::div(class = "projflow-chart-note", "Selected item is no longer available after filtering."))
        }
        shiny::div(
          class = "projflow-selected-record",
          shiny::strong(row$label[[1]]),
          shiny::br(),
          paste("Type:", row$kind[[1]]), shiny::br(),
          paste("Status:", row$status[[1]]), shiny::br(),
          paste("Start:", row$start[[1]], "| End:", row$end[[1]])
        )
      })
    } else {
      output$gantt_plot <- shiny::renderPlot({
        dashboard_plot_gantt(gantt_records())
      })
      output$gantt_plot_full <- shiny::renderPlot({
        dashboard_plot_gantt(gantt_records())
      })
    }

    output$chart_branch_ui <- shiny::renderUI({
      choices <- dashboard_chart_branch_choices(diagnostics(), network = dashboard_network_data(state))
      selected <- input$chart_branch %||% "all"
      if (!selected %in% choices) {
        selected <- "all"
      }
      shiny::selectInput(session$ns("chart_branch"), "Focus branch", choices = choices, selected = selected)
    })

    wbs_data <- shiny::reactive({
      opts <- chart_options()
      dashboard_wbs_data(
        diagnostics(),
        max_items = opts$max_items,
        selected = input$chart_branch %||% "all"
      )
    })

    pert_data <- shiny::reactive({
      opts <- chart_options()
      dashboard_pert_data(
        diagnostics(),
        network = dashboard_network_data(state),
        max_items = opts$max_items,
        selected = input$chart_branch %||% "all",
        include_external_dependencies = isTRUE(input$show_external_dependencies)
      )
    })

    if (dashboard_has_visnetwork()) {
      output$wbs_network <- visNetwork::renderVisNetwork({
        dashboard_wbs_visnetwork(wbs_data(), height = "520px")
      })
      output$wbs_network_full <- visNetwork::renderVisNetwork({
        dashboard_wbs_visnetwork(wbs_data(), height = "75vh")
      })
      output$pert_network <- visNetwork::renderVisNetwork({
        dashboard_pert_visnetwork(pert_data(), height = "520px")
      })
      output$pert_network_full <- visNetwork::renderVisNetwork({
        dashboard_pert_visnetwork(pert_data(), height = "75vh")
      })

      shiny::observeEvent(input$chart_branch, {
        data <- wbs_data()
        visible_nodes <- as.character(data$nodes$id %||% character())
        if (length(visible_nodes) > 0L) {
          wbs_proxy <- visNetwork::visNetworkProxy("wbs_network")
          visNetwork::visFit(wbs_proxy, nodes = visible_nodes)
        }
        root_id <- data$selected_root_id %||% NULL
        if (!is.null(root_id) && length(root_id) == 1L && root_id %in% visible_nodes) {
          wbs_proxy <- visNetwork::visNetworkProxy("wbs_network")
          visNetwork::visFocus(wbs_proxy, id = root_id, scale = 1.35)
        }

        pdata <- pert_data()
        visible_pert <- as.character(pdata$nodes$id %||% character())
        if (length(visible_pert) > 0L) {
          pert_proxy <- visNetwork::visNetworkProxy("pert_network")
          visNetwork::visFit(pert_proxy, nodes = visible_pert)
        }
        pert_root <- pdata$selected_root_id %||% NULL
        if (!is.null(pert_root) && length(pert_root) == 1L && pert_root %in% visible_pert) {
          pert_proxy <- visNetwork::visNetworkProxy("pert_network")
          visNetwork::visFocus(pert_proxy, id = pert_root, scale = 1.35)
        }
      }, ignoreInit = TRUE)
    } else {
      output$wbs_plot <- shiny::renderPlot({
        dashboard_plot_wbs(wbs_data())
      })

      output$wbs_plot_full <- shiny::renderPlot({
        dashboard_plot_wbs(wbs_data())
      })

      output$pert_plot <- shiny::renderPlot({
        dashboard_plot_pert(pert_data())
      })

      output$pert_plot_full <- shiny::renderPlot({
        dashboard_plot_pert(pert_data())
      })
    }

    shiny::observeEvent(input$expand_gantt, {
      body <- if (dashboard_has_timevis()) {
        timevis::timevisOutput(session$ns("gantt_timeline_full"), height = "75vh")
      } else {
        shiny::plotOutput(session$ns("gantt_plot_full"), height = "75vh")
      }
      shiny::showModal(shiny::modalDialog(
        title = "Gantt chart",
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Close"),
        body
      ))
    })

    shiny::observeEvent(input$expand_wbs, {
      shiny::showModal(shiny::modalDialog(
        title = "Work Breakdown Structure",
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Close"),
        if (dashboard_has_visnetwork()) {
          visNetwork::visNetworkOutput(session$ns("wbs_network_full"), height = "75vh")
        } else {
          shiny::plotOutput(session$ns("wbs_plot_full"), height = "75vh")
        }
      ))
    })

    shiny::observeEvent(input$expand_pert, {
      shiny::showModal(shiny::modalDialog(
        title = "Dependency map",
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Close"),
        if (dashboard_has_visnetwork()) {
          visNetwork::visNetworkOutput(session$ns("pert_network_full"), height = "75vh")
        } else {
          shiny::plotOutput(session$ns("pert_plot_full"), height = "75vh")
        }
      ))
    })
  })
}

dashboard_count_with_dates <- function(data, column) {
  data <- dashboard_safe_data_frame(data)
  if (nrow(data) == 0L || !column %in% names(data)) {
    return(0L)
  }
  values <- dashboard_as_date_vector(data[[column]])
  sum(!is.na(values))
}

dashboard_chart_label <- function(x, width = 28L) {
  if (is.null(x) || length(x) == 0L) {
    x <- ""
  }
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- "unnamed"
  vapply(x, function(one) paste(strwrap(one, width = width), collapse = "\n"), character(1))
}

dashboard_chart_status_class <- function(status) {
  status <- tolower(as.character(status %||% ""))
  if (status %in% c("done", "closed", "mitigated", "accepted")) {
    return("complete")
  }
  if (status %in% c("blocked", "delayed", "critical")) {
    return("blocked")
  }
  if (status %in% c("in_progress", "mitigating", "active")) {
    return("active")
  }
  "planned"
}

dashboard_gantt_records <- function(diagnostics, max_items = 20L, active_only = TRUE, show_completed = FALSE) {
  governance <- diagnostics$governance
  tasks <- dashboard_safe_data_frame(governance$tasks)
  milestones <- dashboard_safe_data_frame(governance$milestones)
  rows <- list()

  add_task_rows <- function(data) {
    if (nrow(data) == 0L) {
      return(invisible(NULL))
    }
    created <- if ("created_at" %in% names(data)) dashboard_as_date_vector(data$created_at) else rep(as.Date(NA), nrow(data))
    due <- if ("due_date" %in% names(data)) dashboard_as_date_vector(data$due_date) else rep(as.Date(NA), nrow(data))
    completed <- if ("completed_at" %in% names(data)) dashboard_as_date_vector(data$completed_at) else rep(as.Date(NA), nrow(data))
    for (i in seq_len(nrow(data))) {
      status <- dashboard_cell(data[i, , drop = FALSE], "status", default = "todo")
      if (!isTRUE(show_completed) && status %in% c("done", "cancelled")) next
      if (isTRUE(active_only) && status %in% c("done", "cancelled")) next
      start <- created[[i]]
      end <- due[[i]]
      if (is.na(start) && !is.na(end)) start <- end - 7
      if (is.na(end) && !is.na(completed[[i]])) end <- completed[[i]]
      if (is.na(start) && !is.na(end)) start <- end
      if (!is.na(start) && is.na(end)) end <- start
      if (is.na(start) || is.na(end)) next
      if (end < start) end <- start
      label <- dashboard_cell(data[i, , drop = FALSE], "title", default = paste("Task", i))
      id <- dashboard_cell(data[i, , drop = FALSE], "id", default = paste0("task_", i))
      rows[[length(rows) + 1L]] <<- data.frame(
        timevis_id = paste0("task__", dashboard_sanitise_stem(id, fallback = paste0("task_", i))),
        label = label,
        kind = "Task",
        group = "tasks",
        status = status,
        status_class = dashboard_chart_status_class(status),
        start = as.Date(start),
        end = as.Date(end),
        stringsAsFactors = FALSE
      )
    }
  }

  add_milestone_rows <- function(data) {
    if (nrow(data) == 0L) {
      return(invisible(NULL))
    }
    due <- if ("due_date" %in% names(data)) dashboard_as_date_vector(data$due_date) else rep(as.Date(NA), nrow(data))
    for (i in seq_len(nrow(data))) {
      status <- dashboard_cell(data[i, , drop = FALSE], "status", default = "planned")
      if (!isTRUE(show_completed) && status %in% c("done", "cancelled")) next
      if (is.na(due[[i]])) next
      label <- dashboard_cell(data[i, , drop = FALSE], "title", default = paste("Milestone", i))
      id <- dashboard_cell(data[i, , drop = FALSE], "id", default = paste0("milestone_", i))
      rows[[length(rows) + 1L]] <<- data.frame(
        timevis_id = paste0("milestone__", dashboard_sanitise_stem(id, fallback = paste0("milestone_", i))),
        label = label,
        kind = "Milestone",
        group = "milestones",
        status = status,
        status_class = dashboard_chart_status_class(status),
        start = as.Date(due[[i]]),
        end = as.Date(due[[i]]),
        stringsAsFactors = FALSE
      )
    }
  }

  add_task_rows(tasks)
  add_milestone_rows(milestones)

  if (length(rows) == 0L) {
    return(data.frame(
      timevis_id = character(),
      label = character(),
      kind = character(),
      group = character(),
      status = character(),
      status_class = character(),
      start = as.Date(character()),
      end = as.Date(character()),
      stringsAsFactors = FALSE
    ))
  }

  data <- do.call(rbind, rows)
  data <- data[order(data$start, data$end, data$kind, data$label), , drop = FALSE]
  if (nrow(data) > max_items) {
    data <- utils::tail(data, max_items)
  }
  rownames(data) <- NULL
  data
}

dashboard_timevis_gantt <- function(records, height = "560px") {
  records <- dashboard_safe_data_frame(records)
  if (nrow(records) == 0L) {
    empty_items <- data.frame(
      id = "empty",
      content = "No dated tasks or milestones",
      start = as.character(Sys.Date()),
      type = "point",
      group = "schedule",
      title = "Add due dates to tasks or milestones to populate the Gantt chart.",
      stringsAsFactors = FALSE
    )
    empty_groups <- data.frame(id = "schedule", content = "Schedule", stringsAsFactors = FALSE)
    return(timevis::timevis(
      data = empty_items,
      groups = empty_groups,
      options = list(height = height, editable = FALSE, stack = TRUE, showCurrentTime = TRUE)
    ))
  }

  content <- paste0(
    htmltools::htmlEscape(records$label),
    "<br><span style='font-size:11px;color:#64748b;'>",
    htmltools::htmlEscape(records$status),
    "</span>"
  )
  title <- paste0(
    records$kind, ": ", records$label,
    "\nStatus: ", records$status,
    "\nStart: ", records$start,
    "\nEnd: ", records$end
  )
  is_point <- records$kind == "Milestone" | records$start == records$end
  items <- data.frame(
    id = records$timevis_id,
    content = content,
    start = as.character(records$start),
    end = ifelse(is_point, NA_character_, as.character(records$end)),
    type = ifelse(is_point, "point", "range"),
    group = records$group,
    title = title,
    className = paste0("projflow-tv-", records$status_class),
    stringsAsFactors = FALSE
  )
  groups <- data.frame(
    id = c("tasks", "milestones"),
    content = c("Tasks", "Milestones"),
    stringsAsFactors = FALSE
  )
  groups <- groups[groups$id %in% unique(items$group), , drop = FALSE]

  xmin <- min(records$start, na.rm = TRUE)
  xmax <- max(records$end, na.rm = TRUE)
  if (is.na(xmin) || is.na(xmax)) {
    xmin <- Sys.Date() - 14
    xmax <- Sys.Date() + 14
  }
  if (identical(xmin, xmax)) {
    xmin <- xmin - 7
    xmax <- xmax + 7
  }

  timevis::timevis(
    data = items,
    groups = groups,
    showZoom = TRUE,
    options = list(
      height = height,
      editable = FALSE,
      stack = TRUE,
      showCurrentTime = TRUE,
      zoomable = TRUE,
      moveable = TRUE,
      selectable = TRUE,
      multiselect = FALSE,
      start = as.character(xmin - 3),
      end = as.character(xmax + 3),
      orientation = "both"
    )
  )
}

dashboard_plot_gantt <- function(records) {
  data <- dashboard_safe_data_frame(records)
  if (nrow(data) == 0L) {
    return(dashboard_plot_empty("No dated tasks or milestones. Add due dates to use the Gantt view."))
  }

  labels <- paste0(data$kind, ": ", data$label)
  labels <- dashboard_chart_label(labels, width = 30L)
  y <- seq_len(nrow(data))
  xmin <- min(data$start, na.rm = TRUE)
  xmax <- max(data$end, na.rm = TRUE)
  if (identical(xmin, xmax)) {
    xmin <- xmin - 7
    xmax <- xmax + 7
  }
  xlim <- as.numeric(c(xmin - 2, xmax + 2))
  tick_at <- pretty(xlim, n = 5)
  today <- Sys.Date()

  old_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = old_mar), add = TRUE)
  graphics::par(mar = c(5, 10, 4, 1))
  graphics::plot(
    xlim, c(0.5, length(y) + 0.5),
    type = "n", xaxt = "n", yaxt = "n", xlab = "Date", ylab = "", main = "Gantt chart"
  )
  graphics::axis(1, at = tick_at, labels = format(as.Date(tick_at, origin = "1970-01-01"), "%Y-%m-%d"), las = 2, cex.axis = 0.78)
  graphics::axis(2, at = y, labels = labels, las = 1, cex.axis = 0.72)
  graphics::abline(h = y, lty = 3, col = "grey88")
  if (today >= xmin && today <= xmax) {
    graphics::abline(v = as.numeric(today), lty = 2, lwd = 1.4)
    graphics::text(as.numeric(today), length(y) + 0.45, "today", pos = 4, cex = 0.75)
  }
  for (i in seq_len(nrow(data))) {
    if (identical(data$kind[[i]], "Milestone")) {
      graphics::points(as.numeric(data$end[[i]]), y[[i]], pch = 18, cex = 1.5)
    } else {
      graphics::segments(as.numeric(data$start[[i]]), y[[i]], as.numeric(data$end[[i]]), y[[i]], lwd = 9, lend = "round")
      graphics::points(as.numeric(data$end[[i]]), y[[i]], pch = 19, cex = 0.8)
    }
    graphics::text(as.numeric(data$end[[i]]) + 0.7, y[[i]], data$status[[i]], pos = 4, cex = 0.68)
  }
  graphics::box(bty = "l")
  invisible(data)
}


dashboard_chart_branch_key <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return("all")
  }
  key <- tolower(trimws(as.character(x[[1]])))
  key <- gsub("^section:", "", key)
  key <- gsub("[^a-z0-9_]+", "_", key)
  key <- gsub("_+", "_", key)
  key <- gsub("^_|_$", "", key)
  if (!nzchar(key)) "all" else key
}

dashboard_chart_branch_aliases <- function(key) {
  key <- dashboard_chart_branch_key(key)
  aliases <- unique(c(
    key,
    sub("s$", "", key),
    paste0(key, "s")
  ))
  switch(key,
    report = unique(c(aliases, "reports")),
    reports = unique(c(aliases, "report")),
    script = unique(c(aliases, "scripts")),
    scripts = unique(c(aliases, "script")),
    output = unique(c(aliases, "outputs", "table", "tables", "figure", "figures")),
    outputs = unique(c(aliases, "output", "table", "tables", "figure", "figures")),
    table = unique(c(aliases, "tables", "output", "outputs")),
    tables = unique(c(aliases, "table", "output", "outputs")),
    figure = unique(c(aliases, "figures", "output", "outputs")),
    figures = unique(c(aliases, "figure", "output", "outputs")),
    task = unique(c(aliases, "tasks", "governance")),
    tasks = unique(c(aliases, "task", "governance")),
    milestone = unique(c(aliases, "milestones", "governance")),
    milestones = unique(c(aliases, "milestone", "governance")),
    risk = unique(c(aliases, "risks", "governance")),
    risks = unique(c(aliases, "risk", "governance")),
    decision = unique(c(aliases, "decisions", "governance")),
    decisions = unique(c(aliases, "decision", "governance")),
    data_source = unique(c(aliases, "data_sources")),
    data_sources = unique(c(aliases, "data_source")),
    project_setup = unique(c(aliases, "component", "components", "deliverable", "deliverables", "infrastructure")),
    aliases
  )
}

dashboard_chart_branch_choices <- function(diagnostics, network = diagnostics$network) {
  choices <- c("All" = "all")
  fixed <- c(
    "Project setup" = "project_setup",
    "Scripts" = "scripts",
    "Reports" = "reports",
    "Outputs" = "outputs",
    "Governance" = "governance",
    "Data sources" = "data_sources"
  )

  has_rows <- function(x) nrow(dashboard_safe_data_frame(x)) > 0L
  project <- diagnostics$project %||% list()
  available <- logical(length(fixed))
  names(available) <- names(fixed)
  available[["Project setup"]] <- length(c(project$components %||% character(), project$deliverables %||% character(), project$infrastructure %||% character())) > 0L
  available[["Scripts"]] <- has_rows(diagnostics$scripts)
  available[["Reports"]] <- has_rows(diagnostics$reports)
  available[["Outputs"]] <- has_rows(diagnostics$outputs)
  governance <- diagnostics$governance %||% list()
  available[["Governance"]] <- any(vapply(c("tasks", "milestones", "risks", "decisions"), function(name) has_rows(governance[[name]]), logical(1)))
  available[["Data sources"]] <- has_rows(diagnostics$data_sources)

  choices <- c(choices, fixed[available])

  nodes <- dashboard_safe_data_frame(network$nodes)
  if (nrow(nodes) > 0L && "type" %in% names(nodes)) {
    types <- sort(unique(tolower(as.character(nodes$type))))
    types <- types[!is.na(types) & nzchar(types)]
    extra <- setNames(types, paste("Type:", gsub("_", " ", types)))
    choices <- c(choices, extra[!extra %in% unname(choices)])
  }

  choices
}

dashboard_make_wbs_empty <- function(selected_root_id = NA_character_) {
  list(
    nodes = data.frame(
      id = character(), label = character(), group = character(), type = character(),
      section = character(), level = integer(), parent_id = character(), status = character(),
      path = character(), title = character(), stringsAsFactors = FALSE
    ),
    edges = data.frame(from = character(), to = character(), edge_type = character(), arrows = character(), stringsAsFactors = FALSE),
    selected_root_id = selected_root_id
  )
}

dashboard_graph_descendants <- function(edges, root_ids) {
  edges <- dashboard_safe_data_frame(edges)
  root_ids <- unique(as.character(root_ids))
  root_ids <- root_ids[!is.na(root_ids) & nzchar(root_ids)]
  if (length(root_ids) == 0L || nrow(edges) == 0L || !all(c("from", "to") %in% names(edges))) {
    return(root_ids)
  }
  keep <- root_ids
  frontier <- root_ids
  while (length(frontier) > 0L) {
    children <- unique(as.character(edges$to[as.character(edges$from) %in% frontier]))
    children <- setdiff(children[!is.na(children) & nzchar(children)], keep)
    if (length(children) == 0L) break
    keep <- unique(c(keep, children))
    frontier <- children
  }
  keep
}

dashboard_limit_chart_nodes <- function(nodes, edges, max_items, always_keep = character()) {
  nodes <- dashboard_safe_data_frame(nodes)
  edges <- dashboard_safe_data_frame(edges)
  max_items <- suppressWarnings(as.integer(max_items %||% nrow(nodes)))
  if (is.na(max_items) || max_items <= 0L || nrow(nodes) <= max_items) {
    return(list(nodes = nodes, edges = edges))
  }

  always_keep <- intersect(unique(as.character(always_keep)), as.character(nodes$id))
  remaining <- setdiff(as.character(nodes$id), always_keep)
  slots <- max(0L, max_items - length(always_keep))

  if (nrow(edges) > 0L && all(c("from", "to") %in% names(edges))) {
    degree <- sort(table(c(as.character(edges$from), as.character(edges$to))), decreasing = TRUE)
    ranked <- names(degree)
    remaining <- c(intersect(ranked, remaining), setdiff(remaining, ranked))
  }
  keep <- unique(c(always_keep, utils::head(remaining, slots)))
  nodes <- nodes[as.character(nodes$id) %in% keep, , drop = FALSE]
  edges <- edges[as.character(edges$from) %in% keep & as.character(edges$to) %in% keep, , drop = FALSE]
  list(nodes = nodes, edges = edges)
}

dashboard_filter_wbs_branch <- function(nodes, edges, selected = "all") {
  nodes <- dashboard_safe_data_frame(nodes)
  edges <- dashboard_safe_data_frame(edges)
  selected_root_id <- NA_character_
  key <- dashboard_chart_branch_key(selected)
  if (identical(key, "all") || nrow(nodes) == 0L) {
    return(list(nodes = nodes, edges = edges, selected_root_id = selected_root_id))
  }

  aliases <- dashboard_chart_branch_aliases(key)
  section_candidates <- paste0("section:", aliases)
  root_ids <- intersect(section_candidates, as.character(nodes$id))

  if (length(root_ids) == 0L && "type" %in% names(nodes)) {
    root_ids <- as.character(nodes$id[tolower(as.character(nodes$type)) %in% aliases])
  }
  if (length(root_ids) == 0L && "group" %in% names(nodes)) {
    root_ids <- as.character(nodes$id[tolower(as.character(nodes$group)) %in% aliases])
  }
  if (length(root_ids) == 0L && "section" %in% names(nodes)) {
    root_ids <- as.character(nodes$id[tolower(as.character(nodes$section)) %in% aliases])
  }
  if (length(root_ids) == 0L) {
    root_ids <- intersect(as.character(selected), as.character(nodes$id))
  }
  if (length(root_ids) == 0L) {
    return(list(nodes = nodes, edges = edges, selected_root_id = selected_root_id))
  }

  selected_root_id <- root_ids[[1]]
  keep <- dashboard_graph_descendants(edges, root_ids)
  nodes <- nodes[as.character(nodes$id) %in% keep, , drop = FALSE]
  edges <- edges[as.character(edges$from) %in% keep & as.character(edges$to) %in% keep, , drop = FALSE]
  list(nodes = nodes, edges = edges, selected_root_id = selected_root_id)
}

dashboard_wbs_data <- function(diagnostics, max_items = 20L, selected = "all") {
  nodes <- list()
  edges <- list()

  add_node <- function(id, label, group, level, parent_id = NA_character_, type = group, section = group, status = NA_character_, path = NA_character_) {
    id <- as.character(id)
    if (!nzchar(id)) return(invisible(NULL))
    label <- as.character(label %||% id)
    nodes[[id]] <<- data.frame(
      id = id,
      label = label,
      group = as.character(group %||% type %||% "object"),
      type = as.character(type %||% group %||% "object"),
      section = as.character(section %||% group %||% "object"),
      level = as.integer(level),
      parent_id = as.character(parent_id %||% NA_character_),
      status = as.character(status %||% NA_character_),
      path = as.character(path %||% NA_character_),
      title = paste0(label, if (!is.na(status) && nzchar(status)) paste0("\nStatus: ", status) else ""),
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  add_edge <- function(from, to, edge_type = "contains") {
    from <- as.character(from); to <- as.character(to)
    if (!nzchar(from) || !nzchar(to) || identical(from, to)) return(invisible(NULL))
    edges[[length(edges) + 1L]] <<- data.frame(
      from = from,
      to = to,
      edge_type = edge_type,
      arrows = "",
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  add_section <- function(id, label, group = id) {
    section_id <- paste0("section:", id)
    add_node(section_id, label, group = group, level = 1L, parent_id = "project:root", type = "section", section = id)
    add_edge("project:root", section_id)
    section_id
  }

  add_child <- function(parent, id, label, group, level, type = group, section = group, status = NA_character_, path = NA_character_) {
    add_node(id, label, group = group, level = level, parent_id = parent, type = type, section = section, status = status, path = path)
    add_edge(parent, id)
  }

  project <- diagnostics$project %||% list()
  root_label <- project$title %||% project$name %||% "Project"
  add_node("project:root", root_label, group = "project", type = "project", section = "project", level = 0L)

  setup_id <- add_section("project_setup", "Project setup", "project_setup")
  components_id <- paste0(setup_id, ":components")
  deliverables_id <- paste0(setup_id, ":deliverables")
  infrastructure_id <- paste0(setup_id, ":infrastructure")

  components <- project$components %||% character()
  if (length(components) > 0L) {
    add_child(setup_id, components_id, "Components", "component", 2L, type = "component_group", section = "project_setup")
    for (component in components) {
      key <- dashboard_sanitise_stem(component, fallback = "component")
      add_child(components_id, paste0("component:", key), component, "component", 3L, type = "component", section = "project_setup")
    }
  }

  deliverables <- project$deliverables %||% character()
  if (length(deliverables) > 0L) {
    add_child(setup_id, deliverables_id, "Deliverables", "deliverable", 2L, type = "deliverable_group", section = "project_setup")
    for (deliverable in deliverables) {
      key <- dashboard_sanitise_stem(deliverable, fallback = "deliverable")
      add_child(deliverables_id, paste0("deliverable:", key), deliverable, "deliverable", 3L, type = "deliverable", section = "project_setup")
    }
  }

  infrastructure <- project$infrastructure %||% character()
  if (length(infrastructure) > 0L) {
    add_child(setup_id, infrastructure_id, "Infrastructure", "infrastructure", 2L, type = "infrastructure_group", section = "project_setup")
    for (item in infrastructure) {
      key <- dashboard_sanitise_stem(item, fallback = "infrastructure")
      add_child(infrastructure_id, paste0("infrastructure:", key), item, "infrastructure", 3L, type = "infrastructure", section = "project_setup")
    }
  }

  scripts <- dashboard_safe_data_frame(diagnostics$scripts)
  scripts_id <- add_section("scripts", "Scripts", "script")
  if (nrow(scripts) > 0L) {
    for (i in seq_len(nrow(scripts))) {
      name <- dashboard_cell(scripts[i, , drop = FALSE], "name", default = paste("Script", i))
      type <- dashboard_cell(scripts[i, , drop = FALSE], "type", default = "script")
      path <- dashboard_cell(scripts[i, , drop = FALSE], "path", default = NA_character_)
      add_child(scripts_id, paste0("script:", dashboard_sanitise_stem(name, fallback = paste0("script_", i))), name, "script", 2L, type = type, section = "scripts", status = type, path = path)
    }
  }

  reports <- dashboard_safe_data_frame(diagnostics$reports)
  reports_id <- add_section("reports", "Reports", "report")
  if (nrow(reports) > 0L) {
    for (i in seq_len(nrow(reports))) {
      name <- dashboard_cell(reports[i, , drop = FALSE], "name", default = paste("Report", i))
      type <- dashboard_cell(reports[i, , drop = FALSE], "type", default = "html_report")
      path <- dashboard_cell(reports[i, , drop = FALSE], "path", default = NA_character_)
      status <- if ("output_exists" %in% names(reports) && isTRUE(reports$output_exists[[i]])) "rendered" else "pending"
      add_child(reports_id, paste0("report:", dashboard_sanitise_stem(name, fallback = paste0("report_", i))), name, "report", 2L, type = type, section = "reports", status = status, path = path)
    }
  }

  outputs <- dashboard_safe_data_frame(diagnostics$outputs)
  outputs_id <- add_section("outputs", "Outputs", "output")
  if (nrow(outputs) > 0L) {
    for (i in seq_len(nrow(outputs))) {
      name <- dashboard_cell(outputs[i, , drop = FALSE], "name", default = paste("Output", i))
      type <- dashboard_cell(outputs[i, , drop = FALSE], "type", default = "output")
      path <- dashboard_cell(outputs[i, , drop = FALSE], "path", default = NA_character_)
      status <- if ("exists" %in% names(outputs) && isTRUE(outputs$exists[[i]])) "exists" else "missing"
      add_child(outputs_id, paste0("output:", dashboard_sanitise_stem(name, fallback = paste0("output_", i))), name, "output", 2L, type = type, section = "outputs", status = status, path = path)
    }
  }

  governance_id <- add_section("governance", "Governance", "governance")
  governance <- diagnostics$governance %||% list()
  governance_specs <- list(
    tasks = list(label = "Tasks", type = "task"),
    milestones = list(label = "Milestones", type = "milestone"),
    risks = list(label = "Risks", type = "risk"),
    decisions = list(label = "Decisions", type = "decision")
  )
  for (name in names(governance_specs)) {
    data <- dashboard_safe_data_frame(governance[[name]])
    if (nrow(data) == 0L) next
    spec <- governance_specs[[name]]
    group_id <- paste0("governance:", name)
    add_child(governance_id, group_id, spec$label, spec$type, 2L, type = paste0(spec$type, "_group"), section = "governance")
    for (i in seq_len(nrow(data))) {
      label <- dashboard_cell(data[i, , drop = FALSE], "title", default = paste(spec$label, i))
      id <- dashboard_cell(data[i, , drop = FALSE], "id", default = paste0(name, "_", i))
      status <- dashboard_cell(data[i, , drop = FALSE], "status", default = NA_character_)
      add_child(group_id, paste0(spec$type, ":", dashboard_sanitise_stem(id, fallback = paste0(spec$type, "_", i))), label, spec$type, 3L, type = spec$type, section = "governance", status = status)
    }
  }

  data_sources <- dashboard_safe_data_frame(diagnostics$data_sources)
  data_sources_id <- add_section("data_sources", "Data sources", "data_source")
  if (nrow(data_sources) > 0L) {
    for (i in seq_len(nrow(data_sources))) {
      name <- dashboard_cell(data_sources[i, , drop = FALSE], "name", default = paste("Data source", i))
      path <- dashboard_cell(data_sources[i, , drop = FALSE], "path", default = NA_character_)
      status <- if ("readable" %in% names(data_sources) && isTRUE(data_sources$readable[[i]])) "available" else "unavailable"
      add_child(data_sources_id, paste0("data_source:", dashboard_sanitise_stem(name, fallback = paste0("data_source_", i))), name, "data_source", 2L, type = "data_source", section = "data_sources", status = status, path = path)
    }
  }

  if (length(nodes) == 0L) {
    return(dashboard_make_wbs_empty())
  }
  nodes_df <- unique(do.call(rbind, unname(nodes)))
  edges_df <- if (length(edges) == 0L) {
    data.frame(from = character(), to = character(), edge_type = character(), arrows = character(), stringsAsFactors = FALSE)
  } else {
    unique(do.call(rbind, edges))
  }

  # Remove empty section nodes except the root and explicitly selected branch roots.
  child_counts <- table(as.character(edges_df$from))
  empty_sections <- nodes_df$id[nodes_df$type == "section" & !nodes_df$id %in% names(child_counts)]
  empty_sections <- setdiff(empty_sections, "project:root")
  if (length(empty_sections) > 0L) {
    nodes_df <- nodes_df[!nodes_df$id %in% empty_sections, , drop = FALSE]
    edges_df <- edges_df[!edges_df$from %in% empty_sections & !edges_df$to %in% empty_sections, , drop = FALSE]
  }

  filtered <- dashboard_filter_wbs_branch(nodes_df, edges_df, selected = selected)
  limited <- dashboard_limit_chart_nodes(
    filtered$nodes,
    filtered$edges,
    max_items = max_items,
    always_keep = unique(c("project:root", filtered$selected_root_id, filtered$nodes$id[filtered$nodes$level <= 1L]))
  )
  list(nodes = limited$nodes, edges = limited$edges, selected_root_id = filtered$selected_root_id)
}

dashboard_pert_empty <- function(selected_root_id = NA_character_) {
  list(
    nodes = data.frame(id = character(), label = character(), group = character(), type = character(), status = character(), path = character(), title = character(), external = logical(), stringsAsFactors = FALSE),
    edges = data.frame(from = character(), to = character(), relationship = character(), edge_type = character(), arrows = character(), title = character(), stringsAsFactors = FALSE),
    selected_root_id = selected_root_id
  )
}

dashboard_edge_type_from_relationship <- function(relationship) {
  relationship <- tolower(as.character(relationship %||% "depends_on"))
  out <- rep("depends_on", length(relationship))
  out[grepl("contain|group", relationship)] <- "contains"
  out[grepl("block", relationship)] <- "blocks"
  out[grepl("generate|script_to_output", relationship)] <- "generates"
  out[grepl("input|uses|output_to_report", relationship)] <- "uses"
  out[grepl("task_to|milestone_to|risk_to|decision_to|report_to", relationship)] <- "depends_on"
  out
}

dashboard_filter_pert_branch <- function(nodes, edges, selected = "all", include_external_dependencies = FALSE) {
  nodes <- dashboard_safe_data_frame(nodes)
  edges <- dashboard_safe_data_frame(edges)
  selected_root_id <- NA_character_
  key <- dashboard_chart_branch_key(selected)
  if (identical(key, "all") || nrow(nodes) == 0L) {
    return(list(nodes = nodes, edges = edges, selected_root_id = selected_root_id))
  }

  aliases <- dashboard_chart_branch_aliases(key)
  matched <- logical(nrow(nodes))
  if ("type" %in% names(nodes)) matched <- matched | tolower(as.character(nodes$type)) %in% aliases
  if ("group" %in% names(nodes)) matched <- matched | tolower(as.character(nodes$group)) %in% aliases
  if ("label" %in% names(nodes)) matched <- matched | vapply(nodes$label, dashboard_chart_branch_key, character(1)) %in% aliases
  matched <- matched | as.character(nodes$id) %in% as.character(selected)

  base_ids <- as.character(nodes$id[matched])
  base_ids <- base_ids[!is.na(base_ids) & nzchar(base_ids)]
  if (length(base_ids) == 0L) {
    return(list(nodes = nodes, edges = edges, selected_root_id = selected_root_id))
  }
  selected_root_id <- base_ids[[1]]

  keep_ids <- base_ids
  if (isTRUE(include_external_dependencies) && nrow(edges) > 0L) {
    neighbours <- unique(c(
      as.character(edges$from[as.character(edges$to) %in% base_ids]),
      as.character(edges$to[as.character(edges$from) %in% base_ids])
    ))
    keep_ids <- unique(c(keep_ids, neighbours))
  }

  nodes$external <- !as.character(nodes$id) %in% base_ids
  nodes <- nodes[as.character(nodes$id) %in% keep_ids, , drop = FALSE]
  edges <- edges[as.character(edges$from) %in% keep_ids & as.character(edges$to) %in% keep_ids, , drop = FALSE]
  list(nodes = nodes, edges = edges, selected_root_id = selected_root_id)
}

dashboard_pert_data <- function(diagnostics, network = diagnostics$network, max_items = 24L, selected = "all", include_external_dependencies = FALSE) {
  nodes <- dashboard_safe_data_frame(network$nodes)
  edges <- dashboard_safe_data_frame(network$edges)
  if (nrow(nodes) == 0L) {
    return(dashboard_pert_empty())
  }

  if (!"id" %in% names(nodes)) nodes$id <- seq_len(nrow(nodes))
  if (!"label" %in% names(nodes)) nodes$label <- nodes$id
  if (!"type" %in% names(nodes)) nodes$type <- "object"
  if (!"status" %in% names(nodes)) nodes$status <- NA_character_
  if (!"path" %in% names(nodes)) nodes$path <- NA_character_

  nodes$id <- as.character(nodes$id)
  nodes$label <- as.character(nodes$label)
  nodes$type <- as.character(nodes$type)
  nodes$group <- nodes$type
  nodes$title <- paste0(nodes$label, "\nType: ", nodes$type, ifelse(is.na(nodes$status) | !nzchar(nodes$status), "", paste0("\nStatus: ", nodes$status)))
  nodes$external <- FALSE

  if (nrow(edges) == 0L || !all(c("from", "to") %in% names(edges))) {
    edges <- data.frame(from = character(), to = character(), relationship = character(), stringsAsFactors = FALSE)
  }
  if (!"relationship" %in% names(edges)) edges$relationship <- "depends_on"
  edges$from <- as.character(edges$from)
  edges$to <- as.character(edges$to)
  edges$relationship <- as.character(edges$relationship)
  edges$edge_type <- dashboard_edge_type_from_relationship(edges$relationship)
  edges$arrows <- "to"
  edges$title <- edges$relationship

  # PERT/dependency maps should not use WBS containment edges by default.
  edges <- edges[!identical(nrow(edges), 0L) & edges$edge_type != "contains", , drop = FALSE]

  filtered <- dashboard_filter_pert_branch(
    nodes,
    edges,
    selected = selected,
    include_external_dependencies = include_external_dependencies
  )
  nodes <- filtered$nodes
  edges <- filtered$edges

  if (nrow(edges) > 0L) {
    keep_connected <- unique(c(edges$from, edges$to))
    if (dashboard_chart_branch_key(selected) == "all") {
      nodes <- nodes[nodes$id %in% keep_connected, , drop = FALSE]
    }
  }

  limited <- dashboard_limit_chart_nodes(
    nodes,
    edges,
    max_items = max_items,
    always_keep = unique(c(filtered$selected_root_id, nodes$id[nodes$external %in% TRUE]))
  )
  list(nodes = limited$nodes, edges = limited$edges, selected_root_id = filtered$selected_root_id)
}

dashboard_wbs_visnetwork <- function(data, height = "520px") {
  data <- data %||% dashboard_make_wbs_empty()
  nodes <- dashboard_safe_data_frame(data$nodes)
  edges <- dashboard_safe_data_frame(data$edges)
  if (nrow(nodes) == 0L) {
    nodes <- data.frame(id = "empty", label = "No WBS data", group = "empty", level = 0L, title = "No project objects are available for the Work Breakdown Structure.", stringsAsFactors = FALSE)
    edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  }
  if (!"title" %in% names(nodes)) nodes$title <- nodes$label
  if (!"group" %in% names(nodes)) nodes$group <- nodes$type %||% "object"
  if (!"level" %in% names(nodes)) nodes$level <- 1L
  if (nrow(edges) > 0L) edges$arrows <- ""

  graph <- visNetwork::visNetwork(nodes = nodes, edges = edges, height = height)
  graph <- visNetwork::visHierarchicalLayout(graph, direction = "UD", sortMethod = "directed", levelSeparation = 105, nodeSpacing = 145)
  graph <- visNetwork::visInteraction(graph, navigationButtons = TRUE, zoomView = TRUE, dragView = TRUE)
  graph <- visNetwork::visOptions(graph, highlightNearest = TRUE, nodesIdSelection = TRUE)
  visNetwork::visPhysics(graph, enabled = FALSE)
}

dashboard_pert_visnetwork <- function(data, height = "520px") {
  data <- data %||% dashboard_pert_empty()
  nodes <- dashboard_safe_data_frame(data$nodes)
  edges <- dashboard_safe_data_frame(data$edges)
  if (nrow(nodes) == 0L) {
    nodes <- data.frame(id = "empty", label = "No dependency data", group = "empty", title = "No dependency edges are available. Link scripts, reports, outputs and governance items to build this map.", stringsAsFactors = FALSE)
    edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  }
  if (!"title" %in% names(nodes)) nodes$title <- nodes$label
  if (!"group" %in% names(nodes)) nodes$group <- nodes$type %||% "object"
  if ("external" %in% names(nodes)) {
    nodes$color.background <- ifelse(nodes$external, "#f8fafc", NA_character_)
    nodes$color.border <- ifelse(nodes$external, "#cbd5e1", NA_character_)
  }
  if (nrow(edges) > 0L) edges$arrows <- "to"

  graph <- visNetwork::visNetwork(nodes = nodes, edges = edges, height = height)
  graph <- visNetwork::visEdges(graph, arrows = "to", smooth = TRUE)
  graph <- visNetwork::visInteraction(graph, navigationButtons = TRUE, zoomView = TRUE, dragView = TRUE)
  graph <- visNetwork::visOptions(graph, highlightNearest = TRUE, nodesIdSelection = TRUE)
  visNetwork::visPhysics(graph, stabilization = TRUE)
}

dashboard_plot_wbs <- function(data, max_items = NULL) {
  if (!is.null(data$project) || !is.null(data$governance)) {
    data <- dashboard_wbs_data(data, max_items = max_items %||% 20L)
  }
  nodes <- dashboard_safe_data_frame(data$nodes)
  edges <- dashboard_safe_data_frame(data$edges)
  if (nrow(nodes) == 0L) {
    return(dashboard_plot_empty("No project objects are available for WBS."))
  }

  levels <- sort(unique(nodes$level))
  old_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = old_mar), add = TRUE)
  graphics::par(mar = c(1, 1, 4, 1))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::title("Work Breakdown Structure")

  positions <- data.frame(id = nodes$id, x = NA_real_, y = NA_real_, stringsAsFactors = FALSE)
  for (level in levels) {
    idx <- which(nodes$level == level)
    positions$x[idx] <- seq(0.10, 0.90, length.out = length(idx))
    positions$y[idx] <- 0.92 - (match(level, levels) - 1L) * min(0.20, 0.74 / max(1L, length(levels) - 1L))
  }
  lookup <- stats::setNames(seq_len(nrow(positions)), positions$id)
  if (nrow(edges) > 0L) {
    for (i in seq_len(nrow(edges))) {
      from_key <- as.character(edges$from[[i]])
      to_key <- as.character(edges$to[[i]])
      from <- if (!is.na(from_key) && from_key %in% names(lookup)) lookup[[from_key]] else NA_integer_
      to <- if (!is.na(to_key) && to_key %in% names(lookup)) lookup[[to_key]] else NA_integer_
      if (is.na(from) || is.na(to)) next
      graphics::segments(positions$x[[from]], positions$y[[from]] - 0.025, positions$x[[to]], positions$y[[to]] + 0.025, col = "grey55")
    }
  }
  for (i in seq_len(nrow(nodes))) {
    graphics::rect(positions$x[[i]] - 0.075, positions$y[[i]] - 0.025, positions$x[[i]] + 0.075, positions$y[[i]] + 0.025, lwd = if (nodes$level[[i]] <= 1L) 1.3 else 1)
    graphics::text(positions$x[[i]], positions$y[[i]], dashboard_chart_label(nodes$label[[i]], 17L), cex = if (nodes$level[[i]] <= 1L) 0.66 else 0.55, font = if (nodes$level[[i]] <= 1L) 2 else 1)
  }
  invisible(data)
}

dashboard_plot_pert <- function(data, network = NULL, max_items = NULL) {
  if (!is.null(data$project) || !is.null(data$governance)) {
    data <- dashboard_pert_data(data, network = network %||% data$network, max_items = max_items %||% 24L)
  }
  nodes <- dashboard_safe_data_frame(data$nodes)
  edges <- dashboard_safe_data_frame(data$edges)
  if (nrow(nodes) == 0L || nrow(edges) == 0L || !all(c("from", "to") %in% names(edges))) {
    return(dashboard_plot_empty("No dependency edges are available. Link tasks, scripts, reports and outputs to build this map."))
  }

  type_order <- c("component", "deliverable", "script", "task", "milestone", "report", "output", "table", "figure", "risk", "decision", "data_source", "object")
  node_types <- as.character(nodes$type %||% nodes$group %||% "object")
  node_types[is.na(node_types) | !nzchar(node_types)] <- "object"
  ordered_types <- intersect(type_order, unique(node_types))
  ordered_types <- c(ordered_types, setdiff(unique(node_types), ordered_types))
  x_by_type <- stats::setNames(seq(0.10, 0.90, length.out = length(ordered_types)), ordered_types)
  nodes$x <- x_by_type[node_types]
  nodes$y <- NA_real_
  for (type in ordered_types) {
    idx <- which(node_types == type)
    nodes$y[idx] <- seq(0.84, 0.16, length.out = length(idx))
  }

  old_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = old_mar), add = TRUE)
  graphics::par(mar = c(1, 1, 4, 1))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::title("Dependency map")
  graphics::text(x_by_type, 0.94, names(x_by_type), font = 2, cex = 0.68)
  graphics::abline(v = x_by_type, lty = 3, col = "grey90")

  lookup <- stats::setNames(seq_len(nrow(nodes)), nodes$id)
  for (i in seq_len(nrow(edges))) {
    from_key <- as.character(edges$from[[i]])
    to_key <- as.character(edges$to[[i]])
    from <- if (!is.na(from_key) && from_key %in% names(lookup)) lookup[[from_key]] else NA_integer_
    to <- if (!is.na(to_key) && to_key %in% names(lookup)) lookup[[to_key]] else NA_integer_
    if (is.na(from) || is.na(to)) next
    graphics::arrows(nodes$x[[from]], nodes$y[[from]], nodes$x[[to]], nodes$y[[to]], length = 0.06, angle = 20, lwd = 1.1, col = "grey45")
  }
  for (i in seq_len(nrow(nodes))) {
    graphics::points(nodes$x[[i]], nodes$y[[i]], pch = 21, bg = "white", cex = 1.45, lwd = 1.1)
    graphics::text(nodes$x[[i]], nodes$y[[i]] - 0.035, dashboard_chart_label(nodes$label[[i]], 15L), cex = 0.55)
  }
  invisible(data)
}
