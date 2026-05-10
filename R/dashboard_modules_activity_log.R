dashboard_activity_log_data <- function(activity) {
  activity <- dashboard_safe_data_frame(activity)
  if (nrow(activity) == 0L) {
    return(activity)
  }

  if (!"entry" %in% names(activity)) {
    activity$entry <- seq_len(nrow(activity))
  }
  if (!"timestamp" %in% names(activity)) {
    activity$timestamp <- NA_character_
  }
  if (!"summary" %in% names(activity)) {
    activity$summary <- paste(activity$action %||% "", activity$object_name %||% "")
  }
  if (!"details" %in% names(activity)) {
    activity$details <- NA_character_
  }
  if (!"recommendation" %in% names(activity)) {
    activity$recommendation <- "Review this event and confirm whether a project check, rebuild or registry update is needed."
  }
  if (!"affected_path" %in% names(activity)) {
    activity$affected_path <- NA_character_
  }
  if (!"status" %in% names(activity)) {
    activity$status <- "recorded"
  }

  time <- suppressWarnings(as.POSIXct(activity$timestamp, tz = "UTC"))
  order_idx <- order(time, activity$entry, decreasing = TRUE, na.last = TRUE)
  activity[order_idx, , drop = FALSE]
}

dashboard_activity_log_table_data <- function(activity) {
  activity <- dashboard_activity_log_data(activity)
  keep <- intersect(
    c("entry", "timestamp", "action_label", "object_type", "object_name", "affected_path", "status", "source", "summary"),
    names(activity)
  )
  activity[, keep, drop = FALSE]
}

dashboard_activity_value <- function(row, name, default = "Not recorded") {
  if (is.null(row) || !name %in% names(row)) {
    return(default)
  }
  value <- row[[name]][[1]]
  if (is.na(value) || !nzchar(as.character(value))) default else as.character(value)
}

dashboard_activity_detail_field <- function(label, value) {
  shiny::div(
    class = "projflow-log-detail-field",
    shiny::div(class = "projflow-log-detail-label", label),
    shiny::div(class = "projflow-log-detail-value", value)
  )
}

dashboard_activity_detail_ui <- function(row) {
  if (is.null(row) || nrow(row) == 0L) {
    return(dashboard_empty_state("Select an activity-log row to inspect the complete audit record."))
  }

  details <- dashboard_activity_value(row, "details", default = "No additional metadata were recorded for this event.")
  recommendation <- dashboard_activity_value(row, "recommendation", default = "Review the event and decide whether a check, rebuild or registry update is needed.")
  title <- dashboard_activity_value(row, "action_label", default = dashboard_activity_value(row, "action", "Activity event"))

  shiny::div(
    class = "projflow-log-detail",
    shiny::h4(title),
    shiny::div(
      class = "projflow-log-detail-grid",
      dashboard_activity_detail_field("Entry", dashboard_activity_value(row, "entry")),
      dashboard_activity_detail_field("Timestamp", dashboard_activity_value(row, "timestamp")),
      dashboard_activity_detail_field("Status", dashboard_activity_value(row, "status")),
      dashboard_activity_detail_field("Source", dashboard_activity_value(row, "source")),
      dashboard_activity_detail_field("Object type", dashboard_activity_value(row, "object_type")),
      dashboard_activity_detail_field("Object ID", dashboard_activity_value(row, "object_id")),
      dashboard_activity_detail_field("Object name", dashboard_activity_value(row, "object_name")),
      dashboard_activity_detail_field("Affected path", dashboard_activity_value(row, "affected_path"))
    ),
    shiny::div(
      class = "projflow-log-detail-section",
      shiny::h5("Summary"),
      shiny::p(dashboard_activity_value(row, "summary", default = "No summary was recorded."))
    ),
    shiny::div(
      class = "projflow-log-detail-section",
      shiny::h5("Recommended next action"),
      shiny::p(recommendation)
    ),
    shiny::div(
      class = "projflow-log-detail-section",
      shiny::h5("Full recorded metadata"),
      shiny::tags$pre(class = "projflow-log-details-pre", details)
    )
  )
}

mod_activity_log_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 8,
        dashboard_card(
          "Activity log",
          subtitle = "Audit trail of dashboard, governance and registry actions. Select a row to inspect the full event record.",
          actions = if (manage) shiny::actionButton(ns("clear_log"), "Clear activity log", class = "btn-outline-danger"),
          dashboard_table_ui(ns("activity_table"))
        )
      ),
      shiny::column(
        width = 4,
        dashboard_card(
          "Selected activity record",
          subtitle = "Operational context, metadata and suggested follow-up for the selected event.",
          shiny::uiOutput(ns("activity_detail"))
        )
      )
    )
  )
}

mod_activity_log_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    activity_data <- shiny::reactive(dashboard_activity_log_data(dashboard_diagnostics(state)$activity))
    activity_table_data <- shiny::reactive(dashboard_activity_log_table_data(activity_data()))
    dashboard_render_table(output, "activity_table", activity_table_data, selection = "single", page_length = 15)

    output$activity_detail <- shiny::renderUI({
      data <- activity_data()
      if (nrow(data) == 0L) {
        return(dashboard_empty_state("No activity has been recorded for this project yet."))
      }

      selected <- input$activity_table_rows_selected
      if (is.null(selected) || length(selected) == 0L) {
        selected <- 1L
      }
      selected <- selected[[1]]
      if (is.na(selected) || selected < 1L || selected > nrow(data)) {
        return(dashboard_empty_state("Select an activity-log row to inspect the complete audit record."))
      }

      dashboard_activity_detail_ui(data[selected, , drop = FALSE])
    })

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$clear_log, {
      dashboard_run_action(
        state,
        session,
        "Activity log clear",
        clear_project_activity(root = dashboard_root(state), confirm = TRUE)
      )
    })
  })
}

mod_settings_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        dashboard_card(
          "Project metadata",
          subtitle = "Core diagnostic metadata for the active project.",
          shiny::verbatimTextOutput(ns("project_settings"))
        )
      ),
      shiny::column(
        width = 6,
        dashboard_card(
          "Backups",
          subtitle = "Available registry, local configuration and governance backups.",
          shiny::verbatimTextOutput(ns("backup_settings"))
        )
      )
    )
  )
}

mod_settings_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    output$project_settings <- shiny::renderPrint({
      diagnostics <- dashboard_diagnostics(state)
      list(
        project = diagnostics$project,
        summary = diagnostics$summary
      )
    })

    output$backup_settings <- shiny::renderPrint({
      state$refresh_token
      list_project_backups(dashboard_root(state))
    })
  })
}
