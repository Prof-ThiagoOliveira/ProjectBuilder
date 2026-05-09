mod_tasks_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dashboard_card(
      "Task board",
      subtitle = "Create, filter, edit and close project tasks. Use linked objects to connect work items to scripts, reports or outputs.",
      shiny::fluidRow(
        shiny::column(3, shiny::uiOutput(ns("open_box"))),
        shiny::column(3, shiny::uiOutput(ns("overdue_box"))),
        shiny::column(3, shiny::uiOutput(ns("blocked_box"))),
        shiny::column(3, shiny::uiOutput(ns("done_box")))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        dashboard_form_card(
          "Add task",
          subtitle = "Record a discrete unit of work with ownership, priority and due date.",
          shiny::textInput(ns("task_title"), "Title", placeholder = "e.g. Fit final mixed model"),
          shiny::textAreaInput(ns("task_description"), "Description", rows = 3),
          shiny::selectInput(ns("task_status"), "Status", choices = governance_status_levels("task"), selected = "todo"),
          shiny::selectInput(ns("task_priority"), "Priority", choices = task_priority_levels(), selected = "medium"),
          shiny::dateInput(ns("task_due_date"), "Due date", value = NULL),
          shiny::textInput(ns("task_assigned_to"), "Assigned to", placeholder = "Owner or role"),
          shiny::selectizeInput(ns("task_linked_objects"), "Linked objects", choices = NULL, multiple = TRUE, options = list(placeholder = "Search scripts, reports, outputs or governance records")),
          if (manage) shiny::actionButton(ns("add_task"), "Add task", class = "btn-primary") else dashboard_empty_state("Read-only diagnostic mode.")
        ),
        dashboard_card(
          "Task charts",
          subtitle = "Status and priority distribution for rapid workload review.",
          shiny::plotOutput(ns("task_status_plot"), height = "220px"),
          shiny::plotOutput(ns("task_priority_plot"), height = "220px")
        )
      ),
      shiny::column(
        width = 8,
        dashboard_card(
          "Tasks",
          subtitle = "Select a row to edit status, priority, ownership, due date or linked objects.",
          shiny::fluidRow(
            shiny::column(4, shiny::selectInput(ns("status_filter"), "Status", choices = c("All", governance_status_levels("task")), selected = "All")),
            shiny::column(4, shiny::selectInput(ns("priority_filter"), "Priority", choices = c("All", task_priority_levels()), selected = "All")),
            shiny::column(4, shiny::textInput(ns("task_search"), "Search", placeholder = "Title, owner, linked object"))
          ),
          dashboard_table_ui(ns("tasks_table"))
        ),
        dashboard_form_card(
          "Selected task editor",
          subtitle = "Use this panel to update the selected task without editing the YAML file manually.",
          shiny::uiOutput(ns("selected_task_label")),
          shiny::fluidRow(
            shiny::column(6, shiny::textInput(ns("edit_title"), "Title")),
            shiny::column(3, shiny::selectInput(ns("edit_status"), "Status", choices = governance_status_levels("task"))),
            shiny::column(3, shiny::selectInput(ns("edit_priority"), "Priority", choices = task_priority_levels()))
          ),
          shiny::fluidRow(
            shiny::column(4, shiny::dateInput(ns("edit_due_date"), "Due date", value = NULL)),
            shiny::column(4, shiny::textInput(ns("edit_assigned_to"), "Assigned to")),
            shiny::column(4, shiny::selectizeInput(ns("edit_linked_objects"), "Linked objects", choices = NULL, multiple = TRUE, options = list(placeholder = "Search project objects")))
          ),
          shiny::textAreaInput(ns("edit_description"), "Description", rows = 3),
          shiny::div(
            class = "projflow-actions",
            if (manage) shiny::actionButton(ns("update_task"), "Save task changes", class = "btn-primary"),
            if (manage) shiny::actionButton(ns("mark_done"), "Mark done", class = "btn-outline-success"),
            if (manage) shiny::actionButton(ns("remove_task"), "Remove task", class = "btn-outline-danger") else dashboard_empty_state("Read-only diagnostic mode.")
          )
        )
      )
    )
  )
}

mod_tasks_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    raw_task_data <- shiny::reactive({
      dashboard_safe_data_frame(dashboard_diagnostics(state)$governance$tasks)
    })

    linked_object_choices <- shiny::reactive({
      dashboard_linked_object_choices(state)
    })

    shiny::observeEvent(linked_object_choices(), {
      choices <- linked_object_choices()
      dashboard_update_linked_objects(session, "task_linked_objects", choices, shiny::isolate(input$task_linked_objects))
      dashboard_update_linked_objects(session, "edit_linked_objects", choices, shiny::isolate(input$edit_linked_objects))
    }, ignoreInit = FALSE)

    task_data <- shiny::reactive({
      tasks <- raw_task_data()
      tasks <- dashboard_filter_data(tasks, "status", input$status_filter)
      tasks <- dashboard_filter_data(tasks, "priority", input$priority_filter)
      query <- tolower(dashboard_trim_text(input$task_search))
      if (nzchar(query) && nrow(tasks) > 0L) {
        searchable <- apply(tasks, 1L, function(row) paste(row, collapse = " "))
        tasks <- tasks[grepl(query, tolower(searchable), fixed = TRUE), , drop = FALSE]
      }
      tasks
    })

    dashboard_render_table(output, "tasks_table", task_data)

    output$open_box <- shiny::renderUI({
      dashboard_value_box("Open", dashboard_summary_value(state, "open_tasks", 0L), "primary", note = "Backlog, todo, in progress or blocked.")
    })
    output$overdue_box <- shiny::renderUI({
      value <- dashboard_summary_value(state, "overdue_tasks", 0L)
      dashboard_value_box("Overdue", value, if (as.integer(value) > 0L) "warning" else "success", note = "Tasks past due date.")
    })
    output$blocked_box <- shiny::renderUI({
      tasks <- raw_task_data()
      value <- if ("status" %in% names(tasks)) sum(tasks$status == "blocked", na.rm = TRUE) else 0L
      dashboard_value_box("Blocked", value, if (value > 0L) "danger" else "success", note = "Needs external action.")
    })
    output$done_box <- shiny::renderUI({
      tasks <- raw_task_data()
      value <- if ("status" %in% names(tasks)) sum(tasks$status == "done", na.rm = TRUE) else 0L
      dashboard_value_box("Done", value, "success", note = "Completed tasks.")
    })

    output$task_status_plot <- shiny::renderPlot({
      dashboard_plot_categorical_counts(raw_task_data(), "status", title = "Tasks by status")
    })
    output$task_priority_plot <- shiny::renderPlot({
      dashboard_plot_categorical_counts(raw_task_data(), "priority", title = "Tasks by priority")
    })

    selected_task_row <- shiny::reactive({
      dashboard_selected_row(input, "tasks_table", task_data())
    })

    selected_task_id <- shiny::reactive({
      task <- selected_task_row()
      if (is.null(task) || !"id" %in% names(task)) {
        return(NULL)
      }
      task$id[[1]]
    })

    output$selected_task_label <- shiny::renderUI({
      task <- selected_task_row()
      if (is.null(task)) {
        return(dashboard_empty_state("Select a task row to edit its status, priority, due date, ownership and description."))
      }
      shiny::div(
        class = "projflow-selected-record",
        shiny::strong("Selected task: "),
        shiny::span(task$id[[1]] %||% ""),
        shiny::span(" - "),
        shiny::span(task$title[[1]] %||% "")
      )
    })

    shiny::observeEvent(selected_task_row(), {
      task <- selected_task_row()
      if (is.null(task)) {
        return(invisible(NULL))
      }
      shiny::updateTextInput(session, "edit_title", value = dashboard_cell(task, "title"))
      shiny::updateSelectInput(session, "edit_status", selected = dashboard_choice_or_default(dashboard_cell(task, "status"), governance_status_levels("task"), "todo"))
      shiny::updateSelectInput(session, "edit_priority", selected = dashboard_choice_or_default(dashboard_cell(task, "priority"), task_priority_levels(), "medium"))
      shiny::updateDateInput(session, "edit_due_date", value = dashboard_date_or_null(dashboard_cell(task, "due_date")))
      shiny::updateTextInput(session, "edit_assigned_to", value = dashboard_cell(task, "assigned_to"))
      dashboard_update_linked_objects(session, "edit_linked_objects", linked_object_choices(), dashboard_cell(task, "linked_objects"))
      shiny::updateTextAreaInput(session, "edit_description", value = dashboard_cell(task, "description"))
    }, ignoreInit = TRUE)

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$add_task, {
      dashboard_run_action(
        state = state,
        session = session,
        label = "Task creation",
        expr = add_project_task(
          title = dashboard_required_text(input$task_title, "Task title"),
          root = dashboard_root(state),
          description = dashboard_optional_text(input$task_description),
          status = input$task_status,
          priority = input$task_priority,
          due_date = dashboard_optional_date(input$task_due_date),
          assigned_to = dashboard_optional_text(input$task_assigned_to),
          linked_objects = dashboard_split_values(input$task_linked_objects)
        )
      )
    })

    shiny::observeEvent(input$update_task, {
      shiny::req(selected_task_id())
      dashboard_run_action(
        state = state,
        session = session,
        label = "Task update",
        expr = update_project_task(
          task = selected_task_id(),
          root = dashboard_root(state),
          title = dashboard_required_text(input$edit_title, "Task title"),
          description = input$edit_description %||% "",
          status = input$edit_status,
          priority = input$edit_priority,
          due_date = dashboard_date_update_value(input$edit_due_date),
          assigned_to = input$edit_assigned_to %||% "",
          linked_objects = dashboard_split_values(input$edit_linked_objects)
        )
      )
    })

    shiny::observeEvent(input$mark_done, {
      shiny::req(selected_task_id())
      dashboard_run_action(
        state = state,
        session = session,
        label = "Task update",
        expr = mark_project_task_done(selected_task_id(), root = dashboard_root(state))
      )
    })

    shiny::observeEvent(input$remove_task, {
      shiny::req(selected_task_id())
      dashboard_run_action(
        state = state,
        session = session,
        label = "Task removal",
        expr = remove_project_task(selected_task_id(), root = dashboard_root(state))
      )
    })
  })
}
