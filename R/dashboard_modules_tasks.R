mod_tasks_ui <- function(id, manage = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::textInput(ns("task_title"), "Title"),
        shiny::textAreaInput(ns("task_description"), "Description", rows = 4),
        shiny::selectInput(ns("task_status"), "Status", choices = governance_status_levels("task"), selected = "todo"),
        shiny::selectInput(ns("task_priority"), "Priority", choices = task_priority_levels(), selected = "medium"),
        shiny::dateInput(ns("task_due_date"), "Due date", value = NULL),
        shiny::textInput(ns("task_assigned_to"), "Assigned to"),
        shiny::textInput(ns("task_linked_objects"), "Linked objects", placeholder = "script_name, report_name"),
        if (manage) shiny::actionButton(ns("add_task"), "Add task")
      ),
      shiny::column(
        width = 8,
        shiny::div(
          class = "projflow-actions",
          if (manage) shiny::actionButton(ns("mark_done"), "Mark selected task done"),
          if (manage) shiny::actionButton(ns("remove_task"), "Remove selected task")
        ),
        dashboard_table_ui(ns("tasks_table"))
      )
    )
  )
}

mod_tasks_server <- function(id, state, manage = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    task_data <- shiny::reactive({
      state$diagnostics$governance$tasks
    })

    dashboard_render_table(output, "tasks_table", task_data)

    if (!isTRUE(manage)) {
      return(invisible(NULL))
    }

    shiny::observeEvent(input$add_task, {
      linked <- trimws(strsplit(input$task_linked_objects %||% "", ",", fixed = TRUE)[[1]])
      linked <- linked[nzchar(linked)]
      dashboard_run_action(
        state = state,
        session = session,
        label = "Task creation",
        expr = add_project_task(
          title = input$task_title,
          root = state$root,
          description = input$task_description,
          status = input$task_status,
          priority = input$task_priority,
          due_date = if (is.null(input$task_due_date) || is.na(input$task_due_date)) NULL else as.character(input$task_due_date),
          assigned_to = input$task_assigned_to,
          linked_objects = linked
        )
      )
    })

    selected_task <- shiny::reactive({
      sel <- input$tasks_table_rows_selected
      tasks <- task_data()
      if (length(sel) != 1L || nrow(tasks) < sel[[1]]) {
        return(NULL)
      }
      tasks$id[[sel[[1]]]]
    })

    shiny::observeEvent(input$mark_done, {
      shiny::req(selected_task())
      dashboard_run_action(
        state = state,
        session = session,
        label = "Task update",
        expr = mark_project_task_done(selected_task(), root = state$root)
      )
    })

    shiny::observeEvent(input$remove_task, {
      shiny::req(selected_task())
      dashboard_run_action(
        state = state,
        session = session,
        label = "Task removal",
        expr = remove_project_task(selected_task(), root = state$root)
      )
    })
  })
}
