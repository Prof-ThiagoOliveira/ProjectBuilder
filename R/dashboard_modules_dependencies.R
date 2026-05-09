mod_dependencies_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    dashboard_card(
      "Packages and files",
      subtitle = "Review package availability, registered files, workflow files requiring review, and support files that normally do not need registry entries.",
      shiny::fluidRow(
        shiny::column(3, shiny::uiOutput(ns("packages_box"))),
        shiny::column(3, shiny::uiOutput(ns("missing_packages_box"))),
        shiny::column(3, shiny::uiOutput(ns("registered_files_box"))),
        shiny::column(3, shiny::uiOutput(ns("review_files_box")))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        dashboard_card(
          "Package availability",
          subtitle = "Packages declared by the project and whether they are installed in the current R library.",
          dashboard_table_ui(ns("packages_table"))
        )
      ),
      shiny::column(
        width = 6,
        dashboard_card(
          "Registered files",
          subtitle = "Files explicitly tracked by scripts, reports, outputs and projflow metadata.",
          dashboard_table_ui(ns("files_table"))
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        dashboard_card(
          "Workflow files to review",
          subtitle = "Files in analysis, reports, outputs, app, dashboard or data areas that are not currently represented in the registry. These are candidates for registration only when they are part of the reproducible workflow.",
          dashboard_table_ui(ns("orphan_table"))
        )
      ),
      shiny::column(
        width = 6,
        dashboard_card(
          "Support files not requiring registry entries",
          subtitle = "Common project documentation and configuration files. These are intentionally not counted as missing registry records.",
          dashboard_table_ui(ns("support_table"))
        )
      )
    )
  )
}

mod_dependencies_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    package_data <- shiny::reactive(dashboard_safe_data_frame(dashboard_diagnostics(state)$packages))
    file_data <- shiny::reactive(dashboard_safe_data_frame(dashboard_diagnostics(state)$files))
    orphan_data <- shiny::reactive(dashboard_safe_data_frame(dashboard_diagnostics(state)$orphan_files))
    support_data <- shiny::reactive(dashboard_safe_data_frame(dashboard_diagnostics(state)$support_files))

    dashboard_render_table(output, "packages_table", package_data, selection = "none")
    dashboard_render_table(output, "files_table", file_data, selection = "none")
    dashboard_render_table(output, "orphan_table", orphan_data, selection = "none")
    dashboard_render_table(output, "support_table", support_data, selection = "none")

    output$packages_box <- shiny::renderUI({
      dashboard_value_box("Packages", nrow(package_data()), "secondary", note = "Tracked project packages.")
    })
    output$missing_packages_box <- shiny::renderUI({
      value <- dashboard_summary_value(state, "missing_packages", 0L)
      dashboard_value_box("Missing packages", value, if (as.integer(value) > 0L) "warning" else "success")
    })
    output$registered_files_box <- shiny::renderUI({
      dashboard_value_box("Registered files", nrow(file_data()), "secondary")
    })
    output$review_files_box <- shiny::renderUI({
      value <- dashboard_summary_value(state, "orphan_files", 0L)
      dashboard_value_box("Files to review", value, if (as.integer(value) > 0L) "warning" else "success", note = "Only workflow candidates are counted.")
    })
  })
}
