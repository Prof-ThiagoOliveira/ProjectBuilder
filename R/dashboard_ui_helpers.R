dashboard_has_dt <- function() {
  requireNamespace("DT", quietly = TRUE)
}

dashboard_has_visnetwork <- function() {
  requireNamespace("visNetwork", quietly = TRUE)
}

dashboard_table_ui <- function(id) {
  if (dashboard_has_dt()) {
    DT::DTOutput(id)
  } else {
    shiny::tableOutput(id)
  }
}

dashboard_render_table <- function(output, id, data_reactive, selection = "single") {
  if (dashboard_has_dt()) {
    output[[id]] <- DT::renderDT({
      DT::datatable(
        data_reactive(),
        selection = selection,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
  } else {
    output[[id]] <- shiny::renderTable(data_reactive(), striped = TRUE)
  }
}

dashboard_badge <- function(label, status = "secondary") {
  class_name <- paste("badge", status)
  shiny::tags$span(class = class_name, label)
}

dashboard_value_box <- function(title, value, theme = "primary") {
  bslib::value_box(
    title = title,
    value = value,
    theme = theme
  )
}
