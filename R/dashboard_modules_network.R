mod_network_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::verbatimTextOutput(ns("network_summary")),
    shiny::uiOutput(ns("network_widget")),
    dashboard_table_ui(ns("edges_table"))
  )
}

mod_network_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    network_data <- shiny::reactive(state$diagnostics$network)

    output$network_summary <- shiny::renderPrint({
      network <- network_data()
      list(nodes = nrow(network$nodes), edges = nrow(network$edges))
    })

    output$network_widget <- shiny::renderUI({
      network <- network_data()
      if (!dashboard_has_visnetwork() || nrow(network$nodes) == 0L) {
        return(shiny::p(class = "projflow-muted", "visNetwork is not installed, or no network nodes are available. Showing the edge table instead."))
      }

      visNetwork::visNetwork(
        nodes = network$nodes,
        edges = network$edges
      )
    })

    dashboard_render_table(output, "edges_table", shiny::reactive(network_data()$edges))
  })
}
