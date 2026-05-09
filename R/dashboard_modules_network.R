mod_network_ui <- function(id) {
  ns <- shiny::NS(id)
  widget <- if (dashboard_has_visnetwork()) {
    visNetwork::visNetworkOutput(ns("network_widget"), height = "650px")
  } else {
    shiny::uiOutput(ns("network_widget"))
  }

  shiny::tagList(
    dashboard_card(
      "Project network",
      subtitle = "Relationship map across components, scripts, reports, outputs and governance items.",
      shiny::fluidRow(
        shiny::column(4, shiny::uiOutput(ns("nodes_box"))),
        shiny::column(4, shiny::uiOutput(ns("edges_box"))),
        shiny::column(4, shiny::uiOutput(ns("types_box")))
      )
    ),
    dashboard_card(
      "Interactive graph",
      subtitle = "Use the node selector and nearest-neighbour highlighting to inspect project dependencies.",
      shiny::div(
        class = "projflow-network-legend",
        dashboard_pill("component", "secondary"),
        dashboard_pill("script", "primary"),
        dashboard_pill("report", "success"),
        dashboard_pill("output", "warning"),
        dashboard_pill("governance", "danger")
      ),
      widget
    ),
    dashboard_card(
      "Network edges",
      subtitle = "Underlying relationships used to construct the graph.",
      dashboard_table_ui(ns("edges_table"))
    )
  )
}

mod_network_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    network_data <- shiny::reactive({
      state$refresh_token
      dashboard_network_data(state)
    })

    output$nodes_box <- shiny::renderUI({
      network <- network_data()
      dashboard_value_box("Nodes", nrow(dashboard_safe_data_frame(network$nodes)), "secondary")
    })
    output$edges_box <- shiny::renderUI({
      network <- network_data()
      dashboard_value_box("Edges", nrow(dashboard_safe_data_frame(network$edges)), "secondary")
    })
    output$types_box <- shiny::renderUI({
      nodes <- dashboard_safe_data_frame(network_data()$nodes)
      value <- if ("type" %in% names(nodes)) length(unique(nodes$type)) else 0L
      dashboard_value_box("Node types", value, "secondary")
    })

    if (dashboard_has_visnetwork()) {
      output$network_widget <- visNetwork::renderVisNetwork({
        network <- network_data()
        nodes <- dashboard_safe_data_frame(network$nodes)
        edges <- dashboard_safe_data_frame(network$edges)
        if (nrow(nodes) == 0L) {
          return(visNetwork::visNetwork(
            nodes = data.frame(id = character(), label = character(), stringsAsFactors = FALSE),
            edges = data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
          ))
        }

        if (!"group" %in% names(nodes) && "type" %in% names(nodes)) {
          nodes$group <- nodes$type
        }
        graph <- visNetwork::visNetwork(nodes = nodes, edges = edges)
        graph <- visNetwork::visOptions(graph, highlightNearest = TRUE, nodesIdSelection = TRUE)
        graph <- visNetwork::visInteraction(graph, navigationButtons = TRUE)
        visNetwork::visPhysics(graph, stabilization = TRUE)
      })
    } else {
      output$network_widget <- shiny::renderUI({
        dashboard_empty_state("Install the optional visNetwork package to display the interactive project network. The edge table remains available below.")
      })
    }

    dashboard_render_table(output, "edges_table", shiny::reactive(network_data()$edges), selection = "none")
  })
}
