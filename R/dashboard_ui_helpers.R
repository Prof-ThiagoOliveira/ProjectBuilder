dashboard_has_dt <- function() {
  requireNamespace("DT", quietly = TRUE)
}

dashboard_has_visnetwork <- function() {
  requireNamespace("visNetwork", quietly = TRUE)
}

dashboard_has_timevis <- function() {
  requireNamespace("timevis", quietly = TRUE)
}

dashboard_css <- function() {
  shiny::HTML(
    paste(
      ".projflow-app{background:#f5f7fb;min-height:100vh;}",
      ".projflow-shell{max-width:1440px;margin:0 auto;padding:1.25rem;}",
      ".projflow-hero{background:linear-gradient(135deg,#16324f,#235789);color:#fff;border-radius:22px;padding:1.4rem 1.6rem;margin:1rem auto 0;max-width:1440px;box-shadow:0 16px 35px rgba(22,50,79,.18);}",
      ".projflow-hero h1{font-size:1.65rem;font-weight:700;margin:0 0 .25rem;letter-spacing:.01em;}",
      ".projflow-hero p{margin:0;color:rgba(255,255,255,.82);font-size:.98rem;}",
      ".projflow-hero-meta{display:flex;flex-wrap:wrap;gap:.5rem;margin-top:1rem;}",
      ".projflow-hero-pill{display:inline-flex;align-items:center;gap:.35rem;background:rgba(255,255,255,.15);border:1px solid rgba(255,255,255,.24);border-radius:999px;padding:.35rem .65rem;font-size:.82rem;color:#fff;}",
      ".projflow-hero-pill code{color:#fff;background:rgba(0,0,0,.16);padding:.05rem .35rem;border-radius:.35rem;}",
      ".projflow-topbar{display:flex;flex-wrap:wrap;align-items:center;justify-content:space-between;gap:.75rem;margin:1rem 0;}",
      ".projflow-actions{display:flex;flex-wrap:wrap;gap:.5rem;align-items:center;}",
      ".projflow-actions .btn,.projflow-card .btn{border-radius:999px;font-weight:600;}",
      ".projflow-status{min-width:280px;max-width:650px;}",
      ".projflow-alert{border-radius:16px;padding:.75rem .95rem;background:#fff;border:1px solid #dbe3ef;box-shadow:0 8px 18px rgba(31,45,61,.06);font-size:.9rem;}",
      ".projflow-alert-success{border-left:5px solid #198754;}",
      ".projflow-alert-warning{border-left:5px solid #f0ad4e;}",
      ".projflow-alert-danger{border-left:5px solid #dc3545;}",
      ".projflow-alert-info{border-left:5px solid #0d6efd;}",
      ".projflow-shell .navbar{background:#fff;border:1px solid #dbe3ef;border-radius:18px;padding:.35rem .75rem;box-shadow:0 8px 18px rgba(31,45,61,.05);}
      .projflow-shell .navbar .nav-link{font-weight:650;color:#334155;border-radius:999px;margin:.1rem .15rem;}
      .projflow-shell .navbar .nav-link.active{background:#e8f1fb;color:#16324f;}
      .projflow-shell .navbar .dropdown-menu{border-radius:16px;border:1px solid #dbe3ef;box-shadow:0 14px 28px rgba(31,45,61,.12);}
      .projflow-shell .tab-content{padding-top:1rem;}",
      ".projflow-card{background:#fff;border:1px solid #dbe3ef;border-radius:20px;box-shadow:0 10px 24px rgba(31,45,61,.07);padding:1rem;margin-bottom:1rem;}",
      ".projflow-card-header{display:flex;align-items:flex-start;justify-content:space-between;gap:1rem;margin-bottom:.8rem;}",
      ".projflow-card-title{font-size:1.02rem;font-weight:700;color:#1f2d3d;margin:0;}",
      ".projflow-card-subtitle{font-size:.86rem;color:#64748b;margin:.2rem 0 0;}",
      ".projflow-card-body{min-width:0;}",
      ".projflow-section-title{font-size:.76rem;text-transform:uppercase;letter-spacing:.08em;color:#64748b;font-weight:700;margin:.25rem 0 .75rem;}",
      ".projflow-kpi{background:#fff;border:1px solid #dbe3ef;border-radius:20px;padding:1rem;box-shadow:0 8px 20px rgba(31,45,61,.06);height:100%;}",
      ".projflow-kpi-label{font-size:.78rem;text-transform:uppercase;letter-spacing:.06em;color:#64748b;font-weight:700;margin-bottom:.35rem;}",
      ".projflow-kpi-value{font-size:1.85rem;line-height:1;font-weight:750;color:#1f2d3d;margin-bottom:.35rem;}",
      ".projflow-kpi-note{font-size:.84rem;color:#64748b;}",
      ".projflow-kpi-success{border-top:5px solid #198754;}",
      ".projflow-kpi-warning{border-top:5px solid #f0ad4e;}",
      ".projflow-kpi-danger{border-top:5px solid #dc3545;}",
      ".projflow-kpi-primary{border-top:5px solid #0d6efd;}",
      ".projflow-kpi-secondary{border-top:5px solid #6c757d;}",
      ".projflow-form{background:#f8fafc;border:1px solid #e5edf6;border-radius:18px;padding:1rem;}",
      ".projflow-help{font-size:.86rem;color:#64748b;margin:.25rem 0 .75rem;}",
      ".projflow-muted{color:#64748b;}",
      ".projflow-empty{border:1px dashed #cbd5e1;border-radius:16px;background:#f8fafc;color:#64748b;padding:1rem;text-align:center;}",
      ".projflow-pills{display:flex;flex-wrap:wrap;gap:.35rem;}",
      ".projflow-pill{display:inline-block;border-radius:999px;background:#eef2ff;color:#334155;padding:.22rem .55rem;font-size:.78rem;font-weight:600;}",
      ".projflow-pill-success{background:#eaf7ef;color:#146c43;}",
      ".projflow-pill-warning{background:#fff4df;color:#8a5a00;}",
      ".projflow-pill-danger{background:#fdecef;color:#b02a37;}",
      ".projflow-pill-secondary{background:#eef2f7;color:#475569;}",
      ".projflow-list{margin:0;padding-left:1.1rem;}",
      ".projflow-list li{margin-bottom:.25rem;}",
      ".projflow-table-wrap{background:#fff;border-radius:16px;overflow:hidden;}",
      ".projflow-code{background:#f1f5f9;border-radius:8px;padding:.12rem .4rem;color:#334155;}",
      ".projflow-danger-zone{border-left:5px solid #dc3545;}",
      ".projflow-network-legend{display:flex;flex-wrap:wrap;gap:.4rem;margin-bottom:.6rem;}",
      ".projflow-selected-record{background:#eef6ff;border:1px solid #cfe3ff;border-radius:14px;padding:.7rem .85rem;margin-bottom:.8rem;color:#1f2d3d;}",
      ".projflow-chart-note{font-size:.82rem;color:#64748b;margin-top:.35rem;}",
      ".projflow-tv-planned{background:#eef2f7;border-color:#94a3b8;color:#334155;}
      .projflow-tv-active{background:#e8f1fb;border-color:#0d6efd;color:#16324f;}
      .projflow-tv-complete{background:#eaf7ef;border-color:#198754;color:#146c43;}
      .projflow-tv-blocked{background:#fdecef;border-color:#dc3545;color:#b02a37;}
      .modal-lg{--bs-modal-width:95vw;max-width:95vw;}
      .modal-lg .modal-body{min-height:76vh;}",
      ".tab-pane{outline:none;}",
      sep = "\n"
    )
  )
}

dashboard_table_ui <- function(id) {
  shiny::div(
    class = "projflow-table-wrap",
    if (dashboard_has_dt()) {
      DT::DTOutput(id)
    } else {
      shiny::tableOutput(id)
    }
  )
}

dashboard_render_table <- function(output, id, data_reactive, selection = "single", page_length = 10) {
  if (dashboard_has_dt()) {
    output[[id]] <- DT::renderDT({
      data <- dashboard_safe_data_frame(data_reactive())
      DT::datatable(
        data,
        selection = selection,
        filter = if (ncol(data) > 0L) "top" else "none",
        rownames = FALSE,
        class = "compact stripe hover order-column",
        options = list(
          pageLength = page_length,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = "tip"
        )
      )
    })
  } else {
    output[[id]] <- shiny::renderTable(
      dashboard_safe_data_frame(data_reactive()),
      striped = TRUE,
      bordered = TRUE,
      spacing = "s"
    )
  }
}

dashboard_card <- function(title, ..., subtitle = NULL, class = NULL, actions = NULL) {
  shiny::div(
    class = paste("projflow-card", class %||% ""),
    shiny::div(
      class = "projflow-card-header",
      shiny::div(
        shiny::h3(class = "projflow-card-title", title),
        if (!is.null(subtitle)) shiny::p(class = "projflow-card-subtitle", subtitle)
      ),
      actions
    ),
    shiny::div(class = "projflow-card-body", ...)
  )
}

dashboard_kpi <- function(label, value, note = NULL, theme = "secondary") {
  shiny::div(
    class = paste0("projflow-kpi projflow-kpi-", theme),
    shiny::div(class = "projflow-kpi-label", label),
    shiny::div(class = "projflow-kpi-value", value),
    if (!is.null(note)) shiny::div(class = "projflow-kpi-note", note)
  )
}

dashboard_value_box <- function(title, value, theme = "primary", note = NULL) {
  dashboard_kpi(title, value, note = note, theme = theme)
}

dashboard_empty_state <- function(message) {
  shiny::div(class = "projflow-empty", message)
}

dashboard_alert <- function(message, type = "info") {
  shiny::div(class = paste0("projflow-alert projflow-alert-", type), message)
}

dashboard_pill <- function(label, status = "secondary") {
  shiny::span(class = paste0("projflow-pill projflow-pill-", status), label)
}

dashboard_pill_list <- function(values, empty = "None recorded", status = "secondary") {
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0L) {
    return(shiny::span(class = "projflow-muted", empty))
  }
  shiny::div(class = "projflow-pills", lapply(values, dashboard_pill, status = status))
}

dashboard_section_label <- function(label) {
  shiny::div(class = "projflow-section-title", label)
}

dashboard_form_card <- function(title, ..., subtitle = NULL) {
  dashboard_card(
    title = title,
    subtitle = subtitle,
    shiny::div(class = "projflow-form", ...)
  )
}

dashboard_status_theme <- function(status) {
  status <- as.character(status %||% "")
  if (identical(status, "Healthy") || identical(status, "Done") || identical(status, "Ready")) {
    return("success")
  }
  if (identical(status, "Broken") || identical(status, "Error")) {
    return("danger")
  }
  if (grepl("attention|warning|overdue|missing|stale|blocked", status, ignore.case = TRUE)) {
    return("warning")
  }
  "secondary"
}

dashboard_header <- function(root, mode, manage = TRUE) {
  shiny::div(
    class = "projflow-hero",
    shiny::h1("projflow Project Manager"),
    shiny::p("Operational control centre for project structure, governance, outputs, reports, data sources and diagnostics."),
    shiny::div(
      class = "projflow-hero-meta",
      shiny::span(class = "projflow-hero-pill", shiny::strong("Project"), safe_basename(root)),
      shiny::span(class = "projflow-hero-pill", shiny::strong("Root"), shiny::code(normalize_absolute_path(root))),
      shiny::span(class = "projflow-hero-pill", shiny::strong("Mode"), if (isTRUE(manage)) "Manage" else "Diagnose"),
      shiny::span(class = "projflow-hero-pill", shiny::strong("Metadata"), project_metadata_relative_dir(root))
    )
  )
}
