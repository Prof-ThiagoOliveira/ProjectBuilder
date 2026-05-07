# Render R Markdown reports ------------------------------------------------

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop(
    "Package 'rmarkdown' is required to render R Markdown reports.",
    call. = FALSE
  )
}

rmarkdown::render("reports/final_report.Rmd")
