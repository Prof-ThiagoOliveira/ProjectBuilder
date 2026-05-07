# Render Quarto reports ----------------------------------------------------

if (!requireNamespace("quarto", quietly = TRUE)) {
  stop(
    "Package 'quarto' is required to render Quarto reports.",
    call. = FALSE
  )
}

quarto::quarto_render("reports")
