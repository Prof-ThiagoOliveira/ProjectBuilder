#' Create report style template files
#'
#' @param path Project root path.
#' @param overwrite Logical. Should existing files be overwritten?
#' @param use_quarto Logical. Should Quarto-specific style files be created?
#' @param use_rmarkdown Logical. Should R Markdown style files be created?
#'
#' @return A list with created and skipped file paths.
scaffold_report_styles <- function(
    path,
    overwrite = FALSE,
    use_quarto = TRUE,
    use_rmarkdown = FALSE,
    template_data = list()) {
  write_registered_templates(
    path = path,
    project_name = NULL,
    registry = template_registry(
      "report_styles",
      options = list(
        use_quarto = use_quarto,
        use_rmarkdown = use_rmarkdown
      )
    ),
    overwrite = overwrite,
    template_data = template_data
  )
}
