#' Create an RStudio project file
#'
#' @param path Project root path.
#' @param project_name Project name.
#' @param overwrite Logical. Should an existing file be overwritten?
#'
#' @return A list describing the write outcome.
create_rproj_file <- function(path, project_name, overwrite = FALSE) {
  content <- c(
    "Version: 1.0",
    "",
    "RestoreWorkspace: Default",
    "SaveWorkspace: Default",
    "AlwaysSaveHistory: Default",
    "",
    "EnableCodeIndexing: Yes",
    "UseSpacesForTab: Yes",
    "NumSpacesForTab: 2",
    "Encoding: UTF-8",
    "",
    "RnwWeave: Sweave",
    "LaTeX: pdfLaTeX"
  )

  write_template_file(
    fs::path(path, paste0(project_name, ".Rproj")),
    paste(content, collapse = "\n"),
    overwrite = overwrite
  )
}
