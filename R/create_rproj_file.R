#' Create an RStudio project file
#'
#' @param path Project root path where the \code{.Rproj} file should be written.
#' @param project_name Project name used as the \code{.Rproj} filename stem.
#' @param overwrite Logical scalar. If \code{TRUE}, an existing \code{.Rproj} file with
#'   the same name is replaced.
#'
#' @return A list describing the write outcome.
#' @examples
#' \dontrun{
#' tmp <- tempfile("projflow-rproj-")
#' dir.create(tmp)
#' projflow:::create_rproj_file(tmp, "demo_project")
#' }
#' @author Thiago de Paula Oliveira
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
