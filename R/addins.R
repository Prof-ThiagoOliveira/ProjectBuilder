#' Open the projflow Project Manager addin
#'
#' @return Invisibly returns the launched dashboard.
#' @examples
#' \dontrun{
#' open_projflow_project_manager()
#' }
#' @author Thiago de Paula Oliveira
#' @export
open_projflow_project_manager <- function() {
  launch_project_manager()
}

#' Run the projflow project check addin
#'
#' @return Invisibly returns the printed project-check result.
#' @examples
#' \dontrun{
#' run_projflow_check()
#' }
#' @author Thiago de Paula Oliveira
#' @export
run_projflow_check <- function() {
  print(check_project())
  invisible(NULL)
}

prompt_addin_value <- function(prompt) {
  value <- trimws(readline(prompt))
  if (!nzchar(value)) {
    rlang::abort("A non-empty value is required.")
  }
  value
}

#' Add a projflow script from RStudio
#'
#' @return Invisibly returns the created script path.
#' @examples
#' \dontrun{
#' add_projflow_script()
#' }
#' @author Thiago de Paula Oliveira
#' @export
add_projflow_script <- function() {
  name <- prompt_addin_value("Script name: ")
  new_script(name, open = FALSE)
}

#' Add a projflow report from RStudio
#'
#' @return Invisibly returns the created report path.
#' @examples
#' \dontrun{
#' add_projflow_report()
#' }
#' @author Thiago de Paula Oliveira
#' @export
add_projflow_report <- function() {
  name <- prompt_addin_value("Report name: ")
  new_report(name, open = FALSE)
}

#' Add a projflow task from RStudio
#'
#' @return Invisibly returns the created task identifier.
#' @examples
#' \dontrun{
#' add_projflow_task()
#' }
#' @author Thiago de Paula Oliveira
#' @export
add_projflow_task <- function() {
  title <- prompt_addin_value("Task title: ")
  add_project_task(title)
}
