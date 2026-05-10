#' projflow: Component-driven analysis project scaffolding
#'
#' @description
#' `projflow` creates analysis project scaffolds that keep code, reports, and
#' lightweight outputs in the repository while treating raw and large data as
#' external by default. The package registry links scripts, reports, and output
#' artefacts so projects can be built and checked reproducibly without forcing
#' unnecessary files into a new scaffold. Project metadata live in
#' `.projflow/`, with machine-local paths stored in `.projflow/local.yml`.
#'
#' @section Typical workflow:
#' 1. Create a project with [new_project()] or inspect a plan with [plan_project()].
#' 2. Configure machine-local data roots with [set_project_data_root()].
#' 3. Add scripts with [new_script()] and save artefacts explicitly with
#'    [save_project_object()].
#' 4. Build registered scripts and reports with [build_project()].
#' 5. Check project structure and registered outputs with [check_project()].
#'
#' @examples
#' \dontrun{
#' plan_project(
#'   path = "demo-project",
#'   preset = "basic_analysis",
#'   infrastructure = character(),
#'   include_example = FALSE
#' )
#' }
#' @author Thiago de Paula Oliveira
#' @importFrom stats setNames
"_PACKAGE"
