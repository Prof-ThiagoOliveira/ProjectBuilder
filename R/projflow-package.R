#' projflow: DAG-first analysis project scaffolding
#'
#' @description
#' `projflow` creates reproducible R analysis project scaffolds organised around
#' four layers: project structure, executable analysis DAG, governance, and
#' interfaces/integrations. The executable DAG links data inputs, scripts,
#' outputs, reports and deliverables so projects can be checked, run and
#' translated into `targets` pipelines.
#'
#' @section Typical workflow:
#' 1. Inspect a plan with [plan_project()], then create it with [new_project()].
#' 2. Inspect durable files and metadata with [project_structure()].
#' 3. Add scripts, outputs and reports with [new_script()], [new_output()] and
#'    [new_report()].
#' 4. Validate execution with [project_analysis_dag()] and [validate_project_dag()].
#' 5. Build the project with [run_project()] or [build_project()].
#' 6. Manage governance with tasks, risks, decisions and milestones.
#'
#' @author Thiago de Paula Oliveira
#' @keywords internal
"projflow"
