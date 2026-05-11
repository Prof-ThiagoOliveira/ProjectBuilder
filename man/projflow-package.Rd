% Generated manually for the redesigned package overview.
\docType{package}
\name{projflow-package}
\alias{projflow}
\alias{projflow-package}
\title{projflow: DAG-first analysis project scaffolding}
\description{
\code{projflow} creates reproducible R analysis project scaffolds organised
around four layers: project structure, executable analysis DAG, governance, and
interfaces/integrations. The executable DAG links data inputs, scripts, outputs,
reports and deliverables so projects can be checked, run and translated into
\pkg{targets} pipelines.
}
\section{Typical workflow}{
\enumerate{
\item Inspect a plan with \code{plan_project()}, then create it with \code{new_project()}.
\item Inspect durable files and metadata with \code{project_structure()}.
\item Add scripts, outputs and reports with \code{new_script()}, \code{new_output()} and \code{new_report()}.
\item Validate execution with \code{project_analysis_dag()} and \code{validate_project_dag()}.
\item Build the project with \code{run_project()} or \code{build_project()}.
\item Manage governance with tasks, risks, decisions and milestones.
}
}
\examples{
\dontrun{
plan <- plan_project(
  path = "demo-project",
  components = c("data_preparation", "statistical_analysis", "report"),
  infrastructure = c("git", "targets")
)
new_project(plan = plan, open = FALSE)
validate_project_dag(root = "demo-project")
}
}
\author{Thiago de Paula Oliveira}
