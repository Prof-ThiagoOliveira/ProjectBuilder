% Generated manually for the redesigned layered API.
\name{topological_project_order}
\alias{topological_project_order}
\title{Return the project topological execution order}
\usage{
topological_project_order(root = ".", type = c("all", "scripts", "reports"))
}
\arguments{
\item{root}{Path inside an existing projflow project.}
\item{type}{Character scalar. Return all DAG nodes, only scripts, or only reports.}
}
\description{
Returns the DAG order for scripts, reports or all executable nodes. Scripts with
explicit dependencies or input/output links are ordered by graph topology rather
than by filename alone.
}
\value{
A character vector of ordered node IDs or object names.
}
\examples{
\dontrun{
topological_project_order(type = "scripts")
}
}
