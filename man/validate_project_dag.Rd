% Generated manually for the redesigned layered API.
\name{validate_project_dag}
\alias{validate_project_dag}
\title{Validate the executable analysis DAG}
\usage{
validate_project_dag(root = ".", dag = NULL, strict = FALSE)
}
\arguments{
\item{root}{Path inside an existing projflow project. Ignored when \code{dag} is supplied.}
\item{dag}{Optional object returned by \code{project_analysis_dag()}.}
\item{strict}{Logical scalar. If \code{TRUE}, abort when DAG errors are found.}
}
\description{
Checks that the executable project graph is valid: every edge endpoint must be
present, outputs should not have multiple producers, and the execution graph
must be acyclic.
}
\value{
A structured object with \code{ok}, \code{errors}, \code{warnings}, \code{info},
and \code{issues} fields.
}
\examples{
\dontrun{
validate_project_dag()
}
}
