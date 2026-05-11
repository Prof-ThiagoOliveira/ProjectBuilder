% Generated manually for the redesigned DAG-first execution layer.
\name{run_project}
\alias{run_project}
\title{Run the project workflow}
\usage{
run_project(root = ".")
}
\arguments{
\item{root}{Existing project root whose registered scripts should be run.}
}
\description{
Executes registered analysis scripts in topological DAG order. If the registry
does not contain enough dependency information to build a valid DAG, the
function warns and falls back to the explicit registry script order.
}
\value{
Invisibly returns executed script paths.
}
\examples{
\dontrun{
run_project()
}
}
