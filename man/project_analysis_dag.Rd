% Generated manually for the redesigned layered API.
\name{project_analysis_dag}
\alias{project_analysis_dag}
\title{Build the executable analysis DAG}
\usage{
project_analysis_dag(root = ".", include_deliverables = TRUE)
}
\arguments{
\item{root}{Path inside an existing projflow project.}
\item{include_deliverables}{Logical scalar. If \code{TRUE}, include registered deliverables as terminal DAG nodes.}
}
\description{
Builds the strict execution graph for an existing projflow project. Unlike
\code{project_network_data()}, this graph only includes nodes that can affect
analysis execution or deliverable production.
}
\details{
The graph direction follows analytical causality: external data and declared
inputs point to scripts; scripts point to outputs; outputs point to reports;
reports point to deliverables.
}
\value{
A list with \code{nodes} and \code{edges} data frames and class
\code{"project_analysis_dag"}.
}
\examples{
\dontrun{
dag <- project_analysis_dag()
validate_project_dag(dag = dag)
}
}
