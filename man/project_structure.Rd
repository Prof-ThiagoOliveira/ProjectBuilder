% Generated manually for the redesigned layered API.
\name{project_structure}
\alias{project_structure}
\title{Inspect the project structure layer}
\usage{
project_structure(root = ".")
}
\arguments{
\item{root}{Path inside an existing projflow project.}
}
\description{
Returns the canonical filesystem and metadata contract for an existing projflow
project, including the status of core folders, metadata files and local data
roots.
}
\value{
A data frame describing canonical project paths and their existence status.
}
\examples{
\dontrun{
project_structure()
}
}
