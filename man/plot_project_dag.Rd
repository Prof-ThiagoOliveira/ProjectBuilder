% Generated manually for the redesigned layered API.
\name{plot_project_dag}
\alias{plot_project_dag}
\title{Plot or return the executable analysis DAG}
\usage{
plot_project_dag(root = ".", ...)
}
\arguments{
\item{root}{Path inside an existing projflow project.}
\item{...}{Reserved for future plotting options.}
}
\description{
Provides a lightweight visual entry point to the analysis DAG. If
\pkg{visNetwork} is installed, an interactive network is returned; otherwise the
function prints the ordered edge table and invisibly returns the DAG object.
}
\value{
Invisibly returns the DAG object, or a \code{visNetwork} object when that
optional package is available.
}
\examples{
\dontrun{
plot_project_dag()
}
}
