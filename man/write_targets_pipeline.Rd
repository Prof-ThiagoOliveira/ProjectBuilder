% Generated manually for the redesigned layered API.
\name{write_targets_pipeline}
\alias{write_targets_pipeline}
\title{Write a targets pipeline from the project DAG}
\usage{
write_targets_pipeline(root = ".", path = "_targets.R", overwrite = FALSE)
}
\arguments{
\item{root}{Path inside an existing projflow project.}
\item{path}{Output path for the targets pipeline, relative to \code{root} unless an absolute path is supplied.}
\item{overwrite}{Logical scalar. If \code{TRUE}, replace an existing file.}
}
\description{
Converts the registered analysis DAG into a minimal \file{_targets.R} file. The
generated file delegates execution to \code{projflow::run_project_object()} while
preserving graph dependencies among scripts where the registry declares
\code{depends_on}, \code{inputs}, \code{outputs}, or \code{generated_by} fields.
}
\value{
Invisibly returns the written file path.
}
\examples{
\dontrun{
write_targets_pipeline(overwrite = TRUE)
}
}
