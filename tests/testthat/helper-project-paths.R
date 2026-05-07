make_project_path <- function(prefix = "analysis-project") {
  file.path(
    tempdir(),
    paste0(prefix, "-", as.integer(stats::runif(1, 1, 1e9)))
  )
}
