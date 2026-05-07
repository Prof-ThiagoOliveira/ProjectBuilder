#' Save an object as RDS
#'
#' @param object R object to save.
#' @param path Output path.
#'
#' @return Invisibly returns the output path.
save_rds_safe <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(object, path)
  invisible(path)
}

#' Read an RDS object with a clear error message
#'
#' @param path Input path.
#'
#' @return R object.
read_rds_safe <- function(path) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }

  readRDS(path)
}
