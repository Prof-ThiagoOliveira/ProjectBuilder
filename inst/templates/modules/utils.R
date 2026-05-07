save_rds_safe <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(object, path)
  invisible(path)
}

read_rds_safe <- function(path) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }

  readRDS(path)
}

if ("box" %in% loadedNamespaces()) {
  box::export(save_rds_safe, read_rds_safe)
}
