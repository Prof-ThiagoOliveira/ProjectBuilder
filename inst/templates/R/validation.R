#' Check required columns
#'
#' @param data A data frame.
#' @param required_cols Character vector of required column names.
#'
#' @return Invisibly returns TRUE if all columns are present.
check_required_columns <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Check unique key
#'
#' @param data A data frame.
#' @param key Character vector of key columns.
#'
#' @return Invisibly returns TRUE if the key is unique.
check_unique_key <- function(data, key) {
  check_required_columns(data, key)

  duplicated_key <- duplicated(data[key])

  if (any(duplicated_key)) {
    stop(
      "The key is not unique. Duplicated rows detected for key: ",
      paste(key, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
