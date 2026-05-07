#' Validate a project path
#'
#' @param path Target project path.
#'
#' @return A normalized project path.
resolve_project_path <- function(path) {
  normalizePath(path.expand(trimws(path)), winslash = "/", mustWork = FALSE)
}

detect_path_construction_warning <- function(raw_path, resolved_path) {
  normalized_raw_path <- gsub("\\\\", "/", trimws(raw_path))
  temp_path_candidates <- unique(
    tolower(c(
      gsub("\\\\", "/", tempdir()),
      gsub("\\\\", "/", normalizePath(tempdir(), winslash = "/", mustWork = FALSE)),
      if (.Platform$OS.type == "windows") {
        gsub("\\\\", "/", utils::shortPathName(tempdir()))
      } else {
        character()
      }
    ))
  )
  has_navigation <- grepl("(^|/)\\.{1,2}(/|$)", normalized_raw_path)
  is_temp_prefixed <- any(startsWith(tolower(normalized_raw_path), temp_path_candidates))

  if (!has_navigation || !is_temp_prefixed) {
    return(NULL)
  }

  paste0(
    "`path` resolved to `",
    resolved_path,
    "` from a temporary-path prefix. If you meant to create the project in a regular folder, pass that folder directly (for example `./../../R Packages/anal`) or use `tempfile(tmpdir = './../../R Packages', pattern = 'anal')`."
  )
}

#' Validate whether a project path can be used
#'
#' @param path Target project path.
#' @param overwrite Logical. Should existing files be overwritten?
#'
#' @return Invisibly returns the validated path.
validate_project_path <- function(path, overwrite = FALSE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    rlang::abort("`path` must be a non-empty character scalar.")
  }

  if (fs::file_exists(path) && !fs::dir_exists(path)) {
    rlang::abort("`path` points to an existing file, not a directory.")
  }

  if (fs::dir_exists(path)) {
    existing_entries <- fs::dir_ls(path, all = TRUE, fail = FALSE)

    if (length(existing_entries) > 0L && !isTRUE(overwrite)) {
      rlang::abort(
        paste0(
          "Refusing to use existing non-empty directory: ",
          path,
          ". Set `overwrite = TRUE` to allow writing additional files."
        )
      )
    }
  }

  invisible(path)
}

#' Validate a project name
#'
#' @param project_name Project name supplied by the user.
#'
#' @return A validated project name.
validate_project_name <- function(project_name) {
  if (!is.character(project_name) ||
      length(project_name) != 1L ||
      is.na(project_name) ||
      !nzchar(trimws(project_name))) {
    rlang::abort("`project_name` must be a non-empty character scalar.")
  }

  safe_name <- gsub("[/\\\\\r\n\t]+", "_", trimws(project_name))
  safe_name <- gsub("\\s+", "_", safe_name)

  if (!nzchar(safe_name)) {
    rlang::abort("`project_name` must contain at least one non-whitespace character.")
  }

  safe_name
}

#' Validate a logical scalar
#'
#' @param x Object to validate.
#' @param arg Argument name.
#'
#' @return Invisibly returns `TRUE`.
validate_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    rlang::abort(paste0("`", arg, "` must be TRUE or FALSE."))
  }

  invisible(TRUE)
}

#' Validate a character vector
#'
#' @param x Object to validate.
#' @param arg Argument name.
#' @param allow_null Logical. Whether `NULL` is allowed.
#'
#' @return Invisibly returns `TRUE`.
validate_character_vector <- function(x, arg, allow_null = FALSE) {
  if (isTRUE(allow_null) && is.null(x)) {
    return(invisible(TRUE))
  }

  if (!is.character(x)) {
    rlang::abort(paste0("`", arg, "` must be a character vector."))
  }

  if (anyNA(x)) {
    rlang::abort(paste0("`", arg, "` must not contain NA values."))
  }

  trimmed <- trimws(x)

  if (any(!nzchar(trimmed))) {
    rlang::abort(paste0("`", arg, "` must not contain empty strings."))
  }

  invisible(TRUE)
}

#' Validate package names
#'
#' @param packages Package vector.
#' @param allow_null Logical. Whether `NULL` is allowed.
#'
#' @return Cleaned package names.
validate_package_names <- function(packages, allow_null = TRUE) {
  validate_character_vector(packages, "packages", allow_null = allow_null)

  if (is.null(packages)) {
    return(NULL)
  }

  cleaned <- unique(trimws(packages))

  invalid <- !grepl("^[A-Za-z][A-Za-z0-9.]*$", cleaned)

  if (any(invalid)) {
    rlang::abort(
      paste0(
        "Invalid package names: ",
        paste(cleaned[invalid], collapse = ", ")
      )
    )
  }

  cleaned
}

#' Validate a single choice
#'
#' @param x Choice to validate.
#' @param choices Allowed choices.
#' @param arg Argument name.
#'
#' @return The validated choice.
validate_choice <- function(x, choices, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    rlang::abort(paste0("`", arg, "` must be a character scalar."))
  }

  if (!x %in% choices) {
    rlang::abort(
      paste0(
        "`", arg, "` must be one of: ",
        paste(choices, collapse = ", "),
        "."
      )
    )
  }

  x
}
