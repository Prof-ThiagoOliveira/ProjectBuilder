template_root <- function() {
  installed_path <- system.file("templates", package = "projectSetupR")

  if (nzchar(installed_path)) {
    return(installed_path)
  }

  fs::path("inst", "templates")
}

template_path <- function(...) {
  fs::path(template_root(), ...)
}

read_template <- function(...) {
  template_file <- template_path(...)

  if (!fs::file_exists(template_file)) {
    rlang::abort(paste0("Template file not found: ", template_file))
  }

  paste(readLines(template_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

render_template <- function(content, data = list()) {
  if (!is.list(data)) {
    rlang::abort("`data` must be a list.")
  }

  rendered <- do.call(
    glue::glue_data,
    c(
      list(
        .x = data,
        .envir = emptyenv(),
        .open = "{{",
        .close = "}}",
        .trim = FALSE
      ),
      list(content)
    )
  )

  rendered <- as.character(rendered)
  enc2utf8(rendered)
}

#' Write a template file
#'
#' @param path Destination file path.
#' @param content File content.
#' @param overwrite Logical. Should an existing file be overwritten?
#'
#' @return A list describing the write outcome.
write_template_file <- function(path, content, overwrite = FALSE) {
  fs::dir_create(fs::path_dir(path), recurse = TRUE)

  if (fs::file_exists(path) && !isTRUE(overwrite)) {
    return(list(path = path, status = "skipped"))
  }

  status <- if (fs::file_exists(path)) "overwritten" else "created"

  if (grepl("\\.ya?ml$", path, ignore.case = TRUE)) {
    yaml::yaml.load(enc2utf8(content))
  }

  writeLines(enc2utf8(content), path, useBytes = TRUE)

  list(path = path, status = status)
}

collect_template_results <- function(results) {
  statuses <- vapply(results, `[[`, character(1), "status")
  paths <- fs::path_norm(vapply(results, `[[`, character(1), "path"))

  list(
    files_created = unname(paths[statuses %in% c("created", "overwritten")]),
    files_skipped = unname(paths[statuses == "skipped"])
  )
}

write_registered_templates <- function(
    path,
    project_name = NULL,
    registry,
    overwrite = FALSE,
    template_data = list()) {
  if (!is.null(project_name)) {
    template_data$project_name <- project_name
  }

  results <- lapply(
    registry,
    function(entry) {
      destination <- fs::path(path, entry$target)
      content <- render_template(
        read_template(entry$source),
        data = template_data
      )
      write_template_file(destination, content, overwrite = overwrite)
    }
  )

  collect_template_results(results)
}
