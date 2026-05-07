gitkeep_paths <- function() {
  c(
    "data/raw/.gitkeep",
    "data/external/.gitkeep",
    "data/interim/.gitkeep",
    "data/processed/.gitkeep",
    "data/metadata/.gitkeep",
    "outputs/.gitkeep",
    "outputs/tables/.gitkeep",
    "outputs/figures/.gitkeep",
    "outputs/models/.gitkeep",
    "outputs/reports/.gitkeep",
    "outputs/logs/.gitkeep"
  )
}

#' Create Git support files
#'
#' @param path Project root path.
#' @param overwrite Logical. Should existing files be overwritten?
#' @param mode Git scaffold mode. Use `"simple"` for analyst-facing projects
#'   and `"advanced"` for the full scaffold.
#' @param gitkeep_files Relative `.gitkeep` paths to create.
#'
#' @return A list with created and skipped file paths.
create_git_files <- function(
    path,
    overwrite = FALSE,
    mode = c("advanced", "simple"),
    gitkeep_files = NULL) {
  mode <- match.arg(mode)

  registry <- if (identical(mode, "simple")) {
    list(
      list(source = "gitignore", target = ".gitignore")
    )
  } else {
    list(
      list(source = "gitignore", target = ".gitignore"),
      list(source = "rbuildignore", target = ".Rbuildignore"),
      list(source = "gitattributes", target = ".gitattributes")
    )
  }

  if (is.null(gitkeep_files)) {
    gitkeep_files <- gitkeep_paths()
  }

  base_results <- lapply(
    registry,
    function(entry) {
      write_template_file(
        fs::path(path, entry$target),
        read_template(entry$source),
        overwrite = overwrite
      )
    }
  )

  gitkeep_results <- lapply(
    gitkeep_files,
    function(relative_path) {
      write_template_file(
        fs::path(path, relative_path),
        "",
        overwrite = overwrite
      )
    }
  )

  collect_template_results(c(base_results, gitkeep_results))
}
