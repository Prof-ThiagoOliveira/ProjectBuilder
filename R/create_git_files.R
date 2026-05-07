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
#'
#' @return A list with created and skipped file paths.
create_git_files <- function(path, overwrite = FALSE) {
  registry <- list(
    list(source = "gitignore", target = ".gitignore"),
    list(source = "rbuildignore", target = ".Rbuildignore"),
    list(source = "gitattributes", target = ".gitattributes")
  )

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
    gitkeep_paths(),
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
