local_mock_quarto_render <- function(.local_envir = parent.frame()) {
  withr::local_options(
    list(
      projflow.quarto_available = function() {
        TRUE
      },
      
      projflow.quarto_render = function(input,
                                        output_file = NULL,
                                        quiet = TRUE,
                                        execute_dir = NULL,
                                        ...) {
        if (is.null(output_file)) {
          output_file <- paste0(
            tools::file_path_sans_ext(basename(input)),
            ".html"
          )
        }
        
        is_absolute <- grepl("^(?:[A-Za-z]:[\\\\/]|/|\\\\\\\\)", output_file)
        
        render_dir <- if (!is.null(execute_dir)) {
          execute_dir
        } else {
          dirname(input)
        }
        
        output_path <- if (is_absolute) {
          output_file
        } else {
          file.path(render_dir, output_file)
        }
        
        dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
        
        writeLines(
          c(
            "<!doctype html>",
            "<html>",
            "<body>",
            "<p>Mock Quarto render for package tests.</p>",
            "</body>",
            "</html>"
          ),
          output_path
        )
        
        normalizePath(output_path, winslash = "/", mustWork = FALSE)
      }
    ),
    .local_envir = .local_envir
  )
}