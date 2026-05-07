# Lint project --------------------------------------------------------------

if (!requireNamespace("lintr", quietly = TRUE)) {
  stop(
    "Package 'lintr' is required to lint this project.",
    call. = FALSE
  )
}

lintr::lint_dir("R")

if (dir.exists("modules")) {
  lintr::lint_dir("modules")
}

lintr::lint_dir("scripts")
