# Style project -------------------------------------------------------------

if (!requireNamespace("styler", quietly = TRUE)) {
  stop(
    "Package 'styler' is required to style this project.",
    call. = FALSE
  )
}

if (dir.exists("R")) {
  styler::style_dir("R")
}

if (dir.exists("modules")) {
  styler::style_dir("modules")
}

styler::style_dir("scripts")
styler::style_dir("tests")
