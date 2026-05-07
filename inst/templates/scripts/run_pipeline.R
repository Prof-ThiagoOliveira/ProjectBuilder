# Run targets pipeline ------------------------------------------------------

if (!requireNamespace("targets", quietly = TRUE)) {
  stop(
    "Package 'targets' is required to run this pipeline.",
    call. = FALSE
  )
}

targets::tar_make()
