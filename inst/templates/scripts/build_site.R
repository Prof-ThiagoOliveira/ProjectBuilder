# Build pkgdown site --------------------------------------------------------

if (!requireNamespace("pkgdown", quietly = TRUE)) {
  stop(
    "Package 'pkgdown' is required to build the package website.",
    call. = FALSE
  )
}

pkgdown::build_site()
