# Load project code --------------------------------------------------------

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop(
    "Package 'pkgload' is required for package-style project loading. ",
    "Install it with install.packages('pkgload').",
    call. = FALSE
  )
}

pkgload::load_all(".", quiet = TRUE)

message("Project code loaded.")
