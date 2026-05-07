# Restore project environment ---------------------------------------------

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

renv::restore()
