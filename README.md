# projectSetupR

`projectSetupR` builds safe, reproducible R analysis-project scaffolds. It
creates a standard folder layout, helper R files, report templates, Git-safe
defaults, and optional `renv` and `targets` integration.

## Main function

```r
create_analysis_project(
  path = "my_project",
  use_quarto = TRUE,
  use_rmarkdown = FALSE,
  use_renv = TRUE,
  use_targets = FALSE,
  use_git = TRUE
)
```

## Development

The package stores project template assets under `inst/templates/` and keeps
the scaffold logic in small helpers under `R/`.
