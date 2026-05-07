# projectSetupR

`projectSetupR` builds reproducible R analysis-project scaffolds while keeping
the default user surface intentionally small.

## Recommended interface

```r
projectSetupR::create_analysis_project("my_project")
```

This creates an analyst-facing scaffold with one obvious entry point:

- `README.md`
- `run_project.R`
- `project.yml`
- `analysis/`
- `reports/`
- `data/raw/`
- `data/processed/`
- `outputs/`
- `my_project.Rproj`

The default is meant for analysts, not package developers.

## Presets

```r
create_analysis_project("my_project", preset = "analysis")
create_analysis_project("my_project", preset = "modelling")
create_analysis_project("my_project", preset = "geospatial")
create_analysis_project("my_project", preset = "pipeline")
create_analysis_project("my_project", preset = "package", mode = "advanced")
```

Use `mode = "advanced"` when you explicitly want package-style scaffolding such
as `DESCRIPTION`, `NAMESPACE`, `R/`, `tests/testthat/`, advanced loaders, or
targets-oriented support files.

## User-facing helpers

The package also provides helpers that reduce path and file-format decisions:

```r
projectSetupR::project_status()
projectSetupR::new_project_object("clean_trial_data", type = "data_cleaning")
projectSetupR::run_project_object("clean_trial_data")
projectSetupR::save_project_object(mtcars, name = "trial_data", type = "dataset")
projectSetupR::run_project()
projectSetupR::render_project_reports()
```

## Development

Template assets live under `inst/templates/` and scaffold logic lives under
`R/`.
