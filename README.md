# projflow

`projflow` creates lightweight, component-driven analysis projects. The repository is for code, reports and project metadata; data are expected to live outside the repository by default.

## Default workflow

```r
projflow::new_project(
  "my_project",
  components = c("data_preparation", "statistical_analysis", "report"),
  deliverables = c("html_report", "tables", "figures")
)

projflow::set_project_data_root("D:/shared/project_data/my_project")

projflow::new_script(
  name = "clean_phenotypes",
  type = "data_cleaning"
)

projflow::check_project(deep = TRUE)
projflow::build_project()
```

## Core idea

Projects are assembled from:

- `components`
- `deliverables`
- `infrastructure`
- an optional `preset`

`projflow` infers an internal scaffold level from those choices, but that is not part of the main public API.

## Main helpers

```r
projflow::set_project_data_root("path/to/external/data")
projflow::project_data_path("file.csv")
projflow::new_script("clean_phenotypes", type = "data_cleaning")
projflow::check_project(deep = TRUE)
projflow::build_project()
```

## Typical structure

- `README.md`
- `run_project.R`
- `project.yml`
- `analysis/`
- `reports/`
- `outputs/`
- `.projectSetupR/project_registry.yml`
- `.projectSetupR/local.yml`
- `my_project.Rproj`

`outputs/` is intended for small generated artefacts such as tables, plots, model summaries and governance summaries. Raw data, cleaned data and large intermediate files should normally stay outside the repository.
