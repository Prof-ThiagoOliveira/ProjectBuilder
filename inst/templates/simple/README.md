# {{ project_name }}

## Start here

1. Open the `.Rproj` file.
2. Put raw input files in `data/raw/`.
3. Open `run_project.R`.
4. Run:

```r
source("run_project.R")
```

## What to edit

- `run_project.R`: the main entry point for the project.
- `project.yml`: editable project settings and parameters.
- `analysis/`: analysis scripts and workflow steps.
- `reports/`: report source files.

## Where things go

- `data/raw/`: original input data.
- `data/processed/`: cleaned or derived datasets.
- `outputs/`: final tables, figures, models, and rendered reports.

## Common commands

```r
projectSetupR::project_status()
projectSetupR::new_project_object("clean_trial_data", type = "data_cleaning")
projectSetupR::run_project_object("clean_trial_data")
projectSetupR::save_project_object(mtcars, name = "trial_data", type = "dataset")
projectSetupR::run_project()
projectSetupR::render_project_reports()
```

## Notes

- The default scaffold is intentionally small. Most project machinery lives in the installed `projectSetupR` package.
- The hidden `.projectSetupR/` folder is package-managed support state. You normally do not need to edit it.
- Suggested package preset: `{{ preset }}`.
