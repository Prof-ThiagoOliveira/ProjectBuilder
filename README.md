# projflow

`projflow` helps you create, amend, diagnose, and manage reproducible analysis projects without turning the project itself into a heavy framework.

The package is designed for statistical and analytical work where:

- source code, reports, registry files, and lightweight outputs belong in Git;
- raw and large data usually live outside the repository;
- projects should be easy to check, extend, and repair;
- common tasks such as adding scripts, reports, tables, figures, apps, and outputs should be obvious.

## Who it is for

`projflow` is aimed at analysts, statisticians, data scientists, and research teams who want a repeatable project structure without manually wiring folders, registry files, or helper scripts.

## Core workflow

```r
projflow::new_project(
  "trial_analysis",
  preset = "statistical_report",
  include_example = FALSE
)

projflow::set_project_data_root("/path/to/external/data")

projflow::new_script(
  "fit_mixed_model",
  type = "statistical_analysis"
)

projflow::new_report("main_report")

projflow::check_project()
projflow::build_project()
```

## What a project contains

A typical project created by `projflow` includes:

- `analysis/` for runnable source scripts
- `reports/` for Quarto or R Markdown source files
- `outputs/` for lightweight generated artefacts
- `.projflow/project_registry.yml` for registry metadata
- `.projflow/local.yml` for machine-specific local settings
- `.projflow/activity_log.yml` for recorded management actions
- `.projflow/backups/` for automatic safety backups
- `project.yml` for project-level configuration
- `README.md` and `run_project.R` for project guidance

## Source files, generated outputs, local files, and external data

These categories are intentionally distinct:

- Source files: scripts, reports, app code, configuration, and governance docs that should normally be version-controlled.
- Generated outputs: tables, figures, models, rendered reports, and other artefacts under `outputs/`.
- Local files: machine-specific settings such as external data roots stored in `.projflow/local.yml`.
- External data: raw and large data outside the repository, resolved through `project_data_root()` and `project_data_path()`.

## Create and amend a project

Create a minimal project:

```r
projflow::new_project(
  "my_project",
  preset = "basic_analysis",
  infrastructure = character(),
  include_example = FALSE
)
```

Preview a scaffold without writing files:

```r
projflow::plan_project(
  path = "planned_project",
  preset = "statistical_report",
  include_example = FALSE
)
```

Populate missing standard files in an existing directory:

```r
projflow::new_project(
  "existing_project",
  preset = "basic_analysis",
  repair = TRUE,
  include_example = FALSE
)
```

Dry-run project creation:

```r
projflow::new_project(
  "planned_project",
  preset = "basic_analysis",
  dry_run = TRUE
)
```

## Add scripts, reports, tables, figures, apps, and outputs

Create a script only:

```r
projflow::new_script("clean_inputs", type = "data_cleaning")
```

Create a script and register an explicit output:

```r
projflow::new_script(
  "fit_model",
  type = "model",
  output = "outputs/models/model_fit.rds"
)
```

Register outputs directly:

```r
projflow::new_table("summary_statistics")
projflow::new_figure("heritability_plot")
projflow::new_output("model_fit", type = "model")
```

Create reports and apps:

```r
projflow::new_report("main_report")
projflow::new_app(type = "shiny")
```

By default, `new_script()` does not create `.rds` files or register placeholder outputs. Outputs are opt-in.

## External data roots

Store machine-specific data locations outside the repository:

```r
projflow::set_project_data_root("/mnt/project_data", name = "default")
projflow::set_project_data_root("/mnt/reference_data", name = "reference")
```

Resolve paths reproducibly:

```r
projflow::project_data_path("phenotypes.csv")
projflow::project_data_path("maps", "field_map.gpkg", source = "reference")
```

`projflow` writes these local settings to `.projflow/local.yml`, which should not be committed.

## Run, build, and render

Set up a script environment:

```r
projflow::setup_project()
```

Run registered scripts:

```r
projflow::run_project()
```

Build the project:

```r
projflow::build_project(render_reports = FALSE)
projflow::build_project(render_reports = TRUE)
```

Render reports only:

```r
projflow::render_project_reports()
```

## Diagnose a project

Run a read-only project check:

```r
projflow::check_project()
```

The returned object contains machine-readable issues by severity:

- `error`
- `warning`
- `suggestion`
- `info`

For a richer read-only inspection:

```r
projflow::project_diagnostics_data()
projflow::diagnose_project(output = "data")
projflow::diagnose_project(output = "app")
```

The diagnostics app focuses on metadata, paths, registry state, dependencies, and file existence. It does not inspect raw data contents by default.

## Interactive Project Manager

`projflow` also includes an optional Shiny Project Manager. It remains optional:
normal project creation, checks, builds, and registry operations work without
Shiny installed.

```r
projflow::launch_project_manager()
projflow::diagnose_project(output = "html")
projflow::diagnose_project(output = "data")
```

The dashboard is backed by the same command-line functions used elsewhere in the
package. It can:

- review project health and registered objects;
- add scripts, reports, outputs, tasks, risks, milestones, and decisions;
- inspect missing or stale outputs;
- view the project network;
- preview safe repairs;
- review activity logs and registry backups.

## Remove and rename objects safely

Remove registry entries without deleting files:

```r
projflow::remove_project_output("summary_statistics")
projflow::remove_project_script("clean_inputs")
```

Delete files only with explicit confirmation:

```r
projflow::remove_project_script(
  "clean_inputs",
  delete_files = TRUE,
  confirm = TRUE
)
```

Preview destructive changes first:

```r
projflow::remove_project_output("summary_statistics", dry_run = TRUE)
projflow::rename_project_script("fit_model", "fit_final_model", dry_run = TRUE)
```

Rename registered objects coherently:

```r
projflow::rename_project_script("fit_model", "fit_final_model")
projflow::rename_project_report("main_report", "client_report")
projflow::rename_project_output("model_fit", "model_fit_v2")
```

## Governance helpers

`projflow` can also track project-management metadata:

```r
projflow::add_project_task("Review diagnostics", priority = "high")
projflow::add_project_milestone("Draft report")
projflow::add_project_decision("Use external data", "Keep raw data outside the repository")
projflow::add_project_risk("Missing inputs", mitigation = "Confirm availability before modelling")
projflow::project_status_report(output = "data")
```

These records live in `.projflow/tasks.yml`.

## Git, renv, Quarto, and GitHub Actions

- Git: `projflow` creates `.gitignore` entries for local settings and generated artefacts.
- `renv`: if enabled, restore packages with `renv::restore()`.
- Quarto: if selected, `projflow` can scaffold `_quarto.yml` and render `.qmd` reports.
- GitHub Actions: workflow files can be scaffolded when requested.

## Learn more

See `vignette("projflow")` for the main package workflow and
`vignette("project-manager")` for the interactive Project Manager.
