# projflow

`projflow` scaffolds and governs reproducible R analysis projects. The package is now organised around four explicit layers:

1. **Project structure**: folders, `project.yml`, `.projflow/`, registry files, local data-root configuration and path conventions.
2. **Analysis DAG**: the executable Directed Acyclic Graph from data inputs to scripts, outputs, reports and deliverables.
3. **Governance**: tasks, risks, decisions, milestones and activity records.
4. **Interfaces and integrations**: diagnostics, dashboard, GitHub Actions, `renv` and `targets` support.

The central design principle is that analysis should be represented as a DAG: inputs flow into scripts, scripts produce outputs, outputs feed reports, and reports produce deliverables. Governance and dashboard objects are useful, but they are deliberately kept separate from the executable analysis graph.

## Minimal workflow

```r
library(projflow)

plan <- plan_project(
  path = "analysis-project",
  components = c("data_preparation", "statistical_analysis", "report"),
  infrastructure = c("git", "renv", "targets")
)

new_project(plan = plan, open = FALSE)
setup_project("analysis-project")

new_script("clean_inputs", script_type = "data_preparation", root = "analysis-project")
new_script("fit_model", script_type = "statistical_analysis", root = "analysis-project")
new_output("model_results", type = "dataset", root = "analysis-project")

validate_project_dag(root = "analysis-project")
run_project(root = "analysis-project")
```

## Layer 1: project structure

Use this layer to create, inspect and initialise the durable project scaffold.

```r
project_layers()
project_structure("analysis-project")
setup_project("analysis-project")
```

The recommended default is to keep raw or large data outside the repository and configure machine-specific paths through local metadata:

```r
set_project_data_root("/path/to/data", root = "analysis-project")
project_data_path("phenotypes.csv", root = "analysis-project")
```

## Layer 2: analysis DAG

Use this layer to inspect, validate and execute the project workflow.

```r
dag <- project_analysis_dag("analysis-project")
dag$nodes
dag$edges

validate_project_dag(dag = dag)
topological_project_order("analysis-project", type = "scripts")
run_project("analysis-project")
```

`run_project()` now prefers DAG order over filename order. If the DAG is invalid, it warns and falls back to registry script order.

A minimal `targets` pipeline can be generated from the registered DAG:

```r
write_targets_pipeline("analysis-project", overwrite = TRUE)
```

## Layer 3: governance

Governance records help users manage analytical work without changing the executable DAG.

```r
add_project_task("Review model diagnostics", root = "analysis-project")
add_project_risk("Input data dictionary may be incomplete", root = "analysis-project")
add_project_decision("Use REML for variance-component estimation", root = "analysis-project")
add_project_milestone("Draft report complete", root = "analysis-project")

project_status_report(root = "analysis-project")
```

## Layer 4: interfaces and integrations

Diagnostics and interactive interfaces sit above the three core layers.

```r
check_project("analysis-project")
diagnose_project("analysis-project")
open_dashboard("analysis-project")
use_github_actions("analysis-project")
```

## Public API policy

The intended public API is the layered interface above. Older convenience aliases are not part of the redesigned public surface. Prefer canonical verbs such as `new_script()`, `new_report()`, `add_project_task()`, `mark_project_task_done()`, and `add_project_decision()`.

## Development checks

```r
check_project(deep = TRUE)
validate_project_dag()
```

The package is designed to help users maintain a clean project structure, preserve raw data, treat outputs as disposable and reproducible, and keep governance separate from execution.
