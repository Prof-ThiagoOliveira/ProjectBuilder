# projflow 0.0.1.9000

## Redesign

- Reorganised the intended package architecture around four layers:
  1. project structure;
  2. executable analysis DAG;
  3. governance;
  4. interfaces and integrations.
- Added `project_layers()` and `project_structure()` as the canonical structure-layer inspection functions.
- Added a DAG-first execution layer:
  - `project_analysis_dag()`
  - `validate_project_dag()`
  - `topological_project_order()`
  - `plot_project_dag()`
  - `write_targets_pipeline()`
- Updated `run_project()` to prefer topological DAG order and to fall back to registry script order only when the DAG is invalid.
- Added DAG validation to `check_project()`.
- Added tests for DAG construction, ordering and cycle detection.

## API clean-up

- Reduced the intended public API by removing legacy aliases from `NAMESPACE`.
- Kept lower-level helpers internally where the dashboard and wrappers still depend on them.
- Removed user-facing manual pages for legacy aliases.
- Rewrote README and vignettes around the layered architecture and DAG-first workflow.

## Bug fixes

- Resolved the duplicate `validate_project_plan()` definition by renaming the keyword-level helper to `validate_project_plan_keywords()` and keeping `validate_project_plan()` for project-plan object validation only.
