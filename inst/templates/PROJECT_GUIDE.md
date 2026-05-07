# Project guide

## Where do I put raw data?

Put original input files in `data/raw/`.

## Where do I put cleaned data?

Use `data/interim/` for intermediate files and `data/processed/` for final
analysis-ready data.

## Where do I write reusable functions?

Write reusable functions in `{{ reusable_code_path }}`.

## Where do I write step-by-step scripts?

Write procedural workflow scripts in `scripts/`.

## Where do I write reports?

Write Quarto or R Markdown source reports in `reports/`.

## Where are generated outputs saved?

Generated tables, figures, models, reports, and logs should go to `outputs/`.

## What should I edit first?

Start with `scripts/01_import_data.R`, then update reports as needed.

## What should I avoid editing manually?

Avoid manually editing hidden support files such as {{ hidden_support_files }}.

## How do I install packages?

Run:

```r
source("scripts/install_packages.R")
```

## How do I render reports?

{{ project_guide_render_section }}

## How do I run the pipeline, if enabled?

{{ project_guide_pipeline_section }}
