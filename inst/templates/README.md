# Start here

1. Open the `.Rproj` file.
2. Run:

```r
source("scripts/00_start_here.R")
```

3. Add raw data to `data/raw/`.
4. Edit `scripts/01_import_data.R`.
5. Write reports in `reports/`.

## Key folders

| Folder | Purpose |
| --- | --- |
| `data/raw/` | Original input data |
| `data/interim/` | Intermediate cleaned data |
| `data/processed/` | Final analysis-ready data |
| `{{ reusable_code_path }}` | Reusable project functions |
| `scripts/` | Step-by-step workflow scripts |
| `reports/` | Quarto or R Markdown source reports |
| `outputs/` | Generated tables, figures, models, reports, and logs |

Read `PROJECT_GUIDE.md` for more detail.
