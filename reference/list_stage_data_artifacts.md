# Inspect and categorize all on-disk data artifacts for a pipeline stage

Scans a directory for data files (.rds, .xlsx, .csv) and distinguishes
primary pipeline handoff datasets from intermediate analysis tables.

## Usage

``` r
list_stage_data_artifacts(
  stage_dir,
  stage = NA_character_,
  primary_patterns = c("^DATA\\.rds$", "^INDEX\\.rds$", "Index_Scores",
    "imputed_DATA")
)
```

## Arguments

- stage_dir:

  Directory path to scan (e.g. analysis/2024_09/EDA/data).

- stage:

  Optional stage identifier to tag the artifacts with.

- primary_patterns:

  Regex pattern identifying primary handoff datasets.

## Value

A tibble of data artifacts with name, path, stage, role, size_kb, and
mtime.
