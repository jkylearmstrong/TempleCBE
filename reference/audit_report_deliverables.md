# Audit Deliverable File Tokens in Analysis Scripts

Scans source Quarto and R scripts to extract read/write file references
and verify whether declared data and report deliverables exist on disk.

## Usage

``` r
audit_report_deliverables(
  analysis_path = ".",
  file_pattern = "\\.(xlsx|rds|pdf|docx|html)$"
)
```

## Arguments

- analysis_path:

  Directory containing Quarto (.qmd) and R (.R) scripts.

- file_pattern:

  Regular expression matching deliverable extensions (default:
  `"\.(xlsx|rds|pdf|docx|html)$"`).

## Value

A tibble summarizing referenced deliverable files, source lines, and
on-disk existence.
