# Package Pipeline Deliverables into a Structured Zip Archive

Traverses computational pipeline objects (or explicit file lists),
gathers all rendered reports and data deliverables, verifies existence
on disk, organizes them into stage-prefixed folders, and builds a
distribution ZIP file.

## Usage

``` r
package_deliverables(
  pipeline_objects,
  output_formats = c("pdf", "docx"),
  data_deliverables = list(),
  zip_path = NULL,
  include_pipeline_graph = TRUE
)
```

## Arguments

- pipeline_objects:

  A list of `FilePath` / `FileOutputs` objects (e.g. from the compute
  graph).

- output_formats:

  Character vector of report formats to include (`"pdf"`, `"docx"`,
  `"html"`, or `"all"`).

- data_deliverables:

  Optional list or vector of `FilePath` objects or file paths for data
  spreadsheets/RDS files.

- zip_path:

  Destination path for the generated zip archive. If NULL, defaults to a
  timestamped archive.

- include_pipeline_graph:

  Logical; whether to include static/interactive pipeline DAG graphs
  (default: TRUE).

## Value

Absolute path to the generated zip file.
