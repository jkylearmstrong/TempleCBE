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
  graph). The `stage` of each `FileOutputs` becomes a folder name in the
  archive: path separators and the characters a file name cannot hold on
  Windows become `-`, and leading or trailing dots, dashes and spaces
  are dropped (`"../x"` gives `"x"`).

- output_formats:

  Character vector of report formats to include (`"pdf"`, `"docx"`,
  `"html"`, or `"all"`).

- data_deliverables:

  Optional list or vector of `FilePath` objects or file paths for data
  spreadsheets/RDS files.

- zip_path:

  Destination path for the generated zip archive. If NULL, defaults to a
  timestamped archive. Deliverables that share a file name within a
  folder are kept, the later ones with a numeric suffix (and a warning),
  rather than overwriting each other.

- include_pipeline_graph:

  Logical; whether to include static/interactive pipeline DAG graphs
  (default: TRUE).

## Value

Absolute path to the generated zip file.
