# FilePath S4 Class

File paths are unique identifiers for files and are used to track
changes in the file system. A file path is a general R, Python, SAS,
Quarto, shell, etc. agnostic way to refer to a file. It is not tied to
any specific software or programming language, but rather to the file
system itself.

## Usage

``` r
# S4 method for class 'FilePath'
show(object)
```

## Arguments

- object:

  A `FilePath` object.

## Details

Represents an individual file node in a computational pipeline
dependency graph. A `FilePath` object encapsulates file-level metadata
and state: tracking modification timestamps (`mtime`), file extensions
(`file_ext`), workflow lifecycle milestones (`stage`), semantic pipeline
roles (`artifact_role`), and rendering status (`renders`).

Upon instantiation, the object automatically inspects the target file on
disk: if the file exists at `path`, `mtime` is populated from
`file.info()$mtime` and `file_ext` is extracted via
[`tools::file_ext()`](https://rdrr.io/r/tools/fileutils.html).

## Slots

- `name`:

  Character. A user-supplied, human-readable name for the file, used to
  refer to the file across other parts of the pipeline and dependency
  graph.

- `path`:

  Character. The absolute or project-relative path to the file on the
  file system.

- `mtime`:

  POSIXct. The modification time of the file on the file system.
  Automatically populated from disk during object initialization if the
  file exists.

- `file_ext`:

  Character. The file extension of the file (e.g., `"xlsx"`, `"rds"`,
  `"qmd"`). Automatically populated from the file path during object
  initialization.

- `renders`:

  Logical. A logical indicating whether the file is a rendered file
  (i.e. a file that is produced by a rendering process) or a rendering
  source script. Defaults to `FALSE`.

- `stage`:

  Character. The workflow lifecycle stage of the file in the pipeline
  (e.g., `"01_EDA"`, `"02_Analysis_Original"`, `"03_Imputation"`).

- `artifact_role`:

  Character. The semantic role of the file in the pipeline. Standard
  roles include:

  - `"raw_data"`: Original immutable input data (e.g. source
    spreadsheets).

  - `"derived_data"`: Primary processed baseline dataset generated from
    raw inputs.

  - `"enhanced_data"`: Imputed or augmented analytical dataset.

  - `"intermediate_data"`: Stage-internal calculation artifact or model.

  - `"report_source"`: Executable source document (e.g., Quarto `.qmd`).

  - `"deliverable_report"`: Rendered deliverable (e.g., `.pdf`,
    `.html`).

  - `"helper_script"`: Child or modular helper script invoked by parent
    reports.

  - `"reference"`: Lookup tables, crosswalks, or project configuration
    files.

- `description`:

  Character. A brief human-readable description of the file's contents
  or analytical purpose.

## See also

[`FileUses`](https://jkylearmstrong.github.io/TempleCBE/reference/FileUses-class.md),
[`FileOutputs`](https://jkylearmstrong.github.io/TempleCBE/reference/FileOutputs-class.md),
[`FilePath`](https://jkylearmstrong.github.io/TempleCBE/reference/FilePath.md),
[`create_qmd_renderer`](https://jkylearmstrong.github.io/TempleCBE/reference/create_qmd_renderer.md)
