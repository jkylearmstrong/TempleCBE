# Construct a FileOutputs object

User-friendly constructor function for instantiating a
[`FileOutputs`](https://jkylearmstrong.github.io/TempleCBE/reference/FileOutputs-class.md)
object.

## Usage

``` r
FileOutputs(
  name,
  path,
  dependencies = list(),
  output = list(),
  renders = TRUE,
  stage = NA_character_,
  artifact_role = "report_source",
  description = NA_character_
)
```

## Arguments

- name:

  Character. User-supplied human-readable name for the file.

- path:

  Character. Absolute or relative file system path.

- dependencies:

  List of
  [`FilePath`](https://jkylearmstrong.github.io/TempleCBE/reference/FilePath-class.md)
  objects that this file depends on.

- output:

  List of
  [`FilePath`](https://jkylearmstrong.github.io/TempleCBE/reference/FilePath-class.md)
  objects produced by this file.

- renders:

  Logical. Indicates whether the file is a rendering script. Defaults to
  `TRUE`.

- stage:

  Character. Pipeline lifecycle stage.

- artifact_role:

  Character. Pipeline role of the artifact. Defaults to
  `"report_source"`.

- description:

  Character. Brief description of the file.

## Value

A new
[`FileOutputs`](https://jkylearmstrong.github.io/TempleCBE/reference/FileOutputs-class.md)
object.
