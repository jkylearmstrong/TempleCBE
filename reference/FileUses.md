# Construct a FileUses object

User-friendly constructor function for instantiating a
[`FileUses`](https://jkylearmstrong.github.io/TempleCBE/reference/FileUses-class.md)
object.

## Usage

``` r
FileUses(
  name,
  path,
  dependencies = list(),
  renders = FALSE,
  stage = NA_character_,
  artifact_role = NA_character_,
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

- renders:

  Logical. Indicates whether the file is a rendered file or rendering
  script. Defaults to `FALSE`.

- stage:

  Character. Pipeline lifecycle stage.

- artifact_role:

  Character. Pipeline role of the artifact.

- description:

  Character. Brief description of the file.

## Value

A new
[`FileUses`](https://jkylearmstrong.github.io/TempleCBE/reference/FileUses-class.md)
object.
