# FileOutputs S4 Class

Represents an executable or rendering pipeline file that produces
downstream output artifacts.

## Usage

``` r
# S4 method for class 'FileOutputs'
show(object)
```

## Arguments

- object:

  A `FileOutputs` object.

## Details

Inherits from
[`FileUses`](https://jkylearmstrong.github.io/TempleCBE/reference/FileUses-class.md)
(inheriting upstream `dependencies`), while adding a list of downstream
output
[`FilePath`](https://jkylearmstrong.github.io/TempleCBE/reference/FilePath-class.md)
objects produced when this file executes or renders.

## Slots

- `output`:

  List of
  [`FilePath`](https://jkylearmstrong.github.io/TempleCBE/reference/FilePath-class.md)
  objects produced by this file.

## See also

[`FilePath`](https://jkylearmstrong.github.io/TempleCBE/reference/FilePath-class.md),
[`FileUses`](https://jkylearmstrong.github.io/TempleCBE/reference/FileUses-class.md),
[`FileOutputs`](https://jkylearmstrong.github.io/TempleCBE/reference/FileOutputs.md),
[`create_qmd_renderer`](https://jkylearmstrong.github.io/TempleCBE/reference/create_qmd_renderer.md)
