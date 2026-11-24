# FileUses S4 Class

Represents a pipeline file that depends on other upstream files. Extends
[`FilePath`](https://jkylearmstrong.github.io/TempleCBE/reference/FilePath-class.md)
by adding upstream input dependency tracking.

## Usage

``` r
# S4 method for class 'FileUses'
show(object)
```

## Arguments

- object:

  A `FileUses` object.

## Details

A `FileUses` node represents a computational artifact whose execution or
validity relies upon one or more input
[`FilePath`](https://jkylearmstrong.github.io/TempleCBE/reference/FilePath-class.md)
objects listed in `dependencies`.

## Slots

- `dependencies`:

  List of
  [`FilePath`](https://jkylearmstrong.github.io/TempleCBE/reference/FilePath-class.md)
  objects that this file depends on.

## See also

[`FilePath`](https://jkylearmstrong.github.io/TempleCBE/reference/FilePath-class.md),
[`FileOutputs`](https://jkylearmstrong.github.io/TempleCBE/reference/FileOutputs-class.md),
[`FileUses`](https://jkylearmstrong.github.io/TempleCBE/reference/FileUses.md)
