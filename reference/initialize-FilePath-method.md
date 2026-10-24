# Initialize a FilePath object

Automatically inspects the file path on disk to populate modification
time (`mtime`) and file extension (`file_ext`) if the file exists.

## Usage

``` r
# S4 method for class 'FilePath'
initialize(.Object, ...)
```

## Arguments

- .Object:

  The FilePath object being initialized.

- ...:

  Additional slot arguments passed to
  [`callNextMethod`](https://rdrr.io/r/methods/NextMethod.html).

## Value

An initialized `FilePath` object.
