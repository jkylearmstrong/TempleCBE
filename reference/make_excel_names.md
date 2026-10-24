# Generate Excel-Compatible Column Names

Formats column names into sanitized, human-readable strings suitable for
Excel output headers.

## Usage

``` r
make_excel_names(names)
```

## Arguments

- names:

  A character vector of column names.

## Value

Sanitized character vector.

## Examples

``` r
make_excel_names(c("patient_id", "body_mass_index"))
#> [1] "Patient Id"      "Body Mass Index"
```
