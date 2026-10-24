# Sanitize Column Names for Excel Worksheets

Replaces characters forbidden by Excel (e.g. \`\\ / ? \* : \[ \]\`) with
underscores or spaces, trims leading/trailing spaces, and optionally
truncates names.

## Usage

``` r
excel_colnames(names, max_char = NULL, replacement = "_")
```

## Arguments

- names:

  Character vector of proposed column or sheet names.

- max_char:

  Optional integer maximum character length (e.g. 31 for Excel sheet
  names).

- replacement:

  Replacement character for invalid symbols (default \`"\_"\`).

## Value

Character vector of Excel-safe names.

## Examples

``` r
excel_colnames(c("Patient/Subject [ID]", "Cost ($/hr)?"))
#> [1] "Patient_Subject _ID_" "Cost ($_hr)_"        
```
