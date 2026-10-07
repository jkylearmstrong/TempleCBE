# Validate a Plug-and-Play Column Mapping

Verifies that a mapping dictionary has the required schema and logical
constraints: columns `INDEX`, `old`, `new`, `X_var`, `Y_var`, `ID_var`,
`Time_var`, `duplicate_of`, and `duplicate_action`.

## Usage

``` r
validate_column_mapping(mapping)
```

## Arguments

- mapping:

  A data frame/tibble containing the column mapping rules.

## Value

`TRUE`, invisibly, if valid; otherwise raises an error listing all
issues.
