# Get or Set Dataset and Database Labels

Extends labelled variable-level conventions to dataset-level and
database-level metadata attributes.

## Usage

``` r
cbe_dataset_label(x)

cbe_dataset_label(x) <- value

cbe_database_name(x)

cbe_database_name(x) <- value

cbe_database_label(x)

cbe_database_label(x) <- value

cbe_set_dataset_labels(x, labels)

cbe_get_dataset_labels(x)
```

## Arguments

- x:

  A data frame (for dataset functions) or a named list of data frames
  (for database functions).

- value:

  A character string specifying the label or name.

- labels:

  A named character vector or list mapping dataset names to dataset
  labels.

## Value

For getters, the label or name string (or `NULL` if unset). For setters,
the modified object `x` with the attribute attached.

## Examples

``` r
df <- mtcars
cbe_dataset_label(df) <- "1974 Motor Trend Car Road Tests"
cbe_dataset_label(df)
#> [1] "1974 Motor Trend Car Road Tests"
```
