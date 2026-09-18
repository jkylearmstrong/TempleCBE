# Apply Metadata and Variable Roles to an R Database

Synchronizes updated metadata (such as variable labels, dataset
descriptions, database names, and variable roles) back into a database
list or data frame.

## Usage

``` r
apply_database_metadata(db, metadata)
```

## Arguments

- db:

  A named list of data frames (an R database) or a single data frame.

- metadata:

  A data frame/tibble of metadata containing columns `columns` and
  optionally `dataset_name`, `labels`, `dataset_label`, `database_name`,
  `role`, `X_var`, `Y_var`, `ID_var`, `Time_var`.

## Value

The modified database or data frame with attributes and labels updated.

## Examples

``` r
db <- list(demo = data.frame(id = 1:2, age = c(20, 30)))
meta <- get_dataset_info(db)
meta$labels[meta$columns == "age"] <- "Age at Enrollment (years)"
db <- apply_database_metadata(db, meta)
```
