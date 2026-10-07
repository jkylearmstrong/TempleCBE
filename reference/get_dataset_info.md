# Summarize a Data Frame or Joint Model's Columns and Components

Per-column metadata: class, variable label (if set via labelled),
mean/sd for numeric columns, most-frequent value, distinct-value count,
and missingness.
[`survival::Surv`](https://rdrr.io/pkg/survival/man/Surv.html) columns
are summarized from their underlying time/status or start/stop
counting-process matrix rather than unrolled as plain numerics. When
provided a
[`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md)
object, summarizes the fitted training data and attaches model metadata.
When provided a list of data frames (an R database), returns a
consolidated data dictionary across all tables.

## Usage

``` r
get_dataset_info(x, ...)

# S3 method for class 'data.frame'
get_dataset_info(
  x,
  subject_id = NULL,
  dataset_name = NULL,
  database_name = NULL,
  dataset_label = NULL,
  ...
)

# S3 method for class 'list'
get_dataset_info(x, subject_id = NULL, database_name = NULL, ...)

# S3 method for class 'joint_model'
get_dataset_info(x, ...)

proc_contents(x, ...)
```

## Arguments

- x:

  A data frame, tibble, named list of data frames, or a fitted
  [`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md)
  object.

- ...:

  Additional arguments passed to methods.

- subject_id:

  Optional character string specifying the subject identifier column for
  repeated-measures longitudinal datasets to audit time-varying vs.
  baseline features.

- dataset_name:

  Optional character string overriding the displayed dataset name.

- database_name:

  Optional character string specifying the parent database name.

- dataset_label:

  Optional character string overriding the dataset-level description.

## Value

A tibble with one row per column: `dataset_name`, (optional
`database_name` and `dataset_label`), `labels`, `columns`, `class`,
`mean`, `sd`, `most_freq`, `n_distinct`, `SumNa`, `PctNa`, and
optionally `variable_type` when `subject_id` is specified.

## Examples

``` r
get_dataset_info(mtcars)
#> # A tibble: 11 × 10
#>    dataset_name labels columns class     mean      sd most_freq n_distinct SumNa
#>    <chr>        <chr>  <chr>   <chr>    <dbl>   <dbl> <chr>          <int> <int>
#>  1 mtcars       mpg    mpg     numer…  20.1     6.03  10.4              25     0
#>  2 mtcars       cyl    cyl     numer…   6.19    1.79  8                  3     0
#>  3 mtcars       disp   disp    numer… 231.    124.    275.8             27     0
#>  4 mtcars       hp     hp      numer… 147.     68.6   110               22     0
#>  5 mtcars       drat   drat    numer…   3.60    0.535 3.07              22     0
#>  6 mtcars       wt     wt      numer…   3.22    0.978 3.44              29     0
#>  7 mtcars       qsec   qsec    numer…  17.8     1.79  17.02             30     0
#>  8 mtcars       vs     vs      numer…   0.438   0.504 0                  2     0
#>  9 mtcars       am     am      numer…   0.406   0.499 0                  2     0
#> 10 mtcars       gear   gear    numer…   3.69    0.738 3                  3     0
#> 11 mtcars       carb   carb    numer…   2.81    1.62  2                  6     0
#> # ℹ 1 more variable: PctNa <dbl>
```
