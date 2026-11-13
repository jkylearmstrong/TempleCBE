# Cox Model on Quantile Bins with an Explicit Missing Level

Fits \`Surv(time, status) ~ bins\` where the bins come from
\[cut_with_missing_category()\] and reports each level against the
reference (the lowest bin).

## Usage

``` r
tbl_cox_binned_missing(
  data,
  time_col,
  status_col,
  feature,
  n_bins = 3,
  labels = NULL,
  missing_label = "Missing",
  reference = NULL,
  subset_mask = NULL,
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame.

- time_col, status_col:

  Survival time and event indicator.

- feature:

  Numeric feature column.

- n_bins, labels, missing_label, reference, subset_mask:

  See \[cut_with_missing_category()\].

- conf_level:

  Confidence level.

## Value

A tibble with one row per level: \`level\`, \`n\`, \`events\`, \`hr\` (1
for the reference), \`conf.low\`, \`conf.high\`, \`p.value\` (\`NA\` for
the reference) and \`reference\`. The Cox fit is in the attribute
\`fit\`.

## See also

\[cut_with_missing_category()\], \[cox_single()\]

## Examples

``` r
set.seed(1)
d <- data.frame(time = rexp(200, 0.1), status = rbinom(200, 1, 0.6), x = rnorm(200))
d$x[sample(200, 60)] <- NA
tbl_cox_binned_missing(d, "time", "status", "x")
#> # A tibble: 4 × 8
#>   level       n events    hr conf.low conf.high p.value reference
#>   <chr>   <int>  <int> <dbl>    <dbl>     <dbl>   <dbl> <lgl>    
#> 1 Low        47     25 1       NA         NA     NA     TRUE     
#> 2 Med        46     27 0.935    0.541      1.62   0.810 FALSE    
#> 3 High       47     25 0.951    0.545      1.66   0.859 FALSE    
#> 4 Missing    60     42 0.966    0.586      1.59   0.892 FALSE    
```
