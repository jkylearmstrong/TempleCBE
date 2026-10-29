# Quantile Bins with an Explicit Missing Level

Cuts a numeric variable into equal-count bins and gives missing values
their own level, so a survival model can ask whether being unmeasured
carries risk of its own. The reference level comes first (what
\`coxph()\` and \`lm()\` treat as the baseline) and the missing level
last.

## Usage

``` r
cut_with_missing_category(
  x,
  n_bins = 3,
  labels = NULL,
  missing_label = "Missing",
  reference = NULL,
  subset_mask = NULL
)
```

## Arguments

- x:

  Numeric vector.

- n_bins:

  Number of bins (at least 2).

- labels:

  Bin labels, lowest to highest; default \`Low\`/\`Med\`/\`High\` for 3
  bins, \`Q1\`... otherwise.

- missing_label:

  Label of the missing level; no such level is added if nothing is
  missing.

- reference:

  Label of the reference level, one of \`labels\`.

- subset_mask:

  Optional logical vector (length of \`x\`): compute the quantile breaks
  on these rows only (for example events only, or the training rows);
  all rows are then binned with those breaks.

## Value

A factor. The breaks are in the attribute \`breaks\` (inner cutpoints).

## See also

\[tbl_cox_binned_missing()\]

## Examples

``` r
x <- c(1:9, NA, NA)
table(cut_with_missing_category(x))
#> 
#>     Low     Med    High Missing 
#>       3       3       3       2 
```
