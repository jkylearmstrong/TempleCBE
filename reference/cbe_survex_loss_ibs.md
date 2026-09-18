# TempleCBE Custom Loss Functions for survex

Provides survival loss functions implementing TempleCBE's
inverse-probability-of-censoring weighted (IPCW) Graf Integrated Brier
Score and time-dependent Brier Score for use with
[`survex::model_parts()`](https://modeloriented.github.io/survex/reference/model_parts.surv_explainer.html)
and
[`survex::model_performance()`](https://modeloriented.github.io/survex/reference/model_performance.surv_explainer.html).

## Usage

``` r
cbe_survex_loss_ibs(normalization = NULL, max_quantile = 1, trunc = 0.05)

cbe_survex_loss_brier(trunc = 0.05)
```

## Arguments

- normalization:

  Optional normalization parameter for `survex` integral calculation.
  Can be `NULL`, `"t_max"`, or `"survival"`.

- max_quantile:

  Upper quantile cutoff for evaluation time window. Default is 1.

- trunc:

  Lower truncation threshold for censoring survival probabilities to
  prevent inflation by rare tails. Default is 0.05.

## Value

A loss function with attributes `"loss_type"` (`"integrated"` or
`"time-dependent"`) and `"loss_name"`, directly passable to `survex`.

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("survex", quietly = TRUE)) {
  loss_ibs <- cbe_survex_loss_ibs()
  # mp <- survex::model_parts(explainer, loss_function = loss_ibs)
}
} # }
```
