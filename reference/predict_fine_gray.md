# Predict the Cumulative Incidence Function in One Step

Fits a Fine-Gray model with \[fine_gray()\] and predicts the cumulative
incidence function for \`new_data\` (or the training data) in one call.

## Usage

``` r
predict_fine_gray(
  data,
  time_col,
  event_col,
  formula,
  new_data = NULL,
  times = NULL,
  failcode = 1,
  cencode = 0,
  ...
)
```

## Arguments

- data:

  A data frame, one row per subject.

- time_col:

  Follow-up time.

- event_col:

  Event type, as in \[tbl_cumulative_incidence()\]: a censoring code,
  then one code per cause.

- formula:

  One-sided formula of the predictors, for example \`~ age + arm\`.

- new_data:

  A data frame of new predictors. \`NULL\` (the default) predicts on
  \`data\`.

- times:

  Times to predict the CIF at; \`NULL\` is every event time of the cause
  of interest in \`data\`.

- failcode:

  Code of the cause of interest (default 1).

- cencode:

  Code for censoring (default 0).

- ...:

  Passed to \[fine_gray()\] (and through it to \`cmprsk::crr()\`).

## Value

As \[predict.fine_gray_model()\]: a tibble with one row per row of
\`new_data\`, with a \`.pred\` list-column of \`.eval_time\` /
\`.pred_cif\` tibbles.

## See also

\[fine_gray()\], \[predict.fine_gray_model()\]

## Examples

``` r
if (requireNamespace("cmprsk", quietly = TRUE)) {
  set.seed(1)
  d <- data.frame(time = rexp(150, 0.1), type = sample(0:2, 150, TRUE, prob = c(0.3, 0.4, 0.3)),
                  age = rnorm(150, 60, 8), arm = rep(c("A", "B"), 75))
  predict_fine_gray(d, "time", "type", ~ age + arm, new_data = d[1:3, ], times = c(5, 10))
}
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [2 × 2]>
#> 2 <tibble [2 × 2]>
#> 3 <tibble [2 × 2]>
```
