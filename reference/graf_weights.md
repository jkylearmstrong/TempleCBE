# Inverse Probability of Censoring Weights (Graf et al.)

Computes the weights time-dependent survival metrics need so that
subjects lost to follow-up don't bias the score: at evaluation time
\\t\\, a subject still under observation (\\T \> t\\) gets \\1/G(t)\\; a
subject with an event by \\t\\ gets \\1/G(T^-)\\; a subject censored by
\\t\\ gets 0, as their status at \\t\\ is unknown.

## Usage

``` r
graf_weights(truth, eval_time, censoring, trunc = 0.05)
```

## Arguments

- truth:

  A right-censored \[survival::Surv()\] object, one element per subject
  (see \[surv_subject_truth()\]).

- eval_time:

  Numeric vector of evaluation times.

- censoring:

  A \[censoring_km()\] object, estimated on the training data.

- trunc:

  Lower bound for \\G\\, capping weights at \`1 / trunc\` (default 0.05,
  as in tidymodels' \`parsnip\`).

## Value

A numeric matrix with one row per element of \`truth\` and one column
per \`eval_time\`.

## Details

The weights assume follow-up starts at time 0 for every subject: they do
not correct for delayed entry (left truncation).

## References

Graf E, Schmoor C, Sauerbrei W, Schumacher M (1999). Assessment and
comparison of prognostic classification schemes for survival data.
\*Statistics in Medicine\*, 18(17-18), 2529-2545.

## See also

\[add_graf_weights()\], \[censoring_km()\]

## Examples

``` r
if (requireNamespace("survival", quietly = TRUE)) {
  train <- survival::Surv(c(2, 4, 6, 8), c(0, 1, 0, 1))
  test <- survival::Surv(c(5, 7), c(1, 0))
  graf_weights(test, eval_time = c(3, 6), censoring = censoring_km(train))
}
#>             3        6
#> [1,] 1.333333 1.333333
#> [2,] 1.333333 2.666667
```
