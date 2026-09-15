# Kaplan-Meier Estimate of the Censoring Distribution

Fits the "reverse" Kaplan-Meier estimator \\G(t) = P(C \> t)\\, treating
censoring as the event, for inverse-probability-of-censoring weights.
Estimate it on the data a model was trained on, and apply it to the data
being scored.

## Usage

``` r
censoring_km(truth)

# S3 method for class 'censoring_km'
predict(object, time, left = FALSE, trunc = 0, ...)
```

## Arguments

- truth:

  A right-censored \[survival::Surv()\] object with one element per
  subject, such as the \`.truth\` column of \[surv_subject_truth()\].

- object:

  A \`censoring_km\` object.

- time:

  Numeric vector of times.

- left:

  If \`TRUE\`, the left limit \\G(t^-)\\, i.e. censoring strictly before
  \`time\`.

- trunc:

  Lower bound applied to \\G\\, so weights stay finite.

- ...:

  Not used.

## Value

An object of class \`censoring_km\`, with \`time\` and \`surv\`; use
\`predict(object, time)\` for \\G(t)\\.

## See also

\[graf_weights()\]

## Examples

``` r
if (requireNamespace("survival", quietly = TRUE)) {
  cens <- censoring_km(survival::Surv(c(2, 4, 6, 8), c(0, 1, 0, 1)))
  predict(cens, c(1, 2, 5, 7))
}
#> [1] 1.000 0.750 0.750 0.375
```
