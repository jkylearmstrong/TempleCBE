# Tidy a \`cbe_nested_survival_run\` Object

Extracts a tidy tibble from a \[nested_survival_run()\] object.

## Usage

``` r
# S3 method for class 'cbe_nested_survival_run'
tidy(
  x,
  component = c("importance", "outer_scores", "coefficients", "candidates", "tuning",
    "censoring", "validation", "overview"),
  ...
)
```

## Arguments

- x:

  A \[nested_survival_run()\] object.

- component:

  Which component table to extract: \`"importance"\` (default; predictor
  selection frequencies and coefficient summaries), \`"outer_scores"\`,
  \`"coefficients"\`, \`"candidates"\`, \`"tuning"\`, \`"censoring"\`,
  \`"validation"\`, or \`"overview"\`.

- ...:

  Unused.

## Value

A tibble.
