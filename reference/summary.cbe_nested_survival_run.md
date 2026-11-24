# Summarize a \`cbe_nested_survival_run\` Object

Summarize a \`cbe_nested_survival_run\` Object

## Usage

``` r
# S3 method for class 'cbe_nested_survival_run'
summary(object, ...)
```

## Arguments

- object:

  A \[nested_survival_run()\] object.

- ...:

  Unused.

## Value

A list of class \`summary.cbe_nested_survival_run\` with elements
\`overview\` (1-row tibble of run metrics), \`model_summary\` (per-model
outer IBS summary), \`top_importance\` (top predictors by selection
frequency), and \`censoring_agreement\` (Spearman and Kendall rank
correlations).
