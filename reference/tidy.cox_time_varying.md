# Tidy a \`cox_time_varying\` Model

Tidy a \`cox_time_varying\` Model

## Usage

``` r
# S3 method for class 'cox_time_varying'
tidy(x, ...)
```

## Arguments

- x:

  A \[cox_time_varying()\] fit.

- ...:

  Not used.

## Value

The \`terms\` tibble: \`term\`, \`estimate\` (log hazard ratio),
\`std_error\`, \`hr\`, \`conf_low\`, \`conf_high\`, \`statistic\`,
\`p_value\`, and \`time_varying\` (\`TRUE\` for the \`tt()\` rows). For
a \`tt()\` row the hazard ratio is the multiplicative change in the
hazard ratio per unit of the time function, not a hazard ratio at a
fixed time; use \[hazard_ratio_at_time()\] for that.
