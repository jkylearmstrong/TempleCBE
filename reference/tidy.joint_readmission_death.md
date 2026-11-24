# Tidy a \`joint_readmission_death\` Model

Tidy a \`joint_readmission_death\` Model

## Usage

``` r
# S3 method for class 'joint_readmission_death'
tidy(x, ...)
```

## Arguments

- x:

  A \[joint_readmission_death()\] fit.

- ...:

  Not used.

## Value

The \`terms\` tibble: \`process\` (\`"readmission"\` or \`"death"\`),
\`term\`, \`estimate\` (log hazard ratio), \`std_error\`, \`hr\`,
\`conf_low\`, \`conf_high\`, \`statistic\` and \`p_value\`.
