# Copy Variable Labels and Attributes Across Data Frames

Transfers column-level attributes (such as \`label\`, \`labels\`, and
\`format.sas\`) from a source data frame (\`from\`) to matching columns
in a target data frame (\`to\`).

## Usage

``` r
copy_var_labels(from, to, attrs = c("label", "labels", "format.sas"))
```

## Arguments

- from:

  Source data frame containing labelled variables.

- to:

  Target data frame to receive variable attributes.

- attrs:

  Character vector of attribute names to copy (default \`c("label",
  "labels", "format.sas")\`).

## Value

Target data frame \`to\` with transferred attributes.

## Examples

``` r
df1 <- data.frame(age = 50, bmi = 24.5)
attr(df1$age, "label") <- "Age at Enrollment (years)"
df2 <- data.frame(age = 60, bmi = 28.0)
df2 <- copy_var_labels(df1, df2)
attr(df2$age, "label")
#> [1] "Age at Enrollment (years)"
```
