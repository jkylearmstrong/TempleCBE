# Univariable GLM / LMM Screens Over Inputs and Outputs

Fits \[glm_single()\] or \[lmm_single()\] for every combination of an
input (predictor) and an output (outcome). With \`normalize = TRUE\` the
main estimate is the one on the 0 to 1 scaled predictor (comparable
across inputs: the change in the output over the full observed range of
the input); with \`include_dy_dx = TRUE\` the estimate on the original
scale (the marginal effect \`dy/dx\`, change per unit of the input) is
added with the suffix \`\_dy_dx\`.

## Usage

``` r
batch_glm_single(
  data,
  inputs,
  outputs,
  normalize = TRUE,
  include_dy_dx = TRUE,
  family = stats::gaussian(),
  conf_level = 0.95,
  p_breaks = c(0.001, 0.01, 0.05),
  p_labels = c("***", "**", "*", "ns")
)

batch_lmm_single(
  data,
  inputs,
  outputs,
  id_col = "subject_id",
  normalize = TRUE,
  include_dy_dx = TRUE,
  conf_level = 0.95,
  p_breaks = c(0.001, 0.01, 0.05),
  p_labels = c("***", "**", "*", "ns")
)
```

## Arguments

- data:

  A data frame (long, with \`id_col\`, for \`batch_lmm_single()\`).

- inputs, outputs:

  Character vectors of predictor and response columns.

- normalize:

  Fit on the 0 to 1 scaled predictor as the main result. If \`FALSE\`,
  only the original scale is fitted and \`include_dy_dx\` is ignored.

- include_dy_dx:

  Add the original-scale columns (see above).

- family:

  Family for \`batch_glm_single()\`, passed to \[glm_single()\].

- conf_level:

  Confidence level.

- p_breaks, p_labels:

  Significance scheme; \`p_labels\` has one more entry than
  \`p_breaks\`.

- id_col:

  Subject column of \`batch_lmm_single()\` (random intercept).

## Value

A tibble with \`output\`, \`input\`, \`term\`, \`estimate\`,
\`std.error\`, \`statistic\`, \`p.value\`, \`conf.low\`, \`conf.high\`,
\`n\`, \`status\`, \`warning\`, \`p.adj\`, \`rank\`, \`significance\`
and, with \`include_dy_dx\`, the same estimate columns suffixed
\`\_dy_dx\`. A combination that could not be fitted keeps a row with its
\`status\`.

## Ranking and significance

\`rank\` orders the inputs within each output by decreasing absolute
main estimate (1 = largest; ties share the lowest rank).
\`significance\` is the label of \`p_breaks\` that the main p-value
falls below (the first break it is strictly below, as in
\[plot_coef_forest()\]), or the last label if none. No multiple-testing
adjustment is applied to \`p.value\`; \`p.adj\` is the
Benjamini-Hochberg adjustment over all combinations of the main fits.

## See also

\[glm_single()\], \[lmm_single()\], \[plot_coef_forest()\]

## Examples

``` r
batch_glm_single(mtcars, c("wt", "hp", "disp"), c("mpg", "qsec"))
#> # A tibble: 6 × 21
#>   output input term  estimate std.error statistic  p.value conf.low conf.high
#>   <chr>  <chr> <chr>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1 mpg    wt    wt      -20.9      2.19     -9.56  1.29e-10   -25.4    -16.4  
#> 2 mpg    hp    hp      -19.3      2.86     -6.74  1.79e- 7   -25.2    -13.5  
#> 3 mpg    disp  disp    -16.5      1.89     -8.75  9.38e-10   -20.4    -12.7  
#> 4 qsec   hp    hp       -5.22     0.951    -5.49  5.77e- 6    -7.17    -3.28 
#> 5 qsec   disp  disp     -2.51     0.951    -2.64  1.31e- 2    -4.45    -0.565
#> 6 qsec   wt    wt       -1.25     1.28     -0.972 3.39e- 1    -3.87     1.37 
#> # ℹ 12 more variables: n <int>, status <chr>, warning <chr>,
#> #   estimate_dy_dx <dbl>, std.error_dy_dx <dbl>, statistic_dy_dx <dbl>,
#> #   p.value_dy_dx <dbl>, conf.low_dy_dx <dbl>, conf.high_dy_dx <dbl>,
#> #   p.adj <dbl>, rank <int>, significance <chr>
```
