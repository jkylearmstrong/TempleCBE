# Compare an Original and an Imputed Version of One Domain's Dataset

Bundles the imputation diagnostics for one set of related variables (a
"domain") into a list of plain R objects, so a report can loop over the
domains and show or assemble the pieces itself. Nothing is rendered.

## Usage

``` r
compare_domain_datasets(
  orig_df,
  imp_df,
  vars = NULL,
  id_col = NULL,
  time_col = NULL,
  pca = TRUE,
  test = c("t", "wilcoxon")
)
```

## Arguments

- orig_df:

  Data frame before imputation.

- imp_df:

  Data frame after imputation: the same rows as \`orig_df\`, in the same
  order.

- vars:

  Character vector of numeric columns to compare. \`NULL\` (the default)
  uses the numeric columns present in both data frames, without
  \`id_col\` and \`time_col\`.

- id_col, time_col:

  Optional names of the subject id and time columns. Both are needed for
  the trajectory pieces.

- pca:

  Logical; also compare PCA loadings?

- test:

  Paired test for the trajectory comparison: \`"t"\`
  (\[stats::t.test()\]) or \`"wilcoxon"\` (\[stats::wilcox.test()\]).

## Value

A list with elements \`ecdf_plots\` (ggplot, or \`NULL\` if \`orig_df\`
has no missing value among \`vars\`), \`ks_tests\` (tibble),
\`corr_diff_table\` (tibble), \`corr_diff_heatmap\` (ggplot, or \`NULL\`
with fewer than two \`vars\`), \`pca_loading_diff\` (ggplot or
\`NULL\`), \`trajectory_plot\` (ggplot or \`NULL\`) and
\`trajectory_deltas\` (tibble with \`variable\`, \`time\`,
\`mean_orig\`, \`mean_imp\`, \`delta\`, \`n_paired\`, \`p_value\`,
\`p_adj\`, or \`NULL\`).

## Details

The two data frames must describe the same rows in the same order:
\`imp_df\` is \`orig_df\` after imputation. When \`id_col\` is given and
present in both, the ids (and \`time_col\`, if given) must agree row by
row, and the function stops otherwise.

The pieces are: \* \*\*Distributions\*\*: the imputed values against the
observed ones, with \[compare_imputed_ecdf()\] and
\[test_imputed_distributions()\] (Kolmogorov-Smirnov and t tests). Only
cells that were missing in \`orig_df\` count as imputed. \*
\*\*Correlations\*\*: \[correlation_diff()\] and
\[correlation_diff_heatmap()\], comparison (\`imp_df\`) minus baseline
(\`orig_df\`). \* \*\*PCA loadings\*\*: with \`pca = TRUE\`, a
\[stats::prcomp()\] fit (centred and scaled) to the complete cases of
\`vars\` in each data frame, then \[pca_loading_diff_heatmap()\].
\`NULL\`, with a warning, if there are fewer than two variables, fewer
complete cases than variables plus one, or a variable without variance
in either fit. \* \*\*Trajectories\*\*, when \`id_col\` and \`time_col\`
are both given: the mean and standard error of each variable at each
time point in each data frame, the difference of the means, and a paired
comparison by \`id_col\` within each time point (\`test\`).

The paired comparison uses the rows that have a value in both data
frames, so for an imputation that leaves the observed values unchanged
(the usual case) every paired difference is zero and \`p_value\` is
\`NA\`; \`delta\` still shows the shift in the mean that the imputed
values cause. The \`p_value\`s are point-wise, one per variable and time
point; \`p_adj\` adjusts them with Holm's step-down method over all of
them.

## See also

\[compare_imputed_ecdf()\], \[test_imputed_distributions()\],
\[correlation_diff()\], \[pca_loading_diff_heatmap()\]

## Examples

``` r
set.seed(1)
d <- expand.grid(id = 1:30, time = 1:3)
d$hr <- rnorm(nrow(d), 80, 10)
d$map <- rnorm(nrow(d), 90, 8)
orig <- d
orig$hr[sample(nrow(d), 15)] <- NA
imp <- orig
imp$hr[is.na(imp$hr)] <- mean(imp$hr, na.rm = TRUE)
out <- compare_domain_datasets(orig, imp, id_col = "id", time_col = "time")
out$ks_tests
#> # A tibble: 2 × 8
#>   variable n_observed n_imputed mean_observed mean_imputed ks_statistic
#>   <chr>         <int>     <int>         <dbl>        <dbl>        <dbl>
#> 1 hr               75        15          80.9         80.9         0.52
#> 2 map              90         0          90.1         NA          NA   
#> # ℹ 2 more variables: ks_p_value <dbl>, t_p_value <dbl>
out$trajectory_deltas
#> # A tibble: 6 × 8
#>   variable  time mean_orig mean_imp   delta n_paired p_value p_adj
#>   <chr>    <int>     <dbl>    <dbl>   <dbl>    <int>   <dbl> <dbl>
#> 1 hr           1      81.2     81.1 -0.0451       25      NA    NA
#> 2 hr           2      79.8     79.9  0.186        25      NA    NA
#> 3 hr           3      81.7     81.6 -0.141        25      NA    NA
#> 4 map          1      90.9     90.9  0            30      NA    NA
#> 5 map          2      87.4     87.4  0            30      NA    NA
#> 6 map          3      91.9     91.9  0            30      NA    NA
```
