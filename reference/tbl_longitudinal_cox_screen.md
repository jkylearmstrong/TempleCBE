# Cox Screen of Longitudinal Summary Statistics

For each feature, summarises every subject's repeated measurements (see
\[transform_longitudinal_features()\]) and fits one univariate Cox model
per summary, with the summary as the only covariate. Answers "does the
way a marker moves over time, not only its level, predict survival?".

## Usage

``` r
tbl_longitudinal_cox_screen(
  data,
  id_col = "subject_id",
  time_col = "tstart",
  status_col = "status",
  features,
  metrics = c("pct_change_directional", "variance", "excursion", "sd"),
  surv_time_col = NULL,
  metadata = NULL,
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame in long format.

- id_col, time_col:

  Subject and measurement-time columns.

- status_col:

  Event indicator (1 = event, 0 = censored; or logical).

- features:

  Numeric feature columns.

- metrics:

  Any of \`"pct_change_directional"\`, \`"variance"\`, \`"excursion"\`,
  \`"sd"\`.

- surv_time_col:

  Optional follow-up time column (see above).

- metadata:

  Optional data frame with \`columns\` and optionally \`labels\` and
  \`domain\`; adds \`label\` and \`domain\` to the result.

- conf_level:

  Confidence level of the hazard ratio.

## Value

A tibble with \`feature\`, \`label\`, \`domain\` (only with
\`metadata\`), \`metric\`, \`n\`, \`events\`, \`hr\`, \`conf.low\`,
\`conf.high\`, \`p.value\`, \`status\` (\`"ok"\` or why no model was
fitted).

## Survival time and status

Each subject has one follow-up time and one status. \`surv_time_col\`
gives the time; if it is \`NULL\`, the subject's largest \`time_col\` is
used (the last measurement). The status is the one on the subject's last
row (by \`time_col\`). A subject whose status is missing is dropped.

## Metrics

\`"pct_change_directional"\`: percent change from the first to the last
value, signed (positive = rose). \`"variance"\` and \`"sd"\`:
within-subject. \`"excursion"\`: \`max - min\`, never negative. Hazard
ratios are per one unit of the summary, so they depend on its scale;
standardise the features first if the ratios should be compared across
features.

## See also

\[transform_longitudinal_features()\], \[batch_cox_single()\]

## Examples

``` r
set.seed(1)
d <- data.frame(id = rep(1:40, each = 4), t = rep(c(0, 5, 15, 30), 40))
d$hr <- rnorm(160, 80, 10)
d$status <- rep(rbinom(40, 1, 0.5), each = 4)
tbl_longitudinal_cox_screen(d, "id", "t", "status", "hr")
#> # A tibble: 4 × 9
#>   feature metric                n events    hr conf.low conf.high p.value status
#>   <chr>   <chr>             <int>  <int> <dbl>    <dbl>     <dbl>   <dbl> <chr> 
#> 1 hr      pct_change_direc…    40     19 1.01     0.975      1.04   0.742 ok    
#> 2 hr      variance             40     19 0.998    0.990      1.01   0.535 ok    
#> 3 hr      excursion            40     19 0.973    0.917      1.03   0.371 ok    
#> 4 hr      sd                   40     19 0.933    0.814      1.07   0.322 ok    
```
