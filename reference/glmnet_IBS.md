# Integrated Brier Score of a Penalized Cox Model on Start/Stop Survival Data

Tunes an elastic-net Cox model on the analysis set of one resampling
split and scores it on the assessment set by the integrated Brier score
(IBS). Lower is better. A convenience wrapper around \[cv_coxnet()\] for
data in counting-process (start/stop) layout, kept for existing analysis
code; new code can call \[cv_coxnet()\] and \[nested_cv_coxnet()\]
directly.

## Usage

``` r
glmnet_IBS(
  object,
  alpha = 1,
  recipe,
  feature_names,
  time_data = NULL,
  formula = NULL,
  internal_folds = 5,
  id_col = "id",
  start_col = "tstart",
  stop_col = "tstop",
  status_col = "status",
  censoring_weights = c("ipcw", "none"),
  prep_on = c("baseline", "all"),
  eval_time = NULL,
  metric = "brier_survival_integrated",
  rule = c("min", "1se"),
  covariates = c("path", "baseline"),
  failure_ibs = NA_real_,
  ...
)
```

## Arguments

- object:

  An \`rsplit\` (e.g. one element of \`rsample\` resamples).

- alpha:

  Elastic net mixing parameter: 1 is lasso, 0 is ridge.

- recipe:

  An unprepped \[recipes::recipe()\].

- feature_names:

  Character vector of baked predictor column names, or a function that
  takes the baked analysis set and returns them – for recipes whose
  output columns vary by fold, such as \`step_pca(threshold = )\`.

- time_data:

  Optional data frame with columns \`start_col\` and \`stop_col\`; its
  positive stop times are used as \`eval_time\` when that is \`NULL\`.

- formula:

  Optional character string of \`+\`-separated feature names (or a
  one-sided formula) to restrict the model to a subset of
  \`feature_names\`. Defaults to all of them.

- internal_folds:

  Number of subject-grouped folds used to choose the penalty.

- id_col, start_col, stop_col, status_col:

  Column names of the subject identifier, interval start, interval stop,
  and event indicator (1 = event).

- censoring_weights:

  Only \`"ipcw"\` is supported. \`"none"\`, which scored interval rows
  without adjusting for censoring, was removed in TempleCBE 0.3.0;
  install TempleCBE 0.2.0 to reproduce results that used it.

- prep_on:

  \`"baseline"\` (default) preps \`recipe\` on each subject's first
  interval in the analysis set; \`"all"\` preps on every analysis row.

- eval_time:

  Evaluation times for the IBS. Defaults to the stop times of
  \`time_data\`, or else deciles of the analysis-set event times.

- metric:

  Name of the yardstick survival metric that chooses the penalty, e.g.
  \`"brier_survival_integrated"\` or \`"concordance_survival"\`.

- rule:

  \`"min"\` or \`"1se"\`: report the penalty at \`lambda.min\` or
  \`lambda.1se\`.

- covariates:

  \`"path"\` or \`"baseline"\`; see \[cv_coxnet()\].

- failure_ibs:

  IBS reported when the model cannot be fit (default \`NA\`); a warning
  gives the reason.

- ...:

  Further arguments passed to \[glmnet::glmnet()\], such as \`cox.ties\`
  (\`"breslow"\` or \`"efron"\`). glmnet 5.0 defaults to Breslow and 5.1
  to Efron, so pass \`cox.ties\` to keep results stable across glmnet
  versions.

## Value

A tibble with columns \`IBS\`, \`lambda\`, \`term\`, \`estimate\`, and
\`alpha\`: one row per feature (including those the penalty set to 0).
When the fit fails, a single row with \`IBS = failure_ibs\`, \`lambda =
NA\`, and \`alpha\`.

## Details

1\. \*\*Preprocessing.\*\* \`recipe\` is prepped on the analysis set –
only each subject's first interval when \`prep_on = "baseline"\` – and
baked onto both sets. The recipe must keep \`id_col\`, \`start_col\`,
\`stop_col\`, and \`status_col\` in its output (e.g. with an \`"id
variable"\` role). 2. \*\*Tuning.\*\* \[cv_coxnet()\] chooses the
penalty on the baked analysis set with \`internal_folds\` folds grouped
by subject, by \`metric\` (the IBS by default). Earlier versions used
\[glmnet::cv.glmnet()\], which chose the penalty by concordance on folds
of rows, splitting subjects' intervals between folds. 3.
\*\*Scoring.\*\* The model, refit on the whole analysis set, predicts
each assessment-set subject's survival from the analysis-set Breslow
baseline hazard along their covariate path (see \[cv_coxnet()\]), and is
scored by \[yardstick::brier_survival_integrated()\] with
inverse-probability-of- censoring (Graf) weights estimated on the
analysis set.

## References

Graf E, Schmoor C, Sauerbrei W, Schumacher M (1999). Assessment and
comparison of prognostic classification schemes for survival data.
\*Statistics in Medicine\*, 18(17-18), 2529-2545.

## See also

\[cv_coxnet()\], \[nested_cv_coxnet()\], \[tune_over_alpha()\],
\[summarize_tune_results()\]

## Examples

``` r
# \donttest{
if (requireNamespace("glmnet", quietly = TRUE) &&
    requireNamespace("yardstick", quietly = TRUE) &&
    requireNamespace("rsample", quietly = TRUE)) {
  set.seed(1)
  # Synthetic start/stop data: 60 subjects, up to 3 intervals each
  long <- do.call(rbind, lapply(1:60, function(i) {
    k <- sample(1:3, 1)
    stops <- c(5, 10, 20)[1:k]
    risk <- rnorm(1)
    data.frame(subject_id = i, tstart = c(0, head(stops, -1)), tstop = stops,
               status = c(rep(0, k - 1), rbinom(1, 1, plogis(risk))),
               x1 = risk + rnorm(k, sd = 0.2), x2 = rnorm(k))
  }))
  rec <- recipes::recipe(~ ., data = long) |>
    recipes::update_role(subject_id, tstart, tstop, status, new_role = "id variable") |>
    recipes::step_range(recipes::all_numeric_predictors())
  split <- rsample::group_initial_split(long, group = subject_id)
  glmnet_IBS(split, alpha = 0.5, recipe = rec, feature_names = c("x1", "x2"),
             time_data = unique(long[, c("tstart", "tstop")]), internal_folds = 3,
             id_col = "subject_id", cox.ties = "breslow")
}
#> # A tibble: 2 × 5
#>     IBS lambda term  estimate alpha
#>   <dbl>  <dbl> <chr>    <dbl> <dbl>
#> 1 0.112 0.0746 x1           0   0.5
#> 2 0.112 0.0746 x2           0   0.5
# }
```
