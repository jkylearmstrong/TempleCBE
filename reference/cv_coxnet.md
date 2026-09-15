# Cross-Validate a Penalized Cox Model With yardstick Survival Metrics

A tidymodels counterpart to \[glmnet::cv.glmnet()\] for right-censored
and start/stop survival data. Every combination of \`mixture\` and
\`penalty\` is fit on each analysis set and scored on the assessment set
with a yardstick survival metric set, and the best settings are chosen
by one metric: the integrated Brier score by default.

## Usage

``` r
cv_coxnet(x, ...)

# Default S3 method
cv_coxnet(x, ...)

# S3 method for class 'data.frame'
cv_coxnet(
  x,
  y,
  subject_id = NULL,
  group = NULL,
  mixture = 1,
  penalty = NULL,
  resamples = NULL,
  v = 10,
  metrics = NULL,
  eval_time = NULL,
  metric = NULL,
  covariates = c("path", "baseline"),
  trunc = 0.05,
  parallel = FALSE,
  ...
)

# S3 method for class 'matrix'
cv_coxnet(
  x,
  y,
  subject_id = NULL,
  group = NULL,
  mixture = 1,
  penalty = NULL,
  resamples = NULL,
  v = 10,
  metrics = NULL,
  eval_time = NULL,
  metric = NULL,
  covariates = c("path", "baseline"),
  trunc = 0.05,
  parallel = FALSE,
  ...
)

# S3 method for class 'formula'
cv_coxnet(
  formula,
  data,
  subject_id = NULL,
  group = NULL,
  mixture = 1,
  penalty = NULL,
  resamples = NULL,
  v = 10,
  metrics = NULL,
  eval_time = NULL,
  metric = NULL,
  covariates = c("path", "baseline"),
  trunc = 0.05,
  parallel = FALSE,
  ...
)

# S3 method for class 'recipe'
cv_coxnet(
  x,
  data,
  subject_id = NULL,
  group = NULL,
  mixture = 1,
  penalty = NULL,
  resamples = NULL,
  v = 10,
  metrics = NULL,
  eval_time = NULL,
  metric = NULL,
  covariates = c("path", "baseline"),
  trunc = 0.05,
  parallel = FALSE,
  ...
)
```

## Arguments

- x:

  A data frame or matrix of predictors, or a \[recipes::recipe()\] whose
  outcome is a single \`Surv\` column.

- ...:

  Further arguments passed to \[glmnet::glmnet()\], such as \`cox.ties\`
  or \`nlambda\`.

- y:

  For the data frame and matrix methods, a \[survival::Surv()\] outcome
  with one element per row of \`x\`.

- subject_id:

  Subject identifier: a column name for the formula and recipe methods
  (for a recipe, a single column with role \`"id"\` is used when
  \`NULL\`), or a vector for the data frame and matrix methods. Required
  for start/stop data; for right-censored data, each row is a subject
  when \`NULL\`.

- group:

  Optional grouping for folds, in the same form as \`subject_id\`.
  Defaults to \`subject_id\`.

- mixture:

  Numeric vector of elastic-net mixing values to try.

- penalty:

  Optional decreasing penalty path; by default glmnet's path for each
  \`mixture\`.

- resamples:

  An \`rset\` of \`data\`, such as \[rsample::group_vfold_cv()\].
  Overrides \`v\` and \`group\`.

- v:

  Number of folds when \`resamples\` is \`NULL\`.

- metrics:

  A \[yardstick::metric_set()\] of survival metrics. Defaults to the
  integrated Brier score, concordance, time-specific Brier score, and
  time-dependent ROC AUC.

- eval_time:

  Evaluation times. Defaults to deciles (10th to 90th percentile) of the
  observed event times.

- metric:

  Name of the metric used to choose settings; the first metric in
  \`metrics\` by default.

- covariates:

  \`"path"\` or \`"baseline"\`; see Details.

- trunc:

  Lower bound for the censoring probability in the Graf weights.

- parallel:

  If \`TRUE\`, fit folds with \[furrr::future_map()\]; set a
  \[future::plan()\] first.

- formula:

  A formula with a \`Surv()\` outcome, as in \[coxnet()\].

- data:

  A data frame with the outcome, predictors, \`subject_id\`, and
  \`group\` columns.

## Value

A \`cv_coxnet\` object, with: \* \`metrics\`: mean, \`n\`, and
\`std_err\` of each metric by \`mixture\`, \`penalty\`, and
\`.eval_time\`; \* \`fold_metrics\`: every fold's metrics; \*
\`best_by_mixture\`: \`lambda_min\` and \`lambda_1se\` for each
\`mixture\`; \* \`mixture\`, \`lambda_min\`, \`lambda_1se\`, \`metric\`,
\`direction\`; \* \`fit\`: a \[coxnet()\] model fit to all of \`data\`
at the chosen \`mixture\`, with \`lambda_min\` as its default penalty;
\* \`eval_time\`, \`resamples\`, and \`covariates\`.

Use \`predict()\`, \`tidy()\`, \`autoplot()\`, and
\`tune::collect_metrics()\` on it.

## Details

\*\*Folds.\*\* Unless \`resamples\` is given, folds come from
\[rsample::group_vfold_cv()\] grouped by \`group\` (default:
\`subject_id\`), so a subject's start/stop rows are never split between
analysis and assessment sets, as they would be by \`cv.glmnet()\`'s
row-level folds. Group by a coarser unit, such as study site, for
leave-sites-out cross-validation; each subject must then belong to one
group.

\*\*Fitting.\*\* For each \`mixture\`, one penalty path is computed on
all of \`data\` (as \`cv.glmnet()\` does) and every fold is fit along
it. With a recipe, the recipe is prepped on each analysis set, so
preprocessing is learned inside the fold.

\*\*Predictions.\*\* Survival for each assessment-set subject is
computed from the Breslow baseline hazard of the analysis set. With
\`covariates = "path"\`, the cumulative hazard is integrated over the
subject's start/stop covariate path, carrying each interval's values
forward to the next interval and past the last one; with \`"baseline"\`,
only the covariates of the first interval are used, which uses no
information from after time 0. The static concordance metric uses
predicted survival at the last \`eval_time\`.

\*\*Scoring.\*\* Start/stop rows are collapsed to one row per subject
(\[surv_subject_truth()\]), weighted for censoring with Graf weights
from the analysis set (\[graf_weights()\]), and passed to \`metrics\`.

\*\*Selection.\*\* \`lambda.min\` is the penalty, across all \`mixture\`
values, with the best mean \`metric\` over folds (for time-specific
metrics, at the first \`eval_time\`). \`lambda.1se\` is the largest
penalty, at the same mixture, whose mean is within one standard error of
that best mean.

## See also

\[coxnet()\], \[nested_cv_coxnet()\], \[surv_subject_truth()\]

## Examples

``` r
# \donttest{
if (requireNamespace("glmnet", quietly = TRUE) &&
    requireNamespace("survival", quietly = TRUE) &&
    requireNamespace("rsample", quietly = TRUE) &&
    requireNamespace("yardstick", quietly = TRUE)) {
  set.seed(1)
  # Start/stop data: up to 3 visits per subject, events only at the last one
  long <- do.call(rbind, lapply(1:80, function(i) {
    k <- sample(1:3, 1)
    stops <- cumsum(stats::runif(k, 2, 6))
    risk <- stats::rnorm(1)
    data.frame(subject_id = i, tstart = c(0, utils::head(stops, -1)), tstop = stops,
               status = c(rep(0, k - 1), stats::rbinom(1, 1, stats::plogis(risk))),
               x1 = risk + stats::rnorm(k, sd = 0.2), x2 = stats::rnorm(k), x3 = stats::rnorm(k))
  }))
  cv <- cv_coxnet(
    survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3,
    data = long, subject_id = "subject_id", mixture = c(0.5, 1),
    v = 5, nlambda = 20, cox.ties = "breslow"
  )
  cv
  generics::tidy(cv, penalty = "lambda.1se")
}
#> # A tibble: 3 × 3
#>   term  estimate penalty
#>   <chr>    <dbl>   <dbl>
#> 1 x1       0.333   0.105
#> 2 x2       0       0.105
#> 3 x3       0       0.105
# }
```
