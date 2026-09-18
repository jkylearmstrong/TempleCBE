# Nested Cross-Validation of a Penalized Cox Model

Estimates how well the whole tuning procedure of \[cv_coxnet()\]
generalizes. For each outer split of an \[rsample::nested_cv()\] object:
\`mixture\` and \`penalty\` are chosen by \[cv_coxnet()\] on that
split's inner resamples; the model is refit on the outer analysis set
with those settings; and the refit is scored on the outer assessment
set, which played no part in tuning.

## Usage

``` r
nested_cv_coxnet(
  object,
  preprocessor,
  subject_id = NULL,
  group = NULL,
  rule = c("min", "1se"),
  mixture = 1,
  penalty = NULL,
  metrics = NULL,
  eval_time = NULL,
  metric = NULL,
  covariates = c("path", "baseline"),
  trunc = 0.05,
  parallel = FALSE,
  importance = c("none", "loco_mp"),
  ...
)

# S3 method for class 'nested_cv_coxnet'
collect_metrics(x, ..., summarize = TRUE)
```

## Arguments

- object:

  An \[rsample::nested_cv()\] object.

- preprocessor:

  A formula with a \`Surv()\` outcome, or a \[recipes::recipe()\] whose
  outcome is a \`Surv\` column.

- subject_id, group:

  Column names, as in \[cv_coxnet()\].

- rule:

  Use \`"min"\` (\`lambda.min\`) or \`"1se"\` (\`lambda.1se\`) from each
  inner cross-validation.

- mixture:

  Numeric vector of elastic-net mixing values to try.

- penalty:

  Optional decreasing penalty path; by default glmnet's path for each
  \`mixture\`.

- metrics:

  A \[yardstick::metric_set()\] of survival metrics. Defaults to the
  integrated Brier score, concordance, time-specific Brier score, and
  time-dependent ROC AUC.

- eval_time:

  Evaluation times, shared by all splits. Defaults to deciles of the
  event times in the full data.

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

- importance:

  Method for evaluating feature importance on the outer analysis sets.
  Options are \`"none"\` (default) or \`"loco_mp"\`
  (Leave-One-Covariate-Out with MiniPatch ensembles).

- ...:

  Further arguments passed to \[glmnet::glmnet()\], such as \`cox.ties\`
  or \`nlambda\`.

- x:

  A \`nested_cv_coxnet\` object.

- summarize:

  For \`collect_metrics()\`: \`TRUE\` for the mean and standard error
  over outer splits, \`FALSE\` for each split's metrics.

## Value

A tibble, of class \`nested_cv_coxnet\`, with one row per outer split:
\`id\`, the chosen \`mixture\` and \`penalty\`, \`.metrics\` (outer
assessment-set metrics), \`.coefs\` (coefficients of the refit), and
\`.inner\` (the inner cross-validation's summarized metrics). If
\`importance = "loco_mp"\`, includes \`.importance\` with tidy LOCO-MP
statistical inference. \`tune::collect_metrics()\` averages \`.metrics\`
over outer splits.

## Details

Build \`object\` with grouped resampling at both levels, e.g.
\`nested_cv(data, outside = group_vfold_cv(group = subject_id), inside =
group_vfold_cv(group = subject_id))\`, so a subject's start/stop rows
stay together.

The outer metrics describe the procedure, not one final model: the
chosen settings can differ between outer splits. To fit a final model,
run \[cv_coxnet()\] on all the data.

## See also

\[cv_coxnet()\], \[coxnet()\], \[cbe_loco_mp_coxnet()\]

## Examples

``` r
# \donttest{
if (requireNamespace("glmnet", quietly = TRUE) &&
    requireNamespace("survival", quietly = TRUE) &&
    requireNamespace("rsample", quietly = TRUE) &&
    requireNamespace("yardstick", quietly = TRUE)) {
  set.seed(1)
  long <- do.call(rbind, lapply(1:90, function(i) {
    k <- sample(1:3, 1)
    stops <- cumsum(stats::runif(k, 2, 6))
    risk <- stats::rnorm(1)
    data.frame(subject_id = i, tstart = c(0, utils::head(stops, -1)), tstop = stops,
               status = c(rep(0, k - 1), stats::rbinom(1, 1, stats::plogis(risk))),
               x1 = risk + stats::rnorm(k, sd = 0.2), x2 = stats::rnorm(k), x3 = stats::rnorm(k))
  }))
  folds <- rsample::nested_cv(
    long,
    outside = rsample::group_vfold_cv(group = subject_id, v = 3),
    inside = rsample::group_vfold_cv(group = subject_id, v = 3)
  )
  res <- nested_cv_coxnet(
    folds, survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3,
    subject_id = "subject_id", mixture = c(0.5, 1),
    metrics = yardstick::metric_set(yardstick::brier_survival_integrated,
                                    yardstick::concordance_survival),
    nlambda = 20, cox.ties = "breslow"
  )
  res
}
#> # A tibble: 3 × 6
#>   id        mixture penalty .metrics         .coefs           .inner           
#>   <chr>       <dbl>   <dbl> <list>           <list>           <list>           
#> 1 Resample1     0.5  0.0193 <tibble [2 × 4]> <tibble [3 × 3]> <tibble [54 × 8]>
#> 2 Resample2     1    0.0628 <tibble [2 × 4]> <tibble [3 × 3]> <tibble [54 × 8]>
#> 3 Resample3     1    0.0447 <tibble [2 × 4]> <tibble [3 × 3]> <tibble [58 × 8]>
# }
```
