# Collapse Start/Stop Predictions to One Scored Row per Subject

Turns row-level linear predictors of a proportional-hazards model fit to
start/stop data into what the yardstick survival metrics need: one
right-censored truth per subject, a survival curve per subject in a
\`.pred\` list-column (with Graf censoring weights), and \`.pred_time\`
for \`concordance_survival()\`. Any yardstick survival \`metric_set()\`
can be called on the result.

## Usage

``` r
collapse_start_stop_predictions(
  truth,
  subject_id,
  lp,
  truth_train,
  subject_id_train,
  lp_train,
  eval_time = NULL,
  covariates = c("path", "baseline"),
  recurrent = c("error", "first"),
  trunc = 0.05
)
```

## Arguments

- truth:

  A \[survival::Surv()\] outcome for the assessment rows: \`Surv(start,
  stop, event)\`, or \`Surv(time, event)\`.

- subject_id:

  Subject identifiers of the assessment rows.

- lp:

  Linear predictors of the assessment rows (larger means higher hazard).
  For a parsnip model, \`predict(fit, new_data, type = "linear_pred",
  increasing = FALSE)\`.

- truth_train, subject_id_train, lp_train:

  The same for the analysis rows the model was fit on.

- eval_time:

  Evaluation times. Defaults to deciles of the event times of the
  analysis subjects.

- covariates:

  \`"path"\` (default) or \`"baseline"\`.

- recurrent:

  \`"error"\` (default) or \`"first"\`, as in \[surv_subject_truth()\].

- trunc:

  Lower bound for the censoring probability in the Graf weights.

## Value

A tibble with one row per assessment subject: \`.subject_id\`,
\`.entry\`, \`.truth\` (right-censored \`Surv\`), \`.pred\` (a list of
tibbles with \`.eval_time\`, \`.pred_survival\` and
\`.weight_censored\`) and \`.pred_time\`.

## Details

Survival is predicted from the Breslow baseline hazard of the analysis
(training) rows. With \`covariates = "path"\` the cumulative hazard is
integrated over each subject's start/stop covariate path; with
\`"baseline"\`, only the first interval's covariates are used. This is
the scoring that \[cv_coxnet()\] does inside its resamples.

## See also

\[fit_resamples_start_stop()\], \[tune_grid_start_stop()\],
\[surv_subject_truth()\], \[graf_weights()\]

## Examples

``` r
if (requireNamespace("survival", quietly = TRUE) &&
    requireNamespace("yardstick", quietly = TRUE)) {
  set.seed(1)
  long <- do.call(rbind, lapply(1:60, function(i) {
    k <- sample(1:3, 1)
    stops <- cumsum(stats::runif(k, 2, 6))
    risk <- stats::rnorm(1)
    data.frame(subject_id = i, tstart = c(0, utils::head(stops, -1)), tstop = stops,
               status = c(rep(0, k - 1), stats::rbinom(1, 1, stats::plogis(risk))),
               x1 = risk + stats::rnorm(k, sd = 0.2))
  }))
  train <- long[long$subject_id <= 40, ]
  test <- long[long$subject_id > 40, ]
  fit <- survival::coxph(survival::Surv(tstart, tstop, status) ~ x1, data = train)
  scored <- collapse_start_stop_predictions(
    survival::Surv(test$tstart, test$tstop, test$status), test$subject_id,
    lp = stats::predict(fit, test, type = "lp", reference = "zero"),
    truth_train = survival::Surv(train$tstart, train$tstop, train$status),
    subject_id_train = train$subject_id,
    lp_train = stats::predict(fit, train, type = "lp", reference = "zero")
  )
  yardstick::brier_survival_integrated(scored, truth = .truth, .pred)
  yardstick::concordance_survival(scored, truth = .truth, estimate = .pred_time)
}
#> # A tibble: 1 × 3
#>   .metric              .estimator .estimate
#>   <chr>                <chr>          <dbl>
#> 1 concordance_survival standard       0.568
```
