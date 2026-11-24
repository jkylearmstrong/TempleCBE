# Predicted Event Risk at One Horizon from a Fitted Survival Model

Predicts the event probability by \`horizon\` (one minus the survival
probability) for new data under a fitted \[coxnet()\], \[cv_coxnet()\],
\[joint_model()\], \[cox_time_varying()\], \[fine_gray()\] or
\[nested_cv_coxnet()\] model. This is the model-level adapter: for the
raw \`predict()\` output, use \[extract_horizon_risk()\]. For a
\[fine_gray()\] fit the risk is the cumulative incidence (CIF) of the
cause of interest at \`horizon\`, not one minus a survival probability.

## Usage

``` r
predict_event_risk(object, new_data, horizon, penalty = NULL, ...)
```

## Arguments

- object:

  A fitted \`coxnet_model\`, \`cv_coxnet\`, \`joint_model\`,
  \`cox_time_varying\`, \`fine_gray_model\` or \`nested_cv_coxnet\`
  object.

- new_data:

  A data frame of new predictors, as for \`predict()\`.

- horizon:

  A single finite, positive time: the horizon the risk applies to.

- penalty:

  For \`coxnet_model\`: the penalty to predict at; defaults to the
  model's \`penalty\`. For \`cv_coxnet\`: \`"lambda.min"\` (default),
  \`"lambda.1se"\`, or a number. Ignored for \`joint_model\`,
  \`cox_time_varying\`, \`fine_gray_model\` and \`nested_cv_coxnet\`
  (each outer split predicts at its own chosen penalty; see Details).

- ...:

  Not used.

## Value

A numeric vector of event probabilities by \`horizon\`, one per row of
\`new_data\`.

## Details

(A plain function dispatching on the model class, not an S3 generic, so
it works as written: the S3 split is a follow-up for the roxygen run
that regenerates \`NAMESPACE\`.)

\*\*Nested cross-validation results.\*\* A \`nested_cv_coxnet()\` result
has no single final model: each outer split refit the model on its own
analysis set at its own chosen \`mixture\` and \`penalty\`. The risk is
therefore one minus the mean of the outer splits' predicted survival at
\`horizon\`, each split predicting at its own chosen \`penalty\` from
its kept refit (the \`.fits\` column). A split that cannot score a row —
a missing predictor, or a factor level its analysis set never saw —
contributes \`NA\` for that row, and the mean is over the splits that
could score it; a row no split could score gets \`NA\` risk. A split
whose prediction fails outright is skipped with a warning naming the
split, and if no split can predict at all the call errors rather than
returning all-\`NA\` risk. Results created before the refits were kept
(no \`.fits\` column) cannot be predicted from: re-run
\`nested_cv_coxnet()\` to get one that can. As for
\[predict.coxnet_model()\], each split's covariates are taken as
constant from time 0, so the risk is the refits' implied survival from
study entry — for a gap-mode run (\`recurrent = "gap"\`) that is the
counting-process model's calendar-time survival, not a gap-time
survival: this adapter does not invent one (gap-time Brier scoring lives
in \`cv_coxnet(recurrent = "gap")\`, which scores gaps directly).

## See also

\[extract_horizon_risk()\], \[calibration_from_model()\],
\[decision_curve_from_model()\]

## Examples

``` r
set.seed(1)
n <- 120
d <- data.frame(time = rexp(n, 0.05), status = rbinom(n, 1, 0.6),
                age = rnorm(n, 60, 10), sex = rbinom(n, 1, 0.5))
fit <- coxnet(survival::Surv(time, status) ~ age + sex, data = d, penalty = 0.1)
predict_event_risk(fit, d[1:5, ], horizon = 10)
#> [1] 0.215163 0.215163 0.215163 0.215163 0.215163

# A nested cross-validation result has no single final model: each outer
# split refit the model at its own chosen penalty, and `predict_event_risk()`
# averages the splits' predicted survival at the horizon, each split
# predicting from its kept refit (the `.fits` column). The same result
# feeds [calibration_from_model()] and [decision_curve_from_model()].
if (requireNamespace("rsample", quietly = TRUE) &&
    requireNamespace("yardstick", quietly = TRUE)) {
  set.seed(2)
  x1 <- stats::rnorm(60)
  x2 <- stats::rnorm(60)
  t_event <- stats::rexp(60, 0.05 * exp(0.8 * x1))
  t_cens <- stats::rexp(60, 0.05)
  d_nested <- data.frame(time = pmin(t_event, t_cens),
                         status = as.integer(t_event <= t_cens),
                         x1 = x1, x2 = x2)
  set.seed(2)
  folds <- rsample::nested_cv(d_nested,
    outside = rsample::vfold_cv(v = 2), inside = rsample::vfold_cv(v = 2))
  res <- nested_cv_coxnet(folds,
    survival::Surv(time, status) ~ x1 + x2,
    mixture = 1,
    metrics = yardstick::metric_set(yardstick::brier_survival_integrated,
                                    yardstick::concordance_survival),
    nlambda = 10, cox.ties = "breslow")
  print(predict_event_risk(res, d_nested[1:5, ], horizon = 10))

  # The same adapter works on start/stop (counting-process) data fit
  # in gap mode — but what it returns is still the refits' implied
  # survival from study entry (calendar-time risk, each subject's
  # covariates held constant from time 0), not a gap-time risk. To
  # score gaps themselves, use the gap-time concordance and Brier
  # metrics of `cv_coxnet(recurrent = "gap")`.
  set.seed(1)
  long <- do.call(rbind, lapply(seq_len(60), function(i) {
    k <- sample(1:3, 1)
    stops <- round(cumsum(stats::runif(k, 2, 6)), 2)
    risk <- stats::rnorm(1)
    data.frame(
      subject_id = i,
      tstart = c(0, utils::head(stops, -1)),
      tstop = stops,
      status = c(rep(0L, k - 1), stats::rbinom(1, 1, stats::plogis(1.5 * risk))),
      x1 = risk + stats::rnorm(k, sd = 0.2),
      x2 = stats::rnorm(k),
      x3 = stats::rnorm(k)
    )
  }))
  set.seed(1)
  folds_gap <- rsample::nested_cv(
    long,
    outside = rsample::group_vfold_cv(group = "subject_id", v = 2),
    inside = rsample::group_vfold_cv(group = "subject_id", v = 2))
  res_gap <- nested_cv_coxnet(folds_gap,
    survival::Surv(tstart, tstop, status) ~ x1 + x2 + x3,
    subject_id = "subject_id", mixture = 1, recurrent = "gap",
    metrics = yardstick::metric_set(yardstick::concordance_survival),
    nlambda = 10, cox.ties = "breslow")
  # One row per subject, predictors as in the fit.
  new_gap <- long[!duplicated(long$subject_id), ]
  predict_event_risk(res_gap, new_gap[1:5, ], horizon = 5)
}
#> [1] 0.2820173 0.4325437 0.6587566 0.2563879 0.3897528
#> [1] 0.11708574 0.06654700 0.10145741 0.03079654 0.05063235
```
