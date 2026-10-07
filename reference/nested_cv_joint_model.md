# Nested Cross-Validation for Joint Models

Scores the
[`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md)
pipeline on the outer splits of an
[`nested_cv`](https://rsample.tidymodels.org/reference/nested_cv.html)
object. As in
[`cv_joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_joint_model.md),
each outer split's model is fit on its analysis set and scored on its
assessment set one row per subject, and the `coxnet` model of a
start/stop outcome is scored along each subject's covariate path
(`covariates`), as
[`cv_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md)
does. The inner resamples are not used to fit anything: the penalty of
each outer split's `coxnet` model is tuned by the internal
cross-validation of
[`joint_model()`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md)
(unless `penalty` is given), and the status and time models choose
theirs by `cv.glmnet`.

## Usage

``` r
nested_cv_joint_model(
  object,
  outcome = survival::Surv(time, status) ~ .,
  subject_id = NULL,
  engine = c("glmnet", "baguette", "stacks"),
  calibration = TRUE,
  parallel = FALSE,
  covariates = c("path", "baseline"),
  check_subject_overlap = TRUE,
  tune_method = c("none", "race_anova", "race_win_loss"),
  ...
)
```

## Arguments

- object:

  An
  [`rsample::nested_cv`](https://rsample.tidymodels.org/reference/nested_cv.html)
  object.

- outcome:

  Survival formula with a
  [`Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) outcome.

- subject_id:

  Subject identifier column. Required for start/stop outcomes.

- engine:

  Modeling engine: `"glmnet"`, `"baguette"`, or `"stacks"` (the
  `"baguette"` models plus a stored Cox meta-learner whose output
  [`predict()`](https://rdrr.io/r/stats/predict.html) adds as extra
  columns; see
  [`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md)).

- calibration:

  Logical; whether to calibrate status predictions (default `TRUE`).

- parallel:

  Logical; whether to run outer splits in parallel.

- covariates:

  `"path"` or `"baseline"`; see
  [`cv_joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_joint_model.md).

- check_subject_overlap:

  If `TRUE` (the default), stop when an outer split, or an inner
  resample, puts the same subject in both its analysis and its
  assessment set; see Details. `FALSE` skips the check.

- tune_method:

  Method for tuning the Cox component penalty on the inner resamples:
  `"none"` (default, uses internal CV of
  [`joint_model()`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md)),
  `"race_anova"`
  ([`tune_race_anova`](https://finetune.tidymodels.org/reference/tune_race_anova.html)
  racing), or `"race_win_loss"`
  ([`tune_race_win_loss`](https://finetune.tidymodels.org/reference/tune_race_win_loss.html)
  racing, which needs BradleyTerry2). With racing, the penalty found on
  each outer split's inner resamples replaces any `penalty` given in
  `...`.

- ...:

  Additional arguments passed to
  [`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md).

## Value

An S3 object of class `c("nested_cv_joint_model", "tbl_df")`:
`outer_id`, `model`, and `ibs`, one row per outer split and model.

## Details

A counting-process (start/stop) outcome needs `subject_id`, and the
outer splits and every inner resample are checked before anything is
fit: if a subject is in both the analysis and the assessment set of one
of them, the function stops, as
[`cv_joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_joint_model.md)
does for supplied `resamples`. Build `object` with grouped resampling at
both levels, e.g.
`nested_cv(data, outside = group_vfold_cv(group = subject_id), inside = group_vfold_cv(group = subject_id))`.
Out-of-bag assessment sets of bootstraps pass.
`check_subject_overlap = FALSE` skips the check.
