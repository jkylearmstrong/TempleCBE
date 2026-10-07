# Cross-Validation for Joint Models

Evaluates the
[`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md)
trio across cross-validation splits (e.g. via
[`vfold_cv`](https://rsample.tidymodels.org/reference/vfold_cv.html) or
[`group_vfold_cv`](https://rsample.tidymodels.org/reference/group_vfold_cv.html)),
benchmarking survival, classification, and regression paradigms using
the IPCW Integrated Brier Score (the
[`yardstick::brier_survival_integrated()`](https://yardstick.tidymodels.org/reference/brier_survival_integrated.html)
value, as
[`cv_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md)
and
[`glmnet_IBS`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md)
report it) and Concordance. Each fold's model is fit on the analysis set
and scored on the assessment set, one row per subject, with Graf
censoring weights from the analysis set.

## Usage

``` r
cv_joint_model(
  data,
  outcome = survival::Surv(time, status) ~ .,
  v = 5,
  resamples = NULL,
  subject_id = NULL,
  engine = c("glmnet", "baguette", "stacks"),
  calibration = TRUE,
  parallel = FALSE,
  covariates = c("path", "baseline"),
  check_subject_overlap = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame.

- outcome:

  Survival formula with a
  [`Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) outcome.

- v:

  Number of cross-validation folds (default 5).

- resamples:

  Optional pre-constructed rsample object. No subject may be in both the
  analysis and the assessment set of a split; see Details and
  `check_subject_overlap`.

- subject_id:

  Subject identifier column. Required for start/stop outcomes; used for
  grouped resampling.

- engine:

  Modeling engine: `"glmnet"`, `"baguette"`, or `"stacks"` (the
  `"baguette"` models plus a stored Cox meta-learner whose output
  [`predict()`](https://rdrr.io/r/stats/predict.html) adds as extra
  columns; see
  [`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md)).

- calibration:

  Logical; whether to calibrate status predictions (default `TRUE`).

- parallel:

  Logical; whether to run folds in parallel via furrr.

- covariates:

  `"path"` or `"baseline"`; see Details. The same as
  [`cv_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md)'s
  argument, and also used when `penalty` is `NULL` to score the
  cross-validation that tunes each fold's `coxnet` penalty.

- check_subject_overlap:

  If `TRUE` (the default), stop when a supplied `resamples` puts the
  same subject in both the analysis and the assessment set of a split;
  see Details. `FALSE` skips the check. Folds that `cv_joint_model()`
  builds itself are never checked, as they are grouped by subject
  already.

- ...:

  Additional arguments passed to
  [`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md).

## Value

An S3 object of class `c("cv_joint_model", "tbl_df")` summarizing
comparative metrics across folds: `model`, `ibs`, `concordance`, and
`fold`.

## Details

**Subjects.** A counting-process (start/stop) outcome needs
`subject_id`, so that a subject's intervals stay in one fold and are
scored together; without it the function stops. For right-censored data
each row is a subject when `subject_id` is `NULL`. Unless `resamples` is
given, folds come from
[`group_vfold_cv`](https://rsample.tidymodels.org/reference/group_vfold_cv.html)
grouped by `subject_id`.

**Supplied resamples.** With `resamples`, every split is checked before
anything is fit: if a subject is in both the analysis and the assessment
set of a split, the function stops, because the models would be scored
on subjects they were fit on. That is what a row-level
[`vfold_cv`](https://rsample.tidymodels.org/reference/vfold_cv.html)
does to start/stop data. Group the folds by the subject instead, with
[`group_vfold_cv`](https://rsample.tidymodels.org/reference/group_vfold_cv.html)
or
[`group_bootstraps`](https://rsample.tidymodels.org/reference/group_bootstraps.html);
the out-of-bag assessment set of a bootstrap never overlaps its analysis
set. `check_subject_overlap = FALSE` switches the check off, for overlap
that is deliberate.

**Scoring start/stop data.** The three models are scored on the same
subjects, but not from the same rows:

- The `coxnet` model is scored as
  [`cv_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md)
  scores it: the survival of each assessment subject comes from the
  Breslow baseline hazard of the analysis set and the subject's
  covariates. With `covariates = "path"` the cumulative hazard is
  integrated over the subject's start/stop covariate path, carrying each
  interval's values forward to the next interval and past the last one;
  with `"baseline"`, only the covariates of the first interval are used,
  which uses no information from after time 0. For the same fit, folds
  and `eval_time`, the `coxnet` IBS and concordance are those of
  [`cv_coxnet()`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md).
  (Concordance uses the predicted survival at the last `eval_time`.)

- The status and time models predict one value per row, so for a subject
  they can only use one row. They use the **baseline** (first-interval)
  row, whatever `covariates` is.

**Missing predictors.** An assessment subject with a missing predictor
after preprocessing, such as a factor level the analysis set never saw,
cannot be predicted by any of the models: it is left out of the scores
of all three, with one warning per resample that gives the count. A
resample with no subject left is skipped.
