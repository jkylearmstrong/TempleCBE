# Cross-Validation for Joint Models

Evaluates the
[`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md)
trio across cross-validation splits (e.g. via
[`vfold_cv`](https://rsample.tidymodels.org/reference/vfold_cv.html) or
[`group_vfold_cv`](https://rsample.tidymodels.org/reference/group_vfold_cv.html)),
benchmarking survival, classification, and regression paradigms using
the IPCW Integrated Brier Score (`brier_survival_integrated`),
Concordance, and Calibration.

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

  Optional pre-constructed rsample object.

- subject_id:

  Optional subject identifier column for grouped resampling.

- engine:

  Modeling engine: `"glmnet"`, `"baguette"`, or `"stacks"`.

- calibration:

  Logical; whether to calibrate status predictions (default `TRUE`).

- parallel:

  Logical; whether to run folds in parallel via furrr.

- ...:

  Additional arguments passed to
  [`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md).

## Value

An S3 object of class `c("cv_joint_model", "tbl_df")` summarizing
comparative metrics across folds.

## Note

Counting-process `Surv(start, stop, event)` outcomes are not yet
supported here (only in
[`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md)
itself): scoring needs one row per subject, but predictions are one row
per interval. Use `Surv(time, status)` outcomes for cross-validation.
