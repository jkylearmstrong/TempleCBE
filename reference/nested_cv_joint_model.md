# Nested Cross-Validation for Joint Models

Implements two-layer nested cross-validation on an
[`nested_cv`](https://rsample.tidymodels.org/reference/nested_cv.html)
object to evaluate the joint model pipeline without tuning leakage.

## Usage

``` r
nested_cv_joint_model(
  object,
  outcome = survival::Surv(time, status) ~ .,
  subject_id = NULL,
  engine = c("glmnet", "baguette", "stacks"),
  calibration = TRUE,
  parallel = FALSE,
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

  Optional subject identifier column.

- engine:

  Modeling engine: `"glmnet"`, `"baguette"`, or `"stacks"`.

- calibration:

  Logical; whether to calibrate status predictions (default `TRUE`).

- parallel:

  Logical; whether to run outer splits in parallel.

- ...:

  Additional arguments.

## Value

An S3 object of class `c("nested_cv_joint_model", "tbl_df")`.

## Note

Counting-process `Surv(start, stop, event)` outcomes are not yet
supported here (only in
[`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md)
itself): scoring needs one row per subject, but predictions are one row
per interval. Use `Surv(time, status)` outcomes for cross-validation.
