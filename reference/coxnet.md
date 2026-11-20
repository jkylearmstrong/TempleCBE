# Penalized Cox Regression for Right-Censored or Start/Stop Survival Data

Fits an elastic-net penalized Cox proportional hazards model with
\[glmnet::glmnet()\], through the tidymodels hardhat interface: a
formula, a recipe, or predictors and outcome given separately. The
outcome is a \[survival::Surv()\] object, either right-censored,
\`Surv(time, event)\`, or counting-process, \`Surv(start, stop,
event)\`, for time-varying covariates.

## Usage

``` r
coxnet(x, ...)

# Default S3 method
coxnet(x, ...)

# S3 method for class 'data.frame'
coxnet(x, y, penalty = NULL, mixture = 1, path = NULL, ...)

# S3 method for class 'matrix'
coxnet(x, y, penalty = NULL, mixture = 1, path = NULL, ...)

# S3 method for class 'formula'
coxnet(formula, data, penalty = NULL, mixture = 1, path = NULL, ...)

# S3 method for class 'recipe'
coxnet(x, data, penalty = NULL, mixture = 1, path = NULL, ...)
```

## Arguments

- x:

  A data frame or matrix of predictors, or a \[recipes::recipe()\] whose
  outcome is a single \`Surv\` column.

- ...:

  Further arguments passed to \[glmnet::glmnet()\], such as \`nlambda\`
  or \`cox.ties\`. Not \`x\`, \`y\`, \`family\`, \`alpha\`, or
  \`lambda\`. \`cox.ties\` defaults to \`"breslow"\`, matching the
  Breslow baseline hazard used for survival predictions, so results
  don't depend on glmnet's own default (Breslow in glmnet 5.0, Efron
  from 5.1).

- y:

  A \[survival::Surv()\] object with one element per row of \`x\`.

- penalty:

  Default penalty (glmnet's \`lambda\`) used by \`predict()\` and
  \`tidy()\`. A single non-negative number, or \`NULL\` to require it
  there. glmnet's default path can end above a small \`penalty\` (it
  stops early once the deviance ratio saturates), so when \`penalty\` is
  below the path it is continued down to \`penalty\`; \`0\` gives the
  unpenalized fit. Penalties between path values are interpolated, as by
  \[glmnet::predict.glmnet()\].

- mixture:

  Elastic-net mixing parameter (glmnet's \`alpha\`): 1 is the lasso, 0
  is ridge regression.

- path:

  Optional decreasing sequence of penalties to fit, instead of glmnet's
  default path. It is used as given: \`predict()\` and \`tidy()\` at a
  penalty below its smallest value warn and use the smallest.

- formula:

  A formula with a \`Surv()\` outcome, such as \`survival::Surv(tstart,
  tstop, status) ~ age + sex\`.

- data:

  A data frame containing the variables in \`formula\` or used by the
  recipe.

## Value

A \`coxnet_model\` object: the glmnet fit, \`penalty\`, \`mixture\`, the
training predictor matrix \`x\` and outcome \`y\` (kept so survival can
be predicted at any penalty), and the hardhat \`blueprint\`.

## Details

The whole regularization path is fit, as glmnet recommends; \`penalty\`
only sets the default penalty for
\[predict()\]\[predict.coxnet_model()\] and
\[tidy()\]\[tidy.coxnet_model()\]. To choose \`penalty\` and \`mixture\`
by cross-validation, use \[cv_coxnet()\].

Predictors must be numeric, and finite: missing values and
\`Inf\`/\`-Inf\` in the predictors or the follow-up times are an error,
because glmnet would give an infinite predictor a coefficient of 0
without a word. The formula interface expands factors into one indicator
column per level, which suits penalized models; with a recipe, add
\`recipes::step_dummy()\`. In a formula, write \`survival::Surv()\`
unless survival is attached.

\`strata()\` and \`offset()\` are not supported in a formula, here or in
\[cv_coxnet()\] and \[nested_cv_coxnet()\]: \`strata()\` would be fit as
ordinary penalized indicator columns, with no stratified baseline
hazard, and \`offset()\` would be dropped, so the model would not be the
one the formula describes. A formula that uses either stops with an
error; use \[survival::coxph()\] for stratified models and offsets.

## See also

\[predict.coxnet_model()\], \[tidy.coxnet_model()\], \[cv_coxnet()\],
\[nested_cv_coxnet()\]

## Examples

``` r
if (requireNamespace("glmnet", quietly = TRUE) &&
    requireNamespace("survival", quietly = TRUE)) {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog", "wt.loss")])
  fit <- coxnet(survival::Surv(time, status) ~ ., data = lung, penalty = 0.05, mixture = 0.5)
  generics::tidy(fit)
  predict(fit, lung[1:3, ], type = "survival", eval_time = c(180, 365))$.pred[[1]]
}
#> # A tibble: 2 × 2
#>   .eval_time .pred_survival
#>        <dbl>          <dbl>
#> 1        180          0.793
#> 2        365          0.494
```
