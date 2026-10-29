# Supervised Linear Encoding of Factors via Penalized Cox Models

`step_lencode_coxnet` creates a *specification* of a recipe step that
encodes categorical predictors into a single numeric variable
representing the estimated log hazard ratio from a penalized Cox
proportional hazards model on a survival outcome (`Surv(time, status)`
or `Surv(tstart, tstop, status)`). Inspired by
[`embed::step_lencode_glm`](https://embed.tidymodels.org/reference/step_lencode_glm.html).
Each factor is encoded on its own: a penalized Cox model with one
indicator per non-reference level is fit to the training data at the
single `penalty`, the reference level (the first level) is encoded as 0,
and levels that
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) has not
seen, or missing values, are encoded as 0. If a model can't be fit, a
warning is raised and the factor's levels are all encoded as 0.

## Usage

``` r
step_lencode_coxnet(
  recipe,
  ...,
  outcome = c("time", "status"),
  role = "predictor",
  trained = FALSE,
  penalty = 0.05,
  mixture = 1,
  mapping = NULL,
  skip = FALSE,
  id = recipes::rand_id("lencode_coxnet")
)

# S3 method for class 'step_lencode_coxnet'
tunable(x, ...)

# S3 method for class 'step_lencode_coxnet'
required_pkgs(x, ...)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose variables to be encoded. Must
  select factor or character columns.

- outcome:

  Character vector naming the survival outcome columns in the training
  data: `c("time", "status")` or `c("tstart", "tstop", "status")`.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the function assumes that the new variables
  will be used as predictors in a model.

- trained:

  A logical indicating whether the recipe has been trained.

- penalty:

  A non-negative numeric value specifying the L1/L2 penalty for
  `coxnet`. Default is 0.05.

- mixture:

  Elastic net mixing parameter between 0 and 1. Default is 1 (Lasso).

- mapping:

  A named list of numeric vectors containing the level-to-score
  mappings, generated during
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html).

- skip:

  A logical indicating whether the step should be skipped when the
  recipe is baked.

- id:

  A unique identifier for the step.

- x:

  A `step_lencode_coxnet` object.

## Value

An updated version of `recipe` with the new step added.

## Missing values

A missing value in a factor counts as the reference level: in
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) it is
recoded to the first level before the model is fit, so its row stays in
the fit (and, with the other rows of that level, sets the baseline that
the other levels are compared with), and in
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) it is
encoded as 0.
[`step_lencode_joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/step_lencode_joint_model.md)
follows the same policy.

## The training data are encoded with their own fit

[`prep()`](https://recipes.tidymodels.org/reference/prep.html) fits the
encoding on the training rows, and `bake(new_data = NULL)`, or baking
the training data, encodes those same rows with the coefficients fitted
to them: the encoding is *not* out-of-fold (cross-fitted). The encoded
training column therefore carries the outcome, so it looks more
predictive in the training data than it is for new subjects, and a model
fit to it over-trusts it. The effect grows with the number of levels
relative to the number of events, so it is worst for factors with many
or rare levels, whose coefficients are fitted to a handful of subjects
each: a factor with 100 levels and no relation to the outcome can show a
clearly better than chance concordance in the baked training data and
chance level on new data. To limit it, use a large `penalty` (most
levels then shrink to 0) or pool rare levels before encoding (for
example with
[`recipes::step_other()`](https://recipes.tidymodels.org/reference/step_other.html)).
Do not evaluate a model on data that it was encoded with: estimate
performance on subjects the step did not see in
[`prep()`](https://recipes.tidymodels.org/reference/prep.html), for
example by resampling the whole recipe, so that every
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) sees only
the analysis set, and not by encoding once and splitting afterwards.

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("recipes", quietly = TRUE) && requireNamespace("survival", quietly = TRUE)) {
  lung <- survival::lung
  lung_df <- na.omit(lung[, c("time", "status", "sex", "ph.ecog")])
  lung_df$ph.ecog <- factor(lung_df$ph.ecog)
  lung_df$sex <- factor(lung_df$sex)

  rec <- recipes::recipe(time + status ~ ., data = lung_df) |>
    step_lencode_coxnet(ph.ecog, sex, outcome = c("time", "status"))
  prepped <- recipes::prep(rec)
  baked <- recipes::bake(prepped, new_data = NULL)
}
} # }
```
