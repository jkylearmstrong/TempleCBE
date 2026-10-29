# Supervised Linear Encoding of Factors via the Cox Component of a Joint Model

`step_lencode_joint_model` creates a *specification* of a recipe step
that encodes categorical predictors into a numeric score: the log hazard
ratio of each level relative to the reference level (the first level,
encoded as 0), taken from the Cox component of a
[`joint_model`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md)
fit to that factor alone. Only the Cox component's `.pred_risk_score` is
used, so the encoding is a penalized Cox log hazard ratio, like that of
[`step_lencode_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/step_lencode_coxnet.md);
it is *not* a composite that blends the survival, status and follow-up
time components of the joint model, and `engine` does not affect it.
Levels that
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) has not
seen, or missing values, are encoded as 0. If a model can't be fit, a
warning is raised and the factor's levels are all encoded as 0.

## Usage

``` r
step_lencode_joint_model(
  recipe,
  ...,
  outcome = c("time", "status"),
  role = "predictor",
  trained = FALSE,
  engine = "glmnet",
  penalty = 0.05,
  mapping = NULL,
  skip = FALSE,
  id = recipes::rand_id("lencode_joint_model")
)

# S3 method for class 'step_lencode_joint_model'
required_pkgs(x, ...)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose variables to be encoded.

- outcome:

  Character vector naming the survival outcome columns in the training
  data, as for
  [`step_lencode_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/step_lencode_coxnet.md).

- role:

  Role for the encoded variables. Default is `"predictor"`.

- trained:

  A logical indicating whether the step has been trained.

- engine:

  Has no effect on the encoding and is ignored when the step is prepped:
  only the Cox component of the joint model is used, and it does not
  depend on the engine. Any value other than the default `"glmnet"`
  gives a warning. Kept so that existing code keeps running.

- penalty:

  Penalty of the Cox component. Default is 0.05.

- mapping:

  A named list of level-to-score mappings generated during
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html).

- skip:

  Logical; skip step when baking? Default is `FALSE`.

- id:

  A unique identifier for the step.

- x:

  A `step_lencode_joint_model` object.

## Value

An updated version of `recipe`.

## Missing values

A missing value in a factor counts as the reference level, exactly as in
[`step_lencode_coxnet`](https://jkylearmstrong.github.io/TempleCBE/reference/step_lencode_coxnet.md):
in [`prep()`](https://recipes.tidymodels.org/reference/prep.html) it is
recoded to the first level before the model is fit, so its row stays in
the fit, and in
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) it is
encoded as 0.

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
