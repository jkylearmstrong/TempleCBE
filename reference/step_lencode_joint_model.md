# Supervised Linear Encoding of Factors via Joint Survival-Status-Time Model

`step_lencode_joint_model` creates a *specification* of a recipe step
that encodes categorical predictors into a composite numeric risk score
learned from a `joint_model` blending survival, binary status
classification, and follow-up duration.

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

  A call to `recipes::vars()` selecting the survival outcome columns.

- role:

  Role for the encoded variables. Default is `"predictor"`.

- trained:

  A logical indicating whether the step has been trained.

- engine:

  Engine passed to
  [`joint_model()`](https://jkylearmstrong.github.io/TempleCBE/reference/joint_model.md):
  `"glmnet"`, `"baguette"`, or `"stacks"`.

- penalty:

  Penalty parameter for the model. Default is 0.05.

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
