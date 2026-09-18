# Supervised Linear Encoding of Factors via Penalized Cox Models

`step_lencode_coxnet` creates a *specification* of a recipe step that
encodes categorical predictors into a single numeric variable
representing the estimated log hazard ratio from a penalized Cox
proportional hazards model on a survival outcome (`Surv(time, status)`
or `Surv(tstart, tstop, status)`). Inspired by
[`embed::step_lencode_glm`](https://embed.tidymodels.org/reference/step_lencode_glm.html).

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
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose variables to be encoded. Must
  select factor or character columns.

- outcome:

  A call to `recipes::vars()` selecting the survival outcome columns,
  e.g. `recipes::vars(time, status)` or
  `recipes::vars(tstart, tstop, status)`.

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

## Value

An updated version of `recipe` with the new step added.

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
