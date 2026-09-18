# Define and Manage Clinical Variable Roles

Assigns functional roles (e.g. predictor, outcome, identifier, time
variable, strata, weights) to variables across a single data frame or an
entire R database list. Produces standardized schemas compatible with
[`validate_column_mapping`](https://jkylearmstrong.github.io/TempleCBE/reference/validate_column_mapping.md),
recipes, and modeling workflows.

## Usage

``` r
cbe_variable_roles(
  data,
  id = NULL,
  outcome = NULL,
  time = NULL,
  predictors = NULL,
  strata = NULL,
  weight = NULL,
  ignore = NULL,
  roles = NULL
)

cbe_set_roles(data, ...)

cbe_get_roles(data)

cbe_get_predictors(roles)

cbe_get_outcomes(roles)

cbe_get_id_cols(roles)

cbe_get_time_cols(roles)
```

## Arguments

- data:

  A data frame or a named list of data frames (an R database).

- id:

  Character vector of column names acting as subject/patient/cluster
  identifiers.

- outcome:

  Character vector of column names acting as response/outcomes
  (`Y_var`).

- time:

  Character vector of column names acting as longitudinal visit/time
  variables (`Time_var`).

- predictors:

  Character vector of column names acting as features/predictors
  (`X_var`).

- strata:

  Character vector of column names used for stratification or subgroup
  analysis.

- weight:

  Character vector of column names used for case weights or survey
  weights.

- ignore:

  Character vector of column names to ignore/exclude from modeling.

- roles:

  Role table (e.g. from `cbe_variable_roles`) or data object.

- ...:

  Additional arguments passed to `cbe_variable_roles`.

## Value

A tibble with columns `columns`, `role`, `X_var`, `Y_var`, `ID_var`,
`Time_var`, and `dataset_name` (when multi-table).

## Examples

``` r
df <- data.frame(id = 1:5, time = 1:5, sbp = c(120, 130, 115, 140, 125), death = c(0, 0, 1, 0, 1))
roles <- cbe_variable_roles(df, id = "id", time = "time", outcome = "death", predictors = "sbp")
```
