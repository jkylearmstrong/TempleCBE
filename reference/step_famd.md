# Factor Analysis of Mixed Data (FAMD) Recipe Step

\`step_famd\` creates a \*specification\* of a recipe step that will
extract Factor Analysis of Mixed Data (FAMD) principal components from
numeric and categorical variables.

## Usage

``` r
step_famd(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_comp = 2,
  threshold = NA,
  options = list(),
  res = NULL,
  columns = NULL,
  prefix = "PC",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = recipes::rand_id("famd")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of steps for
  this recipe.

- ...:

  One or more selector functions to choose variables.

- role:

  Role for created variables (default "predictor").

- trained:

  Logical indicating if step has been trained.

- num_comp:

  Number of components to extract (default 2). Ignored if `threshold` is
  set.

- threshold:

  A fraction of the total variance that should be covered by the
  components (a number in `(0, 1]`). When set (non-`NA`), the step
  extracts the smallest number of components whose cumulative variance
  meets this threshold, overriding `num_comp`.

- options:

  A named list of additional arguments passed on to
  [`FAMD`](https://rdrr.io/pkg/FactoMineR/man/FAMD.html) (e.g.
  `list(row.w = ...)`); merged over the step's own defaults (`ncp` and
  `graph = FALSE`), so `options` can override those defaults too if
  desired.

- res:

  FAMD object.

- columns:

  Vector of column names.

- prefix:

  Component prefix (default "PC").

- keep_original_cols:

  Logical; whether to retain original columns.

- skip:

  Logical; skip step when baking.

- id:

  Unique step identifier.

## Value

An updated recipe object.
