# Factor Analysis of Mixed Data (FAMD) Recipe Step

\`step_famd()\` creates a \*specification\* of a recipe step that
converts a mix of numeric and categorical variables into principal
components with Factor Analysis of Mixed Data (\[FactoMineR::FAMD()\]).
It is the mixed-data counterpart of \[recipes::step_pca()\]: numeric
variables are standardized and categorical variables are weighted, so
each variable contributes on a comparable scale.

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
  levels = NULL,
  prefix = "FAMD",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = recipes::rand_id("famd")
)

# S3 method for class 'step_famd'
tidy(x, type = "coef", ...)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  \[recipes::selections()\].

- role:

  Role of the new component columns (default \`"predictor"\`).

- trained:

  A logical indicating whether the step has been trained.

- num_comp:

  Number of components to keep (default 2); ignored when \`threshold\`
  is set. \`0\` leaves the data unchanged.

- threshold:

  Fraction of total variance, in (0, 1\], that the kept components
  should cover. Overrides \`num_comp\` when not \`NA\`.

- options:

  A named list of further arguments to \[FactoMineR::FAMD()\].

- res:

  The fitted FAMD object, once trained.

- columns:

  The selected column names, once trained.

- levels:

  The factor levels of the categorical columns, once trained.

- prefix:

  Prefix of the new column names (default \`"FAMD"\`), numbered by
  \[recipes::names0()\]. It differs from \[recipes::step_pca()\]'s
  \`"PC"\`, so both steps can be used in one recipe.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  \`FALSE\`.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  \[recipes::bake()\]?

- id:

  A character string that is unique to this step.

- x:

  A \`step_famd\` object.

- type:

  For \`tidy()\`: \`"coef"\` (variable contributions) or \`"variance"\`.

## Value

An updated recipe object.

## Details

The selected variables must include at least one numeric and one
categorical (factor, character, or logical) variable; use
\[recipes::step_pca()\] for all-numeric data. Character and logical
variables are treated as factors, with levels learned by \`prep()\`.
Missing values, and categories that \`bake()\` meets but training did
not, are errors: impute or collapse levels first, e.g. with
\[recipes::step_impute_mode()\] or \[recipes::step_other()\].

FAMD has at most \\\min(n - 1, p + \sum_j (L_j - 1))\\ dimensions, for
\\n\\ rows, \\p\\ numeric variables, and categorical variables with
\\L_j\\ levels, which can be more than the number of variables.
\`num_comp\` is capped there. With \`threshold\`, the step keeps the
fewest components whose cumulative share of variance reaches
\`threshold\`, choosing among all dimensions.

Frequency weights (\[hardhat::frequency_weights()\]) are passed to FAMD
as row weights. Like other unsupervised recipe steps, such as
\[recipes::step_pca()\], it ignores importance weights.

\# Tidying

\`tidy()\` with \`type = "coef"\` returns each variable's percentage
contribution to each component (\`value\`): FactoMineR's per-variable
summary, which is comparable between numeric and categorical variables.
With \`type = "variance"\`, it returns each component's variance,
cumulative variance, percent variance, and cumulative percent variance,
as for \[recipes::step_pca()\].

## References

Pagès J (2004). Analyse factorielle de données mixtes. \*Revue de
Statistique Appliquée\*, 52(4), 93-111.

## Examples

``` r
if (requireNamespace("FactoMineR", quietly = TRUE)) {
  library(recipes)
  rec <- recipe(~ ., data = iris) |>
    step_famd(all_predictors(), num_comp = 3)
  prepped <- prep(rec)
  head(bake(prepped, new_data = NULL))
  tidy(prepped, number = 1, type = "variance")
}
#> Loading required package: dplyr
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> 
#> Attaching package: ‘recipes’
#> The following object is masked from ‘package:stats’:
#> 
#>     step
#> # A tibble: 12 × 4
#>    terms                        value component id        
#>    <chr>                        <dbl>     <int> <chr>     
#>  1 variance                     3.87          1 famd_IfjPL
#>  2 variance                     1.34          2 famd_IfjPL
#>  3 variance                     0.592         3 famd_IfjPL
#>  4 cumulative variance          3.87          1 famd_IfjPL
#>  5 cumulative variance          5.21          2 famd_IfjPL
#>  6 cumulative variance          5.80          3 famd_IfjPL
#>  7 percent variance            64.5           1 famd_IfjPL
#>  8 percent variance            22.4           2 famd_IfjPL
#>  9 percent variance             9.86          3 famd_IfjPL
#> 10 cumulative percent variance 64.5           1 famd_IfjPL
#> 11 cumulative percent variance 86.9           2 famd_IfjPL
#> 12 cumulative percent variance 96.7           3 famd_IfjPL
```
