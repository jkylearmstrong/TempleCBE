# Sample Candidate Model Formulas

Draws a reproducible set of distinct model formulas from a pool of
predictors, for the candidate-model step of a nested cross-validation
(random or weighted subset search over predictors). It does not look at
data: it only chooses which terms go into each formula.

## Usage

``` r
sample_candidate_formulas(
  predictors,
  response = NULL,
  required = character(),
  n = 100L,
  min_size = 1L,
  max_size = NULL,
  weights = NULL,
  exclude = NULL,
  hierarchy = TRUE,
  unique = TRUE,
  seed = NULL,
  env = parent.frame(),
  data = NULL
)
```

## Arguments

- predictors:

  Character vector of candidate terms (see Terms).

- response:

  Optional left-hand side as text, for example \`"Surv(time, status)"\`.
  Without it the formulas are one-sided (\`~ a + b\`).

- required:

  Terms that are in every formula. Need not be in \`predictors\`.

- n:

  Number of distinct formulas wanted.

- min_size, max_size:

  Smallest and largest number of terms per formula (\`max_size\`
  defaults to all terms).

- weights:

  Optional non-negative numeric vector, one value per entry of
  \`predictors\` (in the same order), or named by term; a named vector
  may leave terms out, which then get the mean of the given weights, and
  may not name a term that is not in \`predictors\`. A weight of 0 keeps
  a term out unless it is required. To feed back the output of
  \[calculate_feature_weights()\], use \`level = "term"\` there and pass
  \`setNames(w\$weight, w\$feature)\`.

- exclude:

  Optional list of character vectors of terms that may not all appear
  together.

- hierarchy:

  Logical; see Constraints.

- unique:

  Logical; drop repeated term sets (default \`TRUE\`).

- seed:

  Optional integer. The random-number state of the caller is restored
  afterwards.

- env:

  Environment of the formulas (default: the caller's).

- data:

  Optional data frame. A term that is a column of \`data\` with fewer
  than two distinct non-missing values (zero variance) cannot be
  estimated, so it gets weight 0 and is never drawn unless it is
  \`required\`; a message lists the terms. Terms that are not plain
  column names (transformations, interactions) are not checked. Only use
  training rows.

## Value

A tibble with one row per formula: \`id\`, \`formula\` (list of
formulas), \`terms\` (list of character vectors, required terms first)
and \`n_terms\`. The terms kept out for zero variance are in the
attribute \`"degenerate"\`.

## Terms

Each element of \`predictors\` is \*\*one term\*\*, written as it should
appear in the formula: a variable (\`"age"\`), a transformation
(\`"log(creatinine)"\`, \`"poly(age, 2)"\`) or an interaction
(\`"age:sex"\`). A transformed term counts as one term, an interaction
counts as one term of its own, and \`size\` limits count all terms
(required ones included). Terms are compared as text, so \`"age:sex"\`
and \`"sex:age"\` are different terms; list each once.

## Constraints

\* \`required\` terms are in every formula. \* \`min_size\` and
\`max_size\` bound the number of terms. \* \`hierarchy = TRUE\`
(default) rejects a formula that has an interaction without all its
variables as terms of their own (\`"age:sex"\` needs \`"age"\` and
\`"sex"\`). \* \`exclude\` is a list of character vectors; a formula may
not contain all terms of any one vector (for example \`list(c("bmi",
"weight"))\` keeps two near-duplicates apart).

## Sampling

For each draw the size is chosen uniformly between \`min_size\` and
\`max_size\`, then the non-required terms are drawn without replacement,
with probability proportional to \`weights\` (equal by default). A draw
that breaks a constraint is rejected. Formulas with the same set of
terms are the same formula, whatever the order, and are kept once
(\`unique = TRUE\`). If fewer than \`n\` distinct valid formulas can be
found within \`n \* 50\` attempts, the ones found are returned with a
warning.

## Data boundaries

No data are passed, so nothing here can use held-out rows. If you give
\`weights\`, compute them from the \*\*training\*\* rows of the current
fold only (for instance with \[calculate_feature_weights()\] on scores
obtained in the training fold); weights from the full data would leak
the held-out rows into candidate selection.

## See also

\[calculate_feature_weights()\]

## Examples

``` r
cand <- sample_candidate_formulas(
  c("age", "sex", "bmi", "log(lactate)", "age:sex"),
  response = "Surv(time, status)", required = "age", n = 5, max_size = 3, seed = 1
)
cand$formula[[1]]
#> Surv(time, status) ~ age
#> <environment: 0x55c678478810>
cand$terms
#> [[1]]
#> [1] "age"
#> 
#> [[2]]
#> [1] "age" "sex" "bmi"
#> 
#> [[3]]
#> [1] "age"          "log(lactate)" "bmi"         
#> 
#> [[4]]
#> [1] "age"          "log(lactate)"
#> 
#> [[5]]
#> [1] "age"     "sex"     "age:sex"
#> 
```
