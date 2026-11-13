# Weight Features by the Candidate Models That Use Them

Turns a set of scored candidate formulas into one weight per feature:
how much of the total model score sits in the formulas that contain the
feature. Use it to learn, in a training fold, which predictors to favour
when sampling the next round of candidates
(\[sample_candidate_formulas()\] \`weights\`).

## Usage

``` r
calculate_feature_weights(
  candidates,
  scores = NULL,
  method = c("score", "inclusion"),
  higher_is_better = TRUE,
  level = c("variable", "term"),
  normalize = c("none", "sum", "max")
)
```

## Arguments

- candidates:

  A tibble from \[sample_candidate_formulas()\], or a list of formulas
  or of character vectors of terms.

- scores:

  Numeric vector, one score per candidate (needed unless \`method =
  "inclusion"\`). Candidates with an \`NA\` score are dropped, with a
  warning.

- method:

  \`"score"\` (default) or \`"inclusion"\`.

- higher_is_better:

  Logical; set \`FALSE\` for losses such as the Brier score.

- level:

  \`"variable"\` (default) or \`"term"\`.

- normalize:

  \`"none"\` (default), \`"sum"\` or \`"max"\`.

## Value

A tibble with \`feature\`, \`weight\`, \`n_formulas\` (how many formulas
contain it) and \`rank\`, ordered by weight (ties by name).

## Definition

Each formula gets a share \`s_i\` of the total score: scores are shifted
so the worst is 0 (\`score - min(score)\`, after flipping the sign when
\`higher_is_better = FALSE\`) and divided by their sum; if all scores
are equal every formula gets \`1 / n\`. The weight of a feature is the
sum of the shares of the formulas that contain it, a number between 0
and 1: 1 means it is in every formula that scores above the worst, 0
that it is in none. Because the worst candidate always gets a share of
0, a feature found only in the worst formula gets weight 0, and a tiny
score gap can swing weights between 0 and 1; if you pass these weights
to \[sample_candidate_formulas()\] a 0 keeps the term out of the next
round, so add a small constant to the weights if you want every term to
stay possible. \`method = "inclusion"\` ignores the scores and gives the
plain fraction of formulas that contain the feature. \`normalize =
"sum"\` rescales the weights to add to 1 and \`"max"\` rescales the
largest to 1.

## What counts as a feature

With \`level = "variable"\` (default) a feature is a variable:
\`log(x)\` and \`poly(x, 2)\` count for \`x\`, an interaction \`a:b\`
counts for both \`a\` and \`b\`, and a variable counts once per formula
however many terms use it. With \`level = "term"\` a feature is a whole
term as written (\`"log(x)"\`, \`"a:b"\`).

## Data boundaries

The function only sees formulas and their scores. The scores must be
computed on the \*\*training\*\* rows of the fold you are in (for
example by an inner cross-validation inside the training set); a score
computed on the held-out rows would leak them into the weights.

## See also

\[sample_candidate_formulas()\]

## Examples

``` r
cand <- sample_candidate_formulas(c("age", "sex", "bmi", "lactate"), n = 8, max_size = 3, seed = 2)
scores <- c(0.61, 0.70, 0.66, 0.72, 0.58, 0.69, 0.64, 0.71)[seq_len(nrow(cand))]
calculate_feature_weights(cand, scores)
#> # A tibble: 4 × 4
#>   feature weight n_formulas  rank
#>   <chr>    <dbl>      <int> <int>
#> 1 age      0.672          5     1
#> 2 sex      0.463          4     2
#> 3 bmi      0.418          3     3
#> 4 lactate  0.358          3     4
```
