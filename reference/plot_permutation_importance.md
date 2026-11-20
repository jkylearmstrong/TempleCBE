# Plot Permutation Variable Importance

Generates a horizontal bar chart of the top features ranked by
permutation importance, or a box plot of the dropout loss over the
permutations.

## Usage

``` r
plot_permutation_importance(
  data,
  top_n = 20,
  fill_color = "#A41E35",
  title = "Feature Permutation Importance",
  base_size = 12,
  label_map = NULL,
  style = NULL,
  type = c("bar", "boxplot")
)
```

## Arguments

- data:

  A data frame from \[summarize_dalex_importance()\] containing
  \`variable\` and \`importance\`, or a permutation importance result
  itself (\[DALEX::model_parts()\], \`survex::model_parts()\`).

- top_n:

  Optional integer limit on the number of features to plot (default 20).

- fill_color:

  Bar fill color (default Temple Cherry \`"#A41E35"\`).

- title:

  Optional plot title.

- base_size:

  Base font size (default 12).

- label_map:

  Optional label mapping (see \[cbe_labels()\]).

- style:

  Optional label style (\`"short"\` or \`"full"\`).

- type:

  \`"bar"\` (default; mean importance, the loss above the full model) or
  \`"boxplot"\` (the dropout loss of each permutation, with a dashed
  line at the full-model loss). The box plot needs the per-permutation
  values: a \`model_parts\` result, or a
  \[summarize_dalex_importance()\] summary of one.

## Value

A \`ggplot2\` object.

## See also

\[summarize_dalex_importance()\], \[tbl_permutation_importance()\]
