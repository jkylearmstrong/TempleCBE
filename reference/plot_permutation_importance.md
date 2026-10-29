# Plot Permutation Variable Importance

Generates a horizontal bar chart of the top features ranked by
permutation importance.

## Usage

``` r
plot_permutation_importance(
  data,
  top_n = 20,
  fill_color = "#9D2235",
  title = "Feature Permutation Importance",
  base_size = 12,
  label_map = NULL,
  style = NULL
)
```

## Arguments

- data:

  A data frame from \[summarize_dalex_importance()\] containing
  \`variable\` and \`importance\`.

- top_n:

  Optional integer limit on the number of features to plot (default 20).

- fill_color:

  Bar fill color (default Temple Cherry \`"#9D2235"\`).

- title:

  Optional plot title.

- base_size:

  Base font size (default 12).

- label_map:

  Optional label mapping (see \[cbe_labels()\]).

- style:

  Optional label style (\`"short"\` or \`"full"\`).

## Value

A \`ggplot2\` object.

## See also

\[summarize_dalex_importance()\], \[tbl_permutation_importance()\]
