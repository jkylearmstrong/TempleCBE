# Heatmap of Features by Transformation, Coloured by Significance

After screening several transformations of many features (see
\[transform_longitudinal_features()\] and \[batch_cox_single()\]), shows
every feature (rows) under every transformation (columns). Each tile is
coloured by the significance bucket of its p-value and shows the
feature's rank within that transformation.

## Usage

``` r
plot_feature_transform_heatmap(
  results_df,
  feature_col = "feature",
  transform_col = "transform_method",
  p_col = "p.value",
  rank_col = "rank",
  p_breaks = c(0.001, 0.01, 0.05, 0.2),
  palette = c("#1a9850", "#91cf60", "#fee08b", "#fc8d59", "#d73027"),
  order_features = c("input", "best_p")
)
```

## Arguments

- results_df:

  A data frame with one row per feature and transformation.

- feature_col, transform_col, p_col, rank_col:

  Column names. Set \`rank_col = NULL\` to print no rank.

- p_breaks:

  Increasing p-value cut-offs.

- palette:

  Colours, one more than \`p_breaks\`.

- order_features:

  \`"input"\` (default; order of first appearance) or \`"best_p"\`
  (smallest p over the transformations first).

## Value

A ggplot2 object.

## Buckets

With \`p_breaks = c(0.001, 0.01, 0.05, 0.20)\` the buckets are \`p \<
0.001\`, \`p \< 0.01\`, \`p \< 0.05\`, \`p \< 0.2\` and \`NS\` (p of 0.2
or more, or missing). A p-value goes in the first bucket whose break it
is strictly below. \`palette\` has one colour per bucket, most
significant first.

## Ranks

If \`rank_col\` is not a column of \`results_df\`, the rank is computed
within each transformation by ascending p-value (ties share the smaller
rank; missing p-values are not ranked).

## Examples

``` r
res <- expand.grid(feature = c("a", "b", "c"), transform_method = c("Raw", "SD"))
res$p.value <- c(0.0004, 0.03, 0.4, 0.2, 0.008, 0.06)
plot_feature_transform_heatmap(res)
```
