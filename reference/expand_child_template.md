# Expand and Knit a Child Template Once per Row of a Parameter Grid

The loop that recurs in reports with one section per feature:
\[knitr::knit_expand()\] the child template for each row, join the
pieces and \[knitr::knit_child()\] them. Use it in a chunk with
\`results = "asis"\`.

## Usage

``` r
expand_child_template(
  template_path,
  param_grid,
  quiet = TRUE,
  sep = "\n\n",
  delim = c("{{", "}}"),
  expand_only = FALSE,
  envir = parent.frame()
)
```

## Arguments

- template_path:

  Path of the child template.

- param_grid:

  A data frame (or named list of equal-length vectors), one row per
  child.

- quiet:

  Passed to \[knitr::knit_child()\] (default \`TRUE\`).

- sep:

  Text between the expanded children (default a blank line).

- delim:

  Template markers for \[knitr::knit_expand()\] (default \`c("", "")\`).

- expand_only:

  Return the expanded text without knitting it (default \`FALSE\`).

- envir:

  Environment to knit in (default: the caller's).

## Value

The knitted text, as \`knitr::knit_child()\` returns it (print it with
\`cat()\`, or return it from a chunk with \`results = "asis"\`). With
\`expand_only = TRUE\`, the expanded text.

## Details

Each column of \`param_grid\` is a template variable: \`feature\` in the
child is replaced by the value in column \`feature\` (change the markers
with \`delim\`). The rows are expanded in order.

## Examples

``` r
tmp <- tempfile(fileext = ".qmd")
writeLines("## {{label}}\n\nFeature `{{feature}}`.\n", tmp)
grid <- data.frame(feature = c("age", "bmi"), label = c("Age", "BMI"))
cat(expand_child_template(tmp, grid, expand_only = TRUE))
#> ## Age
#> 
#> Feature `age`.
#> 
#> 
#> ## BMI
#> 
#> Feature `bmi`.
```
