# Set or Retrieve the Package-Level Variable Label Map

Configures or retrieves a unified mapping from raw model/dataset
variable names (\`term\`) to \`"short"\` labels (for compact figure
axes) and \`"full"\` labels (for tables, legends, and report
appendices). Plot and table helpers across TempleCBE (\`plot_tune\_\*\`,
\`tbl_tune\_\*\`, \`plot_cox_forest\*\`, \`cbe_cox_table\`,
\`plot_permutation_importance\`, \`tbl_permutation_importance\`,
\`summarize_outer_importance\`, \`plot_outer_importance\`,
\`tbl_censoring_comparison\`) honour both the active package map set via
\`cbe_labels()\` and an explicit \`label_map =\` argument.

## Usage

``` r
cbe_get_dataset_labels(x)

dataset_label(x)

dataset_label(x) <- value

database_name(x)

database_name(x) <- value

database_label(x)

database_label(x) <- value

set_dataset_labels(x, labels)

get_dataset_labels(x)

cbe_labels(map = NULL, style = c("short", "full"), reset = FALSE)
```

## Arguments

- map:

  Optional label map: \* A data frame or tibble with columns \`term\`,
  \`short\`, and (optionally) \`full\`. \* A named list mapping each
  variable name to \`c(short = "...", full = "...")\` or a length-2
  character vector \`c("short", "full")\`. \* A named character vector
  \`c(var = "Label")\`. \* \`NULL\` (default) to query the currently
  active label map or change its active \`style\`.

- style:

  Default label style to apply when resolving labels: \`"short"\`
  (default) or \`"full"\`.

- reset:

  Logical (default \`FALSE\`). When \`TRUE\`, clears the active
  package-level label map and resets \`style\` to \`"short"\`.

## Value

A \`cbe_label_map\` tibble with columns \`term\`, \`short\`, and
\`full\` (or \`NULL\` when no map is set).

## See also

\[tbl_label_map()\], \[pretty_name()\]

## Examples

``` r
cbe_labels(
  tibble::tibble(
    term = c("x1", "x2"),
    short = c("FEV1", "DLCO"),
    full = c("Forced Expiratory Volume in 1s (L)", "Diffusing Capacity for CO")
  ),
  style = "short"
)
#> # A tibble: 2 × 3
#>   term  short full                              
#>   <chr> <chr> <chr>                             
#> 1 x1    FEV1  Forced Expiratory Volume in 1s (L)
#> 2 x2    DLCO  Diffusing Capacity for CO         
tbl_label_map()
#> # A tibble: 2 × 3
#>   term  short full                              
#>   <chr> <chr> <chr>                             
#> 1 x1    FEV1  Forced Expiratory Volume in 1s (L)
#> 2 x2    DLCO  Diffusing Capacity for CO         
cbe_labels(reset = TRUE)
```
