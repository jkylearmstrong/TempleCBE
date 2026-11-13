# Abbreviation Glossary From a Label Dictionary

Turns the label dictionary used by \[pretty_name()\] into an
abbreviation reference table for a report appendix: abbreviation (short
label), definition (full label), unit and domain.

## Usage

``` r
tbl_abbreviations_glossary(
  dict,
  domains = NULL,
  only_abbreviations = TRUE,
  include_term = FALSE
)
```

## Arguments

- dict:

  A data frame with a term column (\`term\`, \`variable\`, \`name\`,
  ...), a short-label column (\`short\`, \`abbreviation\`, \`label\`)
  and a full-label column (\`full\`, \`definition\`, \`description\`),
  and optionally \`unit\` and \`domain\` columns. Column names are
  matched without regard to case.

- domains:

  Optional character vector of domains to keep, in the order given.
  Needs a \`domain\` column.

- only_abbreviations:

  If \`TRUE\` (default), entries whose short and full labels are the
  same, or that lack one of them, are left out: they are not
  abbreviations.

- include_term:

  If \`TRUE\`, the raw column name is kept as the first column. Default
  \`FALSE\`.

## Value

A tibble with \`Abbreviation\`, \`Definition\`, and \`Unit\` /
\`Domain\` when the dictionary has them, one row per distinct
abbreviation and definition, sorted by domain (in \`domains\` order, or
alphabetically) and then by abbreviation without regard to case.

## See also

\[pretty_name()\], \[tbl_label_map()\]

## Examples

``` r
dict <- data.frame(
  term = c("liv_rl", "thb_osm3", "age"),
  short = c("LIV (%RL)", "tHb", "Age"),
  full = c("Lung injury volume", "Total hemoglobin", "Age"),
  unit = c("% right lung", "g/dL", "years"),
  domain = c("Imaging", "Blood", "Demographics")
)
tbl_abbreviations_glossary(dict)
#> # A tibble: 2 × 4
#>   Abbreviation Definition         Unit         Domain 
#>   <chr>        <chr>              <chr>        <chr>  
#> 1 tHb          Total hemoglobin   g/dL         Blood  
#> 2 LIV (%RL)    Lung injury volume % right lung Imaging
pretty_name(c("liv_rl", "age", "not_in_dict"), dict = dict, form = "unit")
#> [1] "Lung injury volume (% right lung)" "Age (years)"                      
#> [3] "Not in Dict"                      
```
