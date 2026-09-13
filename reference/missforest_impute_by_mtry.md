# Assemble Imputed Columns From Their Best-\`mtry\` Runs

Given a sweep produced by \[missforest_sweep_mtry()\] and a table naming
the winning \`mtry\` for each column, pulls each column out of the run
that won it. Different columns may come from different runs.

## Usage

``` r
missforest_impute_by_mtry(sweep, best)
```

## Arguments

- sweep:

  A named list of \[missforest_oob_by_mtry()\] results, named by
  \`mtry\` as a character string (as \[missforest_sweep_mtry()\] builds
  it).

- best:

  A data frame with one row per column, containing at least the columns
  \`column\` and \`mtry\` – typically the \`best\` element returned by
  \[missforest_sweep_mtry()\].

## Value

A tibble of imputed columns, one per row of \`best\`.

## Details

Runs are looked up by \*\*name\*\*
(\`sweep\[\[as.character(mtry)\]\]\`), not by position. Positional
lookup happens to work only when the swept \`mtry\` values are exactly
\`1:n\`; it silently returns the wrong run for any other sweep, such as
one starting above 1 or skipping values.

## See also

\[missforest_sweep_mtry()\]
