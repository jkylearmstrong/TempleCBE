# Directory of TempleCBE SAS Macros

Returns the file system path to the directory containing TempleCBE's
bundled SAS macros (\`cbe_brier_score.sas\`, \`cbe_cox_phreg.sas\`,
\`cbe_counting_process.sas\`, and \`cbe_macros.sas\`).

## Usage

``` r
cbe_sas_macro_dir()
```

## Value

Absolute path to the SAS macro directory.

## See also

\[cbe_sas_macro_path()\], \[run_sas_script()\], \[find_sas()\]

## Examples

``` r
try(cbe_sas_macro_dir())
#> [1] "/home/runner/.cache/R/renv/library/TempleCBE-357df843/linux-ubuntu-noble/R-4.6/x86_64-pc-linux-gnu/TempleCBE/sas"
```
