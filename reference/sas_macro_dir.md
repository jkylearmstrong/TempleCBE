# Directory of TempleCBE SAS Macros

Returns the file system path to the directory containing TempleCBE's
bundled SAS macros (\`cbe_brier_score.sas\`, \`cbe_cox_phreg.sas\`,
\`cbe_counting_process.sas\`, and \`cbe_macros.sas\`).

## Usage

``` r
sas_macro_dir()
```

## Value

Absolute path to the SAS macro directory.

## See also

\[sas_macro_path()\], \[run_sas_script()\], \[find_sas()\]

## Examples

``` r
try(sas_macro_dir())
#> [1] "/tmp/RtmpJccCnY/temp_libpath21c2116530e2/TempleCBE/sas"
```
