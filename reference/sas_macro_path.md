# Path to a TempleCBE SAS Macro File

Returns the absolute path to an installed TempleCBE SAS macro or
benchmark script.

## Usage

``` r
sas_macro_path(macro = "cbe_macros.sas")
```

## Arguments

- macro:

  Filename of the SAS macro or script (default: \`"cbe_macros.sas"\`).
  Available files include \`"cbe_macros.sas"\`,
  \`"cbe_brier_score.sas"\`, \`"cbe_cox_phreg.sas"\`,
  \`"cbe_counting_process.sas"\`, \`"coxtvc.sas"\`, \`"cpdata.sas"\`,
  \`"benchmark_brier_lung.sas"\`, \`"benchmark_brier_lung_full.sas"\`,
  and \`"example_85_7.sas"\`.

## Value

Absolute path to the requested \`.sas\` file.

## See also

\[sas_macro_dir()\], \[run_sas_script()\], \[find_sas()\]

## Examples

``` r
try(sas_macro_path("cbe_macros.sas"))
#> [1] "/tmp/RtmppxIjyF/temp_libpath2188179f1d87/TempleCBE/sas/cbe_macros.sas"
```
