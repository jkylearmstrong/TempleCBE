# The SAS Numerical-Parity Boundary

Documents, as data, which SAS procedures TempleCBE claims numerical
parity with, which R function is the counterpart, and what is explicitly
out of scope. The README frames "SAS numerical parity" as a pillar, but
the procedures in scope are only the ones listed here; everything else
is a follow-up (see \[compare_km_lifetest()\] for the first increment
beyond them, PROC LIFETEST).

## Usage

``` r
sas_parity_boundary()
```

## Value

A tibble with one row per SAS procedure (or parity topic) and columns
\`proc\` (SAS procedure or topic), \`r_counterpart\`, \`status\`
(\`"parity"\`, \`"partial"\` or \`"not_started"\`), \`evidence\` (where
the agreement was checked) and \`notes\`.

## See also

\[compare_df()\], \[compare_km_lifetest()\],
\[compare_recurrent_phreg()\], \[read_sas_data()\], \[write_sas_data()\]

## Examples

``` r
sas_parity_boundary()
#> # A tibble: 11 × 5
#>    proc                   r_counterpart                    status evidence notes
#>    <chr>                  <chr>                            <chr>  <chr>    <chr>
#>  1 PROC PHREG             cox_multi() / coxnet()           parity inst/sa… Bres…
#>  2 PROC COMPARE           compare_df()                     parity inst/sa… METH…
#>  3 PROC PRINCOMP          run_pca()                        parity inst/sa… PRIN…
#>  4 Zero-cell Fisher       exact2x2()                       parity cbe_exa… Zero…
#>  5 Missing codes          add_missing() / compare_df()     parti… compare… SAS …
#>  6 PROC LIFETEST          km_single() / survfit() via com… parity inst/sa… Firs…
#>  7 PROC PHREG (recurrent) recurrent_cox() via compare_rec… parity inst/sa… Recu…
#>  8 PROC LOGISTIC          glm_single()                     not_s… Follow-… glm_…
#>  9 PROC MIXED / GLIMMIX   lmm_single() / piecewise_lmm()   not_s… Follow-… Mixe…
#> 10 PROC FREQ              tbl_contingency_2x2()            not_s… Follow-… FREQ…
#> 11 PROC MEANS             summary_table()                  not_s… Follow-… MEAN…
```
