# Kaplan-Meier / PROC LIFETEST Parity (First Increment Beyond the SAS Boundary)

The first increment of broader SAS parity (B5): a \`PROC LIFETEST\`
counterpart to the package's Kaplan-Meier tooling. The SAS side is the
\` program \`inst/sas/benchmark_lifetest_km.sas\`); the R side here
computes the same quantities from \[survival::survfit()\] /
\[survival::survdiff()\], parses a SAS listing back into the same long
shape, and compares the two.

## Usage

``` r
km_lifetest_quantities(data, time, status, group = NULL, conf_level = 0.95)

parse_lifetest_lst(lines)

compare_km_lifetest(r, sas, atol = 6e-05, rtol = 5e-04)

# S3 method for class 'km_lifetest_comparison'
print(x, ...)
```

## Arguments

- data:

  A data frame.

- time:

  Name of the follow-up-time column (numeric, non-negative).

- status:

  Name of the event-indicator column (0/1, or \`TRUE\`/\`FALSE\`).

- group:

  Optional name of a grouping column (strata for the log-rank test).
  \`NULL\` (default) fits a single curve.

- conf_level:

  Confidence level for \`survfit()\` (stored, not compared).

- lines:

  Character vector of listing lines (from a \`.lst\` file, as read by
  \[readLines()\]), or a single path to one.

- r, sas:

  Long-form quantity tables as returned by \[km_lifetest_quantities()\]
  and \[parse_lifetest_lst()\].

- atol, rtol:

  Absolute and relative tolerances, as in the other SAS parity checks
  (\`helper-sas-reference.R\`: SAS prints 4-5 decimals). The defaults
  are set to the listing's own precision: SAS prints 4 decimals, so
  rounding alone can move a small quantity by up to 5e-05, and \`atol =
  6e-5\` accepts that while still failing a genuinely different value.

- x:

  The object to print (the result of the function).

- ...:

  Not used.

## Value

\[km_lifetest_quantities()\]: a tibble with \`quantity\`, \`term\`,
\`time\` and \`value\`. \[parse_lifetest_lst()\]: the same shape, parsed
from listing lines. \[compare_km_lifetest()\]: a list of class
\`km_lifetest_comparison\` with \`table\` (one row per compared
quantity, with \`r_value\`, \`sas_value\`, \`abs_diff\` and \`matched\`)
and \`matched\` (single \`TRUE\`/\`FALSE\`).

## Quantities compared

Product-limit survival estimates (one row per event time per stratum:
\`quantity = "km_surv"\`, \`term\` = stratum, \`time\` = event time,
\`value\` = survival probability), the median survival time (\`quantity
= "median"\`, \`time = NA\` — the first event time at which the
product-limit estimate is at most 0.5, which is SAS's definition; not
\[stats::quantile()\] on the \`survfit\` object, which interpolates when
survival hits exactly 0.5 at an event time), and, when a grouping
variable is given, the log-rank test (\`"logrank_chisq"\`,
\`"logrank_df"\`, \`"logrank_p"\`, \`term = "model"\`). Standard errors
and confidence limits are computed by \`survfit()\` but are not part of
the comparison: SAS prints them with the Greenwood formula on a log-log
scale by default, while \`survfit()\` defaults differ, so they are a
follow-up, not a first increment.

## Status

Run once on SAS 9.4 (TS1M8, Temple University licence, 11 November
2026). The benchmark program \`inst/sas/benchmark_lifetest_km.sas\` was
run and its listing stored at
\`inst/sas/list/benchmark_lifetest_km.lst\` (log at
\`inst/sas/logs/benchmark_lifetest_km.log\`). \[parse_lifetest_lst()\]
parses the stored listing and \[compare_km_lifetest()\] agrees with
\[km_lifetest_quantities()\] on the benchmark data (13 rows: 8 KM
survival estimates, both medians, the log-rank test) at the tolerances
the Tier-1 test uses (\`atol = 1e-4\`, \`rtol = 1e-3\`; SAS prints 4
decimals), asserted in \`tests/testthat/test-gaps-0811-w2-data.R\`;
\[sas_parity_boundary()\] lists PROC LIFETEST as \`"parity"\` on that
evidence. No reference CSV is kept: the test parses the stored listing
directly, so no \`tests/testthat/reference/sas_lifetest_km.csv\` exists
or is needed. Note the stored log still echoes the pre-run "DRAFT / has
not been run" comment text from the program as it stood at run time; the
program header has since been updated, and the log is kept as run
evidence, not re-written.

## Follow-up PROCs (not in this increment)

\`PROC LOGISTIC\` (counterpart \[glm_single()\]), \`PROC MIXED\` /
\`GLIMMIX\` (\[lmm_single()\] / \[piecewise_lmm()\]), \`PROC FREQ\`
(\[tbl_contingency_2x2()\]) and \`PROC MEANS\` (\[summary_table()\])
remain \`"not_started"\` in \[sas_parity_boundary()\]; each needs its
own SAS macro, benchmark program typed on the same numbers, and
reference listing, in the style of this increment.

## See also

\[sas_parity_boundary()\], \[km_single()\], \[sas_macro_path()\],
\[run_sas_script()\]

## Examples

``` r
df <- data.frame(
  time = c(5, 6, 6, 8, 10, 12, 3, 4, 7, 9, 9, 11),
  status = c(1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0),
  group = rep(c("A", "B"), each = 6)
)
km_lifetest_quantities(df, "time", "status", group = "group")
#> # A tibble: 13 × 4
#>    quantity      term   time  value
#>    <chr>         <chr> <dbl>  <dbl>
#>  1 km_surv       A         5 0.833 
#>  2 km_surv       A         6 0.667 
#>  3 km_surv       A         8 0.444 
#>  4 km_surv       A        10 0.222 
#>  5 median        A        NA 8     
#>  6 km_surv       B         3 0.833 
#>  7 km_surv       B         4 0.667 
#>  8 km_surv       B         7 0.5   
#>  9 km_surv       B         9 0.333 
#> 10 median        B        NA 8     
#> 11 logrank_chisq model    NA 0.0198
#> 12 logrank_df    model    NA 1     
#> 13 logrank_p     model    NA 0.888 
```
