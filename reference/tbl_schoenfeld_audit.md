# Proportional-Hazards Audit of a Cox Model

Tests the proportional-hazards assumption of a fitted Cox model with
scaled Schoenfeld residuals (\[survival::cox.zph()\]) and returns one
row per model term plus the global test, with a flag for the terms whose
hazard ratio appears to change over follow-up.

## Usage

``` r
tbl_schoenfeld_audit(fit, transform = "km", alpha = 0.05)
```

## Arguments

- fit:

  A model fitted by \[survival::coxph()\], or a tidymodels \`workflow\`
  / parsnip \`model_fit\` wrapping one (unwrapped to the engine fit).

- transform:

  Time transform passed to \[survival::cox.zph()\]: \`"km"\` (default),
  \`"rank"\`, \`"identity"\` or \`"log"\`.

- alpha:

  p-value below which a term is flagged.

## Value

A tibble with \`term\`, \`chisq\`, \`df\`, \`p.value\` and
\`non_proportional\` (logical). The \`GLOBAL\` row is the joint test and
is flagged like a term.

## Reading the result

A small p-value means the term's effect is not constant over time. With
many terms or a large sample some small departures will be flagged, so
look at the Schoenfeld plot (\`plot(survival::cox.zph(fit))\`) before
changing the model. If a term is flagged, stratify on it, interact it
with a function of time (\`survival::coxph(..., tt = )\`) or split
follow-up (\`survival::survSplit()\`) and report a hazard ratio per
period.

## See also

\[cox_check()\]

## Examples

``` r
fit <- survival::coxph(survival::Surv(time, status) ~ age + sex + ph.ecog, data = survival::lung)
tbl_schoenfeld_audit(fit)
#> # A tibble: 4 × 5
#>   term    chisq    df p.value non_proportional
#>   <chr>   <dbl> <dbl>   <dbl> <lgl>           
#> 1 age     0.188     1   0.665 FALSE           
#> 2 sex     2.31      1   0.129 FALSE           
#> 3 ph.ecog 2.05      1   0.152 FALSE           
#> 4 GLOBAL  4.46      3   0.216 FALSE           
```
