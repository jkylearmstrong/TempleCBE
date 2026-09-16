# Tidy Proportional Hazards Diagnostics for Cox Models

Standalone proportional hazards (PH) assumption diagnostics, usable on
any fitted
[`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html) model,
or on a `cbe_cox` or `cbe_cox_multi` object. Wraps
[`survival::cox.zph()`](https://rdrr.io/pkg/survival/man/cox.zph.html)
with a tidy per-term table, a per-term violation flag, and an automated
text summary.

## Usage

``` r
cbe_cox_check(fit)
```

## Arguments

- fit:

  A fitted
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html)
  model, or a `cbe_cox` or `cbe_cox_multi` object.

## Value

An object of class `cbe_cox_check` containing:

- `zph`: The
  [`survival::cox.zph`](https://rdrr.io/pkg/survival/man/cox.zph.html)
  object (or `NULL` if it could not be computed).

- `zph_table`: Data frame version of `zph$table` (one row per term, plus
  a `GLOBAL` row for multivariable fits).

- `zph_violated`: Named logical vector, one entry per term (excluding
  `GLOBAL`), `TRUE` where the assumption is violated (p \< 0.05). For a
  univariable fit this is a length-1 named logical vector.

- `zph_text`: Automated summary sentence (a single string for a
  univariable fit with one term; a two-element character vector,
  per-term then global, for a multivariable fit).

## See also

\[cbe_cox_single()\], \[cbe_cox_multi()\]
