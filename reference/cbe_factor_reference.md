# Relevel a Factor's Reference Level

Standalone helper for choosing the reference (baseline) level of a
factor before passing it to \[cbe_cox_single()\] or \[cbe_cox_multi()\].
Wraps \[stats::relevel()\], defaulting to the first level (matching the
reference-row convention already used by
[`cbe_cox_single()`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_cox_single.md)/[`cbe_cox_multi()`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_cox_multi.md)),
and attaches a `"cbe_reference_level"` attribute recording the chosen
level.

## Usage

``` r
cbe_factor_reference(x, ref_level = NULL)
```

## Arguments

- x:

  A factor, or a vector coercible to one via
  [`as.factor()`](https://rdrr.io/r/base/factor.html).

- ref_level:

  Character string naming the level to use as reference. Defaults to the
  first level of `x` (its current or natural ordering).

## Value

The releveled factor, with attribute `"cbe_reference_level"` set to the
chosen reference level.

## Details

[`cbe_cox_single()`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_cox_single.md)
and
[`cbe_cox_multi()`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_cox_multi.md)
are not rewired to require this helper; it is an optional convenience
for callers who want to choose the reference level explicitly before
fitting.

## See also

\[cbe_cox_single()\], \[cbe_cox_multi()\]
