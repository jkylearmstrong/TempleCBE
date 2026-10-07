# Autoplot Method for LOCO-MP Feature Importance

Generates a ggplot2 forest/lollipop chart displaying
Leave-One-Covariate-Out importance scores, confidence intervals, and
significance thresholds.

## Usage

``` r
# S3 method for class 'cbe_loco_mp_coxnet'
autoplot(object, ...)
```

## Arguments

- object:

  A `cbe_loco_mp_coxnet` object.

- ...:

  Additional arguments.

## Value

A ggplot2 plot object.

## Details

The bars are the two-sided `1 - alpha` confidence intervals, which are
not adjusted for multiple testing, but a predictor is coloured
"Significant" when its adjusted *one-sided* p-value is below `alpha`,
not when its bar excludes 0. The two criteria can disagree in either
direction: with `p_adjust = "none"`, a "Significant" predictor whose
p-value is between `alpha / 2` and `alpha` has a bar that still crosses
0, and after a correction such as Bonferroni a bar that excludes 0 can
be "Not Significant".
