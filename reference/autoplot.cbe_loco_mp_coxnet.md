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
