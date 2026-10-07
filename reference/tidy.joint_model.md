# Tidy Method for Joint Models

Extracts and aligns nonzero coefficients across the survival (`coxnet`),
binary classification (`status`), and duration regression (`time`)
models.

## Usage

``` r
# S3 method for class 'joint_model'
tidy(x, ...)
```

## Arguments

- x:

  A `joint_model` object.

- ...:

  Additional arguments.

## Value

A tibble with comparative coefficients per feature.
