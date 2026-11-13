# Plot a Decision Curve

Net benefit against the threshold probability for each model, with the
"treat all" and "treat none" strategies. The vertical range is cut below
at a small negative net benefit, so that a poor "treat all" at high
thresholds does not flatten the plot.

## Usage

``` r
plot_decision_curve(x, ..., ymin = NULL)
```

## Arguments

- x:

  A \[decision_curve()\] result, or the \`truth\` for it, in which case
  \`pred\` and the other arguments of \[decision_curve()\] are taken
  from \`...\`.

- ...:

  Passed to \[decision_curve()\] when \`x\` is not a \`decision_curve\`
  result.

- ymin:

  Lower limit of the net benefit axis. Default: \`-0.05\` times the
  largest net benefit.

## Value

A \`ggplot2\` object.

## See also

\[decision_curve()\]

## Examples

``` r
set.seed(1)
x <- rnorm(400)
y <- rbinom(400, 1, plogis(-1 + x))
plot_decision_curve(y, pred = fitted(glm(y ~ x, family = binomial())))
```
