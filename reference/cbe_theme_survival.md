# CBE ggplot2 Theme for Survival Analysis Visualizations

A variant of
[`theme_cbe()`](https://jkylearmstrong.github.io/TempleCBE/reference/theme_cbe.md)
tuned for Cox model and Kaplan-Meier plots (forest plots,
predicted/observed survival curves, marginal risk curves), giving a
single consistent look across
[`plot_cox_forest()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_cox_forest.md),
[`plot_cox_forest_multi()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_cox_forest_multi.md),
[`plot_cox_survival()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_cox_survival.md),
and
[`plot_cox_marginal()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_cox_marginal.md).

## Usage

``` r
cbe_theme_survival(base_size = 12, base_family = "")
```

## Arguments

- base_size:

  Base font size (default: 12)

- base_family:

  Base font family (default: "")

## Value

A ggplot2 theme object

## See also

\[theme_cbe()\], \[theme_cbe_deck()\]
