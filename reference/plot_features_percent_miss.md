# Plot method for features_percent_miss objects

Creates a bar chart visualizing feature missingness percentages for
objects produced by
[`features_percent_miss`](https://jkylearmstrong.github.io/TempleCBE/reference/features_percent_miss.md).

Generates a horizontal bar chart visualizing the percentage of missing
values per column.

## Usage

``` r
# S3 method for class 'features_percent_miss'
plot(x, top_n = NULL, ...)

plot_features_percent_miss(data, top_n = NULL)
```

## Arguments

- x:

  An object of class `features_percent_miss`.

- top_n:

  Optional integer to limit to the top N features with highest
  missingness.

- ...:

  Additional arguments (currently unused).

- data:

  A data frame, tibble, or the output of \`features_percent_miss()\`.

## Value

A ggplot object representing feature missingness.

A ggplot object.

## Examples

``` r
res <- features_percent_miss(mtcars)
plot(res)


df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, NA, 3, 4))
plot_features_percent_miss(df)
```
