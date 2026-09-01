# Exploratory Data Analysis, Missingness Auditing, and Normalization

## Overview

Clinical datasets and electronic health records (EHR) frequently exhibit
complex missingness patterns, non-standard missing codes (e.g. `"999"`,
`"NA"`, `""`), and extreme statistical outliers. The **`TempleCBE`**
package provides domain-agnostic tools to streamline exploratory data
analysis (EDA), quantify missingness, normalize numerical features, and
flag outliers cleanly.

``` r

library(TempleCBE)
library(dplyr)
```

------------------------------------------------------------------------

## 1. Auditing Missing Data

### Total Missingness with `SumNa()`

[`SumNa()`](https://jkylearmstrong.github.io/TempleCBE/reference/SumNa.md)
counts missing values in vectors, data frames, or matrices, supporting
explicit missing value codes (such as `"NA"` strings or empty strings):

``` r

library(TempleCBE)

# Sample vector with default and non-standard missing values
vec <- c(12, 14, NA, 999, 18, "")

# Count standard NAs
SumNa(vec)
#> [1] 1

# Count NAs including custom missing codes
SumNa(vec, na_list = c("999", ""))
#> [1] 3
```

### Feature-Level Missingness Table

[`features_percent_miss()`](https://jkylearmstrong.github.io/TempleCBE/reference/features_percent_miss.md)
generates a detailed summary table sorted descending by missingness
percentage:

``` r

# Synthetic clinical dataframe
df_clinical <- data.frame(
  patient_id = 1:6,
  age = c(45, 52, NA, 61, 38, NA),
  systolic_bp = c(120, 135, NA, 140, 118, 125),
  bmi = c(22.5, NA, 29.1, NA, 31.0, NA),
  smoker = c("No", "Yes", "", "No", "NA", "Yes"),
  stringsAsFactors = FALSE
)

# Audit missingness with custom codes
features_percent_miss(df_clinical, na_list = c("", "NA"))
#> # A tibble: 5 × 5
#>   feature     SumNa SumComp PctNa PctComp
#>   <chr>       <int>   <int> <dbl>   <dbl>
#> 1 bmi             3       3 0.5     0.5  
#> 2 age             2       4 0.333   0.667
#> 3 smoker          2       4 0.333   0.667
#> 4 systolic_bp     1       5 0.167   0.833
#> 5 patient_id      0       6 0       1
```

### Missingness Visualizations

Visualize missingness across all features using
[`plot_features_percent_miss()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_features_percent_miss.md)
or S3 [`plot()`](https://rdrr.io/r/graphics/plot.default.html):

``` r

# Plot feature missingness chart
plot_features_percent_miss(df_clinical)

# Plot top 3 missing features
plot_features_percent_miss(df_clinical, top_n = 3)
```

Or view complete missingness heatmaps with
[`missmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/missmap.md):

``` r

# Render missingness heatmap
missmap(df_clinical)
```

------------------------------------------------------------------------

## 2. Feature Normalization

`TempleCBE` includes three standard feature scaling transformations:

- **[`min_max_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/min_max_norm.md)**:
  Scales values to the range $`[0, 1]`$.
- **[`z_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/z_norm.md)**:
  Standardizes features to mean $`0`$ and standard deviation $`1`$.
- **[`range_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/range_norm.md)**:
  Scales relative to combined distribution bounds across columns.

``` r

x <- c(10, 20, 30, 40, 50)

# Min-Max Normalization
min_max_norm(x)
#> [1] 0.00 0.25 0.50 0.75 1.00

# Z-Score Normalization
z_norm(x)
#> [1] -1.2649111 -0.6324555  0.0000000  0.6324555  1.2649111
```

------------------------------------------------------------------------

## 3. Outlier Detection

[`detect_outliers()`](https://jkylearmstrong.github.io/TempleCBE/reference/detect_outliers.md)
evaluates numeric vectors or data frame columns using Interquartile
Range (IQR) thresholding, distinguishing between **MILD** (1.5 × IQR)
and **EXTREME** (3.0 × IQR) outliers.

``` r

data_vals <- c(12, 14, 15, 14, 16, 15, 14, 45, 120)

# Detect mild and extreme outliers
detect_outliers(data_vals)
#> # A tibble: 2 × 4
#>   column value .outlier .outlier_type
#>   <chr>  <dbl> <fct>    <fct>        
#> 1 value     45 TRUE     EXTREME      
#> 2 value    120 TRUE     EXTREME
```

------------------------------------------------------------------------

## Summary

Combining
[`SumNa()`](https://jkylearmstrong.github.io/TempleCBE/reference/SumNa.md),
[`features_percent_miss()`](https://jkylearmstrong.github.io/TempleCBE/reference/features_percent_miss.md),
[`min_max_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/min_max_norm.md),
and
[`detect_outliers()`](https://jkylearmstrong.github.io/TempleCBE/reference/detect_outliers.md)
provides a reproducible, auditable workflow for preparing clinical data
for downstream statistical modeling.
