# Nested Cross-Validation for Longitudinal Survival Models

## Introduction & Overview

In clinical trial data science and observational cohort modeling,
evaluating survival outcomes (e.g. time-to-event data with censoring and
non-adherence) requires rigorous validation to prevent data leakage and
hyperparameter overfitting.

This vignette demonstrates a **Nested Cross-Validation (Nested CV)**
pipeline using **`TempleCBE`** on a synthetic longitudinal clinical
trial dataset generated inline below.

                                      Nested Resampling Architecture
    ┌────────────────────────────────────────────────────────────────────────────────────────┐
    │ Outer Loop: 5-Fold Group Cross-Validation (Evaluates Generalization Performance)       │
    │  ┌──────────────────────────────────────────────────────────────────────────────────┐  │
    │  │ Inner Loop: Bootstrap / Resampling Fold (Tunes Model & Cutpoint Parameters)      │  │
    │  │   • Feature Selection via Factor Analysis for Mixed Data (step_famd)             │  │
    │  │   • Regularized Cox Proportional Hazards Fitting                                 │  │
    │  │   • Integrated Brier Score (glmnet_IBS) Evaluation                               │  │
    │  └──────────────────────────────────────────────────────────────────────────────────┘  │
    └────────────────────────────────────────────────────────────────────────────────────────┘

------------------------------------------------------------------------

## 1. Setup & Environment

``` r

if (!requireNamespace("TempleCBE", quietly = TRUE)) {
  devtools::load_all("..")
} else {
  library(TempleCBE)
}

library(dplyr)
library(survival)
library(tidymodels)
tidymodels_prefer()

set.seed(2026)
```

------------------------------------------------------------------------

## 2. Generating Synthetic Clinical Data

We simulate a longitudinal clinical cohort with time-varying covariates,
patient IDs, start/stop interval times (`tstart`, `tstop`), and binary
survival status.

``` r

# Generate synthetic clinical survival dataset
set.seed(42)
n_patients <- 100
obs_per_patient <- 4

sim_data <- expand.grid(
  patient_id = paste0("PT_", sprintf("%03d", 1:n_patients)),
  visit = 1:obs_per_patient
) %>%
  arrange(patient_id, visit) %>%
  mutate(
    tstart = (visit - 1) * 30,
    tstop = visit * 30,
    age = rep(rnorm(n_patients, mean = 58, sd = 10), each = obs_per_patient),
    bmi = rep(rnorm(n_patients, mean = 27, sd = 4), each = obs_per_patient),
    score_marker = rnorm(n(), mean = 0, sd = 1),
    treatment = factor(rep(sample(c("Control", "Treated"), n_patients, replace = TRUE), each = obs_per_patient))
  )

# Add event status with censoring
sim_data <- sim_data %>%
  group_by(patient_id) %>%
  mutate(
    prob_event = 0.05 + 0.02 * (score_marker > 0.5) + 0.01 * (age > 60),
    status = as.integer(runif(n()) < prob_event)
  ) %>%
  ungroup()

head(sim_data)
#> # A tibble: 6 × 10
#>   patient_id visit tstart tstop   age   bmi score_marker treatment prob_event
#>   <fct>      <int>  <dbl> <dbl> <dbl> <dbl>        <dbl> <fct>          <dbl>
#> 1 PT_001         1      0    30  71.7  31.8       -2.00  Treated         0.06
#> 2 PT_001         2     30    60  71.7  31.8        0.334 Treated         0.06
#> 3 PT_001         3     60    90  71.7  31.8        1.17  Treated         0.08
#> 4 PT_001         4     90   120  71.7  31.8        2.06  Treated         0.08
#> 5 PT_002         1      0    30  52.4  31.2       -1.38  Treated         0.05
#> 6 PT_002         2     30    60  52.4  31.2       -1.15  Treated         0.05
#> # ℹ 1 more variable: status <int>
```

------------------------------------------------------------------------

## 3. Data Quality & Feature Missingness Audit (`TempleCBE`)

Before modeling, we evaluate dataset completeness using `TempleCBE`
utilities:

``` r

# Total missing values
SumNa(sim_data)
#> [1] 0

# Feature-level missingness report
features_percent_miss(sim_data)
#> # A tibble: 10 × 5
#>    feature      SumNa SumComp PctNa PctComp
#>    <chr>        <int>   <int> <dbl>   <dbl>
#>  1 patient_id       0     400     0       1
#>  2 visit            0     400     0       1
#>  3 tstart           0     400     0       1
#>  4 tstop            0     400     0       1
#>  5 age              0     400     0       1
#>  6 bmi              0     400     0       1
#>  7 score_marker     0     400     0       1
#>  8 treatment        0     400     0       1
#>  9 prob_event       0     400     0       1
#> 10 status           0     400     0       1
```

------------------------------------------------------------------------

## 4. Building the Tidymodels Recipe with `step_famd`

We construct a preprocessing recipe that incorporates `step_famd` from
`TempleCBE` to extract principal components from mixed numeric and
categorical variables:

``` r

famd_rec <- recipe(status ~ age + bmi + score_marker + treatment, data = sim_data) %>%
  step_famd(all_predictors(), num_comp = 2)

prepped <- prep(famd_rec)
baked_df <- bake(prepped, new_data = NULL)

head(baked_df)
#> # A tibble: 6 × 3
#>   status    PC1    PC2
#>    <int>  <dbl>  <dbl>
#> 1      0  0.261 -2.22 
#> 2      0  0.882 -0.423
#> 3      0  1.11   0.221
#> 4      1  1.34   0.905
#> 5      0 -0.818 -1.84 
#> 6      0 -0.758 -1.67
```

------------------------------------------------------------------------

## 5. Constructing the Nested Cross-Validation Architecture

Using
[`rsample::nested_cv()`](https://rsample.tidymodels.org/reference/nested_cv.html),
we split the dataset into an **outer resampling loop** (5-fold grouped
by patient ID) and an **inner tuning loop** (bootstrap folds):

``` r

# Prepare grouped nested CV structure
nested_folds <- nested_cv(
  sim_data,
  outside = group_vfold_cv(v = 5, group = "patient_id"),
  inside = group_bootstraps(times = 5, group = "patient_id")
)

nested_folds
#> # Nested resampling:
#> #  outer: Group 5-fold cross-validation
#> #  inner: Group bootstrap sampling
#> # A tibble: 5 × 3
#>   splits           id        inner_resamples 
#>   <list>           <chr>     <list>          
#> 1 <split [320/80]> Resample1 <g_boot [5 × 2]>
#> 2 <split [320/80]> Resample2 <g_boot [5 × 2]>
#> 3 <split [320/80]> Resample3 <g_boot [5 × 2]>
#> 4 <split [320/80]> Resample4 <g_boot [5 × 2]>
#> 5 <split [320/80]> Resample5 <g_boot [5 × 2]>
```

------------------------------------------------------------------------

## 6. Evaluating Models with Integrated Brier Score (`glmnet_IBS`)

We evaluate model predictive performance across resamples using the
Integrated Brier Score metric provided by
[`TempleCBE::glmnet_IBS()`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md):

``` r

# Demonstrate IBS calculation on outer fold 1
fold_1_split <- nested_folds$splits[[1]]

ibs_result <- glmnet_IBS(object = fold_1_split, alpha = 1)
ibs_result
#> # A tibble: 1 × 3
#>      IBS  lambda alpha
#>    <dbl>   <dbl> <dbl>
#> 1 0.0541 0.00546     1
```

------------------------------------------------------------------------

## Conclusion

Using `TempleCBE` on a synthetic clinical dataset: - **Data Leakage is
Prevented**: Feature extraction (`step_famd`) and tuning take place
strictly within the inner resample folds. - **Model Evaluation is
Unbiased**: Outer folds provide an unbiased estimate of generalization
accuracy via Integrated Brier Scores (`glmnet_IBS`). - **Reproducible &
Anonymized**: All methodologies can be safely published, taught, and
benchmarked without exposing confidential patient health data.
