

<!-- README.md is generated from README.qmd. Please edit README.qmd -->

# TempleCBE <img src="man/figures/logo.png" align="right" height="138" alt="TempleCBE logo" />

[![R-CMD-check](https://github.com/jkylearmstrong/TempleCBE/workflows/R-CMD-check/badge.svg)](https://github.com/jkylearmstrong/TempleCBE/actions)
[![Codecov test
coverage](https://codecov.io/gh/jkylearmstrong/TempleCBE/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jkylearmstrong/TempleCBE)
[![License: GPL-3 \|
MIT](https://img.shields.io/badge/License-GPL--3%20%7C%20MIT-yellow.svg)](LICENSE.md)
[![Version](https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fraw.githubusercontent.com%2Fjkylearmstrong%2FTempleCBE%2Fmaster%2FDESCRIPTION&query=%24.Version&label=version&color=blue)](https://github.com/jkylearmstrong/TempleCBE)

**TempleCBE** is an open-source R package developed for Temple
University’s **Center for Biostatistics and Epidemiology (CBE)**. It
provides a clean, domain-agnostic suite of biostatistical testing
functions, data quality and missingness visualizations, normalization
utilities, correlation and PCA helpers, custom Tidymodels recipe steps
(`step_famd`), and a tidymodels-native survival modeling toolkit
(`coxnet()`, `cv_coxnet()`, `glmnet_IBS()`) for start/stop clinical
data.

------------------------------------------------------------------------

## 📦 Installation

You can install the development version of `TempleCBE` directly from
GitHub:

``` r
# Install pak if not already installed
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")

# Install TempleCBE
pak::pak("jkylearmstrong/TempleCBE")
```

Or using `remotes`:

``` r
remotes::install_github("jkylearmstrong/TempleCBE")
```

------------------------------------------------------------------------

## 🚀 Quick Start

``` r
library(TempleCBE)
library(dplyr)
```

### 1. Data Quality & Missingness Analysis

Quickly compute total missing counts and detailed feature-level
missingness tables:

``` r
# Sample dataset with missing values
df <- tibble(
  patient_id = 1:5,
  age = c(45, 52, NA, 61, 38),
  bmi = c(NA, 24.5, 29.1, NA, 31.0),
  blood_pressure = c(120, NA, 135, 140, 118)
)

# Total missing values across dataset
SumNa(df)

# Feature-level missingness summary
features_percent_miss(df)
```

Generate a missingness summary plot:

``` r
plot_features_percent_miss(df)
```

See the [Exploratory Data Analysis, Missingness Auditing, and
Normalization](https://jkylearmstrong.github.io/TempleCBE/articles/01_eda_and_missingness.html)
vignette for a full walkthrough, including non-standard missing codes
and outlier-aware EDA.

### 2. Normalization & Outlier Detection

Standardize features or detect numerical outliers using Interquartile
Range (IQR) thresholding:

``` r
# Min-Max Normalization to [0, 1]
min_max_norm(df$age)

# Z-Score Standardization (mean = 0, sd = 1)
z_norm(df$age)

# Detect numerical outliers via IQR fences (MILD vs EXTREME, by inner/outer fence)
detect_outliers(data.frame(measurement = c(1, 2, 3, 4, 5, 100)))
```

### 3. Correlation & Principal Component Analysis

Pairwise correlation tests across every numeric column, and PCA variance
summaries straight from raw data (no need to fit `prcomp()` yourself
first):

``` r
# Every pairwise correlation, strongest positive correlation first
corr_test_all(mtcars[, c("mpg", "hp", "wt", "qsec")], columns = "tidy", sort = "estimate")

# proc_pca() fits the PCA and summarizes it in one step
proc_pca(mtcars[, 1:4], scale = TRUE)
```

### 4. Penalized Cox Models for Survival Data

`coxnet()` and `cv_coxnet()` fit elastic-net Cox models through the
tidymodels `hardhat` interface, supporting both right-censored and
start/stop (counting-process) outcomes:

``` r
library(survival)

lung_data <- na.omit(lung[, c("time", "status", "age", "sex", "ph.ecog", "wt.loss")])
fit <- coxnet(Surv(time, status) ~ ., data = lung_data, penalty = 0.05, mixture = 0.5)

generics::tidy(fit)
predict(fit, lung_data[1:3, ], type = "survival", eval_time = c(180, 365))$.pred[[1]]
```

For repeated-measures/start-stop data, `glmnet_IBS()` tunes and scores a
penalized Cox model with a proper, subject-grouped integrated Brier
score. See the [Penalized Cox Models and Nested Cross-Validation for
Start/Stop Survival
Data](https://jkylearmstrong.github.io/TempleCBE/articles/nested_survival_cv.html)
vignette for the full nested cross-validation workflow.

### 5. Custom Tidymodels Recipe Step: `step_famd`

Extract Factor Analysis of Mixed Data (FAMD) principal components
seamlessly within the `tidymodels` framework:

``` r
library(recipes)

# Define recipe with mixed numeric and categorical variables
rec <- recipe(Species ~ ., data = iris) %>%
  step_famd(all_predictors(), num_comp = 2)

# Prep and bake
prepped_rec <- prep(rec)
baked_data <- bake(prepped_rec, new_data = NULL)

head(baked_data)
```

### 6. Infix Helper Operators

Convenient syntax for string matching and negation:

``` r
# Pattern matching operators
"patient_cohort_A" %like% "cohort"   # TRUE
"PATIENT_COHORT_A" %ilike% "cohort"  # TRUE (case-insensitive)

# Negated %in% operator
5 %notin% c(1, 2, 3, 4)             # TRUE
```

------------------------------------------------------------------------

## 📚 Vignettes

Beyond the quick-start snippets above, `TempleCBE` ships full
worked-example vignettes:

- **[01. Exploratory Data Analysis, Missingness Auditing, and
  Normalization](https://jkylearmstrong.github.io/TempleCBE/articles/eda_and_missingness.html)**
  — non-standard missing codes, missingness visualization, and
  outlier-aware normalization for messy clinical/EHR data.
- **[02. Penalized Cox Models and Nested Cross-Validation for Start/Stop
  Survival
  Data](https://jkylearmstrong.github.io/TempleCBE/articles/nested_survival_cv.html)**
  — why row-level resampling leaks for counting-process data, and how
  `coxnet()`/`cv_coxnet()`/`nested_cv_coxnet()` avoid it.
- **[03. Visualizing Computational Pipeline
  Dependencies](https://jkylearmstrong.github.io/TempleCBE/articles/compute_graph.html)**
  — treating a multi-report analysis pipeline as a dependency graph with
  `get_render_plan()` and project-level `MakeComputeGraph.R`.
- **[04. Validation and Workflow Guide for SAS Users: Cox Models in
  TempleCBE](https://jkylearmstrong.github.io/TempleCBE/articles/sas_survival.html)**
  — cross-validating time-fixed and time-dependent Cox models against
  SAS PROC PHREG, with `survival::tmerge` and `tidy_tmerge_cox()`.

Once installed, each is also available locally via
`vignette("eda_and_missingness", package = "TempleCBE")` (substituting
the vignette name).

------------------------------------------------------------------------

## 🏛️ Ecosystem Architecture

`TempleCBE` is the public layer of Temple CBE’s biostatistics framework
— general-purpose code that private, protected-data repos import and
re-export, so they can be validated against something publicly auditable
instead of each maintaining their own private, unreviewed copy.

**Public**:

- **[`pslongSim`](https://github.com/jkylearmstrong/pslongSim)**:
  longitudinal propensity score simulation, and the designated source of
  synthetic example/test data across the ecosystem.
- **[`omop-duck-db`](https://github.com/jkylearmstrong/omop-duck-db)**:
  OMOP CDM database creation/querying (DuckDB).
- **[`quarto_temple_brand`](https://github.com/jkylearmstrong-temple/quarto_temple_brand)**:
  Quarto branding/report templates.

Several other Temple Center for Biostatistics and Epidemiology (CBE) PI
studies and analyses import `TempleCBE` for their own private,
protected-data work — keeping study-specific code and data private while
validating the general-purpose statistics they depend on against this
public, auditable layer.

------------------------------------------------------------------------

## 📄 License

Dual-licensed at your option under GPL-3 or MIT — see
[LICENSE.md](LICENSE.md) and [LICENSE](LICENSE).
