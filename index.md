# TempleCBE

# TempleCBE ![](inst/templates/Temple_Logo.png)

[![R-CMD-check](https://github.com/jkylearmstrong/TempleCBE/workflows/R-CMD-check/badge.svg)](https://github.com/jkylearmstrong/TempleCBE/actions)
[![License: GPL-3 \|
MIT](https://img.shields.io/badge/License-GPL--3%20%7C%20MIT-yellow.svg)](https://jkylearmstrong.github.io/TempleCBE/LICENSE.md)
[![Version](https://img.shields.io/badge/version-0.1.2-blue.svg)](https://github.com/jkylearmstrong/TempleCBE)

**TempleCBE** is an open-source R package developed for Temple
University’s **Center for Biostatistics and Epidemiology (CBE)**. It
provides a clean, domain-agnostic suite of biostatistical testing
functions, data quality and missingness visualizations, normalization
utilities, custom Tidymodels recipe steps (`step_famd`), and clinical
survival evaluation metrics (`glmnet_IBS`).

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

### 3. Infix Helper Operators

Convenient syntax for string matching and negation:

``` r

# Pattern matching operators
"patient_cohort_A" %like% "cohort"   # TRUE
"PATIENT_COHORT_A" %ilike% "cohort"  # TRUE (case-insensitive)

# Negated %in% operator
5 %notin% c(1, 2, 3, 4)             # TRUE
```

### 4. Custom Tidymodels Recipe Step: `step_famd`

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

------------------------------------------------------------------------

## 🏛️ Ecosystem Architecture

`TempleCBE` is the public layer of Temple CBE’s biostatistics framework
— general-purpose code that private, protected-data repos import and
re-export, so they can be validated against something publicly auditable
instead of each maintaining their own private, unreviewed copy.

**Public** (this layer):

- **[`TempleCBE`](https://github.com/jkylearmstrong/TempleCBE)**: this
  package — biostatistical, EDA, testing, and modeling utilities.
- **[`pslongSim`](https://github.com/jkylearmstrong/pslongSim)**:
  longitudinal propensity score simulation, and the designated source of
  synthetic example/test data across the ecosystem.
- **[`omop-duck-db`](https://github.com/jkylearmstrong/omop-duck-db)**:
  OMOP CDM database creation/querying (DuckDB).
- **`ML-PScore`**: propensity score / cohort ML tooling (private today,
  pending PI approval to go public — already licensed to match this
  layer).
- **`quarto_temple_brand`**: Quarto branding/report templates.

**Private, imports from the public layer** (never the reverse):

- **`datasci`**: Temple CBE’s protected data store and internal DB
  access layer. Re-exports every function it used to own locally
  straight from `TempleCBE` (`datasci/R/reexports.R`), so nothing that
  calls `datasci::<function>()` needed to change.
- **`Wolfson`**: Dr. Marla Wolfson’s injury-phenotype research project —
  the original source of `step_famd`, `glmnet_IBS`, `proc_pca`, and
  `corr_test_all`, which were copied here (not moved — see `NOTES.md`
  for why) and are now maintained as two deliberately separate copies.
- **`kyle_out_zip`**: the Rubin eDerri project codebase; may hold
  protected multi-site clinical data.

See `NOTES.md` for how this split came about and what’s still open.

------------------------------------------------------------------------

## 📄 License

Dual-licensed at your option under GPL-3 or MIT — see
[LICENSE.md](https://jkylearmstrong.github.io/TempleCBE/LICENSE.md) and
[LICENSE](https://jkylearmstrong.github.io/TempleCBE/LICENSE).
