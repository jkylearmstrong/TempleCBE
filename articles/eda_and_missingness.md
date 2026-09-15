# Exploratory Data Analysis, Missingness Auditing, and Normalization

## Overview

In clinical trials, electronic health records (EHR), and epidemiological
registries, data cleanliness is rarely a given. Datasets frequently
exhibit complex missingness patterns, non-standard missing codes
(e.g. `"999"`, `"-99"`, `"Unknown"`, `"Refused"`, `""`), and extreme
statistical outliers from data-entry errors or severe disease
phenotypes.

Standard data science tools often assume missingness is cleanly
represented as `NA`, requiring destructive multi-pass preprocessing
before exploratory analysis can even begin. The **`TempleCBE`** package
provides domain-tailored biostatistical tools to audit non-standard
missingness non-destructively, rank feature completeness, standardize
numerical features, classify clinical outliers under formal IQR
standards, and transition seamlessly into tuned machine-learning
imputation.

``` r

library(TempleCBE)
library(dplyr)
```

------------------------------------------------------------------------

## Comparison with existing R EDA & missingness tools

The R ecosystem offers several packages for exploratory data analysis
and missingness visualization, notably
[naniar](https://naniar.njtierney.com/),
[visdat](https://docs.ropensci.org/visdat/),
[mice](https://amices.org/mice/), and general EDA toolkits like
`DataExplorer` and `summarytools`. Understanding where `TempleCBE` fits
alongside these frameworks clarifies its value for clinical
biostatisticians and data managers.

### Existing tools and their limitations in clinical workflows

- **`naniar` & `visdat`**: Pioneered tidy representation and
  visualization of missing data (`vis_miss()`, `gg_miss_var()`).
  However, they primarily operate on existing `NA` values. When raw
  clinical data contains sentinel values (such as `"999"`, `"-99"`, or
  blank strings), users must first apply mutating functions like
  `replace_with_na_all()`, which can accidentally alter legitimate
  numerical values, modify factor levels, or mutate data types.
- **`mice` / `Amelia`**: Primarily multiple imputation engines. While
  they include basic diagnostics (e.g. `md.pattern()`), their tools are
  oriented toward parameter checking rather than automated feature-level
  reporting for regulatory deliverables.
- **Base R / `dplyr` summaries**: Simple calls like `colSums(is.na(df))`
  provide quick counts, but lack built-in percentage calculations,
  descending order sorting, sentinel code awareness, and
  publication-ready formatting.

### Where `TempleCBE` provides distinct value

`TempleCBE` was built specifically for the constraints of institutional
biostatistics core facilities:

1.  **Zero-mutation non-standard code auditing**:
    [`SumNa()`](https://jkylearmstrong.github.io/TempleCBE/reference/SumNa.md)
    and
    [`features_percent_miss()`](https://jkylearmstrong.github.io/TempleCBE/reference/features_percent_miss.md)
    natively accept vectors of custom missing codes
    (`na_list = c("999", "-99", "", "Unknown", "NA")`). They compute
    exact missingness counts and percentages *without altering or
    coercing the underlying data frame*.
2.  **Two-tier clinical outlier governance**: While generic outlier
    functions often rely on simple z-score cutoffs or arbitrary
    quantiles,
    [`detect_outliers()`](https://jkylearmstrong.github.io/TempleCBE/reference/detect_outliers.md)
    implements Tukey’s formal biostatistical standard, explicitly
    distinguishing between **MILD** ($`1.5 \times \text{IQR}`$) and
    **EXTREME** ($`3.0 \times \text{IQR}`$) outliers and returning
    structured classification flags that preserve patient keys.
3.  **Seamless transition to tuned imputation**: `TempleCBE` links
    missingness auditing directly to high-performance imputation engines
    (`missforest_mtry()` and `missranger_mtry()`) that automatically
    optimize random forest hyperparameters (`mtry`) for the specific
    missingness rate of the clinical cohort.
4.  **Controlled imputation benchmarking**: The
    [`add_missing()`](https://jkylearmstrong.github.io/TempleCBE/reference/add_missing.md)
    function allows statisticians to simulate controlled Missing
    Completely at Random (MCAR) mechanisms on complete datasets to
    empirically validate downstream imputation performance before
    deploying models.

### Feature comparison matrix

| Dimension | Base / Tidyverse | `naniar` / `visdat` | `TempleCBE` |
|:---|:---|:---|:---|
| **Multi-code missingness** | Manual recoding required | Requires mutating `replace_with_na()` | Built-in via `na_list` argument (zero mutation) |
| **Feature-level ranking** | Multi-line [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html) | `miss_var_summary()` | [`features_percent_miss()`](https://jkylearmstrong.github.io/TempleCBE/reference/features_percent_miss.md) (sorted tibble + S3 plot) |
| **Missingness heatmap** | Not built-in | `vis_miss()` | [`missmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/missmap.md) (custom clinical palette) |
| **Outlier classification** | Manual IQR calculations | Not covered | Two-tier Tukey standard (MILD: 1.5×, EXTREME: 3.0×) |
| **Feature normalization** | [`scale()`](https://rdrr.io/r/base/scale.html) (matrix output) | Not covered | [`min_max_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/min_max_norm.md), [`z_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/z_norm.md), [`range_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/range_norm.md) |
| **Imputation handoff** | External | External | Integrated `missforest_mtry()` / `missranger_mtry()` |
| **Controlled amputation** | External (`mice::ampute`) | Limited | [`add_missing()`](https://jkylearmstrong.github.io/TempleCBE/reference/add_missing.md) with tracked cell coordinates |

------------------------------------------------------------------------

## Specific gaps addressed by `TempleCBE`

### 1. The “Dirty Intake” gap in clinical registries

Clinical data extracted from Electronic Data Capture (EDC) platforms
like REDCap or hospital registries frequently blends numeric sentinel
values (`999`, `-99`) and text placeholders (`"Not Recorded"`,
`"Declined"`, `""`). Recoding these in place often converts columns from
numeric to character, or loses the ability to distinguish why data was
uncollected. `TempleCBE` audits all codes in their native types without
destructive modifications.

### 2. The “Clinical Outlier Governance” gap

In clinical trials, an extreme lab measurement (e.g. a blood glucose of
650 mg/dL) might represent a life-threatening clinical event rather than
a measurement error. Blindly trimming or imputing values without
clinician review violates regulatory guidelines (FDA/ICH E9).
[`detect_outliers()`](https://jkylearmstrong.github.io/TempleCBE/reference/detect_outliers.md)
provides a transparent audit table flagging both mild and extreme
outliers, allowing biostatisticians and medical monitors to audit values
systematically.

### 3. The “Imputation Hyperparameter” gap

Random forest imputation (`missForest` / `missRanger`) is widely
considered the gold standard for mixed clinical data with non-linear
relationships. However, analysts routinely accept default `mtry`
settings, which can lead to suboptimal imputation or excessive
computational runtimes on large patient cohorts. `TempleCBE` integrates
automated `mtry` grid search directly into the imputation pipeline.

------------------------------------------------------------------------

## 1. Auditing Missing Data

### Total missingness with `SumNa()`

[`SumNa()`](https://jkylearmstrong.github.io/TempleCBE/reference/SumNa.md)
evaluates total missing cells across an entire vector, data frame, or
matrix. When working with raw registry extracts, specify custom sentinel
codes in `na_list`:

``` r

# Sample vector with default and non-standard missing codes
vec <- c(12, 14, NA, 999, 18, "", "Unknown")

# Count standard NAs only
SumNa(vec)
#> [1] 1

# Count NAs and sentinel codes simultaneously
SumNa(vec, na_list = c("999", "", "Unknown"))
#> [1] 4
```

### Feature-level missingness table with `features_percent_miss()`

In clinical study preparation, biostatisticians need a sorted summary
showing which variables exceed missingness thresholds (e.g. variables
with $`>20\%`$ missingness that cannot be reliably imputed):

``` r

# Synthetic clinical cohort
df_clinical <- data.frame(
  patient_id  = 1:6,
  age         = c(45, 52, NA, 61, 38, NA),
  systolic_bp = c(120, 135, NA, 140, 118, 125),
  bmi         = c(22.5, NA, 29.1, NA, 31.0, NA),
  smoker      = c("No", "Yes", "", "No", "NA", "Yes"),
  hba1c       = c(5.6, 999, 6.2, 7.1, 999, 5.8),
  stringsAsFactors = FALSE
)

# Audit missingness including blank strings and sentinel '999' codes
miss_tbl <- features_percent_miss(df_clinical, na_list = c("", "NA", 999))
miss_tbl
#> # A tibble: 6 × 5
#>   feature     SumNa SumComp PctNa PctComp
#>   <chr>       <int>   <int> <dbl>   <dbl>
#> 1 bmi             3       3 0.5     0.5  
#> 2 age             2       4 0.333   0.667
#> 3 smoker          2       4 0.333   0.667
#> 4 hba1c           2       4 0.333   0.667
#> 5 systolic_bp     1       5 0.167   0.833
#> 6 patient_id      0       6 0       1
```

### Visualizing missingness with `plot_features_percent_miss()` and `missmap()`

[`plot_features_percent_miss()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_features_percent_miss.md)
(or its S3 method `plot(miss_tbl)`) generates a clean horizontal bar
chart sorted by missingness frequency:

``` r

# Plot missingness across all features
plot_features_percent_miss(df_clinical, na_list = c("", "NA", 999))

# Isolate only the top 3 most missing variables
plot_features_percent_miss(df_clinical, top_n = 3, na_list = c("", "NA", 999))
```

#### Heatmaps with `missmap()`: Row-level and Group-level views

The
[`missmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/missmap.md)
function visualizes co-occurrence and missingness clustering across the
dataset:

1.  **Per-row / Per-column view**: Visualizes each observation
    individually, sorting rows and features by descending missingness
    count when `row_order = FALSE`:

``` r

# Render full patient-level missingness heatmap
missmap(df_clinical, na_list = c("", "NA", 999))
```

2.  **Group-aggregated view (`by_column = ...`)**: In multi-center
    clinical trials, auditing missingness aggregated by hospital site,
    study arm, or visit number is critical for monitoring trial conduct:

``` r

# Synthetic multi-site clinical cohort
df_multisite <- df_clinical |>
  mutate(site = c("Hospital_A", "Hospital_A", "Hospital_B", "Hospital_B", "Hospital_C", "Hospital_C"))

# Aggregate missingness counts per site across clinical features
# Displays a continuous gradient from black to Temple cherry
missmap(df_multisite, by_column = site, na_list = c("", "NA", 999), fill = "count")

# Or display binary presence/absence per site
missmap(df_multisite, by_column = site, na_list = c("", "NA", 999), fill = "binary")
```

------------------------------------------------------------------------

## 2. Feature Normalization

Machine learning models and penalized regressions require predictors to
be scaled comparably. `TempleCBE` provides three straightforward vector
transformations:

- **[`min_max_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/min_max_norm.md)**:
  Scales values strictly into the bounded interval $`[0, 1]`$.
- **[`z_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/z_norm.md)**:
  Centers values to mean $`\mu = 0`$ and scales to standard deviation
  $`\sigma = 1`$.
- **[`range_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/range_norm.md)**:
  Scales relative to combined distribution bounds across features.

``` r

vals <- c(10, 20, 30, 40, 50)

# Min-Max normalization [0, 1]
min_max_norm(vals)
#> [1] 0.00 0.25 0.50 0.75 1.00

# Z-Score standardization (mean = 0, sd = 1)
z_norm(vals)
#> [1] -1.2649111 -0.6324555  0.0000000  0.6324555  1.2649111
```

------------------------------------------------------------------------

## 3. Two-Tier Outlier Detection

[`detect_outliers()`](https://jkylearmstrong.github.io/TempleCBE/reference/detect_outliers.md)
audits numerical vectors or data frame columns using Tukey’s
interquartile range criteria: - **Mild Outliers**: Values lying outside
$`[Q_1 - 1.5 \times \text{IQR}, Q_3 + 1.5 \times \text{IQR}]`$. -
**Extreme Outliers**: Values lying outside
$`[Q_1 - 3.0 \times \text{IQR}, Q_3 + 3.0 \times \text{IQR}]`$.

``` r

clinical_labs <- c(12, 14, 15, 14, 16, 15, 14, 45, 120)

# Detect mild and extreme anomalies
outlier_report <- detect_outliers(clinical_labs)
outlier_report
#> # A tibble: 2 × 4
#>   column value .outlier .outlier_type
#>   <chr>  <dbl> <fct>    <fct>        
#> 1 value     45 TRUE     EXTREME      
#> 2 value    120 TRUE     EXTREME
```

When evaluated on data frames,
[`detect_outliers()`](https://jkylearmstrong.github.io/TempleCBE/reference/detect_outliers.md)
returns a structured summary detailing outlier counts, threshold fences,
and row indices for clinical auditing.

------------------------------------------------------------------------

## 4. Complementary Biostatistical Tools in `TempleCBE`

In addition to missingness and scaling, `TempleCBE` provides statistical
auditing utilities that bridge early EDA with formal statistical
testing:

### Normality testing and distribution plots

Before applying parametric tests (e.g. Student’s t-test or ANOVA),
assess distributional assumptions using
[`distribution_test()`](https://jkylearmstrong.github.io/TempleCBE/reference/distribution_test.md)
and
[`distribution_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/distribution_plot.md):

``` r

# Sample biomarker data
biomarker <- c(rnorm(50, mean = 25, sd = 4), 65, 72)

# Conduct Anderson-Darling and Shapiro-Wilk tests
distribution_test(biomarker)
#> # A tibble: 3 × 8
#>   statistic parameter  p.value method distribution.test p_value_sig distribution
#>       <dbl>     <int>    <dbl> <chr>  <lgl>             <chr>       <chr>       
#> 1    16.4           8 3.72e- 2 Chi-s… FALSE             *           poisson     
#> 2     0.615        NA 1.95e-10 Shapi… FALSE             ***         normal      
#> 3     0.258        NA 2.32e- 9 Lilli… FALSE             ***         normal      
#> # ℹ 1 more variable: is_int <lgl>
```

``` r

# Visual inspection with density overlay and Q-Q plots
distribution_plot(biomarker, feature_name = "Serum Biomarker (ng/mL)")
```

### Automated correlation matrices with `corr_test_all()`

Screen for multi-collinearity across high-dimensional clinical features:

``` r

# Compute pairwise correlations with p-values and confidence intervals
corrs <- corr_test_all(df_clinical |> select(age, systolic_bp, bmi))

# Visualize with correlation heatmap
correlation_plot(corrs)
```

### Controlled imputation benchmarking with `add_missing()`

When developing and validating statistical pipelines,
[`add_missing()`](https://jkylearmstrong.github.io/TempleCBE/reference/add_missing.md)
injects known MCAR missingness into complete datasets:

``` r

set.seed(42)
complete_data <- iris[, 1:4]

# Mask 20% of values across selected features
amputed <- add_missing(complete_data, cols = c(Sepal.Length, Petal.Length), pct_na = 0.20)

# Inspect resulting missingness
features_percent_miss(amputed)
#> # A tibble: 4 × 5
#>   feature      SumNa SumComp PctNa PctComp
#>   <chr>        <int>   <int> <dbl>   <dbl>
#> 1 Sepal.Length    30     120   0.2     0.8
#> 2 Petal.Length    30     120   0.2     0.8
#> 3 Sepal.Width      0     150   0       1  
#> 4 Petal.Width      0     150   0       1

# The 'missing_cells' attribute tracks exact amputed coordinates for scoring
head(attr(amputed, "missing_cells"))
#> # A tibble: 6 × 2
#>     row feature     
#>   <int> <chr>       
#> 1     3 Sepal.Length
#> 2     5 Sepal.Length
#> 3    20 Sepal.Length
#> 4    24 Sepal.Length
#> 5    27 Sepal.Length
#> 6    34 Sepal.Length
```

## 5. Tuned Random Forest Imputation (`missForest` & `missRanger`)

Once missingness patterns have been audited, biostatisticians must
decide how to handle missing values. Complete-case analysis discards
valuable clinical data and introduces bias. Random forest imputation is
widely considered the state of the art for mixed clinical data because
it accommodates non-linear relationships and complex interactions
without requiring parametric distribution assumptions.

### The `mtry` dilemma in clinical data

Random forest imputation iterates through each variable with missing
values, fitting a forest predicting that variable from all other
variables. The single most impactful hyperparameter is **`mtry`** — the
number of candidate variables sampled at each split: - If `mtry` is too
low, strong predictive biomarkers may be missed at key splits. - If
`mtry` is too high, trees become correlated and overfit noisy clinical
features.

Critically, **different clinical variables perform best at different
`mtry` values**. A continuous biomarker (e.g. serum creatinine) may
achieve minimal error at $`mtry = 2`$, whereas a multi-level categorical
diagnosis (e.g. ICD-10 disease subtype) may require $`mtry = 5`$.
Standard implementations force the analyst to choose a single global
`mtry` for the entire dataset.

### The `TempleCBE` solution: Independent per-column best `mtry` assembly

`TempleCBE` provides two complementary swept imputation engines: -
**[`missforest_sweep_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missforest_sweep_mtry.md)**:
Based on the gold-standard \[missForest::missForest()\] algorithm. -
**[`missranger_sweep_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missranger_sweep_mtry.md)**:
High-performance alternative powered by \[missRanger::missRanger()\] and
the C++ `ranger` library, with support for Predictive Mean Matching
(PMM).

Both functions follow a unified, mathematically principled workflow: 1.
**Sweep**: Fit the forest across a grid of candidate `mtry` values
(e.g. `1:(p - 1)`). 2. **Score**: Track variable-wise out-of-bag (OOB)
error for every column at every `mtry`. 3. **Assemble
(`assemble_by_best_mtry`)**: Pull each column independently from the
specific run where that column achieved its minimal OOB error! 4.
**Guardrails**: Exclude identifiers (`exclude`), enforce sparsity limits
(`max_pct_missing`), and refuse all-`NA` columns
(`check_no_all_na_columns`).

### High-accuracy imputation with `missforest_sweep_mtry()`

[`missforest_sweep_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missforest_sweep_mtry.md)
reports raw Mean Squared Error (MSE) for numeric columns and Proportion
of Falsely Classified (PFC) for categorical factors:

``` r

# Synthetic clinical data with missing values
clinical_data <- data.frame(
  patient_id = paste0("PT_", 1:12),
  age        = c(45, 52, NA, 61, 38, 59, 63, NA, 49, 55, 71, 42),
  systolic_bp= c(120, 135, NA, 140, 118, 125, 138, 144, NA, 130, 150, 122),
  bmi        = c(22.5, NA, 29.1, NA, 31.0, 26.4, 28.2, 33.1, 24.8, NA, 27.5, 23.9),
  smoker     = factor(c("No", "Yes", "No", "No", "Yes", "Yes", "No", "Yes", "No", "No", "Yes", "No")),
  stage      = factor(c("I", "II", "II", "III", "I", "II", "IV", "III", "I", "II", "III", "I"))
)

# Impute with mtry sweep (1 to 4), excluding patient_id
res_forest <- missforest_sweep_mtry(
  data            = clinical_data,
  exclude         = "patient_id",
  mtry_values     = 1:3,
  ntree           = 100,
  max_pct_missing = 0.50,
  parallel        = FALSE
)

# 1. Inspect the completed dataset (patient_id reattached in original order)
head(res_forest$imp_data)

# 2. Inspect the winning mtry per column
res_forest$best

# 3. View the full OOB error grid across swept mtry values
res_forest$oob_error
```

### Fast, large-scale imputation with `missranger_sweep_mtry()`

For large clinical cohorts (e.g. $`> 10,000`$ patients or $`> 100`$
variables), `missForest` can be computationally intensive.
[`missranger_sweep_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missranger_sweep_mtry.md)
leverages the C++ `ranger` implementation for high speed:

- **Scaled OOB error**: Reports $`1 - R^2`$ for numeric columns (where
  values near 0 indicate near-perfect recovery and values near 1
  indicate performance no better than predicting the mean).
- **Predictive Mean Matching (`pmm.k`)**: Setting `pmm.k > 0`
  (e.g. `pmm.k = 3`) ensures that imputed values are drawn from actual
  observed patient donor values rather than synthetic regression
  averages — critical for bounded clinical scores (e.g. integer counts,
  survey scores).
- **Parallel evaluation**: Uses `furrr` to evaluate candidate `mtry`
  values across CPU cores in parallel without thread oversubscription
  (`num.threads = 1` inside `ranger`).

``` r

library(furrr)
plan(multisession, workers = 2)

# Fast sweep with predictive mean matching
res_ranger <- missranger_sweep_mtry(
  data        = clinical_data,
  exclude     = "patient_id",
  mtry_values = 1:3,
  num.trees   = 200,
  pmm.k       = 3,
  seed        = 2026,
  parallel    = TRUE
)

# Inspect winning mtry values from ranger
res_ranger$best
```

### Safety and data governance guardrails

Both sweep functions incorporate essential clinical data safeguards: 1.
**Identifier and timestamp protection (`exclude`)**: Clinical metadata
(patient IDs, specimen barcodes, visit dates) are quarantined from the
forest training matrix and re-attached unaltered to the final output. 2.
**Preventing data fabrication (`max_pct_missing`)**: Features that are
$`>50\%`$ missing (or any custom threshold) are automatically held out
from imputation, carried through unimputed, and listed in
`excluded_high_missing`. 3. **Refusing all-`NA` columns
(`check_no_all_na_columns`)**: Standard `missForest` silently drops
all-`NA` columns, while `missRanger` silently returns them still `NA`.
`TempleCBE` explicitly halts and warns the user so schema integrity is
never compromised.

------------------------------------------------------------------------

## How others can use this technology (Adoption Patterns)

Here are three common patterns for applying `TempleCBE`’s EDA tools to
real-world studies:

### Pattern 1: Clinical trial data intake audit

Create a standardized data quality intake report for newly exported
registry data:

``` r

audit_clinical_intake <- function(raw_df) {
  message("Auditing Clinical Intake...")
  
  # 1. Quantify missingness including institutional sentinel values
  miss_summary <- features_percent_miss(
    raw_df, 
    na_list = c("999", "-99", "Unknown", "Refused", "")
  )
  
  # 2. Flag variables with severe missingness (> 30%)
  flagged_vars <- miss_summary |>
    filter(PctNa > 0.30)
  
  # 3. Screen continuous lab values for extreme clinical outliers
  num_cols <- raw_df |> select(where(is.numeric))
  outliers <- lapply(num_cols, detect_outliers)
  
  list(
    missing_summary = miss_summary,
    severe_missing  = flagged_vars,
    outliers        = outliers
  )
}
```

### Pattern 2: Regulatory missingness tables for Study Protocols & CSRs

Clinical Study Reports (CSRs) submitted to regulatory agencies (FDA,
EMA) require clear tables of missing baseline covariates:

``` r

# Export publication-ready missingness tables
csr_missing_table <- features_percent_miss(study_data) |>
  rename(
    `Clinical Variable` = Feature,
    `Missing Count (n)` = N_Missing,
    `Missing Rate (%)`  = PctNa
  ) |>
  mutate(`Missing Rate (%)` = sprintf("%.1f%%", `Missing Rate (%)` * 100))

# Print or write to Excel/RTF
writexl::write_xlsx(csr_missing_table, "tables/CSR_Table_14_1_Missingness.xlsx")
```

### Pattern 3: Outlier screening preserving subject record keys

Flag outliers while maintaining patient identifiers for case review with
clinical investigators:

``` r

# Screen lab values and join back to patient IDs
iqr_audit <- detect_outliers(study_data$troponin_t)

flagged_patients <- study_data |>
  slice(c(iqr_audit$mild_indices, iqr_audit$extreme_indices)) |>
  select(patient_id, site_id, troponin_t) |>
  mutate(
    flag = ifelse(patient_id %in% study_data$patient_id[iqr_audit$extreme_indices],
                  "EXTREME_OUTLIER", "MILD_OUTLIER")
  )
```

------------------------------------------------------------------------

## Summary

`TempleCBE` delivers a clean, auditable, and non-destructive exploratory
workflow engineered specifically for the realities of clinical data. By
combining
[`SumNa()`](https://jkylearmstrong.github.io/TempleCBE/reference/SumNa.md),
[`features_percent_miss()`](https://jkylearmstrong.github.io/TempleCBE/reference/features_percent_miss.md),
[`missmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/missmap.md),
[`detect_outliers()`](https://jkylearmstrong.github.io/TempleCBE/reference/detect_outliers.md),
and
[`add_missing()`](https://jkylearmstrong.github.io/TempleCBE/reference/add_missing.md)
with downstream tuned imputation
([`missforest_sweep_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missforest_sweep_mtry.md)
and
[`missranger_sweep_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missranger_sweep_mtry.md)),
biostatisticians can uphold rigorous data governance standards from
initial data intake through regulatory delivery.
