# TempleCBE Standard Functions & Templates

This directory contains standardized, study-agnostic functions, models, and Quarto templates drafted from the Wolfson analysis pipelines for inclusion into the **`TempleCBE`** package (`c:/Users/jkyle/Documents/GitHub/TempleCBE`).

---

## Complete Catalog of Drafted Components

### 1. Visualization & Styling (`theme_cbe.R`, `plot_cox.R`, `plot_deck.R`)
- **`theme_cbe.R`**: Institutional branding, palettes (`temple_cherry` `#9D2235`, `neutral_grey` `#6F6F6F`), ggplot2 themes (`theme_cbe`, `theme_cbe_deck`), and formatters (`fmt_pct`, `fmt_num`, `fmt_sig`, `fmt_p`, `fmt_hr`, `words`).
- **`plot_cox.R`**:
  - `plot_cox_forest()`: Hazard ratio forest plot with 95% CIs and reference line at 1.0.
  - `plot_cox_survival()`: Model-predicted survival curves across factor strata or continuous quantiles (`ntile4`).
  - `plot_cox_marginal()`: Continuous predictor vs. predicted probability of death curve with 95% confidence bands against binary event observations.
- **`plot_deck.R`**:
  - `plot_survival_km()`: Standardized Kaplan-Meier curves using `ggsurvfit` with Temple Cherry styling, censor marks, and percentage axes.
  - `plot_dynamic_trajectory()`: Longitudinal biomarker trajectory (mean ± SE) over protocol time by cohort.
  - `plot_group_comparison()`: Grouped bar chart with error bars comparing index means across cohorts.
  - `plot_missingness()`: Data completeness audit bar charts.
  - `table_two_by_two()`: Formatted 2x2 contingency tables with row percentages, marginal totals, and Fisher's exact test.

### 2. Statistical Analysis Engines (`cbe_cox_single.R`, `template_cox_single.qmd`)
- **`cbe_cox_single.R`**:
  - Univariable Cox PH screening engine handling numeric, factor, and `haven_labelled` vectors.
  - Fits `coxph` and tests proportional hazards via `cox.zph`.
  - Builds tidy coefficient tables with explicit **Reference** rows for categorical variables.
  - Generates automated plain-language clinical interpretations with risk percentage change and significance statement.
  - Returns a structured `cbe_cox` S3 object with a formatted `print()` method.
- **`template_cox_single.qmd`**: Parameterized Quarto child template that replaces the 418 lines of `cox_single_NEW.qmd` with clean calls to `cbe_cox_single()`.

### 3. Data Integrity & Snapshot Verification (`data_manifest.R`)
*Origin: `analysis/2026_09/R/data_copies.R`*
- **`read_data_manifest(dir, manifest_file)`**: Reads CSV manifest mapping copy files to source paths.
- **`copy_data_manifest(dir, manifest, project_root)`**: Freezes data copies and immediately validates integrity.
- **`validate_data_manifest(dir, manifest, project_root)`**: Compares MD5 checksums and `identical(readRDS(src), readRDS(dst))` to guarantee reports and decks never run on stale data.
- **`stop_if_invalid_manifest(validation, dir)`**: Halts execution with an actionable message if any copy differs from its upstream source.

### 4. Data Dictionary & Schema Mapping Engine (`plug_and_play_schema.R`, `simulate_cohort.R`)
*Origin: `R/plug_and_play_*.R`*
- **`validate_column_mapping(mapping)`**: Validates standardized column dictionaries (`INDEX`, `old`, `new`, `X_var`, `Y_var`, `ID_var`, `Time_var`, `duplicate_of`, `duplicate_action`).
- **`read_mapped_section_data(mapping, index, file, ...)`**: Schema-enforced table reader that maps headers, resolves duplicate columns (`drop`/`prefer`), and reports unmapped or missing columns.
- **`summarize_section_by_time(df, mapping, index, ...)`**: Generates longitudinal summary tables using `arsenal::tableby()` grouped by the section's flagged `Time_var`.
- **`demo_cbe_mapping()`**: Standard demonstration mapping dictionary for documentation and testing.
- **`simulate_section_data(mapping, index, n_subjects, seed, missing_rate, ...)`**: Generates synthetic patient/subject cohorts conforming to schema roles with configurable missingness rates for validation and testing without touching real data.

### 5. Deliverable Packaging & Code Auditing (`bundle_deliverables.R`)
*Origin: `R/CreateReportZip.R`, `R/report_scan_deliverables.R`*
- **`package_deliverables(pipeline_objects, output_formats, data_deliverables, zip_path)`**:
  - Gathers rendered reports (PDF, DOCX, HTML) and client data tables (`.xlsx`, `.rds`) from pipeline objects, organizes them into stage-prefixed folders, and builds a clean distribution ZIP archive.
- **`audit_report_deliverables(analysis_path)`**:
  - Scans scripts for read/write statements and output deliverable tokens to verify that every expected deliverable exists on disk.

---

## How to Test Locally in Wolfson

```r
# Load visualization and Cox modeling functions
source(here::here("R", "to_TempleCBE", "theme_cbe.R"))
source(here::here("R", "to_TempleCBE", "cbe_cox_single.R"))
source(here::here("R", "to_TempleCBE", "plot_cox.R"))
source(here::here("R", "to_TempleCBE", "plot_deck.R"))

# Load data integrity & plug-and-play functions
source(here::here("R", "to_TempleCBE", "data_manifest.R"))
source(here::here("R", "to_TempleCBE", "plug_and_play_schema.R"))
source(here::here("R", "to_TempleCBE", "simulate_cohort.R"))
source(here::here("R", "to_TempleCBE", "bundle_deliverables.R"))
```

---

## Transferring to TempleCBE

To transfer into the `TempleCBE` package repository:
1. Copy all `.R` files from `Wolfson/R/to_TempleCBE/` directly to `TempleCBE/R/`.
2. Copy `template_cox_single.qmd` to `TempleCBE/inst/templates/`.
3. In `TempleCBE`, run:
   ```r
   devtools::document()
   devtools::test()
   devtools::check()
   ```
