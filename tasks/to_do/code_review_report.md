# Comprehensive Independent Code Review: TempleCBE R Package

**Date**: September 17, 2026  
**Package**: `TempleCBE` (v0.3.404.2026.09.17.22.39)  
**Target R Runtime**: R 4.6.1 (`x86_64-w64-mingw32`)  
**Repository Branch**: `gtsummary-_integration`  
**Review Status**: Complete — Read-only Audit (Zero codebase modifications made)

---

## Executive Summary

An independent, exhaustive code review of the `TempleCBE` repository was conducted spanning all R source files ([`R/`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R)), package metadata ([`DESCRIPTION`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/DESCRIPTION), [`NAMESPACE`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/NAMESPACE), [`.Rbuildignore`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/.Rbuildignore)), test suites ([`tests/testthat/`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/tests/testthat)), documentation ([`man/`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/man)), vignettes ([`vignettes/`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/vignettes)), report skeletons ([`inst/`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/inst)), and the dependency lockfile ([`renv.lock`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/renv.lock)).

### Key Audit Metrics
- **Unit Test Suite**: 1,952 test assertions pass (`FAIL 0 | WARN 13 | SKIP 4 | PASS 1952`).
- **Test Warnings**: 13 warnings surfaced during test execution (12 caused by duplicate chunk labels across Quarto/Rmd templates; 1 caused by dynamic chunk evaluation during purling).
- **CRAN Check / Build Status**: `R CMD check` fails on Windows due to unignored deeply-nested vignette assets triggering GNU `@LongLink` header corruption in base R's tar extractor.
- **Findings Summary**:
  - **Category A (Critical / Runtime Blockers)**: 7 defects that crash code or cause silent failures in production/testing.
  - **Category B (Statistical & Modeling Concerns)**: 6 algorithmic issues that distort model estimates, collapse plot axes, or mutate global environment states.
  - **Category C (Package Architecture, Metadata & Build Hygiene)**: 6 issues regarding licensing, rogue files, out-of-sync lockfiles, and codetools global bindings.

---

## Category A: Critical Runtime Defects & Functional Errors

### A1. Nonexistent `recipes::vars()` in Step Constructor Defaults
* **Severity**: Critical / Blocker
* **Locations**: 
  - [`R/step_lencode_survival.R:46`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/step_lencode_survival.R#L46)
  - [`R/step_lencode_survival.R:255`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/step_lencode_survival.R#L255)
* **Code**:
  ```r
  # In step_lencode_coxnet:
  outcome = recipes::vars(time, status),
  
  # In step_lencode_joint_model:
  outcome = recipes::vars(time, status),
  ```
* **Analysis**: `recipes` does **not** export a function named `vars`. In tidyverse, `vars()` is exported by `dplyr` (and superseded in favor of `tidyselect` expressions). 
* **Impact**: Calling `step_lencode_coxnet(recipe, ...)` or `step_lencode_joint_model(recipe, ...)` with default parameters immediately aborts with:
  ```text
  Error: 'vars' is not an exported object from 'namespace:recipes'
  ```
* **Remediation**: Change `outcome = recipes::vars(time, status)` to `outcome = c("time", "status")` or use standard quosure selection via `rlang::enquos(outcome)`.

---

### A2. Broken S3 Method Registration for Custom Recipe Steps
* **Severity**: Critical / Blocker
* **Locations**:
  - [`R/step_lencode_survival.R:91`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/step_lencode_survival.R#L91) (`prep.step_lencode_coxnet`)
  - [`R/step_lencode_survival.R:168`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/step_lencode_survival.R#L168) (`bake.step_lencode_coxnet`)
  - [`R/step_lencode_survival.R:300`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/step_lencode_survival.R#L300) (`prep.step_lencode_joint_model`)
  - [`R/step_lencode_survival.R:392`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/step_lencode_survival.R#L392) (`bake.step_lencode_joint_model`)
  - Resulting in [`NAMESPACE:73-74, 216-217`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/NAMESPACE#L73-L74)
* **Analysis**: In `step_lencode_survival.R`, these methods are decorated with plain `#' @export` instead of `#' @exportS3Method recipes::prep` and `#' @exportS3Method recipes::bake` (compare with the correct implementation in [`R/step_famd.R:95, 147`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/step_famd.R#L95)).
* **Impact**: `roxygen2` exports `prep.step_lencode_*` and `bake.step_lencode_*` as regular functions in `NAMESPACE` rather than registering them in the S3 method table for `recipes::prep` and `recipes::bake`. Calling `recipes::prep()` on a recipe containing either step fails:
  ```text
  Error in UseMethod("prep") : 
    no applicable method for 'prep' applied to an object of class "c('step_lencode_coxnet', 'step')"
  ```
  *(Note: This critical failure was undetected by the test suite because `step_lencode_survival.R` has zero unit tests).*
* **Remediation**: Update roxygen tags to `#' @exportS3Method recipes::prep` and `#' @exportS3Method recipes::bake`, then regenerate `NAMESPACE`. Add a unit test file `test-step_lencode_survival.R`.

---

### A3. Unimported `%>%` Pipe Operator in Exported Functions
* **Severity**: High
* **Locations**:
  - [`R/tidy_tmerge_cox.R:50, 51, 57, 58, 64, 69, 70, 71, 77, 81`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/tidy_tmerge_cox.R#L50-L81)
  - [`R/my_summary_table.r:30-59`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/my_summary_table.r#L30-L59)
* **Analysis**: `tidy_tmerge_cox.R` chains 10 operations using `%>%` without importing `%>%` from `dplyr` or `magrittr`. While commit `45c813b` fixed this in `cbe_compare_df.R`, it missed `tidy_tmerge_cox.R`. Currently, `%>%` is only in the namespace because `my_summary_table.r` has an unrecommended whole-namespace `#' @import dplyr`. If `my_summary_table.r` is refactored to standard `importFrom`, `tidy_tmerge_cox()` crashes in any clean R session where `dplyr` is not explicitly attached:
  ```text
  Error in tidy_tmerge_cox(...) : could not find function "%>%"
  ```
* **Remediation**: Since `TempleCBE` requires `R (>= 4.2.0)` ([`DESCRIPTION:17`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/DESCRIPTION#L17)), replace all instances of `%>%` with base R's native pipe `|>` across both files, and replace `#' @import dplyr` with explicit `@importFrom` directives.

---

### A4. `knitr::purl()` Runtime Evaluation Crash on `has_sas`
* **Severity**: High
* **Locations**:
  - [`vignettes/sas_survival.Rmd:715, 739`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/vignettes/sas_survival.Rmd#L715)
  - [`vignettes/R_to_SAS.Rmd:776`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/vignettes/R_to_SAS.Rmd#L776)
* **Code**:
  ```markdown
  ```{r, eval = has_sas}
  ```
* **Analysis**: When `knitr::purl()` extracts R code from `.Rmd` files (such as in [`tests/testthat/test-purl.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/tests/testthat/test-purl.R)), it parses chunk options without executing chunk bodies. In an isolated R session, `has_sas` does not exist in the evaluation environment.
* **Impact**: `test-purl.R` throws runtime errors:
  ```text
  Error in eval(x, envir = envir) : object 'has_sas' not found
  ```
* **Remediation**: Replace `eval = has_sas` with safe inline checks:
  ```markdown
  ```{r, eval = isTRUE(tryCatch(has_sas, error = function(e) FALSE))}
  ```
  Or check SAS availability dynamically: `eval = nzchar(Sys.which("sas"))`.

---

### A5. Windows Tarball Mangling & `R CMD check` Failure (`ExtendedName`)
* **Severity**: High / Release Blocker
* **Locations**:
  - [`.Rbuildignore`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/.Rbuildignore)
  - `vignettes/*_files/`
  - `.quarto/`
* **Analysis**: When building the source package tarball on Windows, rendered HTML/JS asset paths inside `vignettes/sas_survival_files/`, `vignettes/R_to_SAS_files/`, and `vignettes/.quarto/` exceed standard POSIX tar 100-character header limits. Base R's `utils::tar` switches to GNU `@LongLink` / `ExtendedName` format. When `R CMD check` attempts to extract the archive on Windows, base R's reader fails:
  ```text
  Error in read.dcf(con) : Line starting 'ExtendedName ...' is malformed!
  ```
* **Impact**: Blocks CRAN-style package validation and release tarball generation on Windows.
* **Remediation**: Add the following rules to [`.Rbuildignore`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/.Rbuildignore):
  ```gitignore
  ^vignettes/.*_files$
  ^vignettes/.*\.html$
  ^\.quarto$
  ^vignettes/\.quarto$
  ```

---

### A6. Prefix Matching Collision in Multi-Variable Cox Formatter
* **Severity**: High / Data Distortion
* **Location**: [`R/cbe_cox_multi.R:82`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/cbe_cox_multi.R#L82)
* **Code**:
  ```r
  term_rows <- td[startsWith(td$term, feat), , drop = FALSE]
  ```
* **Analysis**: In multi-variable Cox models, terms for factor covariates are generated by concatenating variable name and level (e.g., `stageII`, `stageIII`). Using `startsWith(td$term, feat)` causes prefix collisions whenever feature names share a common prefix (e.g., `age` and `age_group`, or `stage` and `stage_t`).
* **Impact**: If both `age` and `age_group` are predictors, `term_rows` for `age` captures both `age` and `age_groupHigh`. The `age_group` terms are formatted under `age` using numeric assumptions and then extracted again under `age_group`, resulting in duplicate, corrupted output rows in clinical summary tables.
* **Remediation**: Match terms by exact match or exact prefix boundary:
  ```r
  term_rows <- td[td$term == feat | startsWith(td$term, paste0(feat, ":")) | 
                  (is.factor(data[[feat]]) && startsWith(td$term, feat) && 
                   substring(td$term, nchar(feat) + 1) %in% levels(data[[feat]])), , drop = FALSE]
  ```

---

### A7. Erroneous `data(lung, package = "survival")` Examples
* **Severity**: Medium / Documentation Failure
* **Locations**:
  - [`R/explain_survival.R:31`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/explain_survival.R#L31)
  - [`R/step_lencode_survival.R:33`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/step_lencode_survival.R#L33)
  - Generated `.Rd` files in [`man/`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/man)
* **Code**:
  ```r
  #' data(lung, package = "survival")
  ```
* **Analysis**: In the `survival` package, the dataset file is named `cancer` (with `lung` provided as an alias/subset within `cancer.rda`). Calling `data(lung, package = "survival")` fails with `Warning: data set 'lung' not found`, followed by `Error: object 'lung' not found`.
* **Impact**: Running `devtools::run_examples()` or `R CMD check` fails on documentation examples.
* **Remediation**: Use `lung <- survival::lung` directly instead of `data()`.

---

## Category B: Statistical, Modeling & Algorithmic Concerns

### B1. Pseudo-Joint Model & Inverted Hazard Scale in `step_lencode_joint_model()`
* **Severity**: Medium / Algorithmic Efficiency & Soundness
* **Location**: [`R/step_lencode_survival.R:342-375`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/step_lencode_survival.R#L342-L375)
* **Analysis**:
  1. `step_lencode_joint_model()` fits a full 3-model `joint_model()` (logistic regression for status, linear regression for duration, and Cox PH for survival). However, it sets `calibration = FALSE` and only extracts `.pred_risk_score` from the Cox submodel. The binary classification and duration submodels are completely discarded after expensive fitting.
  2. In line 369, it centers risk scores by subtraction:
     ```r
     enc_map[[col]] <- risk_scores - risk_scores[1]
     ```
     Because `risk_scores` from `joint_model` are relative hazard ratios ($HR = \exp(\mathbf{x}\beta)$), subtracting $HR_1$ produces $HR_i - HR_1$ rather than a log hazard ratio ($\log(HR_i / HR_1) = \mathbf{x}_i\beta - \mathbf{x}_1\beta$). This transforms a multiplicative hazard scale into an uninterpretable shifted linear scale.
* **Remediation**: If only the Cox component is needed for target encoding, fit `coxnet()` directly rather than overhead-heavy `joint_model()`. Encode levels on the log-hazard scale ($\log(HR)$).

---

### B2. Factor Level Collapsing in Multi-level Forest Plots
* **Severity**: Medium / Visualization Distortion
* **Location**: [`R/plot_cox.R:57-61`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/plot_cox.R#L57-L61)
* **Code**:
  ```r
  if (is.factor(data[[data$var_name]])) {
    td$index_label <- data$var_label
  }
  ```
* **Analysis**: For categorical covariates with 3 or more levels (e.g., `Stage I`, `Stage II`, `Stage III`), `cbe_cox_single()` produces multiple contrast terms (`Stage II vs Stage I`, `Stage III vs Stage I`). Line 58 overwrites `td$index_label` for *all* rows with the single parent `data$var_label` ("Stage").
* **Impact**: In `plot_cox_forest()`, all comparison levels share the exact same y-axis label coordinate and are plotted directly on top of each other, concealing estimates and confidence intervals.
* **Remediation**: Retain specific contrast labels (e.g., `paste0(data$var_label, ": ", td$term)` or `td$comparison`) for multi-level factors.

---

### B3. Single-Stratum / Intercept-Only Dimension Drop in `cbe_km_single()`
* **Severity**: Medium / Runtime Output Fallback
* **Location**: [`R/cbe_km_single.R:64-67`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/cbe_km_single.R#L64-L67)
* **Code**:
  ```r
  km_tab <- as.matrix(summary(km_fit)$table)
  km_median <- if ("median" %in% colnames(km_tab)) unname(km_tab[1, "median"]) else NA_real_
  ```
* **Analysis**: When a Kaplan-Meier model has no strata (`Surv(time, status) ~ 1`) or a single stratum, `summary(km_fit)$table` is a 1D named numeric vector. In R, calling `as.matrix()` on a 1D named vector converts it into a single-column matrix where the statistics (`records`, `events`, `median`) are stored in `rownames`, while `colnames` is `1`.
* **Impact**: `"median" %in% colnames(km_tab)` evaluates to `FALSE`, causing `km_median` to silently fall back to `NA_real_`.
* **Remediation**: Use `t(as.matrix(summary(km_fit)$table))` or test dimensionality before subsetting:
  ```r
  km_tab <- summary(km_fit)$table
  km_median <- if ("median" %in% names(km_tab)) {
    unname(km_tab["median"])
  } else if ("median" %in% colnames(km_tab)) {
    unname(km_tab[1, "median"])
  } else {
    NA_real_
  }
  ```

---

### B4. Zero-Row Data Frame Mutation Bug in `read_mapped_section_data()`
* **Severity**: Medium / Data Integrity
* **Location**: [`R/plug_and_play_schema.R:205`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/plug_and_play_schema.R#L205)
* **Code**:
  ```r
  out[[m_new]] <- NA
  ```
* **Analysis**: In base R, assigning `df[["col"]] <- NA` to a 0-row data frame (`nrow(out) == 0`) silently mutates it into a 1-row data frame filled with `NA`.
* **Impact**: If an input table is legitimately empty (e.g., an empty section slice), assigning missing target columns turns it into a corrupted 1-row dataset.
* **Remediation**: Explicitly check row count before scalar assignment:
  ```r
  if (nrow(out) > 0) {
    out[[m_new]] <- NA
  } else {
    out[[m_new]] <- logical(0) # or correct typed vector of length 0
  }
  ```

---

### B5. Global RNG State Mutation in `simulate_section_data()`
* **Severity**: Medium / Reproducibility & Side Effects
* **Location**: [`R/simulate_cohort.R:44`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/simulate_cohort.R#L44)
* **Code**:
  ```r
  if (!is.null(seed)) set.seed(seed)
  ```
* **Analysis**: Directly invoking `set.seed()` in a package function without saving and restoring `.Random.seed` permanently alters the calling user's global session RNG sequence.
* **Impact**: Violates CRAN policies on global session state mutation; breaks upstream user simulation workflows.
* **Remediation**: Use `withr::with_seed(seed, ...)` or a standard `on.exit()` restoration:
  ```r
  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (is.null(old_seed)) rm(".Random.seed", envir = .GlobalEnv)
      else assign(".Random.seed", old_seed, envir = .GlobalEnv)
    })
    set.seed(seed)
  }
  ```

---

### B6. Hardcoded "95% CI" Labels Ignoring Configured `conf_level`
* **Severity**: Low / UI & Reporting Inconsistency
* **Locations**:
  - [`R/cbe_cox_single.R:88, 140`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/cbe_cox_single.R#L88)
  - [`R/cbe_cox_multi.R:117, 143`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/cbe_cox_multi.R#L117)
  - [`R/cbe_cox_table.R:44`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/cbe_cox_table.R#L44)
* **Analysis**: While `cbe_cox_single()` and `cbe_cox_multi()` accept a `conf_level` parameter (e.g. `0.90` or `0.99`), the resulting tibbles hardcode the column name as `"95% CI"` and the summary string as `"95% CI [..., ...]"`. Furthermore, `cbe_cox_table.R` performs hardcoded column selection `tab[, c(..., "95% CI", ...)]`.
* **Impact**: If a user requests a 90% or 99% confidence interval, the output table displays 90% or 99% numbers under a false `"95% CI"` label.
* **Remediation**: Dynamically format the confidence level:
  ```r
  ci_label <- sprintf("%d%% CI", round(conf_level * 100))
  ```

---

## Category C: Package Architecture, Metadata & Build Hygiene

### C1. Rogue 387 KB Scratchpad File in Repository Root
* **Severity**: High / Repo Hygiene
* **File**: `CUsersjkyleAppDataLocalTempclaudeC--Users-jkyle-Documents-GitHub-TempleCBE95323ce0-460c-483f-aa40-eed2c90467d8scratchpaddiff.txt`
* **Analysis**: A 387 KB leftover Claude diff scratchpad file exists in the repository root directory. It is neither ignored by `.gitignore` nor [`.Rbuildignore`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/.Rbuildignore).
* **Impact**: Unintentionally tracked in git status and included in package source builds.
* **Remediation**: Delete the file and add `scratchpad.*` and `claude*` to `.gitignore` and `.Rbuildignore`.

---

### C2. Incomplete `.Rbuildignore` Rules
* **Severity**: High / Build System
* **Location**: [`.Rbuildignore`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/.Rbuildignore)
* **Analysis**: Build artifacts generated by Quarto and R Markdown are not ignored.
* **Impact**: Leads to bloated tarballs and the Windows build failure detailed in **A5**.
* **Remediation**: Append the following patterns to `.Rbuildignore`:
  ```gitignore
  ^vignettes/.*_files$
  ^vignettes/.*\.html$
  ^\.quarto$
  ^vignettes/\.quarto$
  ^.*\.docx$
  ^.*\.rtf$
  ^scripts/.*\.ps1$
  ```

---

### C3. Duplicate Chunk Labels in Quarto & R Markdown Skeletons
* **Severity**: Medium / Knitr Diagnostics
* **Locations**:
  - [`inst/templates/eda_tables.qmd:81-82, 104-105, 122-123, 188-189`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/inst/templates/eda_tables.qmd#L81-L82)
  - [`inst/rmarkdown/templates/eda-tables/skeleton/skeleton.Rmd:81-82, 104-105, 122-123, 188-189`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/inst/rmarkdown/templates/eda-tables/skeleton/skeleton.Rmd#L81-L82)
  - [`inst/rmarkdown/templates/eda-tables/skeleton/skeleton.qmd:81-82, 104-105, 122-123, 188-189`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/inst/rmarkdown/templates/eda-tables/skeleton/skeleton.qmd#L81-L82)
* **Code**:
  ```markdown
  ```{r tbl-data-intro}
  #| label: tbl-data-intro
  ```
* **Analysis**: Specifying the chunk label in both the `{r label}` header and the YAML `#| label:` comment triggers 4 knitr duplicate label warnings per document. Across the 3 template documents, this produces 12 of the 13 warnings observed during test runs.
* **Impact**: Pollutes rendering logs and `devtools::test()` outputs.
* **Remediation**: Standardize on Quarto pipe syntax (````{r}` with `#| label: ...`) and remove the redundant label from the backtick header.

---

### C4. License Metadata Inconsistency
* **Severity**: Medium / CRAN & Governance Compliance
* **Locations**:
  - [`DESCRIPTION:12`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/DESCRIPTION#L12): `License: MIT + file LICENSE`
  - [`LICENSE.md`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/LICENSE.md): MIT License text
  - [`README.qmd:13`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/README.qmd#L13): States dual licensing (`GPL-3 | MIT`)
  - [`NOTES.md:16`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/NOTES.md#L16): Explicitly states: *"License is dual GPL-3 | MIT, matching pslongSim, omop-duck-db, and ML-PScore — a deliberate ecosystem-wide choice"*
* **Analysis**: If the deliberate architectural intent is ecosystem-wide dual licensing (`GPL-3 | MIT`), `DESCRIPTION` is legally out of sync.
* **Remediation**: Align `DESCRIPTION` with the stated ecosystem policy (`License: GPL-3 | MIT + file LICENSE`) or update `NOTES.md` and `README.qmd` if MIT is the sole intended license.

---

### C5. Dependency Lockfile (`renv.lock`) Drift
* **Severity**: Medium / Environment Reproducibility
* **Location**: [`renv.lock`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/renv.lock)
* **Analysis**: `renv::status()` indicates that 51 packages used in package execution and tests (e.g., `exact2x2`, `ggsurvfit`, `gtsummary`, `gt`, `testthat`, `knitr`) are installed and functional in the local development library but missing from `renv.lock`.
* **Impact**: Other developers or CI/CD pipelines running `renv::restore()` will fail to reproduce the working environment.
* **Remediation**: Run `renv::snapshot()` after verifying package stability to synchronize the lockfile.

---

### C6. Global Variable Bindings in Tidyverse Pipelines
* **Severity**: Low / R CMD Check Diagnostics
* **Locations**: Multiple files across [`R/`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R), including:
  - [`R/cbe_docx_review_extract.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/cbe_docx_review_extract.R)
  - [`R/cbe_four_quadrant_report.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/cbe_four_quadrant_report.R)
  - [`R/cbe_mosaic_plot.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/cbe_mosaic_plot.R)
* **Analysis**: Using unquoted column names in `ggplot2::aes()` or `dplyr` operations without the `.data$` pronoun generates `no visible binding for global variable` notes during `R CMD check`.
* **Remediation**: Use `.data$column` or add `utils::globalVariables(c(...))` in an internal `R/globals.R` file.

---

## Actionable Priority Matrix

| Priority | ID | Issue Summary | Primary File(s) |
| :--- | :--- | :--- | :--- |
| **P0 - Blocker** | **A1** | Nonexistent `recipes::vars` call in step constructors | [`R/step_lencode_survival.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/step_lencode_survival.R) |
| **P0 - Blocker** | **A2** | Broken S3 method registration for `recipes::prep` & `bake` | [`R/step_lencode_survival.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/step_lencode_survival.R), [`NAMESPACE`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/NAMESPACE) |
| **P0 - Blocker** | **A5** | Windows tarball mangling via unignored deep assets | [`.Rbuildignore`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/.Rbuildignore) |
| **P1 - High** | **A3** | Unimported pipe operator `%>%` in exported functions | [`R/tidy_tmerge_cox.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/tidy_tmerge_cox.R), [`R/my_summary_table.r`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/my_summary_table.r) |
| **P1 - High** | **A4** | `knitr::purl()` evaluation crash on dynamic `has_sas` | [`vignettes/sas_survival.Rmd`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/vignettes/sas_survival.Rmd), [`vignettes/R_to_SAS.Rmd`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/vignettes/R_to_SAS.Rmd) |
| **P1 - High** | **A6** | Prefix collision in multi-variable Cox term matching | [`R/cbe_cox_multi.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/cbe_cox_multi.R) |
| **P1 - High** | **C1** | Rogue 387 KB Claude scratchpad file in repo root | Repository Root |
| **P2 - Medium** | **B1** | Discarded submodels & shifted hazard scale in joint step | [`R/step_lencode_survival.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/step_lencode_survival.R) |
| **P2 - Medium** | **B2** | Factor contrast level collapsing in Cox forest plot | [`R/plot_cox.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/plot_cox.R) |
| **P2 - Medium** | **B3** | Dimension drop on single-stratum Kaplan-Meier table | [`R/cbe_km_single.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/cbe_km_single.R) |
| **P2 - Medium** | **B4** | 0-row mutation bug in schema mapping | [`R/plug_and_play_schema.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/plug_and_play_schema.R) |
| **P2 - Medium** | **B5** | Global RNG seed mutation without restoration | [`R/simulate_cohort.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/simulate_cohort.R) |
| **P2 - Medium** | **C3** | 12 duplicate chunk label knitr warnings | [`inst/templates/eda_tables.qmd`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/inst/templates/eda_tables.qmd), [`inst/rmarkdown/...`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/inst/rmarkdown) |
| **P2 - Medium** | **C4** | Discrepancy between `DESCRIPTION` and ecosystem license | [`DESCRIPTION`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/DESCRIPTION), [`NOTES.md`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/NOTES.md) |
| **P2 - Medium** | **C5** | Dependency lockfile drift (51 packages) | [`renv.lock`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/renv.lock) |
| **P3 - Low** | **A7** | `data(lung, package = "survival")` documentation failure | [`R/explain_survival.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/explain_survival.R), [`R/step_lencode_survival.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/step_lencode_survival.R) |
| **P3 - Low** | **B6** | Hardcoded `"95% CI"` labels ignoring `conf_level` | [`R/cbe_cox_single.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/cbe_cox_single.R), [`R/cbe_cox_multi.R`](file:///c:/Users/jkyle/Documents/GitHub/TempleCBE/R/cbe_cox_multi.R) |
| **P3 - Low** | **C6** | Undeclared global variable notes in tidyverse pipelines | Multiple files across `R/` |

---
*Report compiled independently. Zero files in the repository were modified during this review.*
