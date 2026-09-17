# TempleCBE 0.3.4

## Exact Contingency Methods, Chi-Square Testing & Visualizations

* **Exact 2x2 Inference with Automatic Mid-p Default (`cbe_exact2x2`, `cbe_exact2x2_ci`)**:
  * New `cbe_exact2x2()` performs exact inference using `exact2x2`. If any cell count in a 2x2 table is zero (`min(tab) == 0`), it automatically defaults to the mid-p version of Central Fisher's exact test (`midp = TRUE`), preventing extreme conditional conservatism. For non-zero tables, it defaults to standard Central Fisher (`midp = FALSE`).
  * New `cbe_exact2x2_ci()` generates publication-ready odds ratio and confidence interval strings (e.g. `"0.8 (0.3, 2.1)"`).
* **Chi-Square & Exact Testing Suite (`cbe_test_categorical`)**:
  * Added `test = c("auto", "exact", "chisq", "fisher")` and `correct = FALSE` (uncorrected Pearson $\chi^2$) to `cbe_test_categorical()`, providing a drop-in custom test for `gtsummary::add_p()` implementing institutional CBE testing guidelines.
* **Standard 4-Quadrant Square Reports (`cbe_four_quadrant_report`, `cbe_square_plot`)**:
  * New `cbe_four_quadrant_report()` generates the standard clinical 4-quadrant report (`q1 | q2 // q3 | q4 // p = pformat`) returning structured quadrant percentages, console-ready text cards, compact 3-line summaries, and ggplot square tiles with configurable test engines (`"auto"`, `"exact"`, `"chisq"`, `"fisher"`).
  * Dedicated wrapper `cbe_square_plot()` provides direct access to 4-quadrant reports and square glyph plots.
* **Publication p-value Formatter (`pformat`, `cbe_pformat`)**:
  * New exported `pformat()` (and alias `cbe_pformat()`) formats numeric p-values into publication-ready strings (e.g. `pformat(0.042)` -> `"p = 0.042"`, `pformat(0.0001)` -> `"p < 0.001"`).
* **Contingency Plotting Suite (`cbe_contingency_plot`)**:
  * Unified contingency visualization supporting `balloon`, `bar` (`fill`, `dodge`, `stack`), `mosaic`, `heatmap`, `square`, and `corrplot` with automatic hypothesis test calculation (`test = "auto"`, `"exact"`, `"chisq"`, `"fisher"`).
  * New `plot_categorical_associations()` creates pairwise categorical correlation matrices using Cramér's V or $-\log_{10}(p)$ via `corrplot` with Temple University brand palettes.
  * New `cbe_pairwise_combos()` enumerates all pairwise categorical combinations with sequential indexing for child-document expansion.
* **Quarto & R Markdown Multi-Format Rendering (`render`, `render_me`)**:
  * Renamed primary function to `render()` with `render_me` preserved as an alias for full backwards compatibility.
  * Enhanced `path` parameter to accept character vectors, lists of paths, S4 compute graph objects (`FileOutputs`, `FilePath`), render plans from `get_render_plan()`, and full pipeline lists (with automatic filtering to renderable targets).
  * Added `...` forwarding to pass options (such as `params`, `execute_params`, `output_dir`, `quiet`) cleanly to the `quarto::quarto_render()` or `rmarkdown::render()` backend.
  * Added automatic document YAML frontmatter format extraction via `extract_yaml_formats()` when `formats = "yaml"` or `formats = "auto"`.
  * Integrated with `computeGraph`: document-level deliverable formats in `FileOutputs` or pipeline-level `pipeline_config(default_formats = ...)` seamlessly override package default `c("pdf", "docx")`.
  * Expanded native support for both `.qmd` and `.Rmd` documents (`pattern = "\\.(qmd|Rmd|rmd)$"`).
  * Added `engine = c("auto", "quarto", "rmarkdown")` with automatic shorthand format translation (`"pdf"` -> `"pdf_document"`, `"docx"` -> `"word_document"`, `"html"` -> `"html_document"`, `"gfm"` -> `"github_document"`).

* **Institutional Quarto EDA Templates**:
  * Added anonymized `eda_tables.qmd` and `child_eda_chi_square.qmd` under `inst/templates/` and `inst/rmarkdown/templates/eda-tables/` with missingness diagnostics, stacked `gtsummary` baseline tables, association matrices, and pairwise categorical comparisons.
* **Replacement of `arsenal` with `gtsummary`**:
  * `summarize_section_by_time()` now defaults to `engine = "gtsummary"`, retaining `engine = "arsenal"` as backwards-compatible fallback.

## Bug Fixes & Statistical Enhancements

* **Missingness & Auditing (`SumNa`)**:
  * Fixed a critical issue where `SumNa(df, na_list = ...)` failed to detect sentinel values on data frames due to `%in%` list dispatch. It now traverses columns column-by-column, correctly counting both standard `NA`s and multi-code institutional sentinels (e.g. `"999"`, `"-99"`, `"Unknown"`).
* **Time-Dependent Survival (`tidy_tmerge_cox`)**:
  * Fixed counting-process interval construction when events occur between scheduled longitudinal observation visits. Post-event filtering now occurs prior to interval lead calculations, correctly setting `tstop` to the event time and assigning `event = 1` for terminal intervals.
* **Reporting Formatters (`theme_cbe::words`)**:
  * Protected `words()` with `requireNamespace("knitr", quietly = TRUE)` and provided a native base R fallback for Oxford comma text formatting, preventing runtime crashes on minimal installs without `knitr`.
* **Biostatistical Testing (`single_t_test`)**:
  * Guarded `fold_change` and `log2_fold_change` against division by zero and negative values when baseline group mean is zero, safely returning `NA_real_`.
* **Outlier Detection (`detect_outliers`)**:
  * `calculate_fences()` now gracefully returns `NA_real_` bounds when input vectors contain zero non-NA values, preventing unhandled `quantile()` exceptions.
  * Standardized `.outlier` factor levels to `c(FALSE, TRUE)` across all data subsets.
* **PI Anonymizer (`scripts/pi_anonymizer.py` & `R/pi_anonymizer.R`)**:
  * Aligned Python anonymizer with R security policies: default confidential mapping file is now stored in user-scoped data directories (`~/.TempleCBE/pi_mapping.json`) outside the git repository tree, with automated repository root detection and atomic file replacement.
  * Synchronized `@param n_chars` documentation to reflect the default 16-character hexadecimal token length.
* **Repository Safety**:
  * Added `data/`, `*.xlsx`, `*.csv`, and `*.rds` patterns to root `.gitignore` to safeguard against accidental tracking of clinical datasets.
  * Updated `scripts/clean_publish.sh` to default to the active working branch rather than falling back unconditionally to `master`.

# TempleCBE 0.3.3141

## Multivariable Cox Modeling, Kaplan-Meier, and Shared Diagnostics

* **Multivariable Cox Modeling**:
  * New `cbe_cox_multi()` fits a multivariable Cox proportional hazards model (via `formula`, or the
    `outcome`/`features` convenience pair), returning a tidy coefficient table grouped by variable with
    explicit reference rows, a `glance` fit summary, per-term and global proportional hazards diagnostics,
    convergence status, and a formatted `print()` method.
* **Proportional Hazards Diagnostics**:
  * New `cbe_cox_check()` provides standalone, tidy `cox.zph()` diagnostics (per-term table, violation
    flags, and an automated text summary, including the multivariate global test) for any `coxph`,
    `cbe_cox`, or `cbe_cox_multi` object. `cbe_cox_single()` now uses `cbe_cox_check()` internally.
* **Kaplan-Meier**:
  * New `cbe_km_single()` pairs a univariable `cbe_cox_single()` fit with the matching stratified
    Kaplan-Meier curve (quartile-binned for continuous predictors) without refitting the Cox model twice,
    returning the Cox object, the `survfit` object, a tidy KM table, the hazard direction, a combined
    summary table, and a formatted `print()` method.
* **Presentation & Utility Helpers**:
  * New `cbe_cox_table()` formats a `cbe_cox`/`cbe_cox_multi` coefficient table for presentation, adding a
    log(HR) column with optional sorting by magnitude or p-value and significance-star annotation.
  * New `cbe_factor_reference()` relevels a factor's reference level before fitting, with the chosen level
    reported via `message()` and recorded in a `"cbe_reference_level"` attribute.
  * New `cbe_theme_survival()` gives a single consistent ggplot2 look across the Cox/KM visualizations;
    `plot_cox_forest()`, `plot_cox_survival()`, and `plot_cox_marginal()` now use it.
* **Enhanced Visualizations**:
  * `plot_cox_forest()` gains `scale` (`"hr"`/`"log_hr"`), `color_by` (`"none"`/`"significance"`), and
    `order_by` (`"none"`/`"magnitude"`/`"pvalue"`) arguments.
  * New `plot_cox_forest_multi()` renders a `cbe_cox_multi()` result as a forest plot with per-variable
    facet blocks, sharing the same `scale`/`color_by`/`order_by` options as `plot_cox_forest()`.
  * `plot_cox_survival()` gains `overlay_km = FALSE`; when `TRUE`, observed Kaplan-Meier step curves are
    overlaid (dashed) on the Cox-predicted curves (solid), with a legend distinguishing the two.
  * `plot_cox_marginal()` gains `scale` (`"prob"`/`"hr"`/`"log_hr"`) to plot predicted event probability or
    relative hazard (from the centered linear predictor), and now draws its confidence band with
* **SAS PROC PHREG Parity & Validation Datasets**:
  * Added `...` argument passthrough to `cbe_cox_multi()` and `cbe_cox_single()`, supporting `ties = "breslow"`, `id = ID`, `cluster`, and advanced `survival::coxph` options.
  * Added internal SAS Institute Example 85.7 validation datasets `tumor_wide()` (45 rodents, 19 variables) and `tumor_long()` (102 counting-process intervals, 8 variables) for time-dependent papilloma survival benchmarks.
  * Added `tidy_tmerge_cox()` helper function to construct counting-process start/stop intervals from repeated longitudinal measurements and event data frames with baseline covariate integration.
  * Added project-level pipeline orchestration script `MakeComputeGraph.R` (in `vignettes/` and `inst/scripts/`) simulating the Wolfson dependency graph architecture across all four analytical stages.
  * Organized vignette sequence into `01`–`04` numbered stages in documentation and articles navigation:
    * `01. eda_and_missingness.Rmd`
    * `02. nested_survival_cv.Rmd`
    * `03. compute_graph.Rmd`
    * `04. sas_survival.Rmd`

# TempleCBE 0.3.2

## Integrated Standard Biostatistical and Presentation Components from Wolfson

* **Institutional CBE Themes & Formatters**:
  * New `theme_cbe()` and `theme_cbe_deck()` provide minimal, publication-ready and presentation-ready ggplot2 styling.
  * New `cbe_palette` and discrete scales `scale_color_cbe()` and `scale_fill_cbe()` supply Temple Cherry and complementary institutional palettes.
  * New reporting formatters: `fmt_pct()`, `fmt_num()`, `fmt_sig()`, `fmt_p()`, `fmt_hr()`, and `words()`.
* **Univariable Cox Screening & Diagnostics**:
  * New `cbe_cox_single()` screens candidate predictors with automatic proportional hazards testing (`cox.zph`), tidy coefficient tables with explicit reference rows for categorical variables, automated clinical interpretations, and a formatted `print()` method.
  * New `plot_cox_forest()`, `plot_cox_survival()`, and `plot_cox_marginal()` provide diagnostic survival visualizations.
  * New child template `inst/templates/template_cox_single.qmd` automates univariable Cox screening sections in Quarto documents.
* **Presentation Deck Visualizations**:
  * New `plot_survival_km()` generates standardized Kaplan-Meier survival curves using Temple Cherry styling and percentage axes (supports `ggsurvfit` with fallback).
  * New `plot_dynamic_trajectory()` charts longitudinal biomarker trajectories with standard errors over protocol time.
  * New `plot_group_comparison()` and `plot_missingness()` build presentation-ready grouped bar charts and missing data quality audits.
  * New `table_two_by_two()` formats 2x2 contingency tables with row percentages, margins, and Fisher's exact test p-values.
* **Data Integrity & Schema Mapping Engine**:
  * New `read_data_manifest()`, `copy_data_manifest()`, `validate_data_manifest()`, and `stop_if_invalid_manifest()` guarantee that downstream analytical reports and decks never execute on stale data copies using cryptographic MD5 checksums.
  * New `validate_column_mapping()`, `find_section_file()`, `read_raw_table()`, `read_mapped_section_data()`, and `summarize_section_by_time()` provide schema-enforced table ingestion, duplicate column resolution, and longitudinal summaries.
  * New `simulate_section_data()` generates synthetic cohorts conforming to mapping roles for CI/testing without touching real patient data.
* **Deliverable Packaging & Script Auditing**:
  * New `package_deliverables()` collects rendered reports (PDF/DOCX/HTML) and data deliverables across compute graph stages into structured delivery ZIP archives.
  * New `audit_report_deliverables()` audits source scripts for referenced deliverable tokens and verifies on-disk existence.

## Vendored `renv/activate.R` updated

* Picks up upstream renv's fix for a bootstrap crash: a missing or corrupt downloaded archive during renv's own first-run self-install used to abort with a low-level connection error instead of failing gracefully. See [rstudio/renv@532d48d](https://github.com/rstudio/renv/commit/532d48d6303d88900aa11aac3a0a7f339466156d).

## `proc_pca()` accepts raw data

* `proc_pca()` no longer requires a pre-fitted `prcomp` object. Its argument is now `data`, which can be either a `prcomp` object or a numeric matrix/data frame; when given raw data, it fits the PCA itself via `stats::prcomp()`. New `center` and `scale` arguments (default `TRUE`) and `...` are passed through to `prcomp()` in that case, and are ignored when `data` is already a `prcomp` object.

## `use_temple_brand()` warns off-root installs; `create_report()` finds a root install

* `use_temple_brand()` gets a `check_root` argument (default `TRUE`): it now warns when `path` isn't the project root found by `here::here()`, since installing the extension into each report's own subfolder instead of once at the root creates a separate, driftable `_extensions` copy per report. Pass `check_root = FALSE` to install into a subfolder without the warning (what `create_report(..., install_brand = TRUE)` does internally, since that's a deliberate one-report install).
* `create_report()`'s "install the extension" message no longer fires for a `"temple"` report created under a project that already has the extension installed at an ancestor directory (matching how Quarto itself resolves `_extensions` from any project subfolder) — previously it only checked `location` itself.

## `create_report()` gets a `filename` argument

* New `filename` argument names the report file independently of `template_name`. Previously, two reports in the same `location` (e.g. `analysis/analysis1.qmd` and `analysis/analysis2.qmd`) both defaulted to `<template_name>.qmd`, so the second `create_report()` call silently overwrote the first. `create_report()` now warns before overwriting an existing report file, and `filename` lets each report keep its own name: `create_report("analysis", filename = "analysis1")`, `create_report("analysis", filename = "analysis2")`.

## Penalized Cox models for start/stop survival data

* New `coxnet()` fits an elastic-net Cox model with glmnet through the tidymodels hardhat interface: formula, recipe, or predictors and outcome. The outcome can be right-censored, `Surv(time, event)`, or start/stop, `Surv(start, stop, event)`. `predict()` returns `.pred_linear_pred` or a `.pred` list-column of survival probabilities (Breslow baseline hazard), and `tidy()` returns every coefficient.
* New `cv_coxnet()` is a tidymodels counterpart to `glmnet::cv.glmnet()`. Folds are grouped by `subject_id` (or a coarser `group`, such as site), preprocessing from a recipe is learned inside each fold, and every `mixture` and `penalty` is scored with a yardstick metric set: by default the integrated Brier score, concordance, and the time-specific Brier score and ROC AUC. It reports `lambda.min` and `lambda.1se` for the chosen metric, and has `predict()`, `tidy()`, `autoplot()`, and `tune::collect_metrics()` methods. Bootstrap resamples, which repeat subjects, are scored with each copy as its own subject.
* New `nested_cv_coxnet()` runs `cv_coxnet()` on the inner resamples of an `rsample::nested_cv()` object, refits on each outer analysis set, and scores the outer assessment set.
* New `surv_subject_truth()`, `censoring_km()`, `graf_weights()`, and `add_graf_weights()` collapse start/stop outcomes to one row per subject and add inverse-probability-of-censoring (Graf) weights, so any model's survival predictions can be scored with yardstick. Given start/stop truth directly, yardstick returns numbers without complaint, but they count every interval as a subject.
* `hardhat` and `generics` added to Imports.

## `glmnet_IBS()` rebuilt on `cv_coxnet()` (breaking)

* **Results change.** `glmnet_IBS()`, `tune_over_alpha()`, and `summarize_tune_results()` keep their arguments, but `glmnet_IBS()` now uses `cv_coxnet()`:
  * The penalty is chosen by the integrated Brier score (or `metric =`) on folds grouped by subject. `cv.glmnet()` chose it by concordance on folds of rows, which put a subject's intervals in both analysis and assessment sets.
  * Survival is predicted from the model's own Breslow baseline hazard along each subject's covariate path. Previously it was a null model's hazard times a relative risk re-centred on the assessment set.
  * Missing relative risks were filled by averaging with the previous row, which could belong to a different subject. That code is gone.
* `censoring_weights = "none"` is removed and now errors: it scored interval rows without censoring weights, so it was not a proper Brier score. Install TempleCBE 0.2.0 to reproduce results that used it.
* A failed fit returns `IBS = NA` (was 2) with a warning giving the reason; `failure_ibs` still sets the value.
* Output has one row per feature, including coefficients the penalty set to zero.
* New arguments: `eval_time`, `metric`, `rule` (`"min"` or `"1se"`), and `covariates` (`"path"` or `"baseline"`). `type.measure = "C"` is deprecated in favour of `metric = "concordance_survival"`, and `parallel` is ignored.
* glmnet's `cox.ties` defaults to `"breslow"`, matching the baseline hazard, so results don't change with glmnet 5.1's switch to Efron.

## `step_famd()` fixes (breaking)

* `num_comp` was capped at the number of selected variables, but FAMD has more dimensions when categorical variables have several levels (numeric variables plus one fewer than the number of levels, per categorical variable). Components beyond the variable count were silently dropped, and `threshold` only chose among the first few, so a 99% threshold could keep components covering far less. Both now use all of FAMD's dimensions.
* Without FactoMineR installed, `prep()` silently ran a PCA on the numeric variables alone. It now asks for FactoMineR.
* New components are named `FAMD1`, `FAMD2`, ... (`recipes::names0()`, zero-padded from 10 components) instead of `PC1`, `PC2`, ..., so `step_famd()` and `step_pca()` can share a recipe. A name that already exists in the data is an error rather than a duplicate column.
* Character and logical variables are treated as factors, with levels learned by `prep()`. Categories unseen in training and missing values are informative errors.
* Frequency weights are passed to FAMD as row weights (importance weights are ignored, as in `step_pca()`), and `tidy(type = "variance")` reports component variances as for `step_pca()`.
* New `required_pkgs()` method, so tidymodels loads FactoMineR and TempleCBE on parallel workers.
* The README example selected only numeric predictors (iris with `Species` as the outcome), which FAMD rejects; it now uses `Species` as a predictor.

## Other fixes

* `plot.prcomp()` is removed. It replaced \pkg{stats}' `plot()` method for `prcomp` objects for anyone who loaded TempleCBE. Use the new `pca_plot(pca_model, type = )`, which also no longer mistakes an `x =` component argument for the PCA fit.
* `pdf_to_rtf()` wrote page breaks as the literal text `\page`; they are now real page breaks. Non-ASCII characters are written as RTF Unicode escapes. The arguments are now `pdf` and `rtf` (defaulting to `pdf` with an `.rtf` extension), with new `font_size` and `overwrite`.
* `is_normal()` used a Kolmogorov-Smirnov test with the mean and standard deviation estimated from the same data, which gives p-values that are far too large. It now uses the Lilliefors test (`nortest::lillie.test()`, added to Imports). Above 5000 values it no longer runs Shapiro-Wilk on a random subsample, so results are deterministic. New `alpha` argument.
* Minimum versions now match the features used: ggplot2 >= 3.5.0 (the Temple scales omit `scale_name`), ggridges >= 0.5.0, pdftools >= 2.0, scales >= 0.5.0, hardhat >= 1.3.0, and in Suggests furrr >= 0.2.0, missRanger >= 2.4.0, quarto >= 1.4, rsample >= 1.1.0, testthat >= 3.1.7, withr >= 2.3.0, workflowsets >= 1.1.0, yardstick >= 1.3.0, and zip >= 2.3.0. `tools` is declared in Imports, and `dials` (used by `step_famd()`'s `tunable()` method) in Suggests.

## `write_xlsx()` re-exported

* `write_xlsx()` is re-exported from `writexl`, so `TempleCBE::write_xlsx()` works. Analysis code already calls it that way, but it previously failed with "'write_xlsx' is not an exported object". `writexl` moves from Suggests to Imports.

## Temple brand

* New `temple_colors()`, `temple_pal()`, `scale_colour_temple()`/`scale_color_temple()`/`scale_fill_temple()`, and `theme_temple()` draw R graphics in the Temple University palette used by the [quarto_temple_brand](https://github.com/jkylearmstrong-temple/quarto_temple_brand) Quarto extension. `temple_brand_path()` returns a bundled copy of its `brand.yml`, for `quarto::theme_brand_ggplot2()` or `bslib::bs_theme(brand = )`.
* New `use_temple_brand()` installs that extension (from `jkylearmstrong-temple/quarto_temple_brand` by default) into a Quarto project, creating `_quarto.yml` if needed, which enables the `temple-html`, `temple-pdf` (LaTeX title page), `temple-typst`, and `temple-revealjs` formats.
* `create_report(template_name = "temple")` scaffolds a report in those formats; `install_brand = TRUE` also installs the extension.
* Plots use the Temple palette. Diverging heatmaps (`correlation_plot()`, `correlation_diff_heatmap()`, `pca_feature_loading_heatmap()`, `pca_loading_diff_heatmap()`) run Night Owl-white-cherry instead of blue-white-red; `missmap()` counts, `plot_features_percent_miss()`, `plot_pca_bi()`/`pca_biplot()` loadings, `pca_percent_var_explained()`, and the reference lines of `distribution_plot()`, `manhattan_plot()`, and `volcano_plot()` use Temple colors. Only colors change.
* The palette follows Temple's current brand (<https://liberalarts.temple.edu/marcom/logos-and-brand>), matching quarto_temple_brand: cherry, white, and black (`#000000`); Clear Skies and Book Nook; formal accents `academic-gold`, `diamond-acres`, `founders-garden`, `night-owl`; casual accents `owls-eye`, `conwell-blue`, `upward-momentum`, `cherry-blossom`. The earlier names (`taupe`, `icy-blue`, `lime`, `eggshell`, `ochre`, `geranium`, `dark-blue`) are gone. The `"main"` palette is cherry, Night Owl, Owl's Eye, Founder's Garden, Upward Momentum, Diamond Acres, black; `"sequential"` runs Book Nook to cherry.
* Links in the bundled `brand.yml` are standard blue (`#0563c1`) rather than cherry, matching quarto_temple_brand. The brand guide sets no link color, and red links read as errors, especially in print.

## `zip_render()` fixes

* Extension formats such as `titlepage-pdf` or `temple-pdf` weren't matched to their output file, so it was silently left out of the zip. They now resolve to their base format's extension.
* `_quarto.yml`, `_brand.yml`, `_variables.yml`, and `_extensions/` are copied into the build directory, so documents that use a project, brand, or extension format render there as they do in place. With `include_sources = TRUE` they are zipped under their relative paths.

## `zip_reports()` fix

* Reports that share the same source stem (for example `analysis1/analysis.qmd` and `analysis2/analysis.qmd`) no longer overwrite each other inside staged `pdf/`, `docx/`, or `html/` folders. `zip_reports()` now disambiguates staged output names while keeping index links aligned with the copied files.

# TempleCBE 0.2.0

## `glmnet_IBS()` rebuilt for start/stop survival data (breaking)

* **Breaking change.** `glmnet_IBS()` is now a port of the penalized-Cox tuning code it was originally meant to replace, generalized so no column names are hard-coded. The previous version scored plain `time`/`status` data with its own IPCW Brier score, treated every start/stop row as an independent subject, used every numeric column (including identifiers) as a predictor, and returned only `IBS`, `lambda`, and `alpha` -- none of which fit repeated-measures data. The new signature takes an `rsplit`, an unprepped `recipe` (prepped inside the fold), `feature_names`, a `time_data` grid, and `id_col`/`start_col`/`stop_col`/`status_col`, and returns `IBS`, `lambda`, `term`, `estimate`, and `alpha` (one row per coefficient at `lambda.min`), with `IBS = failure_ibs` (default 2) when `cv.glmnet()` cannot fit.
* `censoring_weights = "none"` (default) reproduces the ported code: interval rows scored with censoring weight 1. `censoring_weights = "ipcw"` scores one row per subject with inverse-probability-of-censoring weights (Graf et al., 1999) from the analysis-set censoring distribution. The two give different numbers; compare within one setting.
* New `tune_over_alpha()` and `summarize_tune_results()` tune `glmnet_IBS()` over an `alpha` grid for one split and for every split of a resample. Neither calls `future::plan()`. With `formulas`, they instead fit one model per candidate feature set, each with its own (given or randomly drawn) `alpha`, and add a `formula` column.
* `glmnet_IBS()` accepts `feature_names` as a function of the baked analysis set, for recipes whose output columns vary by fold (e.g. `step_pca(threshold = )`).
* Bugs fixed relative to the ported code: the `alpha` grid hard-coded 6 fixed values, so it produced `num_alpha_values + 1` values when `num_fixed = 6` and overwrote fixed values otherwise -- it now has exactly `num_alpha_values`; the outer map over splits drew random `alpha` values in workers without a seed, so grids were not reproducible -- both maps now run with `furrr_options(seed = TRUE)`; filling a missing relative risk looped forever for a subject with no known value -- it now errors naming the subject.
* The internal `ipcw_brier_score()`/`integrate_brier_score()` helpers of the old implementation are removed; scoring now goes through `yardstick::brier_survival_integrated()`.

## Other new functions

* `get_model_parameters()` returns the preprocessor, model, and best tuning parameters of the workflow ranked `.rank` in tuned workflow set results; `fit_n_rank()` also fits it with `tune::fit_best()`. Fixed while porting: with `group_wflow = FALSE`, the ranked configuration was reported but the workflow's *best* configuration was fitted; and the fit used `fit_best()`'s default metric rather than `rank_metric`.
* `km_summary_to_prism()` expands a Kaplan-Meier summary-by-time table into a GraphPad Prism survival table. Fixed while porting: `strata_levels` was documented but ignored, and `validate_totals` failed when `strata_levels` was set.
* `convert_pdf_to_docx()`, `convert_pdfs_to_docx()`, `check_docx_toolchain()`, `find_python()`, and `find_soffice()` convert PDFs to DOCX via `pdf2docx`, LibreOffice, or Word COM (Windows), with verified backend discovery. `convert_pdf_to_docx` fits `zip_reports(docx_from_pdf = )`. Pinned Python requirements ship in `inst/python/requirements.txt`. Interpreters are configured with `options(templecbe.python)`/`TEMPLECBE_PYTHON` and `options(templecbe.soffice)`/`TEMPLECBE_SOFFICE`. The Word COM subprocess now runs the calling session's own `Rscript` rather than the first one on `PATH`.
* `run_sas_script()` runs a SAS program in batch mode with its log and listing in separate folders; `find_sas()` locates the executable (`options(templecbe.sas)`, `SAS_EXE`, `PATH`, or the default install locations).
* `normalize_safely()`, `parse_here_call_vec()`, `file_meta_fs()`, `extract_win_posix_paths()`, and `extract_all_xlsx_tokens()` are exported: the path helpers behind `scan_data_io()`, for code that audits file paths itself.
* `profvis_summary()` tabulates a `profvis` profile by function: memory, memory increments, call counts, stack depth, and memory over time.
* `parsnip`, `profvis`, `reticulate`, `tune`, `workflows`, `workflowsets`, and `yardstick` added to Suggests.

## CI and packaging fixes

* The "Nested Cross-Validation for Longitudinal Survival Models" vignette uses the new `glmnet_IBS()` arguments (`recipe`, `feature_names`, `time_data`, `id_col`) and shows both censoring weightings; it no longer built against 0.2.0.
* `normalize_safely()` returns forward slashes on every platform, consistent with `scan_data_io()`.
* `scan_data_io()` documents `max_depth`, `zip_render()`'s documentation is regenerated to match its code, and `yaml` (used by `zip_render()`) is declared in Suggests. These were the two `R CMD check` warnings on `master`.
* The pkgdown reference index lists every exported topic, adding the `mtry` sweeps, `render_me()`, `read_search()`, `write_search()`, `scan_data_io()`, and `zip_reports()`.
* The Docker image installs `libuv1-dev`, which `fs` needs at load time.

# TempleCBE 0.1.8

## New `mtry`-sweep imputation (#3)

* `missforest_sweep_mtry()` sweeps `mtry` for `missForest`, scores every column by its out-of-bag error, and assembles each column from whichever run imputed it best. `missforest_oob_by_mtry()` (one fit, tidy per-column OOB table) and `missforest_impute_by_mtry()` (assemble columns from their winning runs) are exported as the building blocks. This consolidates three copies that had drifted apart in analysis code; their differences are now arguments (`exclude` for identifier/time columns) or documented behavior (character-to-factor coercion inside the worker, original column order restored).
* `missranger_sweep_mtry()`, `missranger_oob_by_mtry()`, and `missranger_max_mtry()` run the same sweep on the `missRanger` engine with the same return shape. `missranger_max_mtry()` computes the largest `mtry` `missRanger` admits, which is bounded by the number of complete columns rather than `ncol - 1`. Errors are not comparable across engines -- compare `mtry` within an engine only.
* Bugs fixed relative to the copies this replaces: runs were looked up by position (`sweep[[mtry]]`), which is only correct when the grid is exactly `1:n`; seeding passed to `future::plan(.options = ...)` was ignored, so most sweeps were never seeded -- the seed now reaches `furrr::future_map()`'s own `.options`; all-`NA` columns, which `missForest` silently drops and `missRanger` silently leaves `NA`, are now refused by name.
* `max_pct_missing` holds out columns missing more than a given share, carries them through unimputed, and reports them in `excluded_high_missing`. Defaults to `NULL` (impute everything).
* Neither sweep calls `future::plan()`; the caller's backend is respected. `missForest`, `missRanger`, and `pkgload` added to Suggests.

## `corr_test_all()` output options (#4)

* `columns = "tidy"` returns every `broom::tidy()` column of each `cor.test()` (estimate renamed `cor`), including the statistic, degrees of freedom, and confidence limits. The default `"compact"` output (`var1`, `var2`, `r`, `p_value`) is unchanged.
* `sort` chooses `"p_value"` (default), `"estimate"`, `"abs_estimate"`, or `"none"`.
* `...` is passed to `cor.test()` (`alternative`, `conf.level`, `exact`).
* `use = "complete.obs"` now tests every pair on the same rows. `use` was previously accepted but had no effect on the tests; unsupported values now error.
* **Behavior change:** pairs are enumerated in column order, so `var1` is the column that appears first in `data`. Previously it was whichever name sorted first under the locale's collation. Values are unchanged; only a pair's orientation and tie order can differ.

## New reporting utilities (#1)

* `render_me()` renders Quarto documents, optionally in parallel (`future`/`furrr`/`quarto` in Suggests).
* `read_search()` / `write_search()` locate read and write calls in code.
* `zip_reports()` packages already-rendered reports and a data folder into one indexed, hyperlinked zip, given a plain ordered data frame. DOCX generation is a caller-supplied `docx_from_pdf()` callback.
* `scan_data_io()` cross-references read/write calls in code against files on disk, for any file extension and project root. Fixed while porting: the full-path regex could never match, so full-path resolution silently found nothing. Also fixed after the port: inconsistent result schema on `render_me()`'s parallel path, the path separator on non-Windows platforms, and `scan_data_io()` failing on paths with repeated separators.

## Fixes

* `get_dataset_info()` handles `survival::Surv` columns (#2). `Surv` objects are numeric matrices, so they were summarized as one flattened mean/SD of time and status together, and `dplyr::n_distinct()` recursed infinitely on them. They are now summarized from their time/status columns. Variable labels also fall back to `attr(x, "label")` when `labelled::var_label()` finds none.
* Example templates in `inst/templates/` generate synthetic data inline and no longer depend on private internal datasets; the bundled example PDFs were re-rendered from them.

# TempleCBE 0.1.7

## `correlation_plot_split()` crash fix

* Found by a real render, not by the existing test suite: `correlation_plot_split()`'s hierarchical clustering, cut at a fixed `k = ceiling(n_vars / group_size)`, can leave a cluster with just one variable in it -- confirmed with a real 7-variable dataset at `group_size = 6`. A "group" of one variable has no pairwise correlation to show, and a 1x1 correlation matrix crashes `corrplot()`'s default `order = "FPC"` ordering downstream (`eigen(corr)$vectors[, 1:2]`: subscript out of bounds -- a 1x1 matrix's `eigen()` has no second eigenvector to index). Added `merge_singleton_groups()`, an internal helper that folds any singleton cluster into whichever other group its variable is most correlated with on average (in absolute value), so `correlation_plot_split()` never hands `correlation_plot()` a group of one. Verified against 15 random variable counts (5-9) with no errors and no singleton groups produced.
* `correlation_plot()` itself now errors clearly ("requires at least 2 numeric columns") on single-column input, instead of failing inside `corrplot()`'s internals -- defense in depth for any direct caller, not just calls routed through `correlation_plot_split()`.

# TempleCBE 0.1.6

## Docker/`renv` reproducibility fix

* `Dockerfile` previously ignored the committed `renv.lock` entirely: `remotes::install_deps()` resolved TempleCBE's declared `DESCRIPTION` dependencies against whatever versions happened to be current on live CRAN/r-universe at build time, so the exact package versions baked into a Docker image could silently drift from what `renv::restore()` installs on a Windows dev machine against the pinned lockfile -- exactly the reproducibility gap `renv` exists to close. The image now installs a pinned `renv` (version tracked via a new `RENV_VERSION` build arg, matching the existing `R_VERSION`/`QUARTO_VERSION` arg convention) and runs `renv::restore(prompt = FALSE)` against the committed `renv.lock`, `.Rprofile`, and `renv/activate.R`/`renv/settings.json`, so Docker builds and local `renv::restore()` on Windows now install identical dependency versions. These are still copied in ahead of the rest of the source tree (as `DESCRIPTION` was previously), so the restore layer only invalidates when the lockfile itself changes, not on every source commit. The final package install also switched from `R CMD INSTALL` to `renv::install(".")`, since plain `R CMD INSTALL` doesn't source `.Rprofile` and so can't see the renv-managed library `renv::restore()` populated -- it failed to find `ggplot2`/`corrplot`/etc. even though they were installed correctly. `renv::install()` runs inside the same renv-activated session, avoiding that mismatch. (One caveat found while verifying the built image: `renv::status()` still reports R's own bundled "recommended" packages -- `survival`, `MASS`, `Matrix`, etc. -- as out of sync with the lockfile inside the container, because renv deliberately avoids overwriting a base R installation's own recommended-package versions. This is expected `renv` behavior rather than a gap introduced here, doesn't affect any of TempleCBE's own dependencies, and the built image was confirmed to load and run TempleCBE correctly.)
* `renv::snapshot()` was re-run to confirm the lockfile is current after the 0.1.5 correlation-plot changes; those changes only used already-imported packages (`stats`, `ggplot2`, `corrplot`, `dplyr`, `tibble`), so no package versions needed updating -- `renv.lock` is unchanged.

# TempleCBE 0.1.5

## `correlation_plot()` rendering fixes

* `corrplot()` was never given any top margin, so `title` collided with the 45-degree diagonal variable-name labels sitting just below it in every rendered plot. Added a `mar` argument (default `c(0, 0, 2, 0)`, the standard `par("mar")` `c(bottom, left, top, right)` form that `corrplot()` already accepts) so the title clears the labels by default, while still letting callers override it for longer titles or larger `tl.cex`.
* Coefficient numbers were hardcoded on (`addCoef.col = "black"`) with no clean way to turn them off. On a correlation matrix with many variables the numbers overlap the ellipses and labels; the only workaround was shrinking `tl.cex`/`number.cex` toward zero, which doesn't fix the crowding -- it just deletes every label, leaving an unreadable, unlabeled plot. Added a `show_coef = TRUE` argument; setting it to `FALSE` omits the coefficients cleanly while keeping the diagonal variable labels intact. The default is unchanged, so existing small-matrix callers see no behavior difference.

## New correlation functions

* `correlation_plot_split()`: for a correlation matrix with too many variables to stay legible in one `correlation_plot()` call (e.g. ~40 clinical parameters), automatically groups variables via hierarchical clustering on `as.dist(1 - abs(cor_mat))` -- the same correlation-based distance `corrplot`'s own `order = "hclust"` uses -- and draws one within-group `correlation_plot()`-style plot per group (default target size 12 variables per group, via `ceiling(n_vars / group_size)` groups from `stats::cutree()`). Each sub-plot's title is suffixed `"(Group i of n)"` so the sub-plots can be told apart. Returns the per-group correlation matrices invisibly, as a named list, since it is called (like `correlation_plot()`) for its plotting side effect.
* `correlation_diff()` / `correlation_diff_heatmap()`: compare the correlation matrix of a comparison dataset against a baseline dataset, matching numeric variables by column name (falling back to the intersection if the two datasets' numeric columns differ). Unlike `pca_loading_diff()`, no sign-alignment step is needed -- correlation coefficients, unlike PCA loadings, have no sign ambiguity. Returns/renders only one triangle of the (symmetric) difference matrix, with the (always-zero) diagonal dropped. `correlation_diff_heatmap()` uses the same diverging, zero-centered fill scale as `pca_loading_diff_heatmap()`.

# TempleCBE 0.1.4

## New PCA functions

* `pca_biplot()`: a real PCA loadings biplot. Unlike `plot_pca_bi()` (which draws each *observation* as an arrow to its PC score, labeled by an id column), `pca_biplot()` draws the observation scores as a muted point cloud and overlays the variable loading vectors (from `pca_model$rotation`) as labeled arrows from the origin -- the classic two-panel-in-one biplot. Loadings are rescaled so their max extent is 80% of the score cloud's max extent, since raw (unit-scale) loadings would otherwise be invisible next to the scores. Works directly off a fitted `prcomp` object; no `newdata` argument needed. Added as a new `type = "biplot"` option in `plot.prcomp()`.
* `pca_loading_diff()`: compares variable loadings between two independently-fit `prcomp` objects on the same variables (e.g. the same domain at baseline vs. a later timepoint). Handles PCA's arbitrary component sign by sign-aligning each shared component of the comparison fit to the baseline before differencing, so a component that's merely flipped (not truly changed) reads as ~0 difference instead of a spurious ~2x jump. Matches variables by name and falls back to the intersection if the two fits' variable sets differ.
* `pca_loading_diff_heatmap()`: renders `pca_loading_diff()`'s output as a feature-by-component heatmap with a diverging, zero-centered fill scale, matching `pca_feature_loading_heatmap()`'s visual style.

## Styling

* `pca_percent_var_explained()`: tightened the top margin above the variance bars by adding `expand = ggplot2::expansion(mult = c(0, 0.01))` to the percent-of-variance y scale.

# TempleCBE 0.1.3

## `missmap()` improvements

* `by_column` mode now respects the `row_order` argument: when `row_order = FALSE` (default), groups (x-axis) and features (y-axis) are each ordered by descending total missingness, matching the ordering already applied in the default per-row/column view. Previously features stayed in whatever order `pivot_longer()` produced (alphabetical), ignoring `row_order` entirely.
* `by_column` mode now auto-detects when the aggregated missingness is effectively binary -- i.e. every group has at most one contributing row (checked from actual group sizes via `dplyr::n()`, not just the resulting sums) -- and in that case renders with the same discrete "Missing"/"Present" two-level fill and "Data Status" legend used in the default view, instead of a continuous black-to-red "# missing" gradient that is misleading when every value is 0 or 1 (e.g. `by_column` set to a unique subject/site id with one row per group). Groups with more than one contributing row keep the existing continuous gradient, since a real count is meaningful there (e.g. multiple readings per site over time).
* Added a `fill = c("auto", "binary", "count")` argument to `missmap()` to override the auto-detected fill behavior explicitly when needed.

# TempleCBE 0.1.2

## Statistical correctness fixes

* `is_poisson()`: the chi-squared branch's `distribution.test` flag was inverted relative to every other test in the package (`p < 0.1` was mislabeled as "looks Poisson"), and the test itself used a statistically invalid cross-tabulation instead of a real goodness-of-fit comparison. Replaced with a proper chi-squared goodness-of-fit test using quantile-based binning against the fitted Poisson distribution, with degrees of freedom correctly reduced for the estimated rate. Dropped the accompanying Kolmogorov-Smirnov test: KS assumes a continuous null distribution, and Poisson's real point masses inflate the KS statistic regardless of true fit.
* `is_normal()`: switched from comparing against a freshly simulated random sample (non-deterministic, added unnecessary noise) to a one-sample KS test against the fitted normal CDF directly.
* `glmnet_IBS()`: the per-time-point Brier score was normalized by the sum of IPCW weights that happened to contribute, instead of the full test-set size — this double-counted the effect of exclusions and inflated the score. Fixed to follow the Graf et al. (1999) IPCW estimator exactly; refactored into standalone, independently-tested helpers.
* `single_t_test()`: `paired = TRUE` crashed unconditionally (`broom::tidy()` doesn't return `estimate1`/`estimate2` for a paired test) — fold-change is now computed directly from the group vectors. Also added an optional `.id` argument to pair observations by a subject/record identifier instead of by row order, which previously silently mismatched pairs unless the two groups were pre-sorted identically.

## `step_famd()` fixes

* `ncp` was never passed to `FactoMineR::FAMD()`, so every fit silently capped at FactoMineR's default of 5 components regardless of `num_comp`.
* `threshold` (cumulative-variance component selection) was documented and tunable but had no effect; now implemented.
* `options` (extra arguments to `FactoMineR::FAMD()`) was documented but never forwarded; now implemented.
* `print.step_famd()` always printed an empty column list due to an incorrect `names()` call; now uses `recipes::print_step()` like other recipe steps.
* `tidy.step_famd()` returned fabricated placeholder values (`value = 1.0`, `component = "PC1"` for every term) instead of real per-component loadings/contributions.
* `bake.step_famd()` silently returned the data unchanged if FactoMineR became unavailable after `prep()`; now errors with a clear message.
* Added a clear error when `step_famd()` is given only quantitative or only qualitative columns (FAMD requires mixed data).

## Other bug fixes

* `get_dataset_info()` / `proc_contents()`: crashed on any all-`NA` column.
* `create_toc_from_sas_pdf()`: TOC page numbers drifted from the true PDF page as soon as any earlier page had no top-margin text.
* `zip_render()`: the output-file glob was hardcoded to `html|pdf|docx`, silently dropping any other requested Quarto output format from the zip.
* `plot_pca_bi()`: silently produced a degenerate PC1-vs-PC1 biplot on a single-component model; now errors with a clear message.
* `z_norm()`: the zero-variance branch overwrote original `NA` values with `0`.
* `manhattan_plot()` / `volcano_plot()`: the significance threshold was hardcoded to 0.05 in four places; added an `alpha` argument.

## Code quality

* Removed `proc_pca()`'s unused `data` argument.
* `delete_nul_files()` now builds its shell command via `shQuote()` instead of hand-spliced quoting.

## Testing

* Added regression tests for every fix above.
* Backfilled test coverage for previously-untested files: `t_tests`, `distribution_test`, `correlation_plot`, `manhattan_volcano_plot`, `distribution_plot`, `missmap`, `pca_plots`, `R_names`, `read_workbook`, `dev_utils`, `keep_only`.

# TempleCBE 0.1.1

* **Package Infrastructure**: Fixed R CMD check errors and warnings to ensure full compliance with R package standards.
* **Dependencies**: Added `vctrs` to `Imports` and `FactoMineR` to `Suggests` in `DESCRIPTION`.
* **S3 Method Consistency**: Updated `plot.features_percent_miss` method signature to include `...` for base `plot()` generic compatibility.
* **S3 Dispatch**: Assigned `"features_percent_miss"` class to `features_percent_miss()` output to enable seamless S3 `plot()` dispatch.
* **Documentation & Examples**: Updated `@examples` and roxygen tags across `features_percent_miss`, `infix_helpers`, `my_summary_table`, and `sd.error`. Added missing `@param table` documentation for `%notin%`.
* **Unit Testing**: Expanded test coverage in `tests/testthat/test-features_percent_miss.R` and created `tests/testthat/test-summary.R`.
* **Build Configuration**: Added `.Rbuildignore` to ignore `README.qmd` during R CMD check.

# TempleCBE 0.1.0

* Initial release of TempleCBE biostatistics, clinical data science, and modeling utilities.
