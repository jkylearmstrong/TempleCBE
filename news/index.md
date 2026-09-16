# Changelog

## TempleCBE 0.3.2

### Integrated Standard Biostatistical and Presentation Components from Wolfson

- **Institutional CBE Themes & Formatters**:
  - New
    [`theme_cbe()`](https://jkylearmstrong.github.io/TempleCBE/reference/theme_cbe.md)
    and
    [`theme_cbe_deck()`](https://jkylearmstrong.github.io/TempleCBE/reference/theme_cbe_deck.md)
    provide minimal, publication-ready and presentation-ready ggplot2
    styling.
  - New `cbe_palette` and discrete scales
    [`scale_color_cbe()`](https://jkylearmstrong.github.io/TempleCBE/reference/scale_color_cbe.md)
    and
    [`scale_fill_cbe()`](https://jkylearmstrong.github.io/TempleCBE/reference/scale_fill_cbe.md)
    supply Temple Cherry and complementary institutional palettes.
  - New reporting formatters:
    [`fmt_pct()`](https://jkylearmstrong.github.io/TempleCBE/reference/fmt_pct.md),
    [`fmt_num()`](https://jkylearmstrong.github.io/TempleCBE/reference/fmt_num.md),
    [`fmt_sig()`](https://jkylearmstrong.github.io/TempleCBE/reference/fmt_sig.md),
    [`fmt_p()`](https://jkylearmstrong.github.io/TempleCBE/reference/fmt_p.md),
    [`fmt_hr()`](https://jkylearmstrong.github.io/TempleCBE/reference/fmt_hr.md),
    and
    [`words()`](https://jkylearmstrong.github.io/TempleCBE/reference/words.md).
- **Univariable Cox Screening & Diagnostics**:
  - New
    [`cbe_cox_single()`](https://jkylearmstrong.github.io/TempleCBE/reference/cbe_cox_single.md)
    screens candidate predictors with automatic proportional hazards
    testing (`cox.zph`), tidy coefficient tables with explicit reference
    rows for categorical variables, automated clinical interpretations,
    and a formatted [`print()`](https://rdrr.io/r/base/print.html)
    method.
  - New
    [`plot_cox_forest()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_cox_forest.md),
    [`plot_cox_survival()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_cox_survival.md),
    and
    [`plot_cox_marginal()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_cox_marginal.md)
    provide diagnostic survival visualizations.
  - New child template `inst/templates/template_cox_single.qmd`
    automates univariable Cox screening sections in Quarto documents.
- **Presentation Deck Visualizations**:
  - New
    [`plot_survival_km()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_survival_km.md)
    generates standardized Kaplan-Meier survival curves using Temple
    Cherry styling and percentage axes (supports `ggsurvfit` with
    fallback).
  - New
    [`plot_dynamic_trajectory()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_dynamic_trajectory.md)
    charts longitudinal biomarker trajectories with standard errors over
    protocol time.
  - New
    [`plot_group_comparison()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_group_comparison.md)
    and
    [`plot_missingness()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_missingness.md)
    build presentation-ready grouped bar charts and missing data quality
    audits.
  - New
    [`table_two_by_two()`](https://jkylearmstrong.github.io/TempleCBE/reference/table_two_by_two.md)
    formats 2x2 contingency tables with row percentages, margins, and
    Fisher’s exact test p-values.
- **Data Integrity & Schema Mapping Engine**:
  - New
    [`read_data_manifest()`](https://jkylearmstrong.github.io/TempleCBE/reference/read_data_manifest.md),
    [`copy_data_manifest()`](https://jkylearmstrong.github.io/TempleCBE/reference/copy_data_manifest.md),
    [`validate_data_manifest()`](https://jkylearmstrong.github.io/TempleCBE/reference/validate_data_manifest.md),
    and
    [`stop_if_invalid_manifest()`](https://jkylearmstrong.github.io/TempleCBE/reference/stop_if_invalid_manifest.md)
    guarantee that downstream analytical reports and decks never execute
    on stale data copies using cryptographic MD5 checksums.
  - New
    [`validate_column_mapping()`](https://jkylearmstrong.github.io/TempleCBE/reference/validate_column_mapping.md),
    [`find_section_file()`](https://jkylearmstrong.github.io/TempleCBE/reference/find_section_file.md),
    [`read_raw_table()`](https://jkylearmstrong.github.io/TempleCBE/reference/read_raw_table.md),
    [`read_mapped_section_data()`](https://jkylearmstrong.github.io/TempleCBE/reference/read_mapped_section_data.md),
    and
    [`summarize_section_by_time()`](https://jkylearmstrong.github.io/TempleCBE/reference/summarize_section_by_time.md)
    provide schema-enforced table ingestion, duplicate column
    resolution, and longitudinal summaries.
  - New
    [`simulate_section_data()`](https://jkylearmstrong.github.io/TempleCBE/reference/simulate_section_data.md)
    generates synthetic cohorts conforming to mapping roles for
    CI/testing without touching real patient data.
- **Deliverable Packaging & Script Auditing**:
  - New
    [`package_deliverables()`](https://jkylearmstrong.github.io/TempleCBE/reference/package_deliverables.md)
    collects rendered reports (PDF/DOCX/HTML) and data deliverables
    across compute graph stages into structured delivery ZIP archives.
  - New
    [`audit_report_deliverables()`](https://jkylearmstrong.github.io/TempleCBE/reference/audit_report_deliverables.md)
    audits source scripts for referenced deliverable tokens and verifies
    on-disk existence.

### Vendored `renv/activate.R` updated

- Picks up upstream renv’s fix for a bootstrap crash: a missing or
  corrupt downloaded archive during renv’s own first-run self-install
  used to abort with a low-level connection error instead of failing
  gracefully. See
  [rstudio/renv@532d48d](https://github.com/rstudio/renv/commit/532d48d6303d88900aa11aac3a0a7f339466156d).

### `proc_pca()` accepts raw data

- [`proc_pca()`](https://jkylearmstrong.github.io/TempleCBE/reference/proc_pca.md)
  no longer requires a pre-fitted `prcomp` object. Its argument is now
  `data`, which can be either a `prcomp` object or a numeric matrix/data
  frame; when given raw data, it fits the PCA itself via
  [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html). New `center`
  and `scale` arguments (default `TRUE`) and `...` are passed through to
  [`prcomp()`](https://rdrr.io/r/stats/prcomp.html) in that case, and
  are ignored when `data` is already a `prcomp` object.

### `use_temple_brand()` warns off-root installs; `create_report()` finds a root install

- [`use_temple_brand()`](https://jkylearmstrong.github.io/TempleCBE/reference/use_temple_brand.md)
  gets a `check_root` argument (default `TRUE`): it now warns when
  `path` isn’t the project root found by
  [`here::here()`](https://here.r-lib.org/reference/here.html), since
  installing the extension into each report’s own subfolder instead of
  once at the root creates a separate, driftable `_extensions` copy per
  report. Pass `check_root = FALSE` to install into a subfolder without
  the warning (what `create_report(..., install_brand = TRUE)` does
  internally, since that’s a deliberate one-report install).
- [`create_report()`](https://jkylearmstrong.github.io/TempleCBE/reference/create_report.md)’s
  “install the extension” message no longer fires for a `"temple"`
  report created under a project that already has the extension
  installed at an ancestor directory (matching how Quarto itself
  resolves `_extensions` from any project subfolder) — previously it
  only checked `location` itself.

### `create_report()` gets a `filename` argument

- New `filename` argument names the report file independently of
  `template_name`. Previously, two reports in the same `location`
  (e.g. `analysis/analysis1.qmd` and `analysis/analysis2.qmd`) both
  defaulted to `<template_name>.qmd`, so the second
  [`create_report()`](https://jkylearmstrong.github.io/TempleCBE/reference/create_report.md)
  call silently overwrote the first.
  [`create_report()`](https://jkylearmstrong.github.io/TempleCBE/reference/create_report.md)
  now warns before overwriting an existing report file, and `filename`
  lets each report keep its own name:
  `create_report("analysis", filename = "analysis1")`,
  `create_report("analysis", filename = "analysis2")`.

### Penalized Cox models for start/stop survival data

- New
  [`coxnet()`](https://jkylearmstrong.github.io/TempleCBE/reference/coxnet.md)
  fits an elastic-net Cox model with glmnet through the tidymodels
  hardhat interface: formula, recipe, or predictors and outcome. The
  outcome can be right-censored, `Surv(time, event)`, or start/stop,
  `Surv(start, stop, event)`.
  [`predict()`](https://rdrr.io/r/stats/predict.html) returns
  `.pred_linear_pred` or a `.pred` list-column of survival probabilities
  (Breslow baseline hazard), and
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) returns
  every coefficient.
- New
  [`cv_coxnet()`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md)
  is a tidymodels counterpart to
  [`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html).
  Folds are grouped by `subject_id` (or a coarser `group`, such as
  site), preprocessing from a recipe is learned inside each fold, and
  every `mixture` and `penalty` is scored with a yardstick metric set:
  by default the integrated Brier score, concordance, and the
  time-specific Brier score and ROC AUC. It reports `lambda.min` and
  `lambda.1se` for the chosen metric, and has
  [`predict()`](https://rdrr.io/r/stats/predict.html),
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
  and
  [`tune::collect_metrics()`](https://tune.tidymodels.org/reference/collect_predictions.html)
  methods. Bootstrap resamples, which repeat subjects, are scored with
  each copy as its own subject.
- New
  [`nested_cv_coxnet()`](https://jkylearmstrong.github.io/TempleCBE/reference/nested_cv_coxnet.md)
  runs
  [`cv_coxnet()`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md)
  on the inner resamples of an
  [`rsample::nested_cv()`](https://rsample.tidymodels.org/reference/nested_cv.html)
  object, refits on each outer analysis set, and scores the outer
  assessment set.
- New
  [`surv_subject_truth()`](https://jkylearmstrong.github.io/TempleCBE/reference/surv_subject_truth.md),
  [`censoring_km()`](https://jkylearmstrong.github.io/TempleCBE/reference/censoring_km.md),
  [`graf_weights()`](https://jkylearmstrong.github.io/TempleCBE/reference/graf_weights.md),
  and
  [`add_graf_weights()`](https://jkylearmstrong.github.io/TempleCBE/reference/add_graf_weights.md)
  collapse start/stop outcomes to one row per subject and add
  inverse-probability-of-censoring (Graf) weights, so any model’s
  survival predictions can be scored with yardstick. Given start/stop
  truth directly, yardstick returns numbers without complaint, but they
  count every interval as a subject.
- `hardhat` and `generics` added to Imports.

### `glmnet_IBS()` rebuilt on `cv_coxnet()` (breaking)

- **Results change.**
  [`glmnet_IBS()`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md),
  [`tune_over_alpha()`](https://jkylearmstrong.github.io/TempleCBE/reference/tune_over_alpha.md),
  and
  [`summarize_tune_results()`](https://jkylearmstrong.github.io/TempleCBE/reference/summarize_tune_results.md)
  keep their arguments, but
  [`glmnet_IBS()`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md)
  now uses
  [`cv_coxnet()`](https://jkylearmstrong.github.io/TempleCBE/reference/cv_coxnet.md):
  - The penalty is chosen by the integrated Brier score (or `metric =`)
    on folds grouped by subject. `cv.glmnet()` chose it by concordance
    on folds of rows, which put a subject’s intervals in both analysis
    and assessment sets.
  - Survival is predicted from the model’s own Breslow baseline hazard
    along each subject’s covariate path. Previously it was a null
    model’s hazard times a relative risk re-centred on the assessment
    set.
  - Missing relative risks were filled by averaging with the previous
    row, which could belong to a different subject. That code is gone.
- `censoring_weights = "none"` is removed and now errors: it scored
  interval rows without censoring weights, so it was not a proper Brier
  score. Install TempleCBE 0.2.0 to reproduce results that used it.
- A failed fit returns `IBS = NA` (was 2) with a warning giving the
  reason; `failure_ibs` still sets the value.
- Output has one row per feature, including coefficients the penalty set
  to zero.
- New arguments: `eval_time`, `metric`, `rule` (`"min"` or `"1se"`), and
  `covariates` (`"path"` or `"baseline"`). `type.measure = "C"` is
  deprecated in favour of `metric = "concordance_survival"`, and
  `parallel` is ignored.
- glmnet’s `cox.ties` defaults to `"breslow"`, matching the baseline
  hazard, so results don’t change with glmnet 5.1’s switch to Efron.

### `step_famd()` fixes (breaking)

- `num_comp` was capped at the number of selected variables, but FAMD
  has more dimensions when categorical variables have several levels
  (numeric variables plus one fewer than the number of levels, per
  categorical variable). Components beyond the variable count were
  silently dropped, and `threshold` only chose among the first few, so a
  99% threshold could keep components covering far less. Both now use
  all of FAMD’s dimensions.
- Without FactoMineR installed,
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html)
  silently ran a PCA on the numeric variables alone. It now asks for
  FactoMineR.
- New components are named `FAMD1`, `FAMD2`, …
  ([`recipes::names0()`](https://recipes.tidymodels.org/reference/names0.html),
  zero-padded from 10 components) instead of `PC1`, `PC2`, …, so
  [`step_famd()`](https://jkylearmstrong.github.io/TempleCBE/reference/step_famd.md)
  and
  [`step_pca()`](https://recipes.tidymodels.org/reference/step_pca.html)
  can share a recipe. A name that already exists in the data is an error
  rather than a duplicate column.
- Character and logical variables are treated as factors, with levels
  learned by
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html).
  Categories unseen in training and missing values are informative
  errors.
- Frequency weights are passed to FAMD as row weights (importance
  weights are ignored, as in
  [`step_pca()`](https://recipes.tidymodels.org/reference/step_pca.html)),
  and `tidy(type = "variance")` reports component variances as for
  [`step_pca()`](https://recipes.tidymodels.org/reference/step_pca.html).
- New
  [`required_pkgs()`](https://generics.r-lib.org/reference/required_pkgs.html)
  method, so tidymodels loads FactoMineR and TempleCBE on parallel
  workers.
- The README example selected only numeric predictors (iris with
  `Species` as the outcome), which FAMD rejects; it now uses `Species`
  as a predictor.

### Other fixes

- `plot.prcomp()` is removed. It replaced ’
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
  `prcomp` objects for anyone who loaded TempleCBE. Use the new
  `pca_plot(pca_model, type = )`, which also no longer mistakes an `x =`
  component argument for the PCA fit.
- [`pdf_to_rtf()`](https://jkylearmstrong.github.io/TempleCBE/reference/pdf_to_rtf.md)
  wrote page breaks as the literal text `\page`; they are now real page
  breaks. Non-ASCII characters are written as RTF Unicode escapes. The
  arguments are now `pdf` and `rtf` (defaulting to `pdf` with an `.rtf`
  extension), with new `font_size` and `overwrite`.
- [`is_normal()`](https://jkylearmstrong.github.io/TempleCBE/reference/is_normal.md)
  used a Kolmogorov-Smirnov test with the mean and standard deviation
  estimated from the same data, which gives p-values that are far too
  large. It now uses the Lilliefors test
  ([`nortest::lillie.test()`](https://rdrr.io/pkg/nortest/man/lillie.test.html),
  added to Imports). Above 5000 values it no longer runs Shapiro-Wilk on
  a random subsample, so results are deterministic. New `alpha`
  argument.
- Minimum versions now match the features used: ggplot2 \>= 3.5.0 (the
  Temple scales omit `scale_name`), ggridges \>= 0.5.0, pdftools \>=
  2.0, scales \>= 0.5.0, hardhat \>= 1.3.0, and in Suggests furrr \>=
  0.2.0, missRanger \>= 2.4.0, quarto \>= 1.4, rsample \>= 1.1.0,
  testthat \>= 3.1.7, withr \>= 2.3.0, workflowsets \>= 1.1.0, yardstick
  \>= 1.3.0, and zip \>= 2.3.0. `tools` is declared in Imports, and
  `dials` (used by
  [`step_famd()`](https://jkylearmstrong.github.io/TempleCBE/reference/step_famd.md)’s
  [`tunable()`](https://generics.r-lib.org/reference/tunable.html)
  method) in Suggests.

### `write_xlsx()` re-exported

- [`write_xlsx()`](https://docs.ropensci.org/writexl//reference/write_xlsx.html)
  is re-exported from `writexl`, so
  [`TempleCBE::write_xlsx()`](https://docs.ropensci.org/writexl//reference/write_xlsx.html)
  works. Analysis code already calls it that way, but it previously
  failed with “‘write_xlsx’ is not an exported object”. `writexl` moves
  from Suggests to Imports.

### Temple brand

- New
  [`temple_colors()`](https://jkylearmstrong.github.io/TempleCBE/reference/temple_colors.md),
  [`temple_pal()`](https://jkylearmstrong.github.io/TempleCBE/reference/temple_pal.md),
  [`scale_colour_temple()`](https://jkylearmstrong.github.io/TempleCBE/reference/scale_colour_temple.md)/[`scale_color_temple()`](https://jkylearmstrong.github.io/TempleCBE/reference/scale_colour_temple.md)/[`scale_fill_temple()`](https://jkylearmstrong.github.io/TempleCBE/reference/scale_colour_temple.md),
  and
  [`theme_temple()`](https://jkylearmstrong.github.io/TempleCBE/reference/theme_temple.md)
  draw R graphics in the Temple University palette used by the
  [quarto_temple_brand](https://github.com/jkylearmstrong-temple/quarto_temple_brand)
  Quarto extension.
  [`temple_brand_path()`](https://jkylearmstrong.github.io/TempleCBE/reference/temple_brand_path.md)
  returns a bundled copy of its `brand.yml`, for
  [`quarto::theme_brand_ggplot2()`](https://quarto-dev.github.io/quarto-r/reference/theme_helpers.html)
  or `bslib::bs_theme(brand = )`.
- New
  [`use_temple_brand()`](https://jkylearmstrong.github.io/TempleCBE/reference/use_temple_brand.md)
  installs that extension (from
  `jkylearmstrong-temple/quarto_temple_brand` by default) into a Quarto
  project, creating `_quarto.yml` if needed, which enables the
  `temple-html`, `temple-pdf` (LaTeX title page), `temple-typst`, and
  `temple-revealjs` formats.
- `create_report(template_name = "temple")` scaffolds a report in those
  formats; `install_brand = TRUE` also installs the extension.
- Plots use the Temple palette. Diverging heatmaps
  ([`correlation_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot.md),
  [`correlation_diff_heatmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_diff_heatmap.md),
  [`pca_feature_loading_heatmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_feature_loading_heatmap.md),
  [`pca_loading_diff_heatmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_loading_diff_heatmap.md))
  run Night Owl-white-cherry instead of blue-white-red;
  [`missmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/missmap.md)
  counts,
  [`plot_features_percent_miss()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_features_percent_miss.md),
  [`plot_pca_bi()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_pca_bi.md)/[`pca_biplot()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_biplot.md)
  loadings,
  [`pca_percent_var_explained()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_percent_var_explained.md),
  and the reference lines of
  [`distribution_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/distribution_plot.md),
  [`manhattan_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/manhattan_plot.md),
  and
  [`volcano_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/volcano_plot.md)
  use Temple colors. Only colors change.
- The palette follows Temple’s current brand
  (<https://liberalarts.temple.edu/marcom/logos-and-brand>), matching
  quarto_temple_brand: cherry, white, and black (`#000000`); Clear Skies
  and Book Nook; formal accents `academic-gold`, `diamond-acres`,
  `founders-garden`, `night-owl`; casual accents `owls-eye`,
  `conwell-blue`, `upward-momentum`, `cherry-blossom`. The earlier names
  (`taupe`, `icy-blue`, `lime`, `eggshell`, `ochre`, `geranium`,
  `dark-blue`) are gone. The `"main"` palette is cherry, Night Owl,
  Owl’s Eye, Founder’s Garden, Upward Momentum, Diamond Acres, black;
  `"sequential"` runs Book Nook to cherry.
- Links in the bundled `brand.yml` are standard blue (`#0563c1`) rather
  than cherry, matching quarto_temple_brand. The brand guide sets no
  link color, and red links read as errors, especially in print.

### `zip_render()` fixes

- Extension formats such as `titlepage-pdf` or `temple-pdf` weren’t
  matched to their output file, so it was silently left out of the zip.
  They now resolve to their base format’s extension.
- `_quarto.yml`, `_brand.yml`, `_variables.yml`, and `_extensions/` are
  copied into the build directory, so documents that use a project,
  brand, or extension format render there as they do in place. With
  `include_sources = TRUE` they are zipped under their relative paths.

### `zip_reports()` fix

- Reports that share the same source stem (for example
  `analysis1/analysis.qmd` and `analysis2/analysis.qmd`) no longer
  overwrite each other inside staged `pdf/`, `docx/`, or `html/`
  folders.
  [`zip_reports()`](https://jkylearmstrong.github.io/TempleCBE/reference/zip_reports.md)
  now disambiguates staged output names while keeping index links
  aligned with the copied files.

## TempleCBE 0.2.0

### `glmnet_IBS()` rebuilt for start/stop survival data (breaking)

- **Breaking change.**
  [`glmnet_IBS()`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md)
  is now a port of the penalized-Cox tuning code it was originally meant
  to replace, generalized so no column names are hard-coded. The
  previous version scored plain `time`/`status` data with its own IPCW
  Brier score, treated every start/stop row as an independent subject,
  used every numeric column (including identifiers) as a predictor, and
  returned only `IBS`, `lambda`, and `alpha` – none of which fit
  repeated-measures data. The new signature takes an `rsplit`, an
  unprepped `recipe` (prepped inside the fold), `feature_names`, a
  `time_data` grid, and `id_col`/`start_col`/`stop_col`/`status_col`,
  and returns `IBS`, `lambda`, `term`, `estimate`, and `alpha` (one row
  per coefficient at `lambda.min`), with `IBS = failure_ibs` (default 2)
  when `cv.glmnet()` cannot fit.
- `censoring_weights = "none"` (default) reproduces the ported code:
  interval rows scored with censoring weight 1.
  `censoring_weights = "ipcw"` scores one row per subject with
  inverse-probability-of-censoring weights (Graf et al., 1999) from the
  analysis-set censoring distribution. The two give different numbers;
  compare within one setting.
- New
  [`tune_over_alpha()`](https://jkylearmstrong.github.io/TempleCBE/reference/tune_over_alpha.md)
  and
  [`summarize_tune_results()`](https://jkylearmstrong.github.io/TempleCBE/reference/summarize_tune_results.md)
  tune
  [`glmnet_IBS()`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md)
  over an `alpha` grid for one split and for every split of a resample.
  Neither calls
  [`future::plan()`](https://future.futureverse.org/reference/plan.html).
  With `formulas`, they instead fit one model per candidate feature set,
  each with its own (given or randomly drawn) `alpha`, and add a
  `formula` column.
- [`glmnet_IBS()`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md)
  accepts `feature_names` as a function of the baked analysis set, for
  recipes whose output columns vary by fold
  (e.g. `step_pca(threshold = )`).
- Bugs fixed relative to the ported code: the `alpha` grid hard-coded 6
  fixed values, so it produced `num_alpha_values + 1` values when
  `num_fixed = 6` and overwrote fixed values otherwise – it now has
  exactly `num_alpha_values`; the outer map over splits drew random
  `alpha` values in workers without a seed, so grids were not
  reproducible – both maps now run with `furrr_options(seed = TRUE)`;
  filling a missing relative risk looped forever for a subject with no
  known value – it now errors naming the subject.
- The internal `ipcw_brier_score()`/`integrate_brier_score()` helpers of
  the old implementation are removed; scoring now goes through
  [`yardstick::brier_survival_integrated()`](https://yardstick.tidymodels.org/reference/brier_survival_integrated.html).

### Other new functions

- [`get_model_parameters()`](https://jkylearmstrong.github.io/TempleCBE/reference/get_model_parameters.md)
  returns the preprocessor, model, and best tuning parameters of the
  workflow ranked `.rank` in tuned workflow set results;
  [`fit_n_rank()`](https://jkylearmstrong.github.io/TempleCBE/reference/fit_n_rank.md)
  also fits it with
  [`tune::fit_best()`](https://tune.tidymodels.org/reference/fit_best.html).
  Fixed while porting: with `group_wflow = FALSE`, the ranked
  configuration was reported but the workflow’s *best* configuration was
  fitted; and the fit used `fit_best()`’s default metric rather than
  `rank_metric`.
- [`km_summary_to_prism()`](https://jkylearmstrong.github.io/TempleCBE/reference/km_summary_to_prism.md)
  expands a Kaplan-Meier summary-by-time table into a GraphPad Prism
  survival table. Fixed while porting: `strata_levels` was documented
  but ignored, and `validate_totals` failed when `strata_levels` was
  set.
- [`convert_pdf_to_docx()`](https://jkylearmstrong.github.io/TempleCBE/reference/convert_pdf_to_docx.md),
  [`convert_pdfs_to_docx()`](https://jkylearmstrong.github.io/TempleCBE/reference/convert_pdfs_to_docx.md),
  [`check_docx_toolchain()`](https://jkylearmstrong.github.io/TempleCBE/reference/check_docx_toolchain.md),
  [`find_python()`](https://jkylearmstrong.github.io/TempleCBE/reference/find_python.md),
  and
  [`find_soffice()`](https://jkylearmstrong.github.io/TempleCBE/reference/find_soffice.md)
  convert PDFs to DOCX via `pdf2docx`, LibreOffice, or Word COM
  (Windows), with verified backend discovery. `convert_pdf_to_docx` fits
  `zip_reports(docx_from_pdf = )`. Pinned Python requirements ship in
  `inst/python/requirements.txt`. Interpreters are configured with
  `options(templecbe.python)`/`TEMPLECBE_PYTHON` and
  `options(templecbe.soffice)`/`TEMPLECBE_SOFFICE`. The Word COM
  subprocess now runs the calling session’s own `Rscript` rather than
  the first one on `PATH`.
- [`run_sas_script()`](https://jkylearmstrong.github.io/TempleCBE/reference/run_sas_script.md)
  runs a SAS program in batch mode with its log and listing in separate
  folders;
  [`find_sas()`](https://jkylearmstrong.github.io/TempleCBE/reference/find_sas.md)
  locates the executable (`options(templecbe.sas)`, `SAS_EXE`, `PATH`,
  or the default install locations).
- [`normalize_safely()`](https://jkylearmstrong.github.io/TempleCBE/reference/normalize_safely.md),
  [`parse_here_call_vec()`](https://jkylearmstrong.github.io/TempleCBE/reference/parse_here_call_vec.md),
  [`file_meta_fs()`](https://jkylearmstrong.github.io/TempleCBE/reference/file_meta_fs.md),
  [`extract_win_posix_paths()`](https://jkylearmstrong.github.io/TempleCBE/reference/extract_win_posix_paths.md),
  and
  [`extract_all_xlsx_tokens()`](https://jkylearmstrong.github.io/TempleCBE/reference/extract_all_xlsx_tokens.md)
  are exported: the path helpers behind
  [`scan_data_io()`](https://jkylearmstrong.github.io/TempleCBE/reference/scan_data_io.md),
  for code that audits file paths itself.
- [`profvis_summary()`](https://jkylearmstrong.github.io/TempleCBE/reference/profvis_summary.md)
  tabulates a `profvis` profile by function: memory, memory increments,
  call counts, stack depth, and memory over time.
- `parsnip`, `profvis`, `reticulate`, `tune`, `workflows`,
  `workflowsets`, and `yardstick` added to Suggests.

### CI and packaging fixes

- The “Nested Cross-Validation for Longitudinal Survival Models”
  vignette uses the new
  [`glmnet_IBS()`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md)
  arguments (`recipe`, `feature_names`, `time_data`, `id_col`) and shows
  both censoring weightings; it no longer built against 0.2.0.
- [`normalize_safely()`](https://jkylearmstrong.github.io/TempleCBE/reference/normalize_safely.md)
  returns forward slashes on every platform, consistent with
  [`scan_data_io()`](https://jkylearmstrong.github.io/TempleCBE/reference/scan_data_io.md).
- [`scan_data_io()`](https://jkylearmstrong.github.io/TempleCBE/reference/scan_data_io.md)
  documents `max_depth`,
  [`zip_render()`](https://jkylearmstrong.github.io/TempleCBE/reference/zip_render.md)’s
  documentation is regenerated to match its code, and `yaml` (used by
  [`zip_render()`](https://jkylearmstrong.github.io/TempleCBE/reference/zip_render.md))
  is declared in Suggests. These were the two `R CMD check` warnings on
  `master`.
- The pkgdown reference index lists every exported topic, adding the
  `mtry` sweeps,
  [`render_me()`](https://jkylearmstrong.github.io/TempleCBE/reference/render_me.md),
  [`read_search()`](https://jkylearmstrong.github.io/TempleCBE/reference/read_search.md),
  [`write_search()`](https://jkylearmstrong.github.io/TempleCBE/reference/write_search.md),
  [`scan_data_io()`](https://jkylearmstrong.github.io/TempleCBE/reference/scan_data_io.md),
  and
  [`zip_reports()`](https://jkylearmstrong.github.io/TempleCBE/reference/zip_reports.md).
- The Docker image installs `libuv1-dev`, which `fs` needs at load time.

## TempleCBE 0.1.8

### New `mtry`-sweep imputation ([\#3](https://github.com/jkylearmstrong/TempleCBE/issues/3))

- [`missforest_sweep_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missforest_sweep_mtry.md)
  sweeps `mtry` for `missForest`, scores every column by its out-of-bag
  error, and assembles each column from whichever run imputed it best.
  [`missforest_oob_by_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missforest_oob_by_mtry.md)
  (one fit, tidy per-column OOB table) and
  [`missforest_impute_by_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missforest_impute_by_mtry.md)
  (assemble columns from their winning runs) are exported as the
  building blocks. This consolidates three copies that had drifted apart
  in analysis code; their differences are now arguments (`exclude` for
  identifier/time columns) or documented behavior (character-to-factor
  coercion inside the worker, original column order restored).
- [`missranger_sweep_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missranger_sweep_mtry.md),
  [`missranger_oob_by_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missranger_oob_by_mtry.md),
  and
  [`missranger_max_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missranger_max_mtry.md)
  run the same sweep on the `missRanger` engine with the same return
  shape.
  [`missranger_max_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missranger_max_mtry.md)
  computes the largest `mtry` `missRanger` admits, which is bounded by
  the number of complete columns rather than `ncol - 1`. Errors are not
  comparable across engines – compare `mtry` within an engine only.
- Bugs fixed relative to the copies this replaces: runs were looked up
  by position (`sweep[[mtry]]`), which is only correct when the grid is
  exactly `1:n`; seeding passed to `future::plan(.options = ...)` was
  ignored, so most sweeps were never seeded – the seed now reaches
  [`furrr::future_map()`](https://furrr.futureverse.org/reference/future_map.html)’s
  own `.options`; all-`NA` columns, which `missForest` silently drops
  and `missRanger` silently leaves `NA`, are now refused by name.
- `max_pct_missing` holds out columns missing more than a given share,
  carries them through unimputed, and reports them in
  `excluded_high_missing`. Defaults to `NULL` (impute everything).
- Neither sweep calls
  [`future::plan()`](https://future.futureverse.org/reference/plan.html);
  the caller’s backend is respected. `missForest`, `missRanger`, and
  `pkgload` added to Suggests.

### `corr_test_all()` output options ([\#4](https://github.com/jkylearmstrong/TempleCBE/issues/4))

- `columns = "tidy"` returns every
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  column of each [`cor.test()`](https://rdrr.io/r/stats/cor.test.html)
  (estimate renamed `cor`), including the statistic, degrees of freedom,
  and confidence limits. The default `"compact"` output (`var1`, `var2`,
  `r`, `p_value`) is unchanged.
- `sort` chooses `"p_value"` (default), `"estimate"`, `"abs_estimate"`,
  or `"none"`.
- `...` is passed to
  [`cor.test()`](https://rdrr.io/r/stats/cor.test.html) (`alternative`,
  `conf.level`, `exact`).
- `use = "complete.obs"` now tests every pair on the same rows. `use`
  was previously accepted but had no effect on the tests; unsupported
  values now error.
- **Behavior change:** pairs are enumerated in column order, so `var1`
  is the column that appears first in `data`. Previously it was
  whichever name sorted first under the locale’s collation. Values are
  unchanged; only a pair’s orientation and tie order can differ.

### New reporting utilities ([\#1](https://github.com/jkylearmstrong/TempleCBE/issues/1))

- [`render_me()`](https://jkylearmstrong.github.io/TempleCBE/reference/render_me.md)
  renders Quarto documents, optionally in parallel
  (`future`/`furrr`/`quarto` in Suggests).
- [`read_search()`](https://jkylearmstrong.github.io/TempleCBE/reference/read_search.md)
  /
  [`write_search()`](https://jkylearmstrong.github.io/TempleCBE/reference/write_search.md)
  locate read and write calls in code.
- [`zip_reports()`](https://jkylearmstrong.github.io/TempleCBE/reference/zip_reports.md)
  packages already-rendered reports and a data folder into one indexed,
  hyperlinked zip, given a plain ordered data frame. DOCX generation is
  a caller-supplied `docx_from_pdf()` callback.
- [`scan_data_io()`](https://jkylearmstrong.github.io/TempleCBE/reference/scan_data_io.md)
  cross-references read/write calls in code against files on disk, for
  any file extension and project root. Fixed while porting: the
  full-path regex could never match, so full-path resolution silently
  found nothing. Also fixed after the port: inconsistent result schema
  on
  [`render_me()`](https://jkylearmstrong.github.io/TempleCBE/reference/render_me.md)’s
  parallel path, the path separator on non-Windows platforms, and
  [`scan_data_io()`](https://jkylearmstrong.github.io/TempleCBE/reference/scan_data_io.md)
  failing on paths with repeated separators.

### Fixes

- [`get_dataset_info()`](https://jkylearmstrong.github.io/TempleCBE/reference/get_dataset_info.md)
  handles [`survival::Surv`](https://rdrr.io/pkg/survival/man/Surv.html)
  columns ([\#2](https://github.com/jkylearmstrong/TempleCBE/issues/2)).
  `Surv` objects are numeric matrices, so they were summarized as one
  flattened mean/SD of time and status together, and
  [`dplyr::n_distinct()`](https://dplyr.tidyverse.org/reference/n_distinct.html)
  recursed infinitely on them. They are now summarized from their
  time/status columns. Variable labels also fall back to
  `attr(x, "label")` when
  [`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html)
  finds none.
- Example templates in `inst/templates/` generate synthetic data inline
  and no longer depend on private internal datasets; the bundled example
  PDFs were re-rendered from them.

## TempleCBE 0.1.7

### `correlation_plot_split()` crash fix

- Found by a real render, not by the existing test suite:
  [`correlation_plot_split()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot_split.md)’s
  hierarchical clustering, cut at a fixed
  `k = ceiling(n_vars / group_size)`, can leave a cluster with just one
  variable in it – confirmed with a real 7-variable dataset at
  `group_size = 6`. A “group” of one variable has no pairwise
  correlation to show, and a 1x1 correlation matrix crashes
  `corrplot()`’s default `order = "FPC"` ordering downstream
  (`eigen(corr)$vectors[, 1:2]`: subscript out of bounds – a 1x1
  matrix’s [`eigen()`](https://rdrr.io/r/base/eigen.html) has no second
  eigenvector to index). Added `merge_singleton_groups()`, an internal
  helper that folds any singleton cluster into whichever other group its
  variable is most correlated with on average (in absolute value), so
  [`correlation_plot_split()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot_split.md)
  never hands
  [`correlation_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot.md)
  a group of one. Verified against 15 random variable counts (5-9) with
  no errors and no singleton groups produced.
- [`correlation_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot.md)
  itself now errors clearly (“requires at least 2 numeric columns”) on
  single-column input, instead of failing inside `corrplot()`’s
  internals – defense in depth for any direct caller, not just calls
  routed through
  [`correlation_plot_split()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot_split.md).

## TempleCBE 0.1.6

### Docker/`renv` reproducibility fix

- `Dockerfile` previously ignored the committed `renv.lock` entirely:
  `remotes::install_deps()` resolved TempleCBE’s declared `DESCRIPTION`
  dependencies against whatever versions happened to be current on live
  CRAN/r-universe at build time, so the exact package versions baked
  into a Docker image could silently drift from what
  [`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)
  installs on a Windows dev machine against the pinned lockfile –
  exactly the reproducibility gap `renv` exists to close. The image now
  installs a pinned `renv` (version tracked via a new `RENV_VERSION`
  build arg, matching the existing `R_VERSION`/`QUARTO_VERSION` arg
  convention) and runs `renv::restore(prompt = FALSE)` against the
  committed `renv.lock`, `.Rprofile`, and
  `renv/activate.R`/`renv/settings.json`, so Docker builds and local
  [`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)
  on Windows now install identical dependency versions. These are still
  copied in ahead of the rest of the source tree (as `DESCRIPTION` was
  previously), so the restore layer only invalidates when the lockfile
  itself changes, not on every source commit. The final package install
  also switched from `R CMD INSTALL` to `renv::install(".")`, since
  plain `R CMD INSTALL` doesn’t source `.Rprofile` and so can’t see the
  renv-managed library
  [`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)
  populated – it failed to find `ggplot2`/`corrplot`/etc. even though
  they were installed correctly.
  [`renv::install()`](https://rstudio.github.io/renv/reference/install.html)
  runs inside the same renv-activated session, avoiding that mismatch.
  (One caveat found while verifying the built image:
  [`renv::status()`](https://rstudio.github.io/renv/reference/status.html)
  still reports R’s own bundled “recommended” packages – `survival`,
  `MASS`, `Matrix`, etc. – as out of sync with the lockfile inside the
  container, because renv deliberately avoids overwriting a base R
  installation’s own recommended-package versions. This is expected
  `renv` behavior rather than a gap introduced here, doesn’t affect any
  of TempleCBE’s own dependencies, and the built image was confirmed to
  load and run TempleCBE correctly.)
- [`renv::snapshot()`](https://rstudio.github.io/renv/reference/snapshot.html)
  was re-run to confirm the lockfile is current after the 0.1.5
  correlation-plot changes; those changes only used already-imported
  packages (`stats`, `ggplot2`, `corrplot`, `dplyr`, `tibble`), so no
  package versions needed updating – `renv.lock` is unchanged.

## TempleCBE 0.1.5

### `correlation_plot()` rendering fixes

- `corrplot()` was never given any top margin, so `title` collided with
  the 45-degree diagonal variable-name labels sitting just below it in
  every rendered plot. Added a `mar` argument (default `c(0, 0, 2, 0)`,
  the standard `par("mar")` `c(bottom, left, top, right)` form that
  `corrplot()` already accepts) so the title clears the labels by
  default, while still letting callers override it for longer titles or
  larger `tl.cex`.
- Coefficient numbers were hardcoded on (`addCoef.col = "black"`) with
  no clean way to turn them off. On a correlation matrix with many
  variables the numbers overlap the ellipses and labels; the only
  workaround was shrinking `tl.cex`/`number.cex` toward zero, which
  doesn’t fix the crowding – it just deletes every label, leaving an
  unreadable, unlabeled plot. Added a `show_coef = TRUE` argument;
  setting it to `FALSE` omits the coefficients cleanly while keeping the
  diagonal variable labels intact. The default is unchanged, so existing
  small-matrix callers see no behavior difference.

### New correlation functions

- [`correlation_plot_split()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot_split.md):
  for a correlation matrix with too many variables to stay legible in
  one
  [`correlation_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot.md)
  call (e.g. ~40 clinical parameters), automatically groups variables
  via hierarchical clustering on `as.dist(1 - abs(cor_mat))` – the same
  correlation-based distance `corrplot`’s own `order = "hclust"` uses –
  and draws one within-group
  [`correlation_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot.md)-style
  plot per group (default target size 12 variables per group, via
  `ceiling(n_vars / group_size)` groups from
  [`stats::cutree()`](https://rdrr.io/r/stats/cutree.html)). Each
  sub-plot’s title is suffixed `"(Group i of n)"` so the sub-plots can
  be told apart. Returns the per-group correlation matrices invisibly,
  as a named list, since it is called (like
  [`correlation_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot.md))
  for its plotting side effect.
- [`correlation_diff()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_diff.md)
  /
  [`correlation_diff_heatmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_diff_heatmap.md):
  compare the correlation matrix of a comparison dataset against a
  baseline dataset, matching numeric variables by column name (falling
  back to the intersection if the two datasets’ numeric columns differ).
  Unlike
  [`pca_loading_diff()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_loading_diff.md),
  no sign-alignment step is needed – correlation coefficients, unlike
  PCA loadings, have no sign ambiguity. Returns/renders only one
  triangle of the (symmetric) difference matrix, with the (always-zero)
  diagonal dropped.
  [`correlation_diff_heatmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_diff_heatmap.md)
  uses the same diverging, zero-centered fill scale as
  [`pca_loading_diff_heatmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_loading_diff_heatmap.md).

## TempleCBE 0.1.4

### New PCA functions

- [`pca_biplot()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_biplot.md):
  a real PCA loadings biplot. Unlike
  [`plot_pca_bi()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_pca_bi.md)
  (which draws each *observation* as an arrow to its PC score, labeled
  by an id column),
  [`pca_biplot()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_biplot.md)
  draws the observation scores as a muted point cloud and overlays the
  variable loading vectors (from `pca_model$rotation`) as labeled arrows
  from the origin – the classic two-panel-in-one biplot. Loadings are
  rescaled so their max extent is 80% of the score cloud’s max extent,
  since raw (unit-scale) loadings would otherwise be invisible next to
  the scores. Works directly off a fitted `prcomp` object; no `newdata`
  argument needed. Added as a new `type = "biplot"` option in
  `plot.prcomp()`.
- [`pca_loading_diff()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_loading_diff.md):
  compares variable loadings between two independently-fit `prcomp`
  objects on the same variables (e.g. the same domain at baseline vs. a
  later timepoint). Handles PCA’s arbitrary component sign by
  sign-aligning each shared component of the comparison fit to the
  baseline before differencing, so a component that’s merely flipped
  (not truly changed) reads as ~0 difference instead of a spurious ~2x
  jump. Matches variables by name and falls back to the intersection if
  the two fits’ variable sets differ.
- [`pca_loading_diff_heatmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_loading_diff_heatmap.md):
  renders
  [`pca_loading_diff()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_loading_diff.md)’s
  output as a feature-by-component heatmap with a diverging,
  zero-centered fill scale, matching
  [`pca_feature_loading_heatmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_feature_loading_heatmap.md)’s
  visual style.

### Styling

- [`pca_percent_var_explained()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_percent_var_explained.md):
  tightened the top margin above the variance bars by adding
  `expand = ggplot2::expansion(mult = c(0, 0.01))` to the
  percent-of-variance y scale.

## TempleCBE 0.1.3

### `missmap()` improvements

- `by_column` mode now respects the `row_order` argument: when
  `row_order = FALSE` (default), groups (x-axis) and features (y-axis)
  are each ordered by descending total missingness, matching the
  ordering already applied in the default per-row/column view.
  Previously features stayed in whatever order `pivot_longer()` produced
  (alphabetical), ignoring `row_order` entirely.
- `by_column` mode now auto-detects when the aggregated missingness is
  effectively binary – i.e. every group has at most one contributing row
  (checked from actual group sizes via
  [`dplyr::n()`](https://dplyr.tidyverse.org/reference/context.html),
  not just the resulting sums) – and in that case renders with the same
  discrete “Missing”/“Present” two-level fill and “Data Status” legend
  used in the default view, instead of a continuous black-to-red “#
  missing” gradient that is misleading when every value is 0 or 1
  (e.g. `by_column` set to a unique subject/site id with one row per
  group). Groups with more than one contributing row keep the existing
  continuous gradient, since a real count is meaningful there
  (e.g. multiple readings per site over time).
- Added a `fill = c("auto", "binary", "count")` argument to
  [`missmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/missmap.md)
  to override the auto-detected fill behavior explicitly when needed.

## TempleCBE 0.1.2

### Statistical correctness fixes

- [`is_poisson()`](https://jkylearmstrong.github.io/TempleCBE/reference/is_poisson.md):
  the chi-squared branch’s `distribution.test` flag was inverted
  relative to every other test in the package (`p < 0.1` was mislabeled
  as “looks Poisson”), and the test itself used a statistically invalid
  cross-tabulation instead of a real goodness-of-fit comparison.
  Replaced with a proper chi-squared goodness-of-fit test using
  quantile-based binning against the fitted Poisson distribution, with
  degrees of freedom correctly reduced for the estimated rate. Dropped
  the accompanying Kolmogorov-Smirnov test: KS assumes a continuous null
  distribution, and Poisson’s real point masses inflate the KS statistic
  regardless of true fit.
- [`is_normal()`](https://jkylearmstrong.github.io/TempleCBE/reference/is_normal.md):
  switched from comparing against a freshly simulated random sample
  (non-deterministic, added unnecessary noise) to a one-sample KS test
  against the fitted normal CDF directly.
- [`glmnet_IBS()`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md):
  the per-time-point Brier score was normalized by the sum of IPCW
  weights that happened to contribute, instead of the full test-set size
  — this double-counted the effect of exclusions and inflated the score.
  Fixed to follow the Graf et al. (1999) IPCW estimator exactly;
  refactored into standalone, independently-tested helpers.
- [`single_t_test()`](https://jkylearmstrong.github.io/TempleCBE/reference/single_t_test.md):
  `paired = TRUE` crashed unconditionally
  ([`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  doesn’t return `estimate1`/`estimate2` for a paired test) —
  fold-change is now computed directly from the group vectors. Also
  added an optional `.id` argument to pair observations by a
  subject/record identifier instead of by row order, which previously
  silently mismatched pairs unless the two groups were pre-sorted
  identically.

### `step_famd()` fixes

- `ncp` was never passed to
  [`FactoMineR::FAMD()`](https://rdrr.io/pkg/FactoMineR/man/FAMD.html),
  so every fit silently capped at FactoMineR’s default of 5 components
  regardless of `num_comp`.
- `threshold` (cumulative-variance component selection) was documented
  and tunable but had no effect; now implemented.
- `options` (extra arguments to
  [`FactoMineR::FAMD()`](https://rdrr.io/pkg/FactoMineR/man/FAMD.html))
  was documented but never forwarded; now implemented.
- `print.step_famd()` always printed an empty column list due to an
  incorrect [`names()`](https://rdrr.io/r/base/names.html) call; now
  uses
  [`recipes::print_step()`](https://recipes.tidymodels.org/reference/recipes-internal.html)
  like other recipe steps.
- [`tidy.step_famd()`](https://jkylearmstrong.github.io/TempleCBE/reference/step_famd.md)
  returned fabricated placeholder values (`value = 1.0`,
  `component = "PC1"` for every term) instead of real per-component
  loadings/contributions.
- `bake.step_famd()` silently returned the data unchanged if FactoMineR
  became unavailable after
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html); now
  errors with a clear message.
- Added a clear error when
  [`step_famd()`](https://jkylearmstrong.github.io/TempleCBE/reference/step_famd.md)
  is given only quantitative or only qualitative columns (FAMD requires
  mixed data).

### Other bug fixes

- [`get_dataset_info()`](https://jkylearmstrong.github.io/TempleCBE/reference/get_dataset_info.md)
  /
  [`proc_contents()`](https://jkylearmstrong.github.io/TempleCBE/reference/get_dataset_info.md):
  crashed on any all-`NA` column.
- [`create_toc_from_sas_pdf()`](https://jkylearmstrong.github.io/TempleCBE/reference/create_toc_from_sas_pdf.md):
  TOC page numbers drifted from the true PDF page as soon as any earlier
  page had no top-margin text.
- [`zip_render()`](https://jkylearmstrong.github.io/TempleCBE/reference/zip_render.md):
  the output-file glob was hardcoded to `html|pdf|docx`, silently
  dropping any other requested Quarto output format from the zip.
- [`plot_pca_bi()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_pca_bi.md):
  silently produced a degenerate PC1-vs-PC1 biplot on a single-component
  model; now errors with a clear message.
- [`z_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/z_norm.md):
  the zero-variance branch overwrote original `NA` values with `0`.
- [`manhattan_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/manhattan_plot.md)
  /
  [`volcano_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/volcano_plot.md):
  the significance threshold was hardcoded to 0.05 in four places; added
  an `alpha` argument.

### Code quality

- Removed
  [`proc_pca()`](https://jkylearmstrong.github.io/TempleCBE/reference/proc_pca.md)’s
  unused `data` argument.
- [`delete_nul_files()`](https://jkylearmstrong.github.io/TempleCBE/reference/delete_nul_files.md)
  now builds its shell command via
  [`shQuote()`](https://rdrr.io/r/base/shQuote.html) instead of
  hand-spliced quoting.

### Testing

- Added regression tests for every fix above.
- Backfilled test coverage for previously-untested files: `t_tests`,
  `distribution_test`, `correlation_plot`, `manhattan_volcano_plot`,
  `distribution_plot`, `missmap`, `pca_plots`, `R_names`,
  `read_workbook`, `dev_utils`, `keep_only`.

## TempleCBE 0.1.1

- **Package Infrastructure**: Fixed R CMD check errors and warnings to
  ensure full compliance with R package standards.
- **Dependencies**: Added `vctrs` to `Imports` and `FactoMineR` to
  `Suggests` in `DESCRIPTION`.
- **S3 Method Consistency**: Updated `plot.features_percent_miss` method
  signature to include `...` for base
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) generic
  compatibility.
- **S3 Dispatch**: Assigned `"features_percent_miss"` class to
  [`features_percent_miss()`](https://jkylearmstrong.github.io/TempleCBE/reference/features_percent_miss.md)
  output to enable seamless S3
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) dispatch.
- **Documentation & Examples**: Updated `@examples` and roxygen tags
  across `features_percent_miss`, `infix_helpers`, `my_summary_table`,
  and `sd.error`. Added missing `@param table` documentation for
  `%notin%`.
- **Unit Testing**: Expanded test coverage in
  `tests/testthat/test-features_percent_miss.R` and created
  `tests/testthat/test-summary.R`.
- **Build Configuration**: Added `.Rbuildignore` to ignore `README.qmd`
  during R CMD check.

## TempleCBE 0.1.0

- Initial release of TempleCBE biostatistics, clinical data science, and
  modeling utilities.
