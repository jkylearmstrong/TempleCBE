# TempleCBE (development version)

## Temple brand

* New `temple_colors()`, `temple_pal()`, `scale_colour_temple()`/`scale_color_temple()`/`scale_fill_temple()`, and `theme_temple()` draw R graphics in the Temple University palette used by the [quarto_temple_brand](https://github.com/jkylearmstrong/quarto_temple_brand) Quarto extension. `temple_brand_path()` returns a bundled copy of its `brand.yml`, for `quarto::theme_brand_ggplot2()` or `bslib::bs_theme(brand = )`.
* New `use_temple_brand()` installs that extension into a Quarto project, creating `_quarto.yml` if needed, which enables the `temple-html`, `temple-pdf` (LaTeX title page), `temple-typst`, and `temple-revealjs` formats.
* `create_report(template_name = "temple")` scaffolds a report in those formats; `install_brand = TRUE` also installs the extension.
* Plots use the Temple palette. Diverging heatmaps (`correlation_plot()`, `correlation_diff_heatmap()`, `pca_feature_loading_heatmap()`, `pca_loading_diff_heatmap()`) run Night Owl-white-cherry instead of blue-white-red; `missmap()` counts, `plot_features_percent_miss()`, `plot_pca_bi()`/`pca_biplot()` loadings, `pca_percent_var_explained()`, and the reference lines of `distribution_plot()`, `manhattan_plot()`, and `volcano_plot()` use Temple colors. Only colors change.
* The palette follows Temple's current brand (<https://liberalarts.temple.edu/marcom/logos-and-brand>), matching quarto_temple_brand: cherry, white, and black (`#000000`); Clear Skies and Book Nook; formal accents `academic-gold`, `diamond-acres`, `founders-garden`, `night-owl`; casual accents `owls-eye`, `conwell-blue`, `upward-momentum`, `cherry-blossom`. The earlier names (`taupe`, `icy-blue`, `lime`, `eggshell`, `ochre`, `geranium`, `dark-blue`) are gone. The `"main"` palette is cherry, Night Owl, Owl's Eye, Founder's Garden, Upward Momentum, Diamond Acres, black; `"sequential"` runs Book Nook to cherry.

## `zip_render()` fixes

* Extension formats such as `titlepage-pdf` or `temple-pdf` weren't matched to their output file, so it was silently left out of the zip. They now resolve to their base format's extension.
* `_quarto.yml`, `_brand.yml`, `_variables.yml`, and `_extensions/` are copied into the build directory, so documents that use a project, brand, or extension format render there as they do in place. With `include_sources = TRUE` they are zipped under their relative paths.

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
* Example templates in `inst/templates/` generate synthetic data inline and no longer depend on `datasci` or a private dataset; the bundled example PDFs were re-rendered from them.

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
