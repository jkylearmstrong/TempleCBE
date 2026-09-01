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
