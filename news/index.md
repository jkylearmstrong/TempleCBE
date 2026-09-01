# Changelog

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
- `tidy.step_famd()` returned fabricated placeholder values
  (`value = 1.0`, `component = "PC1"` for every term) instead of real
  per-component loadings/contributions.
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
