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
