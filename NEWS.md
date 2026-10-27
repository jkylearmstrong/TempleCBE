# TempleCBE 0.7.7

Downstream backlog for 0.7.7. Nothing is removed or renamed.

## Docker image

* **All run-time Suggests are in `renv.lock`**: `probably` (with `parsnip`, `workflows`, `tune`), `quarto`, `baguette`, `missRanger`, `missForest`, `FactoMineR`, `finetune`, `stacks`, `lme4`, `furrr`, `glmnet`, `rsample`, `openxlsx` and `zip`, with their dependencies (95 records, all additions; no pinned package changed). The image gets them from the one `renv::restore()`, so the second install phase is gone and both architectures get the same versions. On the DGX Spark this should remove the `joint_model()` calibration errors and about 100 skipped tests (`quarto`, `baguette`, imputation and tuning packages). The build check now also requires these packages to load in a fresh session.

* **Images are also published to GHCR.** When the Docker Hub credentials are set, the merge job of `docker-publish.yml` copies the multi-architecture manifest (`linux/amd64` and `linux/arm64`) it just pushed to `ghcr.io/<owner>/templecbe` under the same tags (`latest` and `master` from master, the version from a release tag), so `docker pull ghcr.io/jkylearmstrong/templecbe:<tag>` works on the DGX Spark. Without Docker Hub credentials GHCR was already the only registry. The GHCR package must be made public once in its settings.

# TempleCBE 0.7.6

Downstream requests GAP-12 to GAP-20. Nothing is removed or renamed. GAP-21 (`validate_data_manifest()`, `stop_if_invalid_manifest()`) already exists with the arguments `dir`, `manifest` and `project_root`, and was not changed.

## New

* **`tbl_contingency_2x2()`**: 2x2 table of `n (percent)` cells with row, column or total percentages, marginal totals, Fisher and/or Pearson p-values, odds ratio and relative risk with Wald intervals (0.5 added to every cell when a cell is zero) and the standard footnote. `table_two_by_two()` stays the institutional layout with the conditional-MLE odds ratio.
* **`pca_equation_latex()`** and **`pca_component_cutoff()`**: one component as a LaTeX linear combination with real feature names (`pca_eqns()` uses `f1`, `f2`, ...), and the number of components that reaches a cumulative variance threshold.
* **`tune_breakpoint_lmm()`** and **`impute_piecewise_lmm()`**: sweep candidate change points of a piecewise-linear mixed model (AIC, BIC, RMSE) and fill missing longitudinal values from it (a single imputation; needs `lme4`).
* **`classify_quadrants()`**, **`tbl_quadrant_distribution()`** and **`quadrant_n()`**: quadrant assignment from two cuts, counts of strata per quadrant, and one count for inline prose.
* **`tbl_paired_assay_agreement()`**: bias, SD of the differences, Bland-Altman limits, Lin's CCC and missingness for each pair of methods.
* **`tbl_correlated_pairs()`**: labelled table of feature pairs at or above a correlation cutoff with the caption "X of the Y features (Z%) are at least W% correlated" (`kable`, `gt` or tibble).
* **`inspect_rank_deficiency()`**: which fixed-effect columns `lmer`/`glmer` (or `lm`/`glm`) dropped, with a markdown callout.
* **`plot_imputation_hazard_stability()`**: density overlay of bootstrap hazard ratios on complete cases and on imputed data, with optional Tukey fences.
* **`tbl_tuning_by_quantile()`**: tuning candidates in performance tiers, with the mean `lambda` and `alpha`, or term retention per tier.

## Docker image

* **`yardstick` is in `renv.lock` (1.4.0) and in the build check of the image.** `joint_model()`, `glmnet_IBS()`, `score_outer()` and `step_lencode_joint_model()` need it; without it 4 errors and 6 failures remained on aarch64 (DGX Spark). It is recorded in the lockfile, not installed after the restore, so the "Packages differ from renv.lock" check cannot trip over it (every dependency of `yardstick` was already pinned).

# TempleCBE 0.7.5

Three downstream requests (GAP-09 to GAP-11). Nothing is removed or renamed.

## New

* **`tbl_feature_importance_dalex()`**: min, Q1, median, mean, Q3 and max of the dropout loss over the permutations of a `DALEX::feature_importance()` result, ranked by a chosen statistic (`top_n`, `id` columns for stacking models). Replaces four copies of a hand-written aggregation.
* **`batch_cutpoint_survdiff()`**: scans midpoint or quantile cutpoints of several numeric features, dichotomises at each, and returns the log-rank statistic and p-value, the group sizes, a significance bin (`p_breaks`) and the univariate Cox coefficient.
* **`first_significant_cutpoint()`**: the first cutpoint below each `alpha` per feature, scanning in the direction of the Cox coefficient.
* **`calc_grouped_summary()`**: long tibble of n, missingness, mean, SD, median, quartiles, IQR and range per variable and group, unformatted, for programmatic filtering.

# TempleCBE 0.7.4

Eight downstream requests (GAP-01 to GAP-08). Nothing is removed or renamed.

## New

* **`sample_candidate_formulas_adaptive()`**: second-stage formula search. Keeps the best `top_quantile` of an earlier tuning run, takes the size range (25th percentile to median of the winners' term counts) and term weights (share among winners) from them, optionally drops terms with an unfavourable univariate HR/OR (`effects`), and draws through `sample_candidate_formulas()`.
* **`tbl_table_one()`**: stratified baseline table (overall, per stratum, omnibus p-value) on `gtsummary`; mean (SD) or median [Q1, Q3] for `non_normal_vars`, n (%) for categorical.
* **`plot_imputed_vs_observed()`**: per-fold boxplots and/or density overlays of observed versus imputed values.
* **`plot_comissingness_matrix()`**: correlation of missingness indicators, optionally clustered, to tell block failure from scattered missingness.
* **`plot_marginal_effect_contrast()`**: raw-unit and normalized estimates of `glm_single()` / `lmm_single()` side by side, or as their ratio.
* **`render_parameterized_study()`**: `render_parameterized_pair()` for a whole folder, one output folder per parameter set.
* **`invalidate_downstream_checkpoints()`**: removes only the checkpoints of the nodes downstream of a changed node of a pipeline graph (`dry_run` available).
* **`expand_child_template()`**: `knit_expand()` a child template per row of a parameter grid and knit the result.

# TempleCBE 0.7.3

The 0.7.3 work. Nothing is removed or renamed.

## Fixed

* **`test_imputed_distributions()` and `plot_imputed_ridgeline()` name a missing column** (they treated it as no data; the ridgeline plot failed on a variable with nothing imputed).
* **`excel_colnames()` now removes the characters Excel forbids** (`\ / ? * : [ ]`). The pattern did not match, so names such as `"Patient/Subject [ID]"` came back unchanged.
* **`summary_table(na.rm = TRUE)`** returns `NA` for the minimum and maximum of a group with no non-missing values, instead of `Inf` and `-Inf` with a warning.

## New

* **`tbl_cox_glance_compare()`**: one row per Cox model with N, events, log-likelihood, AIC, BIC and concordance (SE), sorted by AIC, BIC or concordance; warns when the models were fitted to different rows.
* **`plot_dynamic_trajectory()`** gains `ribbon_type` (`"se"` as before, `"quantile"` with `quantile_probs`, or `"iqr"`) and `central_tendency` (`"mean"` or `"median"`).
* **Univariate screens for continuous outcomes**: `glm_single()` and `lmm_single()` fit one predictor at a time on its original and on a 0 to 1 scale (a status row instead of an error for degenerate predictors; model warnings kept; t-based intervals for gaussian fits), and `plot_coef_forest()` / `tbl_coef_forest()` display stacked results with a configurable significance-star scheme (`p_breaks`, `p_labels`; a p-value gets the label of the first break it is strictly below).
* **`transform_longitudinal_features()`**: per-subject percent change from baseline, variance, SD and the signed largest swing of repeated measurements (long or wide).
* **`plot_feature_transform_heatmap()`**: features by transformation, coloured by significance bucket with the within-transformation rank.
* **`plot_scatter_glm()`**: scatter with a regression line and band and the sentence "Pearson's r of ..., with p-value of ... The regression line is given by y = ...".
* **`plot_normalization_comparison()`**: one variable before and after normalization, side by side (needs `patchwork`, now in Suggests).
* **`adapter_tidymodels_survex()`**: survival probabilities of a tidymodels censored workflow as the N by T matrix that `survex` expects.
* **`read_positional_excel_headers()`**: reads sheets whose header spans several rows, carrying titles to the right (see the limitations in its help) and naming empty columns by their Excel letters; identifiers with leading zeros and letter codes stay text.

## Tests

* Failure recovery of `render_pipeline()` (a failing document does not stop the others; `on_error = "stop"`; the manifest is still written), a nested cross-validation with almost no events, and the column checks of the imputation diagnostics.
* Edge cases of `pretty_name()`, `excel_colnames()`, `copy_var_labels()`, `summary_table()`, `min_max_norm()` and `as_tuning_results()` (all-NA columns, constant columns, empty input, file reading, tidymodels metric tables).

# TempleCBE 0.7.2

More downstream requests. Nothing is removed or renamed.

## New

* **Imputation method selection**: `compare_imputation_rse()` scores imputation methods on cells with a known truth against a null baseline that predicts one constant per variable, the mean or median of the held-out true values (an oracle baseline, shared by all methods; `RSE = (RMSE / null RMSE)^2`; below 1 beats the baseline). This differs from `tune_impute_numeric()`, whose RSE uses the training mean as the baseline; `tbl_best_imputation_method()` picks the lowest RSE per variable; `build_best_imputed_data()` assembles the final data from each variable's winning method; `plot_rse_by_column()` and `plot_rse_vs_missingness()` draw the diagnostics. `tbl_best_imputation_method()` and the plots also accept the output of `tune_impute_numeric()`.
* **`plot_target_correlation_heatmap()`**: one target against many candidate predictors as a single column of correlation tiles, with weak correlations faded below a cut-off.
* **`batch_cox_single()`**: univariate Cox models for many features at once (optionally parallel through `furrr`, start-stop data supported), with hazard ratios, confidence intervals and a multiple-testing adjusted p-value per feature. A feature whose model fails is reported, not fatal, and the warnings `coxph` raised (for example perfect separation) are kept in a `warning` column.
* **`tbl_two_sample_compare()`**: Welch, Student or Wilcoxon comparison of continuous variables between two groups as a manuscript table (N, mean (SD), difference with confidence interval, p-value).
* **`words_list()` and `pluralize()`**: Oxford-comma lists that return a fallback for empty input, and a count with the right plural. (`words()` stays; `words_list()` adds the choice of joining word and ignores `NA`.)
* **`include_landscape_figure()`**: for `results = "asis"` chunks; puts a figure on a landscape page in PDF output (the begin/end of the landscape environment are raw LaTeX blocks, so pandoc still builds the figure and its caption) and writes no LaTeX in HTML or Word.

# TempleCBE 0.7.1

Requested by downstream projects, to replace local copies of the same code. Nothing is removed or renamed.

## New

* **PCA accessors for inline prose**: `pca_loading(pca_mod, feature, pc)` (one signed loading), `pca_variance_pct(pca_mod, pc, cumulative = FALSE)` (percent of variance on a 0 to 100 scale) and `pca_top_features(pca_mod, pc, n, by = "abs")` (the features with the largest loadings, with their signs). They take a `prcomp` object or a `run_pca()` result, and stop with a clear message for an unknown feature or a component the fit does not have.
* **`sample_candidate_formulas()`** draws reproducible, distinct model formulas from a pool of terms (required terms, size limits, optional sampling weights, `exclude` sets, interaction terms that need their variables); **`calculate_feature_weights()`** turns scored candidates into one weight per feature. Neither takes data, so the training-fold boundary is the caller's: weights and scores must come from the training rows only (documented in both help pages).

## Changed

* **`plot_cox_forest()` and `plot_cox_forest_multi()`** gain `exponentiate` (`TRUE`: hazard ratios, reference line at 1; `FALSE`: raw coefficients, reference line at 0, confidence limits transformed with the estimate), `log_scale` (a log axis for hazard ratios, a separate choice from `exponentiate`) and `sig_level` (the cut-off for `color_by = "significance"`, default 0.05). Invalid combinations are errors, for example `log_scale = TRUE` with `exponentiate = FALSE`. `scale = "hr"/"log_hr"` still works and means `exponentiate = TRUE/FALSE`.
* **`plot_cox_forest()`** colors a term with a missing p-value as "Not significant" (it had no color).
* **`plot_survival_km()`** accepts `pval` (the name other survival plotting packages use) as well as `p_value`; the log-rank p-value is now drawn inside the plot instead of replacing the caption, and asking for it without two or more groups warns. `risk_table = TRUE` was already available (through `ggsurvfit`; `survminer` is not needed).
* **`plot_dynamic_trajectory()`** gains `palette` (named colors keep each group's color whichever groups are plotted) and `group_levels` (groups and their order). Data whose groups are not Survived/Died now use the CBE palette instead of grey; the default for Survived/Died is unchanged. Numeric group labels sort as numbers (1, 2, 10).

# TempleCBE 0.7.0

## Removed (breaking): the deprecated wrappers announced for 0.7.0

* **The 71 functions that have warned since 0.5.8 are removed**, with their documentation and `pkgdown` entries, and so is the unexported `%notin%` shim. Each was a one-line wrapper that called its replacement. Code that already uses the new names is unaffected. To find and rename the old names in a project, run `find_deprecated_calls("path/to/project")` and `update_deprecated_calls("path/to/project", dry_run = FALSE)` (they keep their table of old and new names). The old name and its replacement:

  `%like%` -> `match_like`,
  `cbe_check_key_integrity` -> `check_key_integrity`,
  `cbe_compare_df` -> `compare_df`,
  `cbe_control_race` -> `control_race_survival`,
  `cbe_cox_check` -> `cox_check`,
  `cbe_cox_multi` -> `cox_multi`,
  `cbe_cox_single` -> `cox_single`,
  `cbe_database_label` -> `database_label`,
  `cbe_database_label<-` -> `database_label<-`,
  `cbe_database_name` -> `database_name`,
  `cbe_database_name<-` -> `database_name<-`,
  `cbe_database_relationships` -> `database_relationships`,
  `cbe_dataset_label` -> `dataset_label`,
  `cbe_dataset_label<-` -> `dataset_label<-`,
  `cbe_docx_review_extract` -> `docx_review_extract`,
  `cbe_domain_children` -> `domain_children`,
  `cbe_exact2x2` -> `exact2x2`,
  `cbe_explain` -> `explain_joint`,
  `cbe_explain_stack` -> `explain_stack`,
  `cbe_explain_status` -> `explain_status`,
  `cbe_explain_survival` -> `explain_survival`,
  `cbe_explain_time` -> `explain_time`,
  `cbe_factor_reference` -> `relevel_factor`,
  `cbe_find_shared_keys` -> `find_shared_keys`,
  `cbe_get_dataset_labels` -> `get_dataset_labels`,
  `cbe_get_id_cols` -> `get_id_cols`,
  `cbe_get_outcomes` -> `get_outcomes`,
  `cbe_get_predictors` -> `get_predictors`,
  `cbe_get_roles` -> `get_roles`,
  `cbe_get_time_cols` -> `get_time_cols`,
  `cbe_km_single` -> `km_single`,
  `cbe_loco_mp_coxnet` -> `loco_mp_coxnet`,
  `cbe_nested_survival_run` -> `nested_survival_run`,
  `cbe_pairwise_combos` -> `pairwise_combos`,
  `cbe_predict_parts_shap` -> `predict_parts_shap`,
  `cbe_read_database` -> `read_database_from_excel`,
  `cbe_review_tracker` -> `review_tracker`,
  `cbe_sas_macro_dir` -> `sas_macro_dir`,
  `cbe_sas_macro_path` -> `sas_macro_path`,
  `cbe_set_dataset_labels` -> `set_dataset_labels`,
  `cbe_set_roles` -> `set_roles`,
  `cbe_survex_loss_brier` -> `survex_loss_brier`,
  `cbe_survex_loss_ibs` -> `survex_loss_ibs`,
  `cbe_test_categorical` -> `test_categorical`,
  `cbe_tune_race_survival` -> `tune_race_survival`,
  `cbe_variable_roles` -> `variable_roles`,
  `cbe_write_database` -> `write_database_to_excel`,
  `clean_names` -> `clean_names_keep_labels`,
  `controls_datasets` -> `produces_data`,
  `extract_win_posix_paths` -> `extract_xlsx_paths`,
  `factor_reference` -> `relevel_factor`,
  `find_correlation` -> `drop_correlated_columns`,
  `fit_n_rank` -> `workflow_set_rank_fit`,
  `get_model_parameters` -> `workflow_set_params`,
  `install.packages.no_lock` -> `install_package_no_lock`,
  `is.int` -> `is_integer_valued`,
  `keep_only` -> `rm_all_except`,
  `like` -> `match_like`,
  `my_summary_table` -> `summary_table`,
  `normalize_safely` -> `normalize_path_safely`,
  `proc_contents` -> `describe_columns`,
  `proc_pca` -> `run_pca`,
  `R_names` -> `clean_names_keep_labels`,
  `read_search` -> `find_read_calls`,
  `render` -> `render_pipeline`,
  `render_me` -> `render_pipeline`,
  `sd.error` -> `std_error`,
  `summarize_tune_results` -> `tune_alpha_by_split`,
  `SumNa` -> `count_na`,
  `write_search` -> `find_write_calls`,
  `write_xlsx` -> `write_xlsx_file`.

* **`clean_names()` is not a pure rename.** The wrapper returned plain `janitor::clean_names()` output and rejected other input; its named replacement `clean_names_keep_labels()` also stores the original names as variable labels. For a data frame, `janitor::clean_names()` is the like-for-like replacement. `update_deprecated_calls()` rewrites `clean_names(` to `clean_names_keep_labels(`.
* **`clean_names()`, `like()`, `render()` and `%like%` no longer mask the functions of `janitor`, `data.table` and `rmarkdown`** when TempleCBE is attached after them.
* **Internal references to removed names were fixed**: `loco_mp_coxnet()` is what `nested_cv_coxnet()` calls, `estimate_runtime()` recognises `tune_alpha_by_split()` (it compared the name of the removed wrapper), so a projection for it counts every alpha and split, the `eda_tables` templates pass `TempleCBE::test_categorical` to `gtsummary`, and the examples, vignettes and README use the new names. A `gtsummary` call that names `cbe_test_categorical` or `cbe_exact2x2` as its test must name `test_categorical` or `exact2x2`.
* **Help pages**: links and examples that named a removed function (about 20 pages, and the `compare_df()` example, which stopped `R CMD check`) use the new names, and the `...` argument that only the wrappers had is no longer documented. `lifecycle` is no longer in `Imports`: nothing in the package called it once the wrappers were gone.
* **`find_deprecated_calls()` and `update_deprecated_calls()` read code only**: string literals, comments, method calls (`obj$render()`), markdown prose and non-R chunks of `.Rmd`/`.qmd` are ignored, and `.md` files are no longer scanned. An unqualified `clean_names()`, `render()`, `like()` or `write_xlsx()` is reported (as `call_type = "ambiguous"`) only in a file that uses TempleCBE, and is never rewritten, because janitor, rmarkdown, data.table and writexl export the same names. The new `exclude_dirs` argument skips folders such as `ARCHIVE`.
* `find_deprecated_calls()` no longer reports `cbe_exact2x2_ci()`, which is a function that stays (it formats odds ratios and was never deprecated).



# TempleCBE 0.6.5

This is the last release that still contains the deprecated wrappers (they warn; `find_deprecated_calls()` finds them in a project). They are removed in 0.7.0.

## Word documents

* New `extract_docx_qa()` reads a Word document of investigator questions and the answers written for them and returns one row per question with its answer and the report files the answer cites (`question_pattern`, `citation_pattern`). With an index of the deliverables (`index_data`) it also builds Excel `HYPERLINK` formulas for the citations and lists those the index does not know. It reads the document body with `xml2`, so there is no new dependency, and leaves Word comments and the authors of tracked changes to `docx_review_extract()` (insertions are read as text, deletions are not). Requested for the downstream Q&A tracking sheets.

## Rendering

* New `render_parameterized_pair()` renders one parameterized template for an original and an imputed parameter set (`render_variants()` for exactly two variants). The two parameter sets must name the same parameters, so that a difference forgotten in one of them is an error before anything is rendered; the outputs are named after `output_names`. Requested for the downstream report pairs that were copies of each other.

* New `interactive_render_pipeline()`: a menu in front of `render_pipeline()` for an interactive session. It lists the documents in render order and renders the stale ones, everything again, everything up to one document, or one document only (or nothing). It refuses to run non-interactively. Requested for the downstream render scripts.

## Data preparation

* New `reconcile_column_dictionary()` checks the columns of a data frame against a dictionary of source names (for example SAS variable names) and target names: exact, then case-insensitive, then normalized matching; unmapped columns on either side; and conflicts (a column matching two entries, an entry matched by two columns, two entries with one target, a target that is already a column). It renames only what is unambiguous, and only when everything required is mapped unless asked otherwise. Requested for the downstream SAS-to-R reconciliation scripts.

## Imputation diagnostics

* New `compare_domain_datasets()` bundles the comparison of an original and an imputed version of one set of related variables (ECDF plot and KS / t tests, correlation differences and their heatmap, PCA loading differences, and mean trajectories by time with the shift caused by imputation and a paired test) into a list of tibbles and ggplot objects, so a report can loop over several domains. Requested for the downstream EDA reports.
* `compare_imputed_ecdf()` no longer fails when one of the variables has no missing value in the original data ("arguments imply differing number of rows"); that variable contributes no imputed points.

## Docker image and CI

* The `rossi_nested_survival` vignette sets a seed before drawing its nested folds. Its folds were random, and a build of 2026-10-23 drew one without events ("The outcome has no events"), which failed `R CMD check` on a pull request that did not touch it.

* **The Linux x86-64 CI jobs use the main Ubuntu package mirror.** On 2026-10-23 `azure.archive.ubuntu.com`, the mirror the GitHub-hosted runners are configured with, was so slow that installing R and the system libraries took 20 to 40 minutes and the jobs were cancelled at their limits before any test ran. A first step pointed apt at `archive.ubuntu.com` (the jobs that run in the CI image below need no such step).
* The weekly rebuild of the CI image changes a build argument (`CRAN_WEEK`) so that the layer with the CRAN packages is rebuilt instead of served from the cache.
* **A CI base image** (`Dockerfile.ci`, built by `.github/workflows/ci-image.yml` for amd64 and arm64 and published as `ghcr.io/<owner>/templecbe-ci`): R, pandoc, git and every package TempleCBE Imports or Suggests, as the newest CRAN binaries. It is rebuilt every Monday and when `DESCRIPTION` or the Dockerfile changes. The Linux x86-64 jobs of `R-CMD-check` (including the optional tests) and `test-coverage` run in it (the image also carries `jq`, `unzip`, `wget` and `xz-utils`, which the r-lib actions call; the first image lacked `jq`), so they stop reinstalling R and all dependencies on every run, which took 15 to 40 minutes whenever the package cache was cold.

* **The Docker layer cache moved from the GitHub Actions cache to the registry** (`<image>:buildcache-<platform>`, written only by a publishing run). With `mode=max` each Docker run wrote about 5 GB to the Actions cache, which is limited to 10 GB per repository; it held 135 Docker entries and no R package cache, so every `R-CMD-check` run reinstalled R and all packages and the master run of 2026-10-23 reached the 60-minute limit before its check started.
* **The build now fails unless a fresh R session started in `/pkg` finds TempleCBE and every extra package. (`yardstick`, `probably` and `quarto`, which `joint_model()`, `glmnet_IBS()` and the Quarto reports need, are not added: installing them pulls in `parsnip`, `dials` and a newer `future` than `renv.lock` pins, which the lock check refuses. They belong in `renv.lock`.) A DGX Spark run on 2026-10-23 reported that the extra packages added in 0.6.3.9000 were installed but not visible at run time (33 test errors and 405 skips in the image); the cause is not confirmed, and this check makes the build show it.

# TempleCBE 0.6.4

## `clean_publish()` pre-push hook

* **A value of `cleanpublish.cleancommit` or `cleanpublish.privatetip` is a full object id only when it is lower-case hex, in every shell and locale.** The hook checked this with the range `[!0-9a-f]`. In some shells a range follows the locale's collation order, where it also matches `A` to `E` (seen on a macOS runner in `en_US.UTF-8`; reproduced with a bash that uses collating ranges), so an upper-case id without an `F` counted as an id. The hook then used it as a clean commit, and a push of the private tip that it was meant to refuse went through (in a test, one macOS run in about thirteen). The hook now lists the characters, and the two URL-scheme checks (`_tcbe_norm_url`, and the removal of credentials from a refusal message) do the same, which also keeps the hook in step with the R functions that decide the same thing. Found as a flaky macOS test (CPB-06). An already installed hook keeps the old check until `guard_public_remote()` or `clean_publish()` is run again in that repository, which rewrites the managed block.

* **A value of `cleanpublish.cleancommit` or `cleanpublish.privatetip` counts only if git resolves it to that same commit.** The hook checked the shape only (40 or 64 lower-case hex digits). A branch or tag named like an id, or a 40-digit abbreviation in a SHA-256 repository, passes that check and resolves to another object, so a `cleancommit` value like that could make the private tip count as a clean commit. The hook now sends each value to `git cat-file --batch-check` as `<id> <id>` and uses it only when the object name git prints equals the second copy and is a commit. Found by the independent review of the locale fix (CPB-06); needs a hand-made config value or a branch named like an id, so the exposure was small. An already installed hook keeps the old check until `guard_public_remote()` or `clean_publish()` is run again in that repository.

* **The hook ignores `refs/replace`.** `git replace --graft` on a commit that sits on top of a recorded private tip made `rev-list` see it as parentless, so a push of that commit was accepted and the private tip went with it (shown by the independent review of the hook fix). The hook now sets `GIT_NO_REPLACE_OBJECTS=1` while it decides, and puts the variable back as it was. A note about a `cleanpublish.privatetip` value that git does not resolve to a commit with that id now names the value as configured.

## Docker image

* The image now installs the Suggested packages that features need at run time (`lme4`, `furrr`, `glmnet`, `rsample`, `openxlsx`, `zip`, pinned to exact versions) and `pandoc`, and the build fails if that changes any package pinned in `renv.lock`. Found by running the package on a DGX Spark (aarch64): `boot_lmer()`, `tune_alpha_by_split()` and the Word review exports could not run in the image, because `renv.lock` records only Imports, and about 30 tests that need `zip` or `openxlsx` errored. Rendering R Markdown inside the image also failed for want of a pandoc that R could see. The `pandoc` is the distribution's, not the one Quarto bundles.

## Tests

* Tests that cannot run in the Docker image now skip instead of failing: `deploy_release()` needs a git working tree (the image has no `.git`), and the checkpoint-message test needs `furrr`.
* The `check_renv_guard()` test no longer depends on whether renv happens to be active in the test session; renv detection is a small internal function the test replaces.

## `boot_lmer()`

* New argument `parallel = FALSE`. With `parallel = TRUE` the bootstrap replicates are fitted with `furrr::future_map()` under the current future plan (`future::plan("multisession")`, a cluster, ...), each replicate with its own random-number stream. The result for a given `seed` is the same under any plan and any number of workers, but it is not the result of the sequential loop for that `seed`. The default is unchanged and gives the same results as before. The per-replicate work moved into an internal function shared by both paths.
* Found by running the package on a DGX Spark (20 cores): the sequential loop used one core. On a 20-core Windows workstation, 200 replicates of a random-intercept model took 2.5 s sequentially and 1.4 / 0.9 / 0.5 s on 2 / 4 / 8 workers.
* In parallel mode the arguments in `...` are evaluated before fitting. An argument that names a column of `data` (`weights = n`) is refused with a message that says so. (It does not work in the sequential loop either: every replicate fails and the result is empty. That is how it behaved before this change and is not changed here.)

## Release scripts

* **`scripts/deploy_release.R --push` no longer pushes `master`.** `master` is protected (a pull request is required), so the release commit now goes to a branch of its own, `release/<tag>` (`--push-branch NAME`, `push_branch =`), and no tag is made: the script prints the `gh pr create` command and the commands that tag the merge commit afterwards. The local branch is put back where it was; the commit stays on a local `release/<tag>` branch. `--push-branch master` (the `--branch` branch) keeps the direct atomic push of branch and tag, for a repository that does not protect it. An existing local branch of that name, or an invalid name, is refused before anything changes.
* **`release.yaml` (`commit_and_push`) pushes a branch `release/v<version>` instead of `master` and a tag**, and writes the link for the pull request and the tagging commands into the run summary. The version it is given must be numeric (digits and dots), since it is used in a `sed` expression, a commit message and a branch name.
* `REVIEW.md` describes both. (`release.yaml` was not registered by GitHub as a workflow until PR 42 touched it, on 2026-10-22; it has never run.)

# TempleCBE 0.6.3

A CI and packaging release; no R code changed.

## Continuous integration

* **aarch64 Linux is now checked.** `R-CMD-check` runs the full check and the optional-dependency (`future` multisession) tests on `ubuntu-24.04-arm`, the CPU architecture of DGX Spark, in addition to Ubuntu x86-64, Windows and macOS.
* **Container images are built natively per architecture.** The `v0.6.2` tag build was cancelled at the 60-minute limit because the arm64 half ran under QEMU emulation. Tag builds now compile `linux/amd64` and `linux/arm64` on native runners, push each by digest, and merge them into one multi-architecture manifest. Other builds publish `linux/amd64` only. No `0.6.2` image was published from the tag; `0.6.3` is the first multi-architecture release image.

# TempleCBE 0.6.2

## New functions

* `render_variants()`: renders one parameterized Quarto or R Markdown template once per variant of its parameters (for example `use_imputed = FALSE` / `TRUE`), naming the outputs `<template>_<variant>.<ext>`. A parameter that the template does not declare under `params:` is an error before anything is rendered, so a copy of a report no longer has to be kept in step by hand.

* `audit_render_parity(pdf_path, docx_path)`: before a report goes out, compares the rendered PDF with the Word version and lists text that comes too close to the page edge, table and figure captions that are missing, skip a number or come in another order, a table caption above the table in one file and below it in the other, and percentages or decimals that occur in one file only (for example 98.9% in the PDF and 97% in Word). It reads text and positions only and points at places to look at.

* `boot_lmer()` and `boot_p_value()`: a cluster bootstrap of a linear mixed model. Whole groups are resampled, each replicate is fitted (optionally with predictors rescaled to [0, 1] within the resample and a fallback formula when the fit fails), outlying bootstrap estimates of each fixed effect are dropped by Tukey's fences, and each term is summarised by mean, standard error, median, percentile limits and a two-tailed bootstrap p-value. Replaces the copied bootstrap blocks in report templates.

## Review extraction

* `docx_review_extract()` and `review_config()` gain `fork_mode = c("redacted", "collaborative")`. The default, `"redacted"`, is the behaviour so far: a reviewer's fork holds only their own sign-off and note columns. `"collaborative"` also shows the other fork reviewers' columns (sign-off reviewers' columns and Word authors still never go into a fork). Only a reviewer's own columns are merged back, so editing a colleague's column in a fork changes nothing.
* A row a reviewer adds to their fork workbook, which matches no master row, is no longer dropped: it is kept as a general comment with `doc_status = "General Comment"`, in the master and in every fork. Its id is `gen_<n>` (with redacted forks, so the id does not tell the other reviewers who wrote it) or `gen_<reviewer>_<n>` (collaborative forks). Its `file` is kept if it names a document of the tracker and is otherwise `(general)`, which shows up as a pseudo-document in the Documents sheet. A copy of the same row sent back again is not added twice. Later edits to the text or file of a general comment in a fork are not merged back, only the reviewer's own sign-off and note. Rows added to the SuggestedChanges sheet of a fork are kept the same way (without an id; the file, paragraph, original and accepted text and the reviewer's own sign-off and note are taken). Forks written by an earlier version (no baseline sheet) add nothing.
* `docx_review_extract(manifest_path = "auto")` looks for `reports_to_render.xlsx` in the input folder, its two parents and the working directory (also `TEMPLECBE_ANALYSIS_ROOT`), in the folder itself and in `analysis/<folder>/`, when `review_config(analysis_dir =)` finds none. The file used is reported and returned in `$paths$manifest`; several candidates in one place give a warning and the most recent is used. Nothing is searched by default.

## `clean_publish()` security review fixes (L3 review CP-L3-01 to CP-L3-04)

* **CP-L3-02**: `clean_publish()` now refuses `HEAD` (in any case), any other name `git check-ref-format --branch` rejects, and a `publish_branch` or `private_branch` that collides with an existing branch or with each other (`feat` and `feat/x`), before anything is pushed or written. Before, `publish_branch = "HEAD"` or `"feat/x"` next to a branch `feat` passed the checks, force-pushed to the remote and only then failed.
* **CP-L3-01**: `guard_public_remote()` no longer prints credentials: the error that lists the push URLs of the configured remotes, the "could not be normalized" error and the "matches no configured remote" message strip the `user:password@` part of every URL.
* **CP-L3-03**: the pre-push hook no longer echoes credentials when it refuses a push to a guarded public remote. An already installed hook keeps the old message until `guard_public_remote()` or `clean_publish()` is run again in that repository, which rewrites the managed block.
* **CP-L3-04**: `allow_ignored` now matches tracked-but-ignored files whose names contain non-ASCII characters (git printed them quoted, so they could never be allowed). A name that is not valid UTF-8, or that git still quotes (tab, newline, quote), is reported and never allowed.

## Fixes

* `audit_render_parity()` calibration: Word table captions styled with `TableCaption` that omit a "Table <n>" prefix are now recognized as table captions and numbered sequentially, avoiding false missing-table warnings. In addition, multi-line contents and list-of-tables pages in the PDF are identified as front matter, preventing wrapped titles from leaking numbers into the text-comparison check.

* `list_stage_data_artifacts()` now lists files whose extension is upper or mixed case (`DATA.RDS`); the primary-data patterns already ignored case.

* `dedupe_pipeline(strategy = "rename")` no longer returns duplicate names when a numbered name already exists in the list (`c("a", "a", "a_2")` came back as `c("a", "a_2", "a_2")`); the new name skips every name in use.

# TempleCBE 0.6.1

A corrective release that makes the 0.6.0 notes accurate and removes a release-time maintenance hazard.

## Corrections to 0.6.0

* **0.6.0 was a consolidation release, not a code milestone.** Relative to 0.5.12 it changed the version number, release notes and one test; the performance, diffing and migration features described under 0.6.0 shipped in 0.5.9 to 0.5.12.
* **Deprecated wrappers were not removed in 0.6.0.** They were announced for removal in 0.6.0 and remain available. Removal is re-scheduled for **0.7.0**, and the deprecation messages, documentation and tests now say so.

## Checkpoints

* **Checkpoint fingerprints no longer include the package version.** They include an explicit `checkpoint_schema_version()`, bumped by hand only when a change alters what a cached checkpoint contains. Upgrading the package therefore no longer invalidates cached long-running work. Provenance is unchanged: the package version is still recorded in `checkpoint_meta$version`.
* **One-time effect:** because the fingerprint payload changed, checkpoints written by 0.6.0 or earlier are treated as stale on first use and recomputed once.

## Internal

* All package-version lookups go through one helper, `templecbe_version()`; hardcoded fallback versions were removed and the version test no longer pins a version.
* `AGENTS.md` and `CLAUDE.md` are excluded from the built package; a legacy-data reference was removed from `as_tuning_results()` documentation.

# TempleCBE 0.6.0

TempleCBE 0.6.0 is a major milestone release promoting the accumulated performance, deliverable management, codebase modernization, and developer productivity enhancements developed across recent cycles into a stable, production-grade foundation for analytical and clinical workflows.

## Enhanced Performance and Scalability

* **$O(N)$ Hash-Based Exact Paragraph Pairing**: Re-architected `diff_deliverables()` and `pair_paragraphs_by_similarity()` with hash-based exact matching and section-scoped candidate clustering. Large production documents (5,700+ paragraphs) now compare in under a second rather than stalling or hanging.
* **Adaptive Paragraph Pairing Time Budgeting**: Integrated an adaptive pairing time budget (`time_budget_sec = 6.0`) with clean early-exit fallback to prevent unbounded latency on massive documents while surfacing clear progress notifications.
* **Instant Parallel Dispatch & Flexible Calibration**: Added `calibrate = TRUE` (default) / `calibrate = FALSE` to `tune_alpha_by_split()` and `summarize_tune_results()`. Setting `calibrate = FALSE` bypasses serial benchmarking and dispatches tasks immediately to parallel workers, eliminating warm-up delay in high-throughput workflows.
* **100x Faster Deprecation Auditing**: Optimized project codebase scanning in `find_deprecated_calls()` from 136 sequential per-line regular expressions down to a single compiled master pattern, speeding up scans by over 100x (< 0.01 s per file) across extensive script repositories.

## Improved Reporting and Deliverable Management

* **Unicode & Smart Quote Normalization**: Enhanced deliverable diffing to automatically normalize curly quotes (`‘’`, `“”`), typographical dashes (`–`, `—`), non-breaking spaces (`\u00a0`), zero-width spaces, and indentation variations prior to matching, eliminating spurious differences introduced by Word/XML conversions.
* **Accurate Unpaired Paragraph Accounting**: When paragraph diffing reaches the configured time budget, unmatched paragraphs are cleanly isolated under `n_unpaired` and `budget_exceeded = TRUE`, preventing artificial inflation of `n_added` and `n_removed` metrics.
* **Divergence-Focused Excerpting**: Surfaced deliverable edits (`largest_changes`) pinpoint character-level divergence points via `excerpt_diff()`, displaying meaningful contextual modifications rather than truncated identical sentence prefixes.

## Developer Productivity and Code Quality Enhancements

* **Polymorphic Column Header Normalization**: Modernized `clean_names_keep_labels()` to seamlessly accept both character vectors and data frames. Character vectors are cleaned directly via `janitor::make_clean_names()`, preventing runtime errors on character inputs.
* **Multi-Header Excel Spreadsheet Ingestion**: Enhanced `read_excel_multiple_headers(clean_names = TRUE)` to cleanly ingest multi-row concatenated headers without type mismatches.
* **Automated Codebase Scanner and Rewriter**: Provided `find_deprecated_calls(path)` and `update_deprecated_calls(path, dry_run = TRUE)` to scan and migrate user projects (`.R`, `.Rmd`, `.qmd`, `.md`) for deprecated function calls scheduled for retirement.
* **Encoding & Namespace Protection in Migration Tooling**: Deprecation migration tooling gracefully handles non-UTF-8 encodings and strictly protects external namespaces (e.g. `janitor::clean_names()`, `data.table::like()`) and locally defined functions.

## Workflow Configuration and Control

* **Fine-Grained Execution Diagnostics**: Parallel execution routines now capture and report isolated calibration durations (`calibration_sec`) and empirical serial baseline bounds (`measured_serial_baseline_range_sec`, `speedup_range`) in `timing_breakdown` metadata and progress output.
* **User-Controlled Performance Tuning**: Configurable balance between serial warm-up verification and rapid parallel worker dispatch across nested cross-validation and tuning pipelines.

## Modernization and API Cleanup

* **Canonical Verb-Noun API Architecture**: Consolidated package interfaces around clean verb-noun conventions (`clean_names_keep_labels()`, `write_xlsx_file()`, `match_like()`, `render_pipeline()`, `describe_columns()`, `run_pca()`).
* **Zero Internal Deprecation Invocations**: Swept internal package code to eliminate internal calls to deprecated functions, achieving zero deprecation warnings under strict `options(lifecycle_verbosity = "error")`.
* **Environment-Agnostic Version Resolution**: Hardened version metadata resolution during R CMD check and installed test environments via `utils::packageVersion("TempleCBE")`.

## Reliability and Ecosystem Compatibility

* **Disjoint PCA Fit Early Validation**: Enhanced `pca_loading_diff()` and `pca_loading_diff_heatmap()` to detect disjoint PCA fits sharing zero variables and stop early with clear diagnostic errors.
* **Strict Tag Immutability Codification**: Formally re-affirmed permanent tag immutability policy across all historical and future releases (`v0.5.8` through `v0.6.0`).
* **Clean Test and CI Matrix**: Fully passing test suite across Windows, Ubuntu, and macOS platforms.

## Documentation and User Experience

* **Comprehensive Release and Migration Documentation**: Updated reference documentation, man pages, and migration pathways guiding downstream analytical projects from legacy wrappers to canonical APIs.

# TempleCBE 0.5.12

## Header Cleaning & Polymorphic Name Support (#12)

* **Polymorphic `clean_names_keep_labels()` & Character Vector Cleaning (#12)**: `clean_names_keep_labels()` now seamlessly accepts both character vectors and data frames. Character vectors are cleaned directly with `janitor::make_clean_names()`, preventing the runtime error `"clean_names() requires that either names or dimnames be non-null"`.
* **Multi-Header Excel Ingestion (#12)**: `read_excel_multiple_headers(clean_names = TRUE)` cleanly normalizes multi-row concatenated column headers without type errors.

## Robust Deliverable Diffing on Production Reports (#6)

* **Smart Quote & Whitespace Normalization (#6)**: `pair_paragraphs_by_similarity()` and `diff_deliverables()` normalize curly quotes (`‘’`, `“”`), em/en dashes (`–`, `—`), non-breaking spaces (`\u00a0`), zero-width spaces, and whitespace prior to comparison. Word XML quote conversions and indentation formatting variations no longer prevent exact $O(N)$ matching.
* **Separation of Unpaired Paragraphs on Budget Expiry (#6)**: When the paragraph comparison time budget (`time_budget_sec = 6.0`) is reached on massive documents (e.g. 5,700+ paragraphs), remaining unmatched paragraphs are reported under `n_unpaired` and `budget_exceeded = TRUE`. They are cataloged separately in `section_summary` rather than falsely inflating `n_added` and `n_removed`.
* **Divergence-Focused Excerpting (#6)**: Surfaced top changes (`largest_changes`) locate the point of character divergence via `excerpt_diff()`, displaying the actual edit site rather than truncated identical sentence prefixes.

## Parallel Speedup Diagnostics & Optional Calibration (#10)

* **Optional Calibration Argument (`calibrate = TRUE`) (#10)**: Added `calibrate = TRUE` argument to `tune_alpha_by_split()` and `summarize_tune_results()`. Setting `calibrate = FALSE` bypasses serial benchmarking and dispatches tasks immediately to parallel workers.
* **Concise Benchmark Warm-Up (#10)**: Serial calibration benchmarks a tight sample of tasks (`min(2L, ...)`), minimizing serial delay before parallel execution.
* **Isolated Calibration Timing (#10)**: Calibration duration is timed independently and reported as `calibration_sec` in `timing_breakdown` and printed progress messages (`"; calibration took X.XX s"`), preventing calibration from confounding reported parallel throughput.

## Tag Immutability & Release History (#11)

* **Tag History for `v0.5.9` (#11)**: Documented historical release note for tag `v0.5.9`, which was initially tagged at `3c4d7f2` and updated to `4be6191` to resolve installed package DESCRIPTION connection checks during R CMD check before tag immutability policies were codified.
* **Strict Tag Immutability Policy (#11)**: Re-affirmed that all published release tags (`v0.5.8`, `v0.5.9`, `v0.5.10`, `v0.5.11`, `v0.5.12`) are permanently immutable and will never be moved or re-tagged. All bug fixes and enhancements strictly advance in new patch versions.

# TempleCBE 0.5.11

## Fast Paragraph Pairing & Budgeted Deliverable Diffing (P1)

* **High-Performance Paragraph Pairing (P1)**: Modernized `diff_deliverables()` and `pair_paragraphs_by_similarity()` with $O(N)$ hash-based exact matching, section-scoped similarity comparisons, precomputed token sets, sliding positional window candidate capping, and an adaptive time budget fallback (`time_budget_sec = 6.0`). Large reports (5,700+ paragraphs) compare in under a second rather than hanging, with clear progress notifications when diffing extensive documents.

## Robust Deprecated Call Finder and Rewriter (P2)

* **Strict Script Extension & UTF-8 Safety (P2)**: `find_deprecated_calls()` and `update_deprecated_calls()` strictly target `.R`, `.r`, `.Rmd`, `.qmd`, and `.md` files, skipping binary files (`.rds`, `.rtf`) and gracefully recovering from invalid UTF-8 with user notifications.
* **External Namespace & Local Scope Isolation (P2)**: Calls prefixed with external package namespaces (e.g. `janitor::clean_names()`, `data.table::like()`, `writexl::write_xlsx()`) and functions defined locally within the script itself (e.g. local `SumNa <- function(...)`) are strictly preserved and excluded from deprecation matching and rewriting.
* **Single-Pass Master Regex Performance (P2)**: Optimized scanning from 136 regex evaluations per line down to a single compiled master pattern, speeding up scans by over 100x (< 0.01 s per file).
* **Accurate Replacement Counts & Per-File Summaries (P2)**: `update_deprecated_calls()` tracks actual lines modified, reporting exact replacement counts and printing detailed before/after summaries for every updated file.

## Internal Deprecated Call Cleanup & Verification (P3)

* **Zero Internal Deprecation Invocations (P3)**: Replaced `cbe_find_shared_keys()` in `database_relationships()` with canonical `find_shared_keys()`, replaced internal `clean_names()` in `read_excel_multiple_headers()` with `clean_names_keep_labels()`, and updated message references across `cbe_cox_multi.R`, `loco_mp_coxnet.R`, `nested_survival_run.R`, `plot_cox.R`, and `render.R`.
* **Automated Codebase Deprecation Sweep Test (P3)**: Added unit test verifying that every `R/*.R` implementation contains zero internal calls to functions in `templecbe_deprecated_map()` and that `database_relationships()` executes cleanly under `options(lifecycle_verbosity = "error")`.

## Non-Overlapping Variable Diagnostics for PCA (P4)

* **Early Validation for Disjoint PCA Fits (P4)**: `pca_loading_diff()` and `pca_loading_diff_heatmap()` now detect when two PCA fits share zero common variables and stop early with `"The two fits have no variables in common."`, eliminating downstream integer coercion errors and non-missing argument warnings.

## Serial Baseline Range & Speedup Diagnostics (P5)

* **Multi-Task Serial Baseline Calibration & Speedup Range (P5)**: `tune_alpha_by_split()` benchmarks an expanded sample of serial warm-up tasks, computing and reporting both median and empirical bounds (`measured_serial_baseline_range_sec`, `speedup_range`) in timing messages and `timing_breakdown` metadata (`"%.2fx [range: %.2fx - %.2fx] real speedup on %d workers vs %.2f s [range: %.2f - %.2f s] measured serial baseline"`).

## Tag Immutability & Release Policy (P6)

* **Permanent Tag Immutability (P6)**: Strict enforcement that published release tags (`v0.5.8`, `v0.5.9`, `v0.5.10`) are permanent and never moved. All fixes ship as incremental patch releases.

# TempleCBE 0.5.10

## Internal Lifecycle Cleanup & Deprecation Warnings (M1, M2)

* **Robust Installed Package Environment Detection (M1)**: R CMD check and installed test environments safely resolve version metadata via `utils::packageVersion("TempleCBE")` without assuming source tree layout.
* **Zero Deprecation Warnings on Internal Calls (M2)**: Replaced all internal package calls to deprecated functions (`SumNa`, `cbe_exact2x2`, `cbe_compare_df`, `cbe_test_categorical`) with their canonical implementations (`count_na`, `exact2x2`, `compare_df`, `test_categorical`). Updated all error messages and diagnostic text to reference canonical functions (`run_pca` instead of `proc_pca`). Enforced zero internal deprecation warnings across test suites under `options(lifecycle_verbosity = "error")`.

## Principal Component Analysis Interoperability (M3)

* **Unified `cbe_pca` and `prcomp` Support (M3)**: Every PCA helper function (`pca_feature_loading_heatmap()`, `plot_pca_bi()`, `pca_biplot()`, `pca_scree_plot()`, `pca_variables_plot()`, `rotation_matrix()`, `pca_loadings()`, `pca_eqns()`, `pca_percent_var_explained()`, `pca_loading_diff()`, `pca_loading_diff_heatmap()`, `pca_plot()`) now seamlessly accepts both `cbe_pca` objects produced by `run_pca()` and raw `stats::prcomp` fits.

## Calibrated Parallel Baseline Tuning (M4)

* **Warmed Serial Baseline Calibration (M4)**: In `tune_alpha_by_split()`, serial baseline measurement is now calibrated from the median fit duration of a warm-up sample of serial tasks without progress bars, eliminating cold-start initialization artifacts and ensuring reported speedup accurately matches full sequential execution.

## Deprecated Call Migration Tooling (N1)

* **Automated Codebase Scanner and Rewriter (N1)**: Added `find_deprecated_calls(path)` and `update_deprecated_calls(path, dry_run = TRUE)` to scan user projects (`.R`, `.Rmd`, `.qmd`) for legacy TempleCBE functions scheduled for removal in 0.6.0. Supports both package-qualified (`TempleCBE::old()`) and unqualified calls, providing dry-run previews and in-place code migration.

## Pinned Cox Tie-Handling & Quiet Scope (N2)

* **Reproducible Cox Tie Handling Across glmnet 5.0/5.1+ (N2)**: Introduced package option `templecbe.cox_ties` (default `"breslow"`), `get_templecbe_cox_ties()`, `apply_workflow_cox_ties()`, and `with_quiet_cox_ties()` to pin reproducible Breslow tie handling across tidymodels and checkpointed wrappers (`tune_grid_checkpointed()`, `tune_race_survival()`, `nested_cv_coxnet()`, `coxnet()`, `cv_coxnet()`). Silences repetitive glmnet 5.1 transitional tie messages across folds and parallel workers with a single session notification.

## Intelligent Deliverable Diffing (N3)

* **Similarity-Paired Document Diffs (N3)**: Modernized `diff_deliverables()` with token-level paragraph similarity pairing to distinguish edits from delete/add pairs, ignore Word XML tracked-change markup (`<w:del>`), summarize changes by document section/heading, and surface top largest changes.

## Removal Schedule (N4)

* **Scheduled Removals in 0.6.0 (N4)**: Re-affirmed deprecation timeline for legacy masking wrappers (`clean_names`, `write_xlsx`, `render`, `like`, `%like%`) and 58 deprecated `cbe_*` aliases scheduled for final removal in TempleCBE 0.6.0 (Q1 2027).

# TempleCBE 0.5.9

## Reproducible Checkpointing & Cross-Validation Folds (K1)

* **Deterministic Fold Generation When Checkpointing `cv_coxnet()`**: Added formal `seed = NULL` argument across all `cv_coxnet()` methods (`data.frame`, `matrix`, `formula`, `recipe`). When `checkpoint_dir` is supplied and `resamples` is `NULL`, `seed` defaults to `1L` to ensure cross-validation fold partitions are identical across repeated calls, enabling instant checkpoint reuse and cache hits without manual caller seeding. Informative messages clarify when default fold seeding is active.

## Parallel Tuning Diagnostics & Physical Core Guidance (K2)

* **Measured Serial Baseline & Real Speedup**: `tune_alpha_by_split()` now measures an empirical serial execution baseline directly, computing the true wall-clock speedup against measured serial compute rather than summing concurrency-slowed worker CPU times.
* **Worker Concurrency Slowdown & Profiling**: Added detailed diagnostics reporting average task compute time in workers vs. serial baseline, measuring concurrency slowdown factors caused by hyper-threaded core contention, memory bus bandwidth, and multi-threaded library calls.
* **Physical Core Recommendation**: Recommends sizing worker pools to physical CPU cores (`parallel::detectCores(logical = FALSE)`), highlighting why hyper-threaded logical cores share execution units and cache and degrade per-worker throughput. `estimate_runtime()` reports recommended physical worker limits.

## Tuning Comparison Diagnostics (K3)

* **Silent Kolmogorov-Smirnov Test with Tie Notification**: `compare_tuning_runs()` now suppresses tie approximation warnings from `stats::ks.test` and clearly notes `(approximate in presence of ties)` in the printed distribution summary when duplicate metric values are present.

## Masking & Export Cleanup (K4)

* **Unexported `%notin%`**: Removed `%notin%` from package exports to immediately resolve name masking against base R 4.6 (`%notin%`). Deprecated legacy `%notin%` remains available as an internal shim, while `%!in%` and `!(x %in% table)` are the recommended exported alternatives.

## Release Process Integrity (K5)

* **Patch Release Discipline**: Version bumped to `0.5.9` with synchronized `NEWS.md` and `DESCRIPTION`. Immutable tags and non-destructive branch merges ensure strict `renv` pin stability and checkpoint fingerprint reproducibility.

# TempleCBE 0.5.8

## General-Purpose Checkpointing & Envelope Container (H1, H2, H6)

* **General `checkpoint_map()` & Clean Return Objects (H6)**: Added and exported `checkpoint_map(.x, .f, ..., checkpoint_dir, key, fingerprint, shard, checkpoint_mismatch, progress)`, providing a purrr/furrr-style checkpointed mapping engine. Per-task `.rds` checkpoint files are saved in a transparent `templecbe_checkpoint_envelope` container, ensuring returned task results carry no polluting metadata attributes so first runs and resumed runs are strictly `identical()`.
* **Tidymodels Checkpointed Resampling & Tuning Wrappers**: Added `tune_grid_checkpointed()`, `fit_resamples_checkpointed()`, `workflow_map_checkpointed()`, and `tune_race_survival_checkpointed()`. Work unit = one resample ($\times$ workflow $\times$ grid), returning standard `tune_results` objects compatible with `collect_metrics()`, `select_best()`, and `autoplot()`.
* **Comprehensive Workflow & Metrics Fingerprinting (H1)**: `compute_tidymodels_split_fingerprint()` now serializes and digests the metric set (`attr(metrics, "metrics")`), control settings (`unclass(control)`, capturing `save_pred`, `extract`), model/workflow specification digest, resample split IDs, and data hash. Changing any metric (e.g. from `rmse, rsq` to `mae`), control option, or data slice immediately invalidates stale checkpoints and recomputes.
* **Informative Mismatch Reporting (H2)**: When cached checkpoints are invalidated due to changed fingerprints, a clear, unified message is emitted per call stating exactly how many cached tasks were discarded and recomputed.
* **Universal Checkpoint Arguments & Coxnet/Alpha Sharding (H3)**: Added and exposed `checkpoint_dir`, `shard`, and `checkpoint_mismatch` on all `cv_coxnet()` methods (`data.frame`, `matrix`, `formula`, `recipe`) and added `shard` support to `tune_over_alpha()`.
* **Consolidated Fold-Level Checkpoints**: In `nested_survival_run()`, intermediate results are saved consolidated by outer fold (`tune_outer_<fold>.rds` and `score_outer_<weights>.rds`), reducing thousands of task files down to ~10 clean checkpoint files per domain.
* **Checkpoint Utilities**: Upgraded `collect_checkpoints()` and `clean_checkpoints()` to operate uniformly across all checkpointed functions, directories, and envelope containers.

## Function Naming, Deprecation Schedule & Export Cleanup (H4)

* **Export of `write_xlsx_file()`**: Added and exported `write_xlsx_file(x, path, ...)` wrapping `writexl::write_xlsx()` without name collision.
* **Deprecation Schedule for Masked CRAN/Core Names**: Masked exports (`clean_names`, `write_xlsx`, `render`, `render_me`, `like`, `%like%`, `%notin%`) are formally scheduled for complete removal in **TempleCBE 0.6.0 (Q1 2027)** with clear deprecation notices.
* **Deprecation Schedule for 58 Presentation Aliases**: The 58 legacy `cbe_*` aliases are formally scheduled for removal in **TempleCBE 0.6.0 (Q1 2027)** in favor of canonical verb-noun functions.
* **Workflow Map Seed Handling (H5)**: `workflow_map_checkpointed()` handles `seed` and `verbose` as formal parameters rather than forwarding them through `...`, eliminating "`...` are not used: seed" warnings on tidymodels fits.

## Tuning Run Comparison & Diagnostics (J1, H7, J2)

* **Flexible Hyperparameter Configuration Matching (J1)**: Enhanced `compare_tuning_runs(tune1, tune2, match = c("exact", "nearest", "binned"), n_bins = 10)` to support:
  * `"exact"`: Exact matching on `alpha` (and `formula`).
  * `"nearest"`: Matches each hyperparameter configuration in `tune1` to the closest configuration in `tune2` by minimum $|\alpha_1 - \alpha_2|$ distance.
  * `"binned"`: Discretizes $\alpha$ into `n_bins` equal-width intervals for comparing runs with different random $\alpha$ grids.
* **Overall IBS Distribution Comparison (J1)**: Computes global distributional summaries across all model fits in both runs (best IBS, min, Q25, median, mean, Q75, max) and performs a two-sample Kolmogorov-Smirnov test (`ks.test`) to assess distributional shifts between runs.
* **Parallel Tuning Scaling & Timing Breakdown (H7)**: Added `timing_breakdown` attribute and informative progress breakdown to `tune_alpha_by_split()` and `nested_survival_run()`, reporting setup time, task evaluation time, total CPU fit compute, effective parallel speedup, and socket/IPC serialization overhead.
* **Sanitized Child Report Status (J2)**: Stripped stray quotes from domain status formatting in `inst/templates/nested_survival_report.qmd`.

## Function Naming & the `cbe_` Convention

* **Strict `cbe_*` Scoping**: Codified the naming policy across documentation: `cbe_*` is reserved strictly for Temple CBE presentation and formatting only (themes, palettes, scales, plots, report tables, number formatters). Every computational or data transformation function is a plain verb-noun.
* **Deprecation of Legacy Aliases**: All 49 byte-identical `cbe_*` aliases and non-formatting `cbe_*` exports have been deprecated with `lifecycle::deprecate_warn("0.5.8", ...)` in favor of canonical verb-noun functions.
* **Canonical Renamings**:
  * `summarize_tune_results()` $\rightarrow$ `tune_alpha_by_split()`
  * `normalize_safely()` $\rightarrow$ `normalize_path_safely()`
  * `find_correlation()` $\rightarrow$ `drop_correlated_columns()`
  * `proc_contents()` $\rightarrow$ `describe_columns()`
  * `proc_pca()` $\rightarrow$ `run_pca()`
  * `SumNa()` $\rightarrow$ `count_na()`
  * `R_names()` $\rightarrow$ `clean_names_keep_labels()`
  * `keep_only()` $\rightarrow$ `rm_all_except()`
  * `read_search()` / `write_search()` $\rightarrow$ `find_read_calls()` / `find_write_calls()`
  * `extract_win_posix_paths()` $\rightarrow$ `extract_xlsx_paths()`
  * `get_model_parameters()` / `fit_n_rank()` $\rightarrow$ `workflow_set_params()` / `workflow_set_rank_fit()`
  * `factor_reference()` $\rightarrow$ `relevel_factor()`
  * `sd.error()` $\rightarrow$ `std_error()`
  * `is.int()` $\rightarrow$ `is_integer_valued()`
  * `install.packages.no_lock()` $\rightarrow$ `install_package_no_lock()`
  * `my_summary_table()` $\rightarrow$ `summary_table()`
  * `controls_datasets()` $\rightarrow$ `produces_data()`
  * `render()` / `render_me()` $\rightarrow$ `render_pipeline()`
* **Disambiguated Masked Exports**: Replaced exports masking CRAN/core packages with non-conflicting names: `clean_names_keep_labels()`, `match_like()`, `render_pipeline()`, and `write_xlsx_file()`, retaining legacy wrappers with deprecation warnings.

## Packaged Quarto Child Report Template

* **`nested_survival_report.qmd` & `nested_survival_report_template()`**: Packaged an end-to-end Quarto child template in `inst/templates/nested_survival_report.qmd` accessible via `nested_survival_report_template()`. Automatically renders executive summaries, hyperparameter tuning paths, outer-fold mixed models, standardized Cox coefficients, LOCO importance, censoring comparisons, and correlation structures from a `nested_survival_run` object and `cbe_labels()`.

## Bugs & Gaps (G1 – G5)

* **G1 (Parallel Scaling)**: Enforced single-threaded BLAS in future workers and introduced detailed timing breakdowns (`avg s/task`) when `progress = TRUE`.
* **G2 (Repeated Censoring Notice)**: Muffled repeated "no censored subjects" notices inside parallel workers and emitted once per run in the main orchestrating session.
* **G3 (Checkpoint Proliferation)**: Consolidated checkpoints into fold-level files.
* **G4 (Censoring Comparison Checkpointing)**: Added checkpointing and sharding arguments to `compare_censoring_weights()`.
* **G5 (Historical Tuning Results Adapter)**: Added `as_tuning_results()` to adapt legacy tuning files and tidymodels objects into standardized `cbe_tuning_results` compatible with `compare_tuning_runs()`.

# TempleCBE 0.5.7

## Gaps & Bug Fixes

* **Flat Parallelism Scaling & Worker Shared-Object Caching (`summarize_tune_results()`, `score_outer()` — D1):** Eliminated IPC serialization overhead when scaling across parallel workers: shared objects (`data`, `recipe`, and `splits`) are now bundled once per run and loaded once per worker into a process-local cache (`.worker_cache`), each resampling split is prepped and baked once per worker when the recipe is RNG-free (`get_worker_prepared_split()`), and parallel workers enforce single-threaded BLAS/OpenMP (`OMP_NUM_THREADS=1`, `OPENBLAS_NUM_THREADS=1`, `MKL_NUM_THREADS=1`) to prevent thread oversubscription. Added `chunk_size = NULL` to control batching and documented the optimal worker count (`7–8` workers on a 16-core machine).
* **Seed Scheme Versioning & Numeric Change Notice (`summarize_tune_results()`, `score_outer()`, `write_run_manifest()`, `collect_checkpoints()` — D2):** Documented that row-by-row tuning numerics for a given `seed` changed between v0.5.5 and v0.5.6+ because v0.5.6+ pre-generates deterministic per-task seeds across the flat `split × alpha` grid (`sample.int(1e9, n_tasks)`) rather than advancing the RNG sequentially split by split (while overall distributional summaries such as median IBS remain stable). `write_run_manifest()`, `compute_checkpoint_fingerprint()`, `write_verified_checkpoint()`, `read_verified_checkpoint()`, and `collect_checkpoints()` now record and verify `seed_scheme = "v0.5.6"` so checkpoints from earlier seed schemes are never mixed silently.
* **Immediate Error on Missing `feature_names` (`glmnet_IBS()` — D3):** Moved the `feature_names` validation outside the internal `tryCatch()` block in `glmnet_IBS()` so passing `feature_names` where none exist in the baked training data stops immediately with an informative error instead of returning a `failure_ibs` row with a warning.

## New Features

* **One-Call End-to-End Nested Survival Run (`cbe_nested_survival_run()`, `nested_survival_run()` — E1):** Added and exported `cbe_nested_survival_run(data, recipe, id, start, stop, status, domain, run_dir, shard = c(i, n), ...)` executing the full 7-step nested survival workflow in order (`validate_start_stop()` → grouped `nested_cv()` folds → `summarize_tune_results()` → `select_candidate_models()` → `score_outer()` → `summarize_outer_importance()` → `compare_censoring_weights()`), persisting fingerprinted checkpoints and `render_manifest.json` into `run_dir`, resuming automatically on rerun, and returning a `cbe_nested_survival_run` object with `print()`, `summary()`, `plot()`, `autoplot()`, and `tidy()` S3 methods.
* **Self-Rendering Reproducibility Appendix (`repro_appendix()`, `repro_appendix_template()` — E2):** Added and exported `repro_appendix()` (plus packaged Quarto child `inst/templates/repro_appendix.qmd` via `repro_appendix_template()`) that renders package versions, git commit SHA and branch, RNG seed and `seed_scheme`, active `future::plan()` and worker count, dataset SHA-256 hashes, `validate_start_stop()` results, checkpoint fingerprints, and the render manifest.
* **Skip Duplicate Scoring When Training Data Has No Censored Subjects (`compare_censoring_weights()` — E3):** `compare_censoring_weights()` now inspects training splits up front via `has_censored_subjects_in_splits()`. When no subjects are censored in the training data, it runs `score_outer()` only once and populates both `ibs_none` and `ibs_ipcw` from that single fit (setting `attr(x, "single_fit_no_censoring") <- TRUE`).
* **Per-Domain Child Report Helper With Failure Isolation (`cbe_domain_children()`, `domain_children()` — E4):** Added and exported `cbe_domain_children(domains, child, ...)` wrapping `knitr::knit_expand()` and `knitr::knit_child()` across domains in isolated per-domain environments, catching and formatting failing domains into warning callouts (`on_error = "continue"`) while recording per-domain status (`$status`, `$failed_domains`, `$succeeded_domains`).
* **Checkpoint Cleanup & In-Place Mismatch Overwrite (`clean_checkpoints()`, `checkpoint_mismatch = "overwrite"` — E5):** Added and exported `clean_checkpoints(dir, keep = c("current", "matching", "none"), ...)` to inspect and remove stale or mismatched checkpoint `.rds` files. Added `checkpoint_mismatch = "overwrite"` (and global options `templecbe.checkpoint_mismatch = "overwrite"` / `templecbe.checkpoint_overwrite = TRUE`) to `summarize_tune_results()`, `score_outer()`, `tune_over_alpha()`, and `cbe_nested_survival_run()` to overwrite mismatched checkpoints in place without warning.
* **Finish-Time Completion Hook for Long Runs (`on_complete` — E6):** Added `on_complete = getOption("templecbe.on_complete", NULL)` to `summarize_tune_results()`, `score_outer()`, `tune_over_alpha()`, `compare_censoring_weights()`, `cbe_nested_survival_run()`, `render()`, `cv_coxnet()`, and `nested_cv_coxnet()`, invoking `on_complete(payload)` with `fn`, `status` (`"completed"` or `"failed"`), `elapsed_sec`, `result`, `error`, and `meta` when a run finishes or errors.
* **End-to-End `rossi_data` Vignette With Multi-Machine Sharding (`vignettes/rossi_nested_survival.Rmd` — E7):** Added a worked vignette running E1-style steps end-to-end on `rossi_data()`, demonstrating sharding across machines (`shard = c(1, 2)` and `shard = c(2, 2)`), merging with `collect_checkpoints()`, cleaning up old checkpoints with `clean_checkpoints()`, and rendering `repro_appendix()`.

# TempleCBE 0.5.6

## Bug Fixes

* **Checkpoint Input Fingerprinting (`score_outer()`, `summarize_tune_results()`, `tune_over_alpha()` — C1):** Checkpoint `.rds` files now store SHA-256 input fingerprints covering the split row identifiers and outcome/covariate data, recipe specification, feature names, `glmnet` arguments (`cox.ties`, `nlambda`, `internal_folds`, `rule`, `covariates`, `tune_engine`), `censoring_weights`, `seed`, and `TempleCBE` package version. Rerunning with the same `checkpoint_dir` after changing any input or setting now detects the mismatch and either recomputes with a clear warning (`checkpoint_mismatch = "recompute"`, default) or halts immediately (`checkpoint_mismatch = "error"`).
* **All-Failed Tuning Guard (`summarize_tune_results()`, `glmnet_IBS()` — C2):** `glmnet_IBS()` now stops immediately if none of the requested `feature_names` exist in the baked dataset, and `summarize_tune_results()` halts with an informative error when all tuning fits fail or every `IBS` is `NA`, while summarizing partial fit warnings at most once per split.
* **Clean Search Path in `glmnet_IBS()` (`R/survival_metrics.R` — C3):** Removed unconditional `attachNamespace("recipes")` in `glmnet_IBS()`. Unqualified `recipes` step selectors (`all_numeric_predictors()`, etc.) are now resolved via scoped environment binding (`bind_recipes_namespace()`) and an `on.exit()` search-path guard so `package:recipes` is never left attached to the user's session.

## New Features

* **Content-Hash Staleness Tracking (`render()`, `get_render_plan()`, `is_stale()`, `record_pipeline_hashes()` — N1):** Added `stale_by = c("mtime", "hash")` and `hash_file` (`pipeline_hashes.json`). Under `stale_by = "hash"`, SHA-256 digests of declared inputs, scripts, and outputs determine staleness, preventing false rebuilds or missed updates when file modification times shift across Windows, Linux, DGX Spark, or `git checkout`.
* **Multi-Machine Sharding & Checkpoint Merging (`summarize_tune_results()`, `score_outer()`, `collect_checkpoints()` — N2):** Added `shard = c(i, n)` to `summarize_tune_results()` and `score_outer()` so machine $i$ of $n$ evaluates its deterministic share of tasks into a shared `checkpoint_dir`. Added and exported `collect_checkpoints()` to verify matching `checkpoint_run_fingerprint` hashes across shards (preventing accidental mixing of incompatible runs) and assemble the complete result table.
* **Flat `split × alpha` Parallelism (`summarize_tune_results()` — N3):** Flattened `inner splits × alphas` (e.g., $7 \times 10 = 70$ tasks per outer fold) into a single flat `furrr::future_pmap()` task pool with pre-generated deterministic seeds, utilizing all available CPU cores (e.g., 16+ workers) while preserving identical numerical results for the same `seed`.
* **Pre-Flight Start/Stop Survival Data Validation (`validate_start_stop()` — N4):** Added and exported `validate_start_stop(data, id, start, stop, status)` reporting subject, interval, event, and censored counts, overlapping or gapped intervals, non-positive interval lengths, events occurring before a subject's last interval, rows appearing after an event, tied times, and emitting a prominent warning when no subjects are censored (`n_censored == 0`).
* **Unified Package-Level Variable Label Map (`cbe_labels()`, `tbl_label_map()` — N5):** Added `cbe_labels(map, style = c("short", "full"))` and appendix table generator `tbl_label_map()` (alias `cbe_label_table()`), along with `label_map` and `style` support across `plot_tune_*`, `tbl_tune_*`, `plot_cox_forest*`, `cbe_cox_table()` (alias `tbl_cox()`), `plot_permutation_importance()`, `tbl_permutation_importance()`, `tbl_censoring_comparison()`, `summarize_outer_importance()`, and `plot_outer_importance()`.
* **Outer-Fold Importance Summary & Visualization (`summarize_outer_importance()`, `plot_outer_importance()` — N6):** Added and exported `summarize_outer_importance()` (computing selection frequency, median, mean, IQR, and min/max coefficient range across outer folds from `tidy_outer_coefficients()`) and `plot_outer_importance()` (rendering coefficient boxplots/ranges annotated with selection percentage or selection frequency bar charts).
* **Pre-Run Wall-Time Estimator (`estimate_runtime()` — N7):** Added and exported `estimate_runtime(fn, args, n_sample = 3, ...)` to benchmark a small sample of fits and project sequential, single-machine parallel, and multi-machine sharded runtimes across the full grid.
* **Review Response Tracker & Validator (`cbe_review_tracker()`, `validate_review_tracker()` — N8):** Added and exported `cbe_review_tracker()` to construct structured review sheets from `cbe_docx_review_extract()` output, and `validate_review_tracker()` to reject closed non-trivial items lacking a concrete document location (page, section, table, or figure) or substantive rationale (explicitly rejecting bare `"TRUE"`, `"FALSE"`, `"done"`, `"yes"`, `"no"`, `"ok"`).
* **Date-Stamped Deliverable Bundles & DOCX Diffing (`package_deliverables()`, `diff_deliverables()` — N9):** Added `stamp = TRUE` (or custom date/version string) to `package_deliverables()` to date-stamp archives, and exported `diff_deliverables(old_zip, new_zip)` to report added, removed, modified, and unchanged deliverables with paragraph-level text diffs for `.docx` reports.

# TempleCBE 0.5.5

## Execution Governance & Parallel Plan Safety

* **Sequential-by-Default Execution (`render(workers = 1)`):** `render()` now defaults to `workers = 1` (sequential execution). Adhering to the "never plan() inside functions" fleet rule, `render()` never modifies `future::plan()` unless the caller explicitly passes `workers > 1`.
* **Quarto & Nested-CV Safety:** Running sequentially by default eliminates race conditions and directory contention over project-level state (`.quarto`, `_freeze`, and shared build directories) across concurrent Quarto renders, and ensures nested cross-validation models within reports retain full access to multisession worker pools.
* **External Plan Passthrough:** Setting `workers = NULL` respects any pre-configured active `future::plan()` without altering it.

## Pipeline Staleness, Checkpointing & Reproducibility Extensions

* **Never-Rendered Report Staleness Detection (`get_render_plan()`, `render()`):** Documents declared via `FileOutputs(renders = TRUE)` with no declared output files are now correctly recognized as stale when their expected deliverable files (`.pdf`, `.docx`, `.html`) are missing on disk, ensuring dry-run and planning phases include newly added reports.
* **Checkpoint/Resume for Long Fits (`score_outer()`, `summarize_tune_results()`):** Added `checkpoint_dir` parameter to both functions, persisting individual outer-fold $\times$ model fits and inner-resample tuning results to disk and automatically resuming interrupted runs without recomputing finished tasks.
* **Actionable Duplicate Name Errors & Pipeline Deduplication (`dedupe_pipeline()`):** Duplicate pipeline object name errors now report the exact file paths, indices, and S4 object classes of all conflicting definitions. Added and exported `dedupe_pipeline()` with flexible conflict resolution strategies (`"keep_first"`, `"prefer_outputs"`, `"keep_last"`, `"rename"`).
* **Censoring Weight Sensitivity Table (`tbl_censoring_comparison()`):** Renders output from `compare_censoring_weights()` into clean presentation tables with per-model unweighted vs. IPCW IBS, ranks, rank delta, and rank agreement statistics (Spearman's $\rho$ and Kendall's $\tau$). Supports both tibble and publication-ready `gt` output.
* **Cross-Platform Reproducibility Verification (`compare_runs()`):** Compares and diffs two `score_outer()` runs across operating systems (Windows, Linux, macOS) or architectures within configurable numerical tolerances for IBS, $\lambda$, and coefficient estimates, complete with formatted console reporting.
* **CI/CD & Package Hygiene:** Declared suggested packages (`DiagrammeRsvg`, `lme4`, `renv`, `rsvg`) in `DESCRIPTION`, resolved duplicate documentation aliases and example parse issues in `check_renv_guard`, and ensured unit test portability across headless CI environments.

# TempleCBE 0.5.4

## Pipeline Robustness, Outer Resampling & Modeling Extensions

* **Multi-Split Evaluation in `score_outer()`:** Accepts `nested_cv`, `rset`, or a list of `rsplit` objects in addition to single `rsplit`s, evaluating outer splits $\times$ candidate models from `grid` with an `outer_fold` identifier column.
* **Full Coefficient Reporting & Unnest Helpers (`score_outer()`, `tidy_outer_coefficients()`):** Added `coefficients = c("all", "nonzero")` (default `"all"`) to preserve all feature coefficients (including those shrunk to 0). Added and exported `tidy_outer_coefficients()` (and alias `unnest_outer_coefficients()`) to unnest coefficients into long tidy format for variable-importance plots.
* **Row-Level Candidate Model Selection (`select_candidate_models()`):** Added `by = c("row", "alpha")` (default `"row"`), selecting top-performing alphas appearing in the lowest quantile of individual rows (e.g. `ntile(IBS, 5) == 1L`) in addition to mean-summarized alpha selection.
* **Wave-Based Parallel Rendering & Upstream Failure Skipping in `render()`:** Plans are partitioned into topological waves. Independent branches within each wave render concurrently in parallel via `future`/`furrr` multisession workers. When `skip_downstream = TRUE` and an upstream node fails, dependent downstream reports are skipped with status `"skipped_upstream_failure"` while independent branches continue rendering in parallel.
* **Full Rerun Shorthand (`render(rerun = "full")`):** Added `rerun = c("none", "full")` shorthand; specifying `rerun = "full"` sets `only_stale = FALSE` and `cache = "refresh"`.
* **Parallel Optimization & Options in `glmnet_IBS()`:** Re-enabled and extended `parallel = FALSE` and tidymodels-compatible `parallel_over = c("resamples", "everything")` to evaluate internal CV folds concurrently via `furrr::future_map2()` when enabled.
* **Clean Parameter Namespace (`glmnet_IBS()`):** Removed interception of `tune_method` as an alias for `tune_engine`, avoiding clashes with `nested_cv_coxnet(tune_method = ...)`.
* **Strict Mode & Backend Selection in `zip_reports()`:** Added `docx_strict` (default `FALSE`) and `docx_backend` (default `"auto"`). Under `docx_strict = TRUE`, packaging halts with an actionable error if no PDF-to-DOCX conversion backend is available or if conversion fails. Custom `docx_from_pdf` callback errors propagate directly without being swallowed.
* **Legacy `cv.glmnet` Tuning Reproduction in `glmnet_IBS()`:** Added `tune_engine = c("yardstick", "cv.glmnet")` (default `"yardstick"`). Allows historical reproduction of pre-0.3 penalty tuning across un-grouped intervals with a once-per-session advisory warning.
* **Test Coverage:** Added comprehensive unit tests for `tune_best_glmnet_workflow()`, `write_run_manifest()`, `score_outer()` multi-split evaluation, `tidy_outer_coefficients()`, `select_candidate_models(by = "row")`, wave-based parallel `render(skip_downstream = TRUE)`, `glmnet_IBS(parallel = ...)`, and `zip_reports(docx_strict = TRUE)`.

# TempleCBE 0.5.3

## Core Modeling & Statistical Features

### P0 — Computational Graph Rerun & Censoring Weights
* **Restored `censoring_weights = "none"` in `glmnet_IBS()`:** Restored unweighted Integrated Brier Score computation with a once-per-session advisory warning (`rlang::warn(..., .frequency = "once")`), allowing legacy model reproduction and direct comparison against IPCW.
* **Dual Censoring Weights Comparison (`compare_censoring_weights()`):** Evaluates models across identical outer grids under both `"none"` and `"ipcw"`, returning tidy per-model IBS side by side along with rank agreement metrics (Spearman and Kendall rank correlations).
* **Graph-Driven Full Rerun (`render()`, `get_render_plan()`):** Added `only_stale = FALSE` support to force full pipeline reruns; `from` and `to` topological node range filtering; `dry_run = TRUE` execution planning; restriction of `controls_datasets()` to PDF with automatic DOCX derivation (`convert_pdf_to_docx()`); fine-grained cache control (`cache = c("keep", "refresh", "stale")`); and failure recovery (`on_error = c("continue", "stop")`, `resume = <previous_result>`). Exported `is_stale()`.
* **Seeded Draws in Hyperparameter Tuning (`tune_over_alpha()`, `summarize_tune_results()`):** Added `seed` parameter with caller RNG preservation (`local_seed()`) for deterministic random alpha and feature subset draws across distributed `future` worker clusters.

### P1 — Pipeline Speed, Diagnostics & Outer Fold Helpers
* **Run Manifest & renv Guard (`render()`):** Emits structured `render_manifest.json` recording TempleCBE version, git commit hash, package versions, seed, and per-document wall times. Automatically validates `renv` status against `renv.lock` when active, with option suppression (`templecbe_suppress_renv_warning`).
* **Pipeline Graph Serialization (`save_pipeline()`, `load_pipeline()`):** Serializes computational pipeline objects while refreshing file modification times (`update_mtime = TRUE`) to eliminate stale cache artifacts.
* **Tuning Diagnostics Family (`R/tune_diagnostics.R`):** Added `fit_ibs_mixed_model()` (dynamic mixed model fitting without hard-coded regression equations), `plot_tune_alpha_ibs()`, `plot_tune_lambda_ibs()`, `plot_tune_ibs_by_resample()`, `plot_tune_coef_boxplot()`, `tbl_tune_alpha()`, and `tbl_tune_coef()`.
* **Outer-Fold Workflow Helpers (`select_candidate_models()`, `score_outer()`):** Simplifies outer-fold candidate model extraction and automated evaluation via `future_pmap(glmnet_IBS)`.
* **Diagram Visualization Controls (`visualize_pipeline()`, `export_pipeline_diagram()`):** Added controls for font size, node size, orientation (`TB`, `LR`), and node/rank separation. Exported `export_pipeline_diagram()` to render Word-page-optimized static SVG, PDF, or PNG figures.
* **Nested CV vs. Random-Alpha Documentation & Verification:** Verified `nested_cv_coxnet()` with `group_bootstraps` inner resamples on start-stop counting process data with `patient_id` clustering, adding comprehensive documentation and vignette guidance distinguishing nested CV generalization auditing from random-alpha model selection.

### P2 — Duplication Removal & Presentation Helpers
* **Correlation Toolkit (`R/corr_test_all.R`):** Added `correlated_pairs_table()`, `plot_corr_scatter()`, and `plot_rank_heatmap()`.
* **Cutpoint Optimization (`R/km_cutpoints.R`):** Added `km_cutpoint_search()`, `plot_km_cutpoints()`, and `km_cutpoint_time_varying()` for right-censored and counting process cohorts.
* **Composite Prognostic Indices (`R/composite_index.R`):** Added `center()`, `compute_composite_index()`, and `index_variable_table()`.
* **Imputation Quality Auditing (`R/imputation_diagnostics.R`):** Added `compare_imputed_ecdf()`, `test_imputed_distributions()`, `plot_imputed_ridgeline()`, and `tune_impute_numeric()`.
* **Safe Mixed Model Fitting (`R/lmer_safely.R`):** Added `fit_lmer_safely()` with graceful fallback to `stats::lm()` upon convergence or singularity failure.
* **Presentation & Excel Helpers (`R/label_helpers.R`):** Added `pretty_name()`, `excel_colnames()`, and `copy_var_labels()`.
* **Tidymodels Workflow & Importance (`R/workflow_tuning.R`):** Added `tune_best_glmnet_workflow()`, `summarize_dalex_importance()`, `plot_permutation_importance()`, and `tbl_permutation_importance()`.
* **Plot Deck Extensions (`R/plot_deck.R`):** Added `risk_table` and `p_value` options to `plot_survival_km()`, and arbitrary color scale support to `plot_dynamic_trajectory()`.
* **Parameterized Stage Report Template (`inst/templates/stage_report_template.qmd`):** Added template collapsing duplicate impute/non-impute report pairs.
* **Naming Trap Clarifications:** Clarified `find_correlation()` (drops columns) vs `correlated_pairs_table()` (preserves pairs), and `normalize_safely()` (normalizes paths) vs `min_max_norm()` (scales numbers).

# TempleCBE 0.5.2

## Security & Privacy Hardening

* **Hardened Docker Publish Workflow (`.github/workflows/docker-publish.yml`):** Secured `workflow_run` triggers by strictly verifying `event == 'push'` and matching `head_repository.full_name == github.repository`, eliminating vulnerability to unauthorized image building from fork PR runs. Added direct `push: tags: ['v*']` trigger enabling automated multi-architecture image publication (`linux/amd64,linux/arm64`) on release tags.
* **Non-destructive Secrets Directory Permissions (`R/atomic_io.R`):** `validate_secrets_dir()` now tightens permissions (`0700`) only on newly created directories. Existing directories are left untouched (preventing unintended chmod on user home or working directories), while existing world-writable directories without a sticky bit are rejected. `atomic_write_file()` enforces `0600` permissions on temporary files prior to atomic rename on POSIX platforms.

## Core Modeling & Packaging Improvements

* **Directory Deliverable Copy Guards (`R/bundle_deliverables.R`, `R/zip_reports.R`):** Added explicit `!dir.exists()` guards before invoking `file.copy()`, preventing directory paths from silently copying as empty 0-byte files on older R runtimes (e.g. R 4.3.3).
* **Caller RNG State Preservation (`R/racing_workflows.R`):** `tune_race_survival()` now scopes random seeds via `local_seed()` rather than bare `set.seed()`, cleanly preserving the caller's `.Random.seed` state across tuning routines.
* **Cross-Version `cox.ties` Validation (`R/coxnet.R`):** `fit_glmnet_cox()` explicitly validates `cox.ties` via `match.arg(..., c("breslow", "efron"))`, ensuring consistent parameter rejection across all `glmnet` versions (including 4.1.8).
* **Docker Hub Step Credential Scope (`.github/workflows/docker-publish.yml`):** Moved Docker Hub environment variables (`DOCKERHUB_USER`, `DOCKERHUB_PASS`) to job-level scope so conditional step evaluations (`if: ${{ env.DOCKERHUB_USER != '' }}`) evaluate reliably.
* **Dependency Version Floor (`DESCRIPTION`):** Specified `openxlsx (>= 4.2.6)` in `Suggests` to eliminate upstream `sprintf` format string warnings.

## Test Portability & CI Diagnostics

* **Windows Junction Removal (`tests/testthat/helper-links.R`):** `remove_dir_link()` preserves un-normalized link paths when invoking `cmd /c rmdir`, preventing path resolution to the target directory and eliminating reparse point mismatch warnings.
* **Root Execution Guards (`tests/testthat/test-failure_modes.R`):** Filesystem permission failure tests now safely skip when running under the `root` user in container environments where POSIX permissions are bypassed.
* **Soft Dependency Gates (`tests/testthat/test-joint_model.R`):** Added helper skips for environments missing optional packages `yardstick` or `rsample`.

# TempleCBE 0.5.1

## CI/CD Pipeline & Build Architecture

* **Two-Tier CI/CD Pipeline (Strategy A):** Pushes to `master` execute a fast 5-minute sanity check on Ubuntu before deploying documentation (`pkgdown`) and container images (`docker-publish`), eliminating redundant 35-minute Windows rebuilds on merged code. Pull requests and release tags continue to enforce full cross-platform verification across Ubuntu, Windows, and macOS.
* **Windows CI Acceleration:** `check-r-package` skips redundant vignette rendering (`--no-build-vignettes`) on Windows CI runners, cutting Windows check time from 35+ minutes to ~12 minutes while preserving full vignette building on Ubuntu and `pkgdown`.
* **Sequential Test Execution & Deadlock Prevention:** Removed `Config/testthat/parallel: TRUE` from `DESCRIPTION` and enforced `TESTTHAT_PARALLEL: "FALSE"` in CI. Prevents Windows TCP loopback socket deadlocks caused by nested `future::multisession` clusters running inside `callr` worker processes.
* **Docker Publishing Speedup & Docker Hub Integration:** Switched routine master container builds to native `linux/amd64` with GitHub Actions layer caching, reducing Docker build time from ~50 minutes under QEMU software emulation to ~4 minutes. Automated direct deployment to Docker Hub (`<user>/templecbe`) using repository secrets.
* **On-Demand Matrix Verification:** Added `workflow_dispatch` trigger with `full_matrix` parameter to `R-CMD-check.yaml`, enabling manual execution of the full 3-OS check on any branch directly from the GitHub Actions UI.

# TempleCBE 0.5.0

## Breaking Changes

* **Explicit Secret Key Requirement (`R/pi_anonymizer.R`, `scripts/pi_anonymizer.py`):** The keyed HMAC methods of `anonymize_pi()` (`method = "hmac_token"`, the default, and `"hmac_surname"`) and `generate_pseudonym_token(name = )` now stop unless a secret key is supplied through `key` or the `TEMPLECBE_SECRET_KEY` environment variable, because pseudonyms made with a public key can be recomputed from a list of names. The old public built-in key is reachable only through `allow_default_key = TRUE`, which warns that the output is not secret. A key shorter than 16 characters only triggers a warning. `method = "token"` and `"synthetic"` need no key.
* **External Pipeline Stage Configuration (`cbe_docx_review_extract()`):** The internal 38-stage review pipeline catalog, fuzzy stem aliases, and specific project directory defaults have been decoupled from the package repository for privacy and generalization. Downstream pipelines must configure stage matching and directories via `review_config(pipeline_catalog = ..., stem_aliases = ..., analysis_dir = ..., input_dirs = ...)`. Documents without explicit configuration default to category `"Unknown / Extra"` sorted by filename.
* **Mandatory `subject_id` for Counting-Process Cross-Validation (`cv_coxnet()`, `cv_joint_model()`, `nested_cv_joint_model()`):** Cross-validation and out-of-fold scoring on start-stop survival data (`Surv(start, stop, event)`) now strictly require `subject_id`. Resamples that place a subject in both analysis and assessment splits fail immediately with an informative overlap error (`check_subject_overlap = TRUE`), preventing overoptimistic evaluation.

## Security & Privacy

* **Redacted DOCX Review Tracking (`cbe_docx_review_extract()`):** Reviewer fork workbooks now redact sign-off columns, private comments, and original Word author identities across all extracted sheets (`Comments`, `SuggestedChanges`, `TrackedChanges`). CSV exports default to `"fork"` reviewer column projection, and cell values commencing with formula characters (`=`, `+`, `-`, `@`, tab, CR) are escaped with a leading single quote (`'`) to neutralize CSV injection. Document paths are stored relatively (`<tempdir>`, `<input_dir>`, `<home>`) rather than revealing host filesystem user profiles.
* **Safe Atomic File Locking & Process Liveness (`R/atomic_io.R`):** Multi-process concurrency helpers (`with_file_lock()`, `atomic_write_json()`) record owner PID, verify cross-platform process liveness via non-destructive priority polling (`tools::psnice()` on Windows / POSIX signals), safely evict stale locks using atomic directory moves, and resolve concurrency race conditions. Secrets path traversal (`..`) and outside-repository directory traversal are strictly blocked.

## Statistical Enhancements

* **Survival benchmark smoke tests (`tests/testthat/helper-survset.R`, `tests/testthat/test-survset_benchmarks.R`):** Five classic cohorts from R's \pkg{survival} package (`veteran`, `lung`, `rotterdam`, `heart` as counting-process data, and `colon`), reshaped to the column layout of the SurvSet collection, are fitted with `coxnet()`, `cv_coxnet()`, `joint_model()` (glmnet, baguette and stacks engines) and the canonical Cox/Kaplan-Meier wrappers. The tests check that every fit and prediction runs and returns well-formed output (classes, dimensions, finite values); they do not compare results with reference values, and the SurvSet package itself is not used.
* **SAS `PROC PRINCOMP` parity benchmark for `proc_pca()` (`inst/sas/benchmark_princomp_iris.sas`):** SAS 9.4 was run on the 150 rows of `datasets::iris` (typed into the program, so R and SAS see the same numbers); its listing and the numbers copied from it (`tests/testthat/reference/sas_princomp_iris.csv`) are committed, and `test-sas_parity_reference.R` compares the eigenvalue table of `proc_pca()` with them (eigenvalues and differences to 8 decimals, proportions to 4) without needing SAS; `test-sas_parity_live.R` re-runs SAS when `TEMPLECBE_RUN_SAS_TESTS=true`. Until now the PCA parity claim in the README and vignette rested on numbers typed into the vignette.
* **`cbe_compare_df()` is benchmarked against SAS 9.4 `PROC COMPARE` (`inst/sas/benchmark_proc_compare.sas`):** nine comparisons on data typed into the program (`METHOD=ABSOLUTE`, criterion equal to the tolerance, with and without an ID variable, duplicate IDs, differences of exactly the tolerance, decimal and text cases). The SAS listing and log are committed (the licence site number is redacted) and the numbers are in `tests/testthat/reference/sas_proc_compare.csv`. `test-sas_parity_reference.R` compares every mapped statistic (variables and observations in common or in one data set only, observations and values that differ, the maximum difference, every unequal cell) without needing SAS, and `test-sas_parity_live.R` re-runs SAS when `TEMPLECBE_RUN_SAS_TESTS=true`. The two agree on all 252 statistics of eight scenarios; in the ninth, a trailing blank in text, SAS counts one unequal value and R two. `?cbe_compare_df` lists where the definitions differ (the sign of the difference, a type conflict that R compares as text, R classes, `NA` against `""`, attributes such as length, the absolute method only), and "SAS PROC COMPARE Parity" in the title, print header, README and vignette now reads "modelled on".
* **Survival Racing Workflows (`R/racing_workflows.R`):** Introduced ANOVA and win-loss racing interfaces for accelerated hyperparameter tuning via `finetune` (`control_race_survival()`, `tune_race_survival()`) compatible with individual workflows and `workflowsets::workflow_map()`; the results are ordinary `tune_results`, so `tune::select_best()`, `tune::show_best()`, `tune::fit_best()` and `finetune::plot_race()` work on them. When `eval_time` is not given, `tune_race_survival()` uses the deciles of the observed event times read from the workflow's formula or recipe (tune itself needs at least two evaluation times for the integrated metrics and has no default). Racing dispatch (`tune_method = c("grid", "race_anova", "race_win_loss")`) is integrated into `nested_cv_coxnet()` and `nested_cv_joint_model()` (`"none"` instead of `"grid"` in the latter); it needs a right-censored outcome, because parsnip's censored regression cannot fit start/stop data, and stops early with that explanation otherwise. `"race_win_loss"` needs the \pkg{BradleyTerry2} package.
* **Canonical CBE Statistical API Refactoring:** Refactored core functions to clean, standardized, canonical function names (`km_single`, `cox_single`, `cox_multi`, `cox_check`, `cox_table`, `exact2x2`, `exact2x2_ci`, `test_categorical`, `compare_df`, `database_relationships`, `database_venn`, `find_shared_keys`, `check_key_integrity`, `variable_roles`, `set_roles`, `get_roles`, `get_predictors`, `get_outcomes`, `get_id_cols`, `get_time_cols`, `contingency_plot`, `balloon_plot`, `bar_plot`, `mosaic_plot`, `heatmap_plot`, `four_quadrant_report`, `square_plot`, `pairwise_combos`, `docx_review_extract`, `factor_reference`, `sas_macro_dir`, `sas_macro_path`, `dataset_label`, `database_name`, `database_label`) while preserving 100% backwards-compatible `cbe_*` aliases.
* **Imputation and PCA Follow-ups (Chip C4):** Deterministic per-mtry seeding in `missforest_sweep_mtry()`, strict mtry validation and deduplication, caller RNG state restoration, unscaled Kaiser line suppression in `pca_scree_plot()`, 6-column `proc_pca()` output documentation, and factor-level collision guards in `step_famd()`.
* **Clean Document & Manual Publishing (Chip C6):** Versioned PDF manual generation (`build_manual_versioned()`) producing `TempleCBE_0.5.0.pdf` and `TempleCBE_latest.pdf`, restored pkgdown manual navigation, and re-rendered vignette PDFs passing comprehensive denylist clearance.

## Feature: Clean Publish Snapshot Mode & Remote Guard Fences (`R/clean_publish.R`, `scripts/clean_publish.R`)

Audit packet C5 findings in release distribution, incremental history, and push isolation:

* **Clean publish snapshot mode (`mode = "snapshot"`):** `clean_publish()` now supports `mode = "snapshot"` (CLI `--snapshot`), enabling incremental public releases without overwriting earlier releases or requiring force-pushes. In snapshot mode, `clean_publish()` fetches the remote publish branch tip, rejects identical trees (`diff-tree --quiet`), generates a single clean commit with the remote tip as its parent commit, and pushes forward as a standard fast-forward push.
* **Public remote fence (`guard_public_remote()`):** Exported `guard_public_remote(public_url)` (CLI `--fence <url>`). Configures `cleanpublish.publicurl` and updates the pre-push hook to refuse a `git push` whose push URL, after git's own rewriting and a normalisation of its spelling (SSH SCP-style, SSH URL, HTTPS), is a fenced URL, unless the push is made by `clean_publish()` (signaled via the environment variable `TEMPLECBE_PUBLISHING = "1"`, which since the stage 1 hardening below lets through nothing but the clean commits it recorded). It is a hook of command-line git: `git push --no-verify`, clients that run no hooks (such as `gert`) and an explicit URL of another spelling are not stopped, as the sections below and `?guard_public_remote` say.
* **Tracked-but-ignored file validation (A5-15):** `clean_publish()` scans for tracked files that match `.gitignore` patterns (`git ls-files -ci --exclude-standard`). If any are found, publishing is halted immediately to prevent accidental exposure of ignored secrets or cache files, unless explicitly whitelisted via `allow_ignored` (CLI `--allow-ignored <file>`).
* **Remote refs check (A5-15), replaced:** the first version warned, after the push, about other branches and tags of the remote (`git ls-remote --heads --tags`), and never saw `refs/pull/*` or notes. Since the stage 2 hardening below, standalone mode lists all refs of the remote before anything changes and stops unless `allow_other_refs = TRUE`.
* **Force-with-lease push isolation:** In standalone mode, `clean_publish()` pushes with `--force-with-lease`, so that it does not overwrite a remote branch that moved without its knowledge; since the stage 2 hardening below the lease names the branch and the id the listing of the remote showed (`--force-with-lease=refs/heads/<branch>:<id>`) and no longer depends on a remote-tracking ref.

## Behaviour change: `clean_publish()` first round of the pre-0.5.0 review (R-CP-01, 02, 03, 06, 10, 12)

The first fixes of the independent review of the publish path (commit 6cf87a5). Later rounds, below, changed some of them; each bullet says how it stands now. Pinned by tests in `tests/testthat/test-clean_publish.R` that use real git and local bare repositories.

* **There is no default remote (R-CP-02).** `remote = "origin"` was the default, and so was `push = TRUE`: a bare `clean_publish()` in a clone of the private repository force-pushed one commit over the private repository's master (3 commits became 1). `remote` has no default now; it is required for `push = TRUE` and for `mode = "snapshot"`, and a dry run of a standalone publish needs none. The script needs `--remote NAME` with `--push` and with `--snapshot`.
* **Snapshot mode refuses a remote that holds the private history (R-CP-01).** A snapshot built on a remote that held the private history (the default `origin` in a clone of the private repository) got a private commit as its parent, and the pre-push hook trusts what this function makes, so a later `git push <remote> master` uploaded the whole history. The first fix refused a remote tip that shares a commit with the commit being published; the stage 2 hardening below replaced that test by the `Clean-Publish: v1` trailer check, which a shallow clone and a rewritten copy of the history did not defeat.
* **Every check works from the git toplevel (R-CP-03).** The tracked-but-ignored check ran from the directory it was given, so from a subdirectory (the script uses the working directory) it missed top-level files, which were published.
* **`guard_public_remote()` refuses a URL with whitespace (R-CP-06).** The hook compares URLs word by word and never matched such a fence while `Guarded` was printed.
* **Smaller items (R-CP-10, R-CP-12).** A snapshot commit's default message is `Snapshot <date>`, no longer `Initial clean commit`; the script's `--fence` combined with another option is an error (status 2), where the other option used to be dropped without a word; the dry run of a snapshot says that it is read-only.
* **Documented, then fixed (R-CP-04, R-CP-05).** The first round only documented that the guard does not stop `gert` or a renamed private branch; the stage 1 and stage 2 hardening below fixes both.

## Hardening: `clean_publish()` push guard and `guard_public_remote()` fence (`R/clean_publish.R`)

Independent review of the publish path, stage 1 (the pre-push hook, the fence and the self-test). Each item is pinned by tests in `tests/testthat/test-clean_publish.R` that push with real git to a local bare repository or run the generated hook by hand under every POSIX shell found on the machine (`sh`, `dash`, `bash`):

* **The guard protects private history by commit id, not by the name of a branch.** Before it pushes or moves anything, `clean_publish()` records the commit it publishes, and the earlier tips of the private and the publish branch unless they are clean commits, as full ids in the new multi-valued git config key `cleanpublish.privatetip` (append only). The hook treats those ids, and the live tips of the protected branches, as the private history, so renaming, deleting or moving the private branch no longer disables it, and neither does a first publish whose push failed (the private branch did not exist yet, and a plain `git push <remote> master` was accepted). A tag, an object id, a backup ref and `--mirror` stay refused. When protected branches are configured and neither they nor a recorded tip can be found, the hook no longer skips them: it refuses everything that is not made of clean commits and names `git config --unset-all cleanpublish.protectedbranch` as the way to release the guard.
* **`cleanpublish.cleancommit` and `cleanpublish.privatetip` values are read as full object ids and nothing else.** A branch name, `HEAD`, an abbreviated or upper-case id or a tag id used to count as a clean commit and let the private history through; now such a value is ignored (a private tip that does not exist here is ignored with a note on standard error).
* **A tag, tree or blob that does not lead to a commit is checked, not skipped.** An annotated tag is refused whatever it points at; a tree or blob is let through only when a clean commit has it.
* **`TEMPLECBE_PUBLISHING=1` is no longer a general bypass.** Under it the hook lets through only refs whose new object id is a clean commit recorded by `clean_publish()`; the private branch and every other ref are refused. Only the fence rule is bypassed.
* **One URL normalisation in R and in the hook** (`.cp_normalize_url()` and the hook's `_tcbe_norm_url`, compared over a corpus of spellings under every shell): backslashes first, then a scheme and the user name up to the last `@` before the first slash (so `https://github.com/org/repo@v2.git` keeps its `@`), `.GIT` like `.git`, and the same result when the normalised value is normalised again. The hook messages use `printf`, so backslashes in a Windows path survive.
* **`guard_public_remote()` accepts the name of a configured remote** and stores its fetch and push URLs; a URL must match the push URL of a configured remote (relative paths, links, `file://` and `host:path` spellings included) or the call stops, unless `allow_unmatched = TRUE` is given for a remote that will be added later. The hook also compares the physical path of a local directory and the `url.<base>.insteadOf` expansion of each fenced value. After writing, the function runs the hook by hand for every configured remote and every fenced URL, with `TEMPLECBE_PUBLISHING` unset, and stops when the hook is missing, not executable, or lets a push to a fenced remote through. Its documentation now lists what the guard does not stop (`--no-verify`, `gert`, `core.hooksPath`, an explicit URL of another spelling) and the examples use a placeholder URL. Its default repository is the git toplevel of the working directory (`.cp_default_repo_root()`, an error outside a repository), no longer `here::here()`.
* **An unconfirmed guard is an error before anything moves.** The self-test pushes the private branch name, and the commit being published under another branch name and as a lightweight tag, to a temporary repository; each push must be refused by the hook for its content ("contains commits"). A refusal for any other reason stops `clean_publish()` before a branch moves or anything is pushed; `confirm_guard = FALSE` turns that case into a warning. A guard that accepts one of the pushes is an error either way.
* **Hooks.** The managed block is only added to `sh`, `dash`, `bash` and `ksh` hooks (it relies on word splitting; a `zsh` hook stops the run with a message). It works under `set -e`, `set -u` and `set -f` of the hook it sits in and no longer hands a blank record to a following hook when nothing is pushed.

## Hardening: `clean_publish()` main flow, breaking changes (`R/clean_publish.R`, `scripts/clean_publish.R`)

Independent review of the publish path, stage 2 (the flow of `clean_publish()` itself; the hook, the fence and the self-test are the section above). These are **breaking changes for anyone who calls `clean_publish()` or the script**: a call that used to go through, with a warning or without a word, can now stop, and a successful push now changes the configuration of the remote. Each item is pinned by tests in `tests/testthat/test-clean_publish.R`, which use real git and local bare repositories as remotes:

* **The default repository is the git toplevel of the working directory, not `here::here()`.** `here::here()` is fixed when the `here` package is loaded, so a session started in the real repository and moved into a throwaway clone rewrote the real one. `clean_publish()` now prints the repository it acts on, its git directory, the current branch and every remote with its fetch and push URLs (credentials removed) before it checks or changes anything. The command-line script names `getwd()` and prints the same.
* **Identity pre-flight.** `git var GIT_AUTHOR_IDENT` and `GIT_COMMITTER_IDENT` are checked before anything is written; without an identity (a fresh clone with no `user.name` or `user.email`) the run stops with the way to set one, where it used to fail at `commit-tree` after the hook, the configuration and a backup ref had been written, so "nothing is changed until every check has passed" holds again. The author, the committer and the time zone offset that will be published in the clean commit are printed. They come from git's own settings, the `GIT_AUTHOR_*` and `GIT_COMMITTER_*` environment variables included; there is no author argument.
* **Branch names are validated as `refs/heads/<name>`.** `git check-ref-format --branch` expanded `@{-1}` to the previous branch and was skipped for existing branches; a name that starts with `@` or contains `@{` is refused too. A publish branch that differs from the private branch only in case is refused (on a case-insensitive file system it is the same branch, and publishing onto it squashed the private history), and after the moves `clean_publish()` checks that both branches are where it put them and otherwise stops with the backup refs.
* **Every clean commit ends with the trailer `Clean-Publish: v1`, and snapshot mode builds only on a remote branch made entirely of such commits.** After fetching the remote tip, `clean_publish(mode = "snapshot")` requires the history of that tip to be linear, to have one parentless root and every commit to carry the trailer as a line of its own; otherwise it stops and says how many commits lack it and which is the newest, with "Nothing was changed". This replaces the test for a commit shared with the current history, which a shallow clone (a hidden fork point), a rewritten copy of the private history (new commit ids) and a git error (status 128 read as "no shared history") defeated, and which refused for ever once the public tip had been merged back into the private branch (the public snapshots are clean commits, so that is accepted now). A **shallow repository is refused** in snapshot mode. Public branches made by an earlier version, whose commits have no trailer, and public branches that received commits of other people are refused; publish standalone into a new empty repository. The remote tip is fetched without writing a remote-tracking ref or a tag.
* **Standalone mode examines the remote before anything changes.** It lists all refs of the push URL (`git ls-remote`; a failing listing is an error, no longer skipped without a word) and stops unless `allow_other_refs = TRUE` when the remote has any ref besides the publish branch, naming them and saying that GitHub's hidden `refs/pull/*` cannot be removed, so that the target should be a new empty repository (the warning it replaces came after the push, saw only branches and tags, and the SUCCESS line claimed "exactly 1 clean commit"). It stops unless `allow_overwrite_history = TRUE` when the tip of a branch of the remote is a commit that exists here, is reachable from the commit being published, from the private or the publish branch or from a recorded private tip, and is not a clean commit: the remote holds this repository's history, as `origin` does in a clone of the private repository, whose branch a standalone publish overwrote with one commit. The push carries an explicit lease, `--force-with-lease=refs/heads/<branch>:<id from the listing>` (empty when the branch does not exist), so it fails when the branch moved since the listing, and the natural runbook (a clone whose `origin` was renamed and re-pointed) no longer fails with "stale info" because of a stale remote-tracking ref. The SUCCESS message says that the publish branch of the remote now points to the clean commit and lists the other refs that remain.
* **The push URL of the remote is disarmed after a successful push, and the push goes to the URL itself.** `clean_publish()` reads the real push URL with `git remote get-url --push --all`, refuses a remote with more than one push URL, and pushes to that URL (`git push <url> <commit>:refs/heads/<branch>`, with `TEMPLECBE_PUBLISHING=1` as before, which the hook and the fence still see). After the push it records the URL in the git config key `cleanpublish.<remote>.realpushurl` and sets `remote.<remote>.pushurl` to `DISABLED-use-clean_publish`, so that a later `git push <remote>`, `git push --no-verify`, a changed `core.hooksPath` and `gert::git_push()` (which never runs the hook) fail instead of publishing the private branch; fetching is not affected, later runs, snapshot mode included, read the recorded URL, and `guard_public_remote()` fences the recorded URL of a disarmed remote. `disarm_push_url = FALSE` (`--no-disarm`) leaves the remote as it was; `git remote set-url --push <remote> <url>` undoes it. A push to the real URL, `git send-pack` and other tools that do not read the remote's configuration are not stopped. Every URL that is printed or appears in an error message has its credentials removed, and a step that fails after the push reports that the remote is already updated and names the clean commit.
* **Submodules are refused.** A current commit with a gitlink or a tracked `.gitmodules` stops the run, listing the gitlinks and the submodule URLs (credentials removed) that would be published, unless `allow_submodules = TRUE`.
* **Snapshot mode prints what it changes.** Before committing it prints `git diff --stat` between the remote tip and the new tree and the number of paths it deletes (public-only files are replaced wholesale); this is information, not a check.
* **Gates fail closed.** A failing `git ls-files -ci` (the tracked-but-ignored check), `git diff-tree`, `git rev-parse`, `git symbolic-ref`, `git rev-parse --show-toplevel` or `git config --get-all` is an error; each used to be read as "nothing found". The clean commit's message file is written as bytes, so the trailer is a line of its own on Windows too.
* **New arguments and flags**, all logical: `allow_other_refs`, `allow_overwrite_history`, `allow_submodules` (default `FALSE`) and `disarm_push_url` (default `TRUE`) of `clean_publish()`; `--allow-other-refs`, `--allow-overwrite-history`, `--allow-submodules` and `--no-disarm` of `scripts/clean_publish.R`, which are errors with `--fence`.

## Hardening: `clean_publish()` command line and documentation, stage 3 (`R/clean_publish.R`, `scripts/clean_publish.R`)

Independent review of the publish path, stage 3 (the command line and what is written about the publish path). Pinned by tests in `tests/testthat/test-clean_publish.R`.

* **The command line is a function that is tested in-process (`clean_publish_cli()`).** All push control of `scripts/clean_publish.R` (the `--remote` requirement, the refusal of `--fence` with another option, the dry-run default, the exit statuses) was in the script, which is build-ignored, so its subprocess tests were skipped silently under `R CMD check`. It is now the internal function `clean_publish_cli(args)`, installed with the package and returning the exit status: 0 for success and `--help`, 2 for a bad command line, 1 when the run stops with an error. The script only loads the package (`pkgload::load_all()` on the copy it sits in; `pkgload` replaces the `devtools` requirement) and calls it. The new tests run it in-process in throwaway repositories: bad flags, `--help`, `--push` and `--snapshot` without `--remote`, `--fence` with each other option, `--fence NAME` and `--fence URL`, the dry-run default, a standalone and a snapshot publish, `--allow-other-refs`, `--allow-overwrite-history`, `--allow-submodules` and `--no-disarm`, and that nothing is pushed or changed after a bad command line. The subprocess tests of the script stay.
* **Warnings appear when they happen, and one raised before the push stops the run.** The command line runs with `options(warn = 1)`, so a warning is no longer printed after `SUCCESS` (R defers warnings of a script to its end). A warning raised before the push stops the run with status 1 and "Nothing was pushed"; one raised after the push is shown and changes nothing. `--fence` names the repository it acts on (the git toplevel of the working directory), and outside a repository both modes stop with an error.
* **The documentation describes the publish path as it is.** `?clean_publish` and `--help` start by describing both modes (standalone: one parentless commit, force-pushed into a new empty repository; snapshot: one commit on the remote tip, pushed as a fast-forward) where they only described the first, and say that the target must be a NEW EMPTY repository and that a force-push cannot remove GitHub's hidden `refs/pull/*`. The paragraph that told to run once "with the default `origin`" is gone: there is no default remote, so it says to name the remote you added (`git remote add public <url>`). Restoring a backup of the private branch, which is the checked-out branch after every run, says `git checkout <other branch>` first or `git reset --hard <ref>`, because git refuses `git branch -f` for the branch that is checked out. The stale statements in this file about the push guard and the function's signature were corrected.
* **`scripts/deploy_release.R` pushes only with `--push`, to the remote it is told, and rolls back when the push fails (CPD-06, CPD-16).** `push` defaulted to `TRUE`, so a run pushed `HEAD` to the `master` of `origin`, and a tag, without being asked. `push` is now `FALSE` (`--push` on the command line; `--no-push` is still accepted and does nothing), the remote is an argument (`remote = "origin"`, `--remote NAME`), which must be configured and must not start with `-` (checked before anything changes, in a dry run too, and its URL, credentials removed, is in the banner). The branch and the tag go in one atomic push (`git push --atomic`: a tag never goes without its branch); when that push fails, the local release commit and tag are rolled back, so a release that did not go out leaves neither behind, and a commit that fails leaves the index as it was. The new tests found that the commit step never worked with the default message: `system2()` does not quote its arguments, so `git commit -m Automated release for X` took the words for pathspecs; every git argument is quoted now. The tests run the script and the function in scratch repositories with local bare remotes; the script is build-ignored, so they skip under `R CMD check` and run from the source tree.
* **Tests for the gaps that mutation testing found (CPC-05).** A reviewer changed the publish code in 22 places, one at a time, and ran the tests: four changes went unnoticed. They were re-created on the current code (all 22, and the hook's rule about the live tip of the protected branch) and run against the tests. Two behaviours had no test of their own, and have one now: a publish branch or a remote whose name starts with `-` (git would read it as an option) is refused before anything changes (the check was only exercised through the private branch, and removing it at either call site passed), and a private branch that now points at history no recorded tip reaches is still protected by its live tip (a hook that ignored the live tips passed). The other mutants, among them a standalone push with a plain `--force` or without the lease, a hook that refuses everything when a protected branch is missing, and clean commits that are not recorded, are killed by the tests of the earlier stages.
* **`release.yaml` pushes only when asked.** Its input `commit_and_push` (commit, tag, and push `master` and the tag with no review step) defaults to off. The workflow itself cannot be tested without CI; the edit is the default and the description of that one input.
* **`REVIEW.md` and `CONTRIBUTING.md` say what is pushed without a pull request, and how to publish.** The statements that a workflow never pushes to a branch and that nothing is pushed straight to `master` were false for `release.yaml`, `pkgdown.yaml` (the `gh-pages` deployment), `deploy_release.R` and the publish path; they are listed now, with what branch protection has to cover (tags included). New sections give the tag schemes (development builds of the private repository against `v<MAJOR.MINOR.PATCH>` releases of the public one) and the runbook "Publishing to the public repository": a throwaway clone with `public` as its only remote, a dry run and `git diff --stat`, `guard_public_remote()`, the publish, how to verify the guard with `git push --dry-run <empty bare repository> <private branch>`, snapshot releases, the protection of the public `master`, what `(stale info)` means, what the tool cannot stop, and that outside pull requests are never merged on the public repository.

## Feature: Synthetic Investigator Name Pool Expansion & Full Name Generation

* **Expanded Census surname pool:** Packaged `inst/extdata/last_names.csv` containing the top 250 US Census surnames, augmented with existing diverse investigator surnames (total pool: 252 surnames). `generate_pi_names(format = "synthetic")` in both R (`R/pi_anonymizer.R`, `scripts/pi_anonymizer.R`) and Python (`scripts/pi_anonymizer.py`) now samples from this expanded pool, dramatically reducing birthday-bound pseudonym collision rates for investigator cohorts of $k = 20\text{--}50$ PIs.
* **Synthetic full name generation (`format = "full_name"`):** Added a curated pool of 256 diverse top first names (from SSA baby names). Setting `format = "full_name"` (or alias `format = "full"`) generates realistic synthetic full names (`"Sarah Jenkins"`, `"Marcus Chen"`) by sampling unique pairs without replacement across a combinatorial space of over 64,500 distinct full names.
* **Exclusions and alias support:** `generate_pi_names()` accepts `exclude` across all implementations to ensure real investigator names/surnames are never generated, and accepts `format = "surname"` as an alias for `"synthetic"`.

## Hardening: Standalone Pseudonymization Scripts (`scripts/pi_anonymizer.py`, `scripts/pi_anonymizer.R`)

Audit packet A4 findings for standalone identifier pseudonymization tools:

* **Unmapped lookup handling (A4-03):** In `scripts/pi_anonymizer.py`, unmapped lookups with `auto_assign=False` support `on_missing='raise'` (raises `KeyError`) and `on_missing='none'` (returns `None`), preventing accidental exposure or silent passthrough of unmasked real names.
* **Corrupt mapping validation (A4-04):** In `scripts/pi_anonymizer.py`, malformed JSON files raise an informative `ValueError` rather than silently replacing corrupt state with empty mappings.
* **Strict jsonlite requirement (A4-05):** In `scripts/pi_anonymizer.R`, removed regex-based fallback serialization, strictly requiring `jsonlite` for reading and writing mapping files.
* **Advisory locking and atomic writes (A4-13):** In `scripts/pi_anonymizer.py`, added advisory directory locking with stale lock recovery, cross-platform process liveness checking (Windows `OpenProcess` / POSIX `os.kill`), and atomic JSON writes with Windows file-system contention retry.
* **Collision guards and token bounds (A4-14):** In `scripts/pi_anonymizer.py`, enforced `n_chars >= 6` (unless `allow_collisions=True`) and added iterative collision resolution against existing mappings capped at 100 attempts.
* **Vectorized processing and NA handling (A4-15):** In `scripts/pi_anonymizer.R`, `anonymize_pi()` supports vector inputs, treats empty strings `""` and whitespace-only strings as `NA_character_`, and auto-creates non-repository parent directories.
* **Independent RNG instances and seed preservation (A4-16):** In `scripts/pi_anonymizer.py`, `generate_pi_names()` utilizes isolated `random.Random(seed)` instances to avoid mutating global Python RNG state. In `scripts/pi_anonymizer.R`, `generate_pi_names()` preserves local `.Random.seed` and supports deterministic seeded token generation.

## Security & Hardening: Pseudonym Generation and Atomic File Locking (`R/pi_anonymizer.R`, `R/atomic_io.R`)

Audit packet A4 and A5 findings in identifier pseudonymization, secrets containment, and atomic concurrency:

* **Deterministic surname RNG pinning (A4-01):** `generate_last_names()` and `anonymize_pi(method = "hmac_surname")` now pin `.rng_kind = "Mersenne-Twister"`, `.rng_normal_kind = "Inversion"`, and `.rng_sample_kind = "Rejection"` in `withr::with_seed` (and when falling back), guaranteeing identical surnames regardless of user or worker `RNGkind()` or R version.
* **Pseudonym collision detection and token length enforcement (A4-02):** `generate_pseudonym_token()` now enforces `n_chars >= 6` (stopping on shorter lengths unless `allow_collisions = TRUE`), computes birthday bound collision probability warnings when > 1%, and validates uniqueness among distinct input names within a call, failing with an informative collision count unless `allow_collisions = TRUE`. Surnames in `method = "hmac_surname"` enforce the same collision checks.
* **Empty string and whitespace handling (A4-11):** `anonymize_pi()` and `generate_pseudonym_token()` treat empty strings `""` and whitespace-only strings as `NA_character_`, consistent across all methods.
* **Encoding & normalization (A4-09):** Names and keys are normalized to UTF-8 via `enc2utf8()` before hashing. `anonymize_pi()` adds an opt-in `normalize = TRUE` parameter for case-folding and trimming prior to hashing.
* **Bounded token generation (A4-10):** Capped collision resolution loops to 100 attempts in `anonymize_pi(method = "token")`, preventing infinite loops and hanging file locks when token entropy is exhausted.
* **Strict jsonlite requirement (A4-12):** Removed fragile regex-based JSON fallbacks; `method = "token"` strictly requires `jsonlite` for atomic serialization.
* **Weak key warnings (A4-19):** `resolve_hmac_key()` emits a clear warning when `nchar(trimws(key)) < 16` unless `allow_default_key = TRUE`.
* **Lock owner liveness and atomic cleanup (A5-16):** `with_file_lock()` records lock owner PID, verifies process liveness via non-destructive priority querying (`tools::psnice()`) before declaring stale locks, and cleans up expired locks via atomic directory renaming to eliminate race conditions between concurrent waiters.
* **Secrets directory validation and repository containment (A5-21, A5-24):** `validate_secrets_dir()` requires trailing slashes during repo root prefix comparisons (preventing false containment positives on sibling directories), verifies the target is not an existing file, and verifies POSIX file modes. `validate_executable()` rejects directories.

## Bug Fix: SAS `%cbe_counting_process` Counted Decimal Observation Times at the Period

The same bug class as `%cbe_brier_score` below; found by a reviewer running the macro in SAS 9.4.

* **`obs_times = 0.5 1 1.5` was counted as five times** (`inst/sas/cbe_counting_process.sas`): `%let n_obs = %sysfunc(countw(&obs_times))` used SAS's default delimiters, and "." is one of them. The DATA step then declared `pp[5] P1-P5` (P4 and P5 do not exist in the input) and `tt[5] _temporary_ (0.5 1 1.5)`, with `WARNING: Partial value initialization of the array tt.` (exit status 1) as the only sign. The two uninitialised elements are missing values, which compare lower than any time, so every subject who outlived the last observation time got a final row with a missing `Covariate` (the reviewer's ID 1: `T1=2.0 T2=2.0 Covariate=. Status=1`, where the same data with all times doubled and `obs_times = 1 2 3` gives `Covariate=7`). Only the count was wrong: `array tt[&n_obs] _temporary_ (&obs_times)` uses the list as typed. The `countw` call now passes a blank as the only delimiter, and the `OBS_TIMES=` line in the macro header says decimals are fine.
* **Whole-number times behave as before**: on the 45-animal tumor data with the Example 85.7 times (`27 34 ... 71`) the old and the fixed macro return the same 118 rows (compared in SAS 9.4 TS1M8, 2026-09-29). `inst/sas/example_85_7.sas` has its own DATA step and never calls the macro, so its committed log and listing and `tests/testthat/reference/sas_phreg_tumor_counting_process.csv` are unchanged (the Tier 2 test for it still passes).
* **A comma-separated list is no longer split**: it is only possible when quoted (`obs_times = %str(1,2,3)`). The old macro split it at the commas, counted three times and ran; the fixed macro counts one token and stops with `ERROR: Too many variables defined for the dimension(s) specified for the array qq.` (exit status 2, `WORK.LONG` incomplete). The list has to be blank-separated, as the header always said.
* **New opt-in Tier 2 test** (`TEMPLECBE_RUN_SAS_TESTS=true`, `test-sas_parity_live.R`): it writes two small drivers to a temporary folder (6 subjects with `obs_times = 0.5 1 1.5`, and the same data with every time doubled and `obs_times = 1 2 3`; doubling is exact in binary), runs `%cbe_counting_process` on both and compares `T1`, `T2`, `Status` and `Covariate` with a table worked out by hand from the DATA step, and with each other, and checks that neither log has a `WARNING`. On the old macro it fails (the final `Covariate` of the two subjects who outlive the last time is missing instead of 7 and 4, and the log has the array warning); on the fixed macro it passes. The helpers `write_counting_driver()` and `counting_process_rows()` are in `helper-sas-reference.R`.

## Bug Fix: SAS `%cbe_brier_score` Split Decimal Evaluation Times at the Period

Found by a reviewer running the macro in SAS 9.4 with follow-up in years.

* **`eval_times = 0.5 1.5 2.5 3` was read as seven times** (`inst/sas/cbe_brier_score.sas`): the loop over the evaluation times used `%sysfunc(countw(&eval_times))` and `%scan(&eval_times, &i)` with SAS's default delimiters, and "." is one of them, so 0.5 became 0 and 5. There was no `ERROR` and no new `WARNING`: `out_brier` came back with the rows of the pieces that happened to match a `PROC PHREG BASELINE ... TIMELIST=` time (in the reported case only the t = 3 row) and the IBS was built from those rows (0.00000 for one row). The `TIMELIST=` option itself was never affected, only the loop. Both calls now pass a blank as the only delimiter. Whole-number times (100 200 300 400 500) tokenise as before: the two benchmark programs print the same numbers, and their committed logs differ from the old ones only in the line numbers (the macro gained three lines), the run time and the timings; the macro's text is not echoed in them. `EVAL_TIMES=` in the macro header now says decimals are fine. The list has to be blank-separated, as the header always said: a comma-separated list (only possible when quoted, `eval_times=%str(100,200,300)`) was split at the commas before and now fails with SAS errors.
* **New opt-in Tier 2 test** (`TEMPLECBE_RUN_SAS_TESTS=true`, `test-sas_parity_live.R`): it writes a small driver to a temporary folder (`survival::lung` follow-up divided by 365.25, `eval_times = 0.5 1 1.5 2`), runs `%cbe_brier_score` in SAS and compares the `out_brier` rows and the IBS with R (`add_graf_weights()`, `yardstick::brier_survival()` and `brier_survival_integrated()`). On the old macro it fails (rows 0.5 and 1.5 missing, one row repeated, IBS 0.08276 against R's 0.14017); on the fixed macro SAS agrees with R to 3.5e-6 at most over the four Brier scores and the IBS (full-precision values from the run of 2026-09-29; the largest gap is at 0.5 years, SAS 0.190072627 against R 0.190076092, because SAS stops PHREG at GCONV=1E-8), and the listing has 5 decimals, so the test uses `atol = 1e-5`. The test helpers `sas_atol`, `sas_rtol`, `lung_full_data()` and `lung_model()` moved from `test-sas_parity_reference.R` to `helper-sas-reference.R`, and `run_sas_file()` (any path) now backs `run_bundled_sas()`.
## Repository: Files Removed From the Tree for the Public Release

* **The rendered example `inst/templates/t_test_example.pdf` and the three manual PDFs under `pkgdown/assets/manual/` are no longer tracked.** They were generated from analyses that carried study-specific names, so they cannot be part of a public tree. `create_report()` never read the example PDF (it copies the `.qmd`, `.Rmd`, `.bib` and `.tex` templates only), and the manuals are regenerated with `build_manual_versioned()`. **The pkgdown navigation no longer has a "Manual" entry until a clean manual is published again.**
* The working folders `tasks/`, `completed_tasks/` and `figure/` (planning notes, catalog and code drafts, and five rendered plots; none of them is part of the package and `.Rbuildignore` already left them out) are removed.
* No R code, function, argument or export changed.
## Fix: Data Manifest Confinement and Integrity Verification (`data_manifest.R`)

* **Environment/closure identity tolerance (A4-08):** In `validate_data_manifest()`, byte equality (`md5_match = TRUE`) now guarantees `valid = TRUE` and `same_content = TRUE`. Deserialized comparison via `identical()` no longer produces false mismatch errors on closures/environments (e.g. model fits, plots).
* **Path traversal rejection and overwrite protection (A4-20):** `copy_data_manifest()` and `validate_data_manifest()` now enforce that `file` and `source` are relative paths confined to `dir` and `project_root` respectively, explicitly rejecting `..` traversal and absolute paths. `copy_data_manifest()` defaults `overwrite = FALSE` to prevent unintended file overwrites.
* **Excel UTF-8 BOM:** `read_data_manifest()` strips UTF-8 BOM prefixes from column names.
* **Empty and malformed manifests:** `validate_data_manifest()` and `stop_if_invalid_manifest()` safely handle 0-row manifests and assert required column structure.

## Fix: Data I/O Call Auditing and Path Containment (`scan_data_io.R`)

* **Accurate tokenization of relative and bare paths (A4-06):** `scan_data_io()` now extracts file references directly from string literals within matched function calls and cleanly resolves relative paths against `project_root` without swallowing surrounding code text.
* **Type safety on empty writes or reads (A4-07):** Ensured `output_file` and `input_file` columns maintain character type when scripts contain no write calls or no read calls, preventing `dplyr::case_when` type errors.
* **Extension validation and heuristic safeguard (A4-21):** Validates `ext` (stripping leading dots and rejecting special/alternation characters). The key-based fallback heuristic now requires a minimum key length of 3 and whole-word matching, preventing false matches on arbitrary substrings (e.g. `paths$a` matching `banana_report.xlsx`).
* **External directory containment (A4-22):** Confines `missing_write_dirs` to folders inside `project_root`, skips library directories (`renv`, `node_modules`, `.git`), and caps listing depth, preventing unbounded scans of external directories or root drives.

## Fix: Review Findings in PDF Conversion, Report Packaging, and the Clean-up Helpers

Found by the audit of the side-effect and cross-platform code (A5-06 to A5-23, the findings outside the `clean_publish()` tooling). Each fix has a regression test that fails on the code before it; the tests need no converter, Word, LibreOffice, Python or SAS (they use stand-in executables and mocks), and run unchanged with every converter hidden. The items marked **Behaviour change** change what a call that used to run returns, warns, or stops on.

* **A5-06 (P1) Behaviour change: a DOCX that was already at `dest` is never mistaken for a conversion.** The LibreOffice backend ignored the exit status and returned `file.exists(dest)`, and pdf2docx wrote straight to `dest`, so a stale DOCX from an earlier run, or a half-written one from a crashed pdf2docx, came back `converted = TRUE` and `zip_reports()` shipped it. Each backend (python, LibreOffice and Word COM) now writes into a fresh file next to `dest` (LibreOffice into a fresh private folder, because it picks its own file name) and the file is moved over `dest` only when the conversion succeeded and left a non-empty file. pdf2docx and Word COM must exit with status 0 (a non-zero exit after a partial write is the case this fixes); LibreOffice's exit status is still not looked at, as before, because a first start with a fresh profile may exit non-zero after a good conversion and the private, empty output folder already guarantees that whatever it left there was written by this run. A time-out is a failure for every backend. A failed run leaves `dest` exactly as it was (an old file is not deleted, a partial one is discarded). Calls that used to report `converted = TRUE` for such a file now return `FALSE` with the usual warnings.
* **A5-07 (P1) Behaviour change: `zip_reports()` no longer ships a DOCX that is older than its PDF.** The documentation said stale DOCX files are skipped when `docx_from_pdf` is `NULL`, but the file was copied and linked, and a `docx_from_pdf` that failed (`convert_pdf_to_docx()` returns `FALSE`, the old file is still there) was ignored. A DOCX now counts only when it is at least as new as its PDF. Without `docx_from_pdf` a stale one is left out with a warning; with one, its return value (`FALSE` is a failure; any other value, including `NULL`, is not) and the file's age are checked afterwards, and a failure warns and ships no DOCX. In the index, the DOCX cell of a report whose PDF exists but that has no DOCX to ship (missing, stale or not converted) reads `"not converted"` instead of being blank. A missing DOCX without `docx_from_pdf` is still skipped without a warning, as documented.
* **A5-08 (P1) Behaviour change: `zip_reports()` keeps data files that share a file name and reports copies that fail.** `data_deliverables = c("a/results.csv", "b/results.csv")` shipped only `a`'s table, with no word. The later file now keeps a numbered name (`results_2.csv`) and a warning says so, as `package_deliverables()` already did. Every copy is checked: one that fails (a file another program holds open, a folder where a file was expected) warns, and for a report file it is left out of the index instead of being linked.
* **A5-09 (P1) Behaviour change: `zip_render()` reads a relative `build_dir` and `copy_back_dir` against the caller's folder.** The function moves its working directory to the build folder before it uses them, so `zip_render("report.qmd", copy_back_dir = "deliverables")` wrote the zip into the temporary build folder while printing `Created zip: deliverables/report.zip`, and a relative `build_dir` stopped at the zip step ("Cannot open zip file"). Both are now resolved before the working directory changes, and they need not exist yet. The `zip` in the returned list is therefore an absolute path.
* **A5-10 (P1) Behaviour change: `zip_render()` finds the rendered outputs by name.** The files were found with a regular expression built from the file stem, so `Results (final).qmd` was rendered but its outputs were left out of the zip with no word, `a[1.qmd` made `list.files()` fail after the render had finished, and `+` and `.` in a stem were regex syntax too. The files in the build folder are now compared with `<stem>.<extension>` (the case is still ignored), and a render that leaves none of them there warns that the zip holds the sources only (an `output-dir` in `_quarto.yml` is the usual cause).
* **A5-17 (P2) Behaviour change: `keep_only()` stops when a kept name is not there.** `keep_only("reslt")` in a script whose object is `result` listed both objects and removed them, with no word that `reslt` does not exist (scripts are not asked to confirm). It now stops before it removes anything when `vector` is not a character vector of names (an unquoted `c(a, b)` evaluates `a` and `b`) or when a name is not an object of the calling environment itself. Called inside a function it still works on that function's own environment, so the function's arguments are removed too unless they are named; that is documented.
* **A5-18 (P2) Behaviour change: the source PDF cannot be overwritten under another spelling.** `pdf_to_rtf()` compared `pdf` and `rtf` as strings, so `pdf_to_rtf("ex.pdf", "./ex.pdf")` (or another case on Windows) replaced the PDF with RTF text, and `convert_pdfs_to_docx()` only compared `src` with `dest`, so a `temp_dest` equal to `src` replaced the PDF with DOCX bytes. Both now compare canonical paths (new internal `same_file_path()`), `pdf_to_rtf()` refuses an `rtf` path that ends in `.pdf`, and `convert_pdfs_to_docx()` also checks `temp_dest` against `src` and `dest` (an error in both modes). A `dest` or `temp_dest` that is another `.pdf` file is a bad row (an error with `strict = TRUE`, a warning and a skip otherwise). A `temp_dest` copy that fails now warns.
* **A5-19 (P2) Behaviour change: `timeout` limits every converter, and a timeout is reported as one.** Only the Word COM backend had a time limit, so a pdf2docx that loops or a headless LibreOffice that waits on a dialog blocked the whole batch (a fake LibreOffice that takes 6 s ran 6.5 s with `timeout = 1`). `convert_pdfs_to_docx(timeout = )` now applies to the python and LibreOffice backends too (the default is still 600 s, so a conversion that takes longer than that now fails), a file that times out gets its own warning (`<tool> conversion of <file> timed out after <n>s.`) and comes back `converted = FALSE`, and the interpreter probes of `find_python()` give up after 60 s instead of waiting for ever. Stopping a converter stops the process that was started; a process that one started in turn (`soffice.bin`, the Word instance behind Word COM) may keep running. **New argument `run_sas_script(timeout = 0)`** (seconds; `0`, the default, is no limit, so nothing changes unless it is set): a run that is stopped is an error that names the log.
* **A5-20 (P2) Behaviour change: the Word COM launcher quotes its paths and no longer ends every Word.** The PowerShell command put the Rscript and script paths into `-ArgumentList` unquoted, so a space in `tempdir()` made Rscript receive half the path (nothing ran, status 0, the file was reported failed) and an apostrophe (a user name such as O'Brien) was a PowerShell parse error. After a timeout it also ran `Stop-Process -Name WINWORD -Force`, which ends every Word the user has open, unsaved documents included. The command is now built by `.word_com_command()` with every path quoted, and the converter's exit status is passed on. After a timeout only the process that was started is stopped, by process id, and the warning says that a Word instance it started in the background may have to be closed by hand. This was tested with the real launcher (PowerShell and Rscript, folder names with a space, an apostrophe, `&`, `$`, a backtick, `%` and a typographic apostrophe) but not with Word itself.
* **A5-22 (P3) Behaviour change: stage names stay inside the staging folder, and staging is always cleaned up.** A `stage` was used as a path segment as it came, so `stage = "../../../x"` wrote outside the build folder while the zip lacked the file and the index linked it. `zip_reports()` (`stage`) and `package_deliverables()` (the stage of each `FileOutputs`) now clean it (new internal `safe_stage_dir()`): path separators and the characters Windows forbids in a file name become `-`, leading and trailing dots, dashes and spaces are dropped, and ordinary names such as `"01 Results"` are kept as they are. `zip_reports()` no longer leaves a copy of every report in `tempdir()` until the session ends, and `package_deliverables()` removes its staging folder when `zip::zip()` fails. `package_deliverables()` checks every copy, so "Successfully packaged N files" counts only files that were copied (a copy that fails warns). A relative `zip_reports(output_dir = )` is resolved through the folder that exists, as `normalizePath()` leaves a not-yet-existing path as written on Linux and macOS.
* **A5-23 (P3) Behaviour change: `delete_nul_files()` no longer builds a shell command, and stays inside the folder it was given.** The `del` command it handed to `cmd.exe` had `%VAR%` sequences in a folder name expanded (for `x%TEMP%y` the delete failed and the file stayed), and `list.files(recursive = TRUE)` follows junctions, so a link inside the folder to another folder got that folder's `nul` files deleted. The files are now deleted from R, with `file.remove()` on their device-namespace path; a folder reached through a link (a junction or symbolic link) is skipped with a message; a relative `path` works; and an error names any file that could not be deleted. `nul_delete_commands()` is gone, and with `.verify_command = TRUE` the function returns the device paths it would delete instead of the `del` commands.
## Fix: Release Tooling, Docker Gating, and Deployment Script Hardening (task_da864095)

* **`deploy_release.R` defensive checks and hardening (A5-05, A5-11, A5-12, A5-25):**
  - **Refuses dirty working tree and off-branch execution (A5-05):** `deploy_release()` now verifies the working tree is clean (`git status --porcelain`) and that the current branch matches `expected_branch` (default `"master"`) before making modifications.
  - **Targeted artifact staging (A5-05):** Replaced indiscriminate `git add .` with staging only modified release files (`DESCRIPTION`, `NEWS.md`, `README.md`, `README.qmd`, `man/`, `NAMESPACE`).
  - **Discrete argument vectors and exit status verification (A5-05, A5-11):** All git invocations now execute via `system2()` with discrete arguments to prevent shell expansion vulnerabilities. Checked exit statuses on branch push and tag push, stopping on error.
  - **Strict version validation (A5-11):** Throws an error (rather than a warning) if `version_tag` is not a valid numeric R package version.
  - **Git identity check (A5-12):** Verifies that `user.name` and `user.email` are configured in git rather than writing hardcoded personal identities to local git configuration.
  - **Rollback guard (A5-12):** Added an `on.exit()` handler to restore the original `DESCRIPTION` file if any step fails before a successful commit.
  - **Windows line ending protection and aligned version format (A5-25):** `DESCRIPTION` is written via a binary connection (`"wb"`) with LF line endings. Updated default datetime stamp format to `%Y.%m.%d.%H%M` (omitting dot between hour and minute) to align with package version formatting standards.
* **Release pipeline workflow (`release.yaml`):** Pushes releases to `master` instead of `main`, stages targeted release files, sanitizes input interpolation via environment variables, and adopts `%Y.%m.%d.%H%M` timestamps.
* **Docker publishing workflow (`docker-publish.yml`):** Replaced push triggers with a `workflow_run` trigger dependent on successful completion of `R-CMD-check` on `master`, ensuring Docker images are only published from verified green commits.
* **Workflow branch references:** Removed references to deprecated `main` branch across `release.yaml`, `docker-publish.yml`, `R-CMD-check.yaml`, `pkgdown.yaml`, and `test-coverage.yaml`.

## Fix: Excel Header Backward Compatibility (`read_excel_multiple_headers()`)

* **Behaviour change: `fill_merged` defaults to `FALSE` (task_620d0ac9).** In `read_excel_multiple_headers()`, hierarchical forward-filling across upper-tier merged header cells (`fill_merged = TRUE`) is now opt-in, restoring the previous default behavior where empty upper-tier cells were not forward-filled across columns. Callers desiring automatic forward-filling across merged header categories must pass `fill_merged = TRUE`.
* **Argument validation:** `read_excel_multiple_headers()` defensively asserts that `fill_merged` is a single non-missing logical value (`TRUE` or `FALSE`).

## Fix: Reviewer Privacy and Hardening in the Word-Review Extractor (`cbe_docx_review_extract()`)

Found by the audit of `R/cbe_review_extract.R` (A6-02, A6-12, A6-13, A6-14, A6-16, A6-23, A6-24); each fix has a regression test in `tests/testthat/test-cbe_review_extract_privacy.R`. The items marked **Behaviour change** change what a call that used to run now returns, warns, or stops on.

* **Behaviour change: reviewer ids are validated (A6-16).** `review_config()` and `cbe_docx_review_extract()` stop, before anything is read or written, on an id that is empty, longer than 20 characters, uses anything but letters, digits and underscores, ends in `_comment`, is the name of a built-in column (`file`, `author`, `resolved`, ...), repeats (ignoring case) or is both a fork and a sign-off reviewer. Ids used to go unchecked into a regular expression (`"c++"` broke every file-name stem and `"j.doe"` also matched `jXdoe`), into fork file names (an id with `/` or `..`), and into sheet names (more than 20 characters stopped `documents_sheet_mode = "per_reviewer"` half way through), and could duplicate a built-in column. Ids are now also escaped wherever they are part of a pattern.
* **Behaviour change: a fork workbook holds nothing of the other reviewers (A6-02).** The "Documents" sheet of every fork carried every other fork reviewer's id and per-document status and the sign-off reviewers' status and free-text note (a lead's private comment reached each reviewer), contradicting `?review_config` ("never exposed in a fork reviewer's personal workbook"); with `documents_sheet_mode = "per_reviewer"` the fork also held a "Documents_<id>" sheet for every reviewer. In either mode a fork's Documents sheet is now the file, the stage, the counts and that reviewer's own rolled-up column, and its `resolved` column, like the fork's Comments sheet, reflects only that reviewer's own sign-offs. The master workbook is unchanged.
* **Behaviour change: forks and the CSV exports leave out the sign-off reviewers' columns and the Word `author` (A6-24).** The help said sign-off reviewers are tracked only on the Documents sheet, yet their columns were also in `comments.csv`, and the hidden `author` column (the name of every Word author) stayed in every fork and in all three CSVs. A fork now has no sign-off columns and no `author` on any sheet (Comments, SuggestedChanges, TrackedChanges). The CSVs keep the fork reviewers' columns but drop the sign-off reviewers' columns and `author`; the new `review_config(csv_reviewer_columns = )` chooses `"fork"` (the default), `"none"` (no reviewer column at all) or `"all"` (every column, as before). The master workbook keeps every column, so nothing typed there is lost, and `?review_config` says what is where.
* **Behaviour change: CSV cells that Excel would run as a formula get a leading `'` (A6-12).** `comments.csv`, `tracked_changes.csv` and `suggested_changes.csv` wrote reviewer-controlled text unchanged, so a comment, a tracked insertion, selected text or a reviewer's note that starts with `=`, `+`, `-` or `@` (after any leading blanks), or with a tab or carriage return, ran as a formula when the CSV was opened in Excel (hyperlinks, functions that send data out). Every such cell in every character column is now prefixed with a single quote, so code that reads these CSVs back sees the quote. The xlsx workbooks are unchanged: they store text as strings, never as formulas.
* **Stored error text no longer carries local paths or the account name (A6-13).** The message `unzip()` or `xml2` raised for a document it could not read was kept as it was in `$errors` and on the Errors sheet of the master and of every fork, and it quotes absolute paths (for a zip with a drive-letter entry name, `C:/Users/<account>/AppData/Local/Temp/Rtmp.../docx_.../...`). The temporary folder is now stored as `<tempdir>`, the input folder as `<input_dir>` and the home folder (`path.expand("~")`, `HOME`, `USERPROFILE`) as `<home>`, in either slash style. With `verbose = TRUE` the console still shows the full message. In the same spirit, the workbooks no longer record the operating-system account name as their author and last editor (`docProps/core.xml` now says `TempleCBE`; openxlsx used `USERNAME` or `USER`), which put it in every fork.
* **The docXwalk sheet shows where a document really is, and never an absolute path (A6-14).** The `file` column was always built as `<working directory>/<one fixed folder>/<name>`, so documents read from any other folder were reported in a folder they were not in, and `format_repo_path()` fell back to the full absolute path (with the user profile) for anything outside the project root, in the master and in every fork. The path is now built from the folder the document was read from: relative to the project root (as `<root folder>\<relative path>`) when it is inside it, and just the file name otherwise or for a prior-round document that is no longer in the folder. "Inside the root" is now decided on whole path components (`proj_old` is not inside `proj`, and used to produce a path cut in the middle of a folder name). A fork's docXwalk sheet holds file names only, in all four columns.
* **Behaviour change: the package no longer ships a pipeline catalog, stem aliases or folder names, so stage matching needs configuration (A6-23).** The extractor carried the stage catalog (38 stages), four typo aliases and the folder layout of one specific analysis. That revealed the structure and domain of an unpublished study, and gave other projects irrelevant stages and fuzzy matches (60 percent similarity) of their own files to them. All of it is now configuration in the style of the reviewer settings, empty by default: `review_config(pipeline_catalog = , stem_aliases = , analysis_dir = , input_dirs = )` (each can be cleared again). With the defaults every document is labelled `Unknown / Extra` and ordered by file name, there are no fuzzy stage matches, the manifest (`reports_to_render.xlsx`) and the `.qmd` sources shown on the docXwalk sheet are only looked for in `analysis_dir`, and the default input folder is the working directory unless `input_dirs` names others. A project that relied on the built-in stages sets them once in its own setup code, outside the public repository; `?review_config` describes the format.

## Fix: Merge and Identity Correctness in `cbe_docx_review_extract()`

Found by the audit of the Word-review extractor (A6-01, A6-03, A6-04, A6-05, A6-06, A6-08, A6-10, A6-20, A6-25); every finding was reproduced with synthetic documents, and the tests are in `tests/testthat/test-cbe_review_merge.R`. Together, A6-03 to A6-06 and A6-08 are what stopped a second review round from running. The items marked **Behaviour change** change what a call that used to run returns, warns or stops on.

* **Behaviour change (A6-01, P0): tracked changes are numbered by their own paragraph.** Paragraphs were identified with `format(p)`, which is the constant `"<p>"` for every `xml2` node, so every row of `TrackedChanges` (and `tracked_changes.csv`) got `paragraph_number` 1 and the text of paragraph 1 as its context, and the rows were sorted on that constant. They now carry the number the comments and the suggested changes already used (the position in the document's list of paragraphs) and the text of the paragraph that holds them; a change inside a text box belongs to the box's own paragraph. Rows already in a tracker are replaced on the next run for every document that is scanned.
* **A6-03 (P0): a second round no longer stops with a `vctrs` type error.** Rows read back from the tracker are text and freshly extracted rows are integer or logical, and `merge_comments()`, `merge_revisions()` and `merge_redlines()` bound them together, so a revised document with one new comment next to tracked ones, a signed-off comment deleted from the document, or a tracked document that left the folder stopped the run after the backup and before anything was written. Every row is coerced to the schema a first run produces (integer `paragraph_number`, `end_paragraph_number` and `duplicate_count`; logical `is_reply` and `is_toc_or_lof`; text elsewhere) before the rows are combined.
* **Behaviour change (A6-04, P1): a comment is identified by what it says, not by its Word comment id.** Word renumbers comment ids when it saves, and the merge matched on `(file, comment_id)` first, so after the author reordered paragraphs a reviewer's sign-off and note landed on another comment and that comment's text was overwritten, silently. A comment now continues an existing row of the same file only when its text is the same, compared with all whitespace removed; among such rows it takes the one with the same author and date, then the same author (an author missing on one side agrees), preferring the same id and then the nearest paragraph, and every existing row is used at most once. A comment whose id matches but whose text differs is a **new** comment, and the old row is kept as `Prior Round / Not in docx` when it holds a sign-off or a note (a row with neither is dropped, as for any comment deleted from the document). So a comment edited in Word between rounds starts without its sign-offs; they stay on the prior-round row. Because ids can now repeat (a prior-round row and an active row), the fork overlay pairs rows by file, comment id and text. The rule is on the help page of `cbe_docx_review_extract()`.
* **Behaviour change (A6-05, P1): a fork that left a cell alone no longer overwrites the master.** The overlay copied the fork's sign-off and note cells into the master even when blank, so a sign-off typed into a reviewer's columns of `review_tracker.xlsx` (by a coordinator, say) vanished on the next run, for Comments and SuggestedChanges alike. Each fork workbook now carries a hidden sheet, `ForkBaseline`, with that reviewer's sign-off and note for every row as the fork was written (nothing of any other reviewer). A fork cell is applied only when it differs from its baseline, cell by cell, so a fork edit and a master edit to different cells of one row are both kept, and a reviewer who empties a cell empties it in the master. If both were changed to different values, the fork's value is kept and one warning lists the cells (the master as it was is in `backups/`). A fork written by an earlier version has no baseline: a blank fork cell never replaces a master value there, and a different non-blank one does, with the same warning.
* **Behaviour change (A6-06, P1): document sign-offs on the `Documents_<id>` sheets survive.** With `documents_sheet_mode = "per_reviewer"` a sign-off reviewer's status and comment live on `Documents_<id>`, but only the `Documents` sheet was read back, so the next run wrote those sheets blank. Every `Documents` and `Documents_<id>` sheet is now read and merged by file, which also carries sign-offs across a change of `documents_sheet_mode` in either direction.
* **Behaviour change (A6-08, P1): a document that fails to extract no longer loses its rows.** Documents whose extraction failed (a truncated `word/document.xml`, say) counted as scanned, so their comments, tracked changes and suggested changes were deleted from the tracker and the Documents sheet listed them with 0 comments. Only documents that extracted count as scanned now: what the tracker holds for the others is kept (as `Prior Round / Not in docx`; the status text does not say the document was unreadable, `$errors` does), and they are not listed as processed.
* **Behaviour change (A6-10, P2): files that are not readable Word documents are reported.** Empty, non-zip and corrupt files were skipped with a message only when `verbose = TRUE` and were in `$errors` only if every file was invalid, and a renamed workbook (a zip with `[Content_Types].xml` only) counted as a document with no comments. A readable document must now contain `word/document.xml`; each unreadable file, and each file that fails to extract, is in `$errors` and on the Errors sheet with a reason that names no path, and one warning gives the count and the first names. Word's `~$` owner-lock files are skipped quietly. The all-invalid warning now starts "No valid DOCX files found to process:" and gives the count.
* **Behaviour change (A6-20, P2): paragraphs, tabs and line breaks no longer fuse words.** A two-paragraph comment `First para.` / `Second para.` was stored as `First para.Second para.`, a selection across paragraphs as `P4 startP5 end`, and `Hello<tab>World` as `HelloWorld`. Paragraphs of a comment or selection are now joined with a space; a tab, positional tab, line or page break and carriage return in a run give a space (a tab *stop* in the paragraph properties does not); one helper gathers the text of comments, selections, contexts, tracked changes and redlines. A non-breaking hyphen (`w:noBreakHyphen`) gives `-`, not a space, since it is a hyphen. Trackers written before this hold the fused text, so rows are matched on the text with whitespace removed (comments, as in A6-04, and suggested changes) and a matched row takes the new text; no sign-off is lost on the first run after the upgrade.
* **Behaviour change (A6-25, P3): backups, ordering and `review_config()`.** Backups carry the time to the millisecond (and a counter if the name is taken) and never overwrite an existing backup; before, two runs within a second shared a name and the state before the first run was lost. The master workbook is backed up just before it is written, not before the documents are read. A run that finds no readable document creates no output folder and makes no backup (with no arguments it used to create `review_extract/` in the working directory). Rows, files and the lists on the Documents, docXwalk and CommentSummary sheets are ordered in byte order (`method = "radix"`), the same on every machine. **New argument `review_config(reset = TRUE)`** returns every setting to its default (the generic placeholder reviewers, no sign-off reviewers, `"columns"`, and with the privacy fixes' settings: the CSV reviewer columns, the pipeline catalog, stem aliases, analysis folder and input folders) before applying any other argument of the same call; the arguments are validated first, so a rejected call changes nothing.

## Fix: Extraction Fidelity and Speed of `cbe_docx_review_extract()`

A review of how the Word-review extractor reads `.docx` files and writes its workbooks (audit A6-07, 09, 11, 15, 17, 18, 19, 21, 22) found the following; each fix has a regression test in `tests/testthat/test-cbe_review_extract_fidelity.R`. The fixtures follow the OOXML layout and were built by hand: none was written by Word, so they show that the extractor reads that layout, not that Word writes it. The items marked **Behaviour change** change what a call that used to run returns, warns, or stops on.

* **Behaviour change: reply threads and resolved comments are read from `word/commentsExtended.xml` (A6-07).** Word stores a reply as a sibling `w:comment` and keeps the parent link and the `done` flag in that part (one `w15:commentEx` per comment, keyed by the `w14:paraId` of the comment's last paragraph); the extractor only knew a reply nested inside its parent and a `w:done` attribute, so a real Word thread gave `is_reply = FALSE`, `reply_to_id = NA`, `reply_count = 0` and nothing resolved. A reply now carries `is_reply`, `reply_to_id` and the Documents sheet's `reply_count`; a comment is resolved when its own flag or any comment above it in its thread is, because Word resolves a thread as a unit. A part that cannot be parsed is reported in `$errors`. The nested layout still works. **The Comments table, workbook sheet and `comments.csv` gain a `resolved_in_docx` column** (a system column, hidden in the workbook by default): the field was built for every comment but never copied into the merged table. It is Word's flag, separate from the reviewers' sign-off (`resolved`). The Documents sheet's `reply_count` was also 0 for any table whose `is_reply` column was logical, because the filter used a function that answers for one value only.
* **Behaviour change: the workbooks are written one column at a time, 28 times faster (A6-09).** Every cell was written with its own `writeData()` and `addStyle()` call, whose cost grows with every style already added: a 300-paragraph document with 40 comments and 50 redlines took 142.7 s for the first run and 174.6 s for the second (three workbooks per run); a 1,800-paragraph, 300-comment document did not finish in 10 minutes. They now take 5.0 s and 6.0 s, and 10.0 s and 11.9 s (Windows, one machine). Each column is one `writeData()` / `writeFormula()` call and each style one `addStyle(rows, cols, gridExpand = TRUE)`; the cell values, summaries and crosswalk are prepared once and the master and the forks are written from them; the merge no longer runs `clean_review_text()` over every existing row for every incoming row. The workbooks do not change: twelve configurations (typed and text-typed rows, master and forks, both Documents modes, with and without fork and sign-off reviewers, empty and one-row tables) were written by the old and the new code and compared cell by cell after resolving shared strings and style ids (values, types, formulas, style definitions, widths, hidden columns, panes, validations, conditional formats): identical, except `reply_count`, which is the first item.
* **Behaviour change: a `comments.xml` that cannot be parsed is an error of that file (A6-11).** The parse failure was swallowed, so a document whose comment ranges were intact produced rows with no author and empty text and was not in `$errors`; it is now listed there with `word/comments.xml could not be parsed: <reason>` and gets no rows, like a `document.xml` failure.
* **Behaviour change: a file name is matched against the pipeline catalog whole before a trailing `_xx` / `_xxx` is taken for reviewer initials (A6-15).** A stem that legitimately ends in two or three lowercase letters (two stages whose names differ from a third only by such a suffix) was filed under the shorter stem. The date is still stripped first, and initials on a name the catalog does hold (`stage_ABC`, `stage_xy`) are stripped as before; the rank, stage and sort order of such files change.
* **Behaviour change: a text box is read once, as part of the paragraph that holds it (A6-17).** A drawing stored as `mc:AlternateContent` holds the box twice (`mc:Choice` and an `mc:Fallback` copy) and its paragraphs sit under `w:txbxContent`: the extractor produced two insertion rows, text twice in the redline, extra redline rows for the box's own paragraphs, and paragraph numbers that no longer matched Word's. Fallback copies are skipped and text-box paragraphs are not paragraphs of their own. Revisions are also numbered by their paragraph now (they were all reported in paragraph 1, because paragraphs were matched on `format(p)`, which is the same string for every node).
* **Behaviour change: a redlined paragraph names every author who changed it (A6-18).** The row took author and date from the last revision in the paragraph, so Ann's insertion next to Bob's deletion was credited to Bob alone. `author` now lists each distinct author in the order of their first change, separated by `"; "`, and `date` the date of each author's last change in the same order (one author: as before). The columns are hidden by default in the workbook and nothing matches on them.
* **Behaviour change: changes in footnotes, endnotes, headers and footers are reported, not dropped silently (A6-19).** Only `document.xml` and `comments.xml` are read, so tracked changes and comment ranges in those parts were missing with no sign of it. Reading them would need a per-part paragraph numbering and a new source column through the merge, the workbooks and the CSVs, so the limitation is documented in `?cbe_docx_review_extract` and a single warning names each file, part and the number of tracked changes and comment ranges found there.
* **Behaviour change: the Documents-sheet rollup covers the rows written (A6-21).** The formulas compared a `SUMPRODUCT` over rows 2 to 5000 with a `COUNTIF` over the whole column, so a document with rows past 5000 could never show `TRUE` (with 5,200 signed-off rows the two terms were 4,996 and 5,200). The range now ends at the last row written plus 1000, and never before row 5000, so trackers of up to 3,999 rows keep their formulas. The formulas were evaluated in R (there is no spreadsheet engine on the development machine); they were not run in Excel.
* **Behaviour change: a `.docx` is checked against limits before anything is extracted (A6-22).** Every entry of the untrusted archive was unzipped into the temporary folder with no limit (a 61 KB archive holding a 63 MB entry was extracted in full). The listing is now checked first (entries: 10,000; summed uncompressed size: 2 GiB; one XML part that is read: 100 MiB), only the parts that are read are extracted, and a file over a limit is listed in `$errors` while the others carry on. **New argument `docx_limits`** (a named list with `max_entries`, `max_total_bytes`, `max_xml_bytes`; `Inf` switches one off) and the options `review.docx_max_entries`, `review.docx_max_bytes` and `review.docx_max_xml_bytes` set them.

## Fix: Review Findings in the Imputation, FAMD, and PCA Code

A statistical review of the `mtry` imputation sweeps (`missforest_sweep_mtry()`, `missranger_sweep_mtry()`), `step_famd()`, and the PCA plots found the following; each fix has a regression test. The items marked **Behaviour change** are calls that used to run, or to fail with an unrelated error, and now warn or stop.

* **`missranger_oob_by_mtry(seed = )` and the sequential `missranger_sweep_mtry(seed = )` restore the caller's RNG state.** `missRanger` calls `set.seed()` itself and nothing put the generator back, so in a bootstrap or simulation loop that drew `sample()` and then ran a seeded sweep, every replicate after the first drew the same indices. Seeded results are unchanged; random draws made by the calling code after the call used to depend on `seed` alone and now carry on from the caller's own stream, as after `missforest_sweep_mtry()`.
* **`missranger_sweep_mtry()` no longer crashes on data with nothing to impute**, that is complete data or data whose incomplete columns were all held out by `exclude` or `max_pct_missing`. It stopped with a dplyr "Can't recycle" error and a `min()` warning; it now returns the data unchanged with empty `oob_error` and `best` tables, as `missforest_sweep_mtry()` does.
* **Behaviour change: a column whose out-of-bag error is missing at every `mtry` now gets a warning instead of vanishing from `best`.** `missForest` reports `NaN` for a column observed in a single row. `best_mtry_per_column()` dropped such a column without a word, after which `missforest_sweep_mtry()` stopped with "Element `one` doesn't exist" and `missranger_sweep_mtry()` would have returned the column unimputed. Both engines now keep the column, take its imputation from the first swept `mtry` (its `best$error` is `NA`), and warn naming it; hold such columns out with `max_pct_missing` or `exclude`. Columns with a usable error are chosen exactly as before.
* **Behaviour change: `exclude` names that are not columns of `data` are an error** in `missforest_sweep_mtry()` and `missranger_sweep_mtry()`, naming the unknown names. They were dropped silently, so a misspelt identifier or time column stayed in the predictor set and was imputed and used as a predictor.
* **Behaviour change: `pca_variables_plot()` warns when the PCA was not scaled.** Loading times standard deviation is the correlation between a variable and a component only for standardised variables. For `prcomp()`'s default `scale. = FALSE` it is a covariance (up to 1.76 on `iris`), drawn in a unit circle under the title "Variables Correlation Circle", and one arrow ran off the panel. The warning gives the remedy, `prcomp(scale. = TRUE)`; the new optional `data` argument plots `cor(data, scores)` instead, for any scaling. Plots of scaled fits are unchanged.
* **`pca_variables_plot()` and `pca_percent_var_explained()` accept `prcomp(rank. = )` and `prcomp(tol = )` fits**, which stopped them with "non-conformable arguments" and a `seq` error. The percentages are shares of the total variance, as in `pca_scree_plot()` and `pca_biplot()`. `pca_percent_var_explained()` now computes them from `sdev` instead of `broom::tidy()`, so the plotted values no longer carry broom's rounding to five decimals.
* **Behaviour change: `step_famd()` stops when it is trained on a column that is neither numeric nor categorical.** A `Date`, `difftime` or `POSIXct` column was taken for a categorical variable with no levels, and `prep()`, or any later `bake()`, failed with "found categories not seen in training". `prep()` now names each such column and its class and suggests `step_date()` or `step_mutate()`; numeric, factor, character and logical columns are unaffected. `bake()` also names a column that was numeric in training but arrives as something else; a character column used to fail inside FactoMineR with "non-numeric argument to binary operator".

## Fix: Review Findings in the Survival Target-Encoding Steps and `tidy_tmerge_cox()`

A review of `step_lencode_coxnet()`, `step_lencode_joint_model()`, and `tidy_tmerge_cox()` found the following; each fix has a regression test. The items marked **Behaviour change** change what a call that used to run returns, warns, or stops on. The numbers the two steps encode, and the intervals `tidy_tmerge_cox()` builds with the default `post_event = "exclude"`, are unchanged.

* **Behaviour change: `step_lencode_joint_model()` treats a missing factor value as the reference level**, as `step_lencode_coxnet()` does. A factor with any `NA` used to make `joint_model()` refuse its data, so the step warned and encoded every level as 0, silently turning the feature into a constant. The factor is now taken from one internal helper in both steps, which recodes `NA` to the first level in `prep()` before fitting (`bake()` already encoded `NA` as 0 in both). Documented in `?step_lencode_coxnet` and `?step_lencode_joint_model`; the `step_lencode_coxnet()` numbers do not change.
* **Behaviour change: `step_lencode_joint_model(engine = )` warns for any value other than `"glmnet"`, and the engine is ignored.** The step only uses the Cox component's `.pred_risk_score`, which is fitted before the engine is looked at, so `engine` never changed the encoding; it only cost time (two extra models per factor) and made `prep()` need the baguette or stacks packages, so that without them the step warned and encoded zeros. `prep()` now always fits the joint model with `"glmnet"`; the encoding is the same as before. `?step_lencode_joint_model` now says what the step computes: the Cox log hazard ratio of each level against the reference level, not a composite that blends the survival, status and follow-up time models.
* **Documentation: both steps encode the training rows with coefficients fitted to those same rows** (no out-of-fold encoding), so the baked training data carries the outcome and a model fit to it over-trusts the encoded feature, most for factors with many or rare levels. In a check with a 100-level factor unrelated to the outcome and 300 subjects, the concordance of the encoded column was 0.62 in the baked training data and 0.51 on new data (penalty 0.05; 0.68 against 0.50 at 0.01). The help pages now say so, advise a large `penalty` or pooling rare levels, and advise against evaluating a model on data it was encoded with. The computation is unchanged.
* **Behaviour change: `tidy_tmerge_cox()` stops when `event_df` or `baseline_df` has more than one row for a subject.** The join multiplied that subject's measurement rows, gave zero-length intervals and no event, with only the generic `tstop <= tstart` warning. The error names the repeated subjects; `event_df` (and `baseline_df`) must have one row per subject.
* **Behaviour change: `tidy_tmerge_cox()` warns about subjects it drops or truncates.** A subject with no `event_time` in `event_df` (no row, or `NA`) lost its last measurement, and a subject whose `event_time` is at or before its first measurement had no interval at all, both without a word. Two warnings now name the subjects (up to 10): those without an `event_time` in `event_df`, and those with no row in the result. The intervals are the same as before; `?tidy_tmerge_cox` states the behaviour.
* **Behaviour change: `tidy_tmerge_cox(post_event = "include")` no longer returns a zero-length interval or flags the event twice.** The event time was moved to the subject's last measurement time, but that measurement still started an interval `(t, t]`, so the subject had `event = 1` on it and on the real last interval, plus the `tstop <= tstart` warning, and the row had to be removed by hand before `coxph()` or `coxnet()`. The last measurement now ends the last interval and starts none. The relocation itself is kept, as documented: measurements at 0, 3 and 10 with an `event_time` of 8 give `(0, 3]` and `(3, 10]` with `event = 1` on the second, so **the survival time of such a subject is the last measurement time, 10, not 8**; `"exclude"` keeps 8. `?tidy_tmerge_cox` says this and shows it in an example.

## Fix: `clean_publish()` Can No Longer Lose the Private History or Leak It

Found by the audit of the clean-history release tooling (A5-01 to A5-04); all four were reproduced in throwaway repositories with a local bare repository as the remote, and each has a test in `tests/testthat/test-clean_publish.R`, which used to cover `resolve_publish_branch()` only.

* **A5-01 (P0): the private branch is never overwritten by a shorter history.** `clean_publish()` ran `git branch -f <private> HEAD` with no check, so running it with the published branch checked out (after the first publish), or from a branch that is behind the private branch, turned `private-history` from 5 (or 6) commits into 1 (or 3) and left the rest on no branch. It now stops, before anything is changed, unless the private branch does not exist yet or is contained in the current commit; run it from the private branch. Before a branch moves, the old private and publish tips are also kept as `refs/backup/clean_publish/<UTC time>/<branch>` (restore one with `git branch -f <branch> <ref>` from another branch; for the branch that is checked out, which the private branch is after every run, git refuses that: run `git checkout <other branch>` first, or `git reset --hard <ref>`).
* **A5-02 (P1): the pre-push guard now works where it did not.** It was installed in `git rev-parse --git-dir`, which is not where git reads hooks for a linked worktree (`git push origin private-history` went through), silently skipped when a `pre-push` hook already existed or when `core.hooksPath` was set, and named only the first private branch ever used. The hook is now located with `git rev-parse --git-path hooks/pre-push` (worktrees and `core.hooksPath`); an existing shell-script hook gets a clearly marked block at its top and keeps working (its standard input is passed on), a hook in another language stops the run with a message, and a hook of the earlier version is upgraded; the protected names are read from the multi-valued git config key `cleanpublish.protectedbranch`, one per private branch ever used. After installing it, the function pushes the private branch name to a temporary local repository and stops if that push is accepted (a result that cannot be confirmed is an error as well, since the stage 1 hardening below; it was a warning). A `core.hooksPath` inside the working tree that is not ignored is refused, so the guard cannot become part of the published snapshot.
* **A5-03 (P1): the hook checks commits, not only ref names.** It also refuses any ref, tag or branch from which a commit of a protected branch can be reached that is not part of a clean commit made by `clean_publish()` (their ids are the values of `cleanpublish.cleancommit`): a tag on a private commit, a branch that contains the private history, `git push --all`/`--tags`, and the publish branch after the private branch was merged into it. `scripts/deploy_release.R` is unchanged and still has to stop unless it is on the publish branch and check the exit status of the branch push before it pushes the tag.
* **Checks first, then changes.** The working tree (untracked files count, ignored files do not and are never published), the ancestry of the private branch, publish or private branches checked out in another worktree (the default publish branch failed half way from a worktree, after switching it to `private-history`), the remote name, the existing hook and the hook location are all checked before any state changes. The clean commit is built as an object, the remote is updated before the local branches move (a failed push leaves them alone), and if a later step fails the branch that was checked out is checked out again. The remote's push URL (`git remote get-url --push`) is printed before the push.
* Every `git` argument is now quoted, so a repository path with a space works on Windows (it failed on the first git call) and branch or remote names cannot be interpreted by a shell on Linux and macOS (A5-13). The warning `git branch --unset-upstream` raised on every successful run is gone (A5-25), and `push`, `remote` and the branch names are validated.
* **Behaviour change of `scripts/clean_publish.R` (A5-04).** The script ignored every argument it did not know, so `--nopush`, `--dry-run`, `--no_push` and `--remote=backup` all FORCE-PUSHED to `origin`, and `--remote --no-push` used `--no-push` as the remote name. It is now a dry run by default (local branches rewritten, nothing pushed) and needs an explicit `--push` to publish; `--no-push` is still accepted and does nothing. An unknown option, a stray argument, a repeated option, a missing value and a value that starts with `-` are errors (status 2) that print the accepted options; `--flag value` and `--flag=value` both work (`--message=-text` passes a message that starts with `-`). The script loads the copy of the package it sits in, wherever it is started from. The parsing is the internal `parse_clean_publish_args()`, tested directly and through the script. The R function keeps `push = TRUE` as its default, but `remote` no longer has a default (see "no default remote" below), so a call that pushes must name the remote. The script has since become a wrapper around the internal function `clean_publish_cli()` (see the stage 3 section).

## Fix: Review Findings in the Penalized Cox (coxnet) Family

A statistical review of `coxnet()`, `cv_coxnet()`, `nested_cv_coxnet()`, and `cbe_loco_mp_coxnet()` found the following; each fix has a regression test. The first item is a behaviour change: a call that used to run now stops.

* **Behaviour change: `cv_coxnet()` and `nested_cv_coxnet()` stop when supplied resamples put a subject on both sides of a split.** A row-level `rsample::vfold_cv()` on start/stop data ran with no error or warning and scored every model on subjects it was fit on (28, 26 and 33 of 80 subjects per fold in the review), and `nested_cv_coxnet()` did the same on its outer splits, so the "unseen" outer metrics were optimistic. `resamples` (and, for `nested_cv_coxnet()`, the outer splits and every inner resample) are now checked before anything is fit: any subject, or with `group` any group, in both the analysis and the assessment set of a split is an error that names the resample and some of the subjects. Group the folds by subject with `rsample::group_vfold_cv()` or `rsample::group_bootstraps()`; bootstrap out-of-bag sets never overlap and pass. **New argument `check_subject_overlap = TRUE`** (all four `cv_coxnet()` methods and `nested_cv_coxnet()`) switches the check off for overlap that is deliberate. Folds that `cv_coxnet()` builds itself are not checked, and the check reads row indices only (about a second for a million rows and 10 folds on the development machine).
* **`Inf` and `-Inf` are rejected** in the predictors and in the follow-up times that `coxnet()`, `cv_coxnet()`, `nested_cv_coxnet()` (for its analysis sets), and `cbe_loco_mp_coxnet()` fit, naming the offending predictors. glmnet took them without a word and gave an infinite predictor a coefficient of 0, so the most important predictor could drop out of the model.
* **`strata()` and `offset()` in a formula are an error** ("strata() and offset() are not supported") instead of being fit as penalized indicator columns and dropped, respectively. A column that is merely named `strata` or `offset` is still an ordinary predictor. Documented in `?coxnet`.
* **Assessment subjects with missing predictors are left out for every penalty.** A factor level the analysis set never saw becomes `NA` in the assessment set (hardhat only warns), and its subject used to be scored at the penalties where the level's coefficient is 0 but dropped by yardstick at the others, so penalties were compared on different subjects; an assessment set with no seen level crashed in yardstick. `cv_coxnet()` and `nested_cv_coxnet()` now drop such a subject, with all its rows, at every penalty and warn once per resample with the count; a resample with nothing left is skipped (`cv_coxnet()`) or an error naming the outer split (`nested_cv_coxnet()`).
* **`cbe_loco_mp_coxnet()` requires 3 predictors.** With 2, every minipatch had 1 predictor, which `coxnet()` rejects, and the call ended in the misleading "Fewer than 3 minipatches succeeded". It now says that each minipatch needs 2 predictors and leaves 1 out.
* **`cbe_loco_mp_coxnet()` validates its data before drawing patches and reports the patches that fail.** A single `NA` cell used to cut 40 minipatches to 19 with no message, and a non-numeric `x` or an unsupported argument gave the same misleading error. Missing or infinite values, a non-numeric `x`, and arguments such as `weights` are now errors up front (an `NA` that used to shrink the ensemble is now an error, since `coxnet()` cannot take it either); a patch that still fails is counted, a warning gives the count and the first error, and the "Fewer than 3 minipatches" error carries the first error. Calls with valid data draw the same patches as before.
* **`cbe_loco_mp_coxnet()` uses the subject a recipe's `"id"` role names**, as `cv_coxnet()` does, so a recipe with start/stop data no longer needs `subject_id`, and `nested_cv_coxnet(importance = "loco_mp")` no longer warns on every outer split and returns no importance in that case.
* **`nested_cv_coxnet(importance = "loco_mp")` passes the glmnet arguments in `...`** (`standardize`, `cox.ties`, `penalty.factor`, ...) to `cbe_loco_mp_coxnet()`, so the minipatches are fit like the tuned model; before, the importance came from glmnet's defaults. Arguments `cbe_loco_mp_coxnet()` sets itself, such as `B`, are not forwarded. `group` is not forwarded, because LOCO-MP draws its minipatches by subject; a warning says so when both are given.
* **Documentation.** The LOCO-MP p-values are one-sided (`importance > 0`) while the confidence intervals are two-sided at `alpha`, and `autoplot()` colours "Significant" by the one-sided adjusted p-value; `?cbe_loco_mp_coxnet`, `?tidy.cbe_loco_mp_coxnet`, and `?autoplot.cbe_loco_mp_coxnet` now say so. `?predict.cv_coxnet` no longer points to `type = "time"`, which `match.arg()` has always refused.

## Fix: `convert_pdfs_to_docx()` Checks the Input Before the Machine

* **Input is validated before any backend is looked for.** The fences added in 0.4.0003 (missing or empty source, `src` equal to `dest`) ran after `check_docx_toolchain()`, so on a machine with no converter (GitHub's Linux and macOS runners, among others) `strict = TRUE` reported "No PDF -> DOCX backend is available" instead of the problem with the rows, and the tests that expect the input errors failed there while passing on a machine with Python or Word. The whole `conversions` table is now checked first; only then is a backend looked up, and only when at least one row is left to convert. `strict = TRUE` still stops at the first bad row, now before anything is converted; `strict = FALSE` still warns and skips it, returns `converted = FALSE` for that row, and no longer needs a backend when every row was skipped. A `src` that resolves to the same file as `dest` remains an error in both modes. Arguments and defaults are unchanged; `convert_pdf_to_docx()` gets the same order through `convert_pdfs_to_docx()`.
* A missing or empty `dest` is now reported like a bad source row (error with `strict = TRUE`, warning and skip otherwise) instead of failing later inside the conversion.
* `tests/testthat/test-pdf_to_docx.R` no longer depends on what is installed: tests of input errors fail if the toolchain is probed, tests that get past validation mock the toolchain and the backends, and only the opt-in tests that run a real converter need one.
* **`testthat (>= 3.2.0)` is now the minimum in `Suggests`.** The test suite uses `local_mocked_bindings()` (six test files), which testthat introduced in 3.2.0; `DESCRIPTION` said 3.1.7, so the tests could not run on 3.1.x.

## Fix: Mapping-File Guard Follows Links and Relative Paths

* **A `secrets_path` (or `R_USER_DATA_DIR`) that reaches a repository through a link is now refused before anything is created.** `normalizePath()` leaves a path that does not exist yet exactly as written, so the walk up looking for `.git` started from an unresolved spelling. With a symbolic link, or a Windows junction, pointing at a subdirectory of a repository it never reached the repository root: the package function still refused, but only after creating empty directories inside the repository, and `scripts/pi_anonymizer.R`, run on its own, did not refuse at all and wrote the mapping file there.
* **On Linux and macOS a relative `secrets_path` whose folders do not exist yet escaped the up-front check when the working directory was inside a repository but not at its root**: the relative spelling never led the walk to the repository above the working directory, so the package function created the folders before refusing and the script did not refuse.
* Both now resolve the part of the path that exists and then walk up (internal `.canonical_path()`). The same resolution makes the check independent of the macOS `/var` -> `/private/var` and Windows 8.3 short-name spellings of a temporary directory. `default_secrets_path()` returns the same string as before.

## Fix: Review Findings in the Joint Model (`joint_model()`, `cv_joint_model()`, `nested_cv_joint_model()`, `cbe_explain()`)

A statistical review of the joint model and its explainers found the following; each fix has a regression test that fails on the previous code. Four items are **behaviour changes** (a call that used to run now stops, or returns other numbers) and are labelled as such: if you compared joint-model IBS values with earlier runs, expect them to move.

* **Behaviour change: `cv_joint_model()`, `nested_cv_joint_model()` and the shared fold scorer stop for start/stop `Surv(start, stop, event)` outcomes without `subject_id`.** They used to run: every interval was scored as its own subject (237 pseudo-subjects instead of 120 in the review) and, with the default folds, a subject's intervals fell on both sides of a split, with no message.
* **Behaviour change: supplied `resamples` (or the outer splits and inner resamples of a `nested_cv` object) that put a subject in both the analysis and the assessment set of a split are an error**, as in `cv_coxnet()`. A row-level `rsample::vfold_cv()` on start/stop data scored every model on subjects it was fit on (38 to 45 subjects per fold in the review). Use `rsample::group_vfold_cv()` or `rsample::group_bootstraps()` grouped by the subject (a bootstrap's out-of-bag assessment set never overlaps its analysis set). The new argument `check_subject_overlap = TRUE` (same name as in `cv_coxnet()`) switches the check off, for overlap that is deliberate. Folds the functions build themselves are never checked. The inner resamples of `nested_cv_joint_model()` are checked too, although that function uses only the outer splits.
* **Behaviour change (results): the coxnet component of a start/stop joint model is scored like `cv_coxnet()`, and the joint-model IBS is `yardstick`'s IBS.** New argument `covariates = c("path", "baseline")` on `cv_joint_model()`, `nested_cv_joint_model()` and `joint_model()`, with the meaning and default of `cv_coxnet()`'s: `"path"` integrates the subject's hazard over their start/stop covariate path, `"baseline"` uses only their first interval. Before, each subject's last-interval covariates were held constant from time 0, which uses values from after the times being scored (IBS 0.2581 where the covariate path gave 0.2047 and the baseline 0.2120 on the same fit in the review); that rule is no longer offered. For the same fit, folds and `eval_time`, the `coxnet` rows of `cv_joint_model()` now equal `cv_coxnet()`'s IBS and concordance (its concordance uses the survival at the last `eval_time`). The status and time models predict one value per row, so for start/stop data they use each subject's **baseline (first-interval)** row. **IBS numbers change for start/stop users.**
* **Behaviour change (results): the IBS formula of the joint model is now `yardstick::brier_survival_integrated()`'s.** `score_surv_matrix_ibs()`, behind `cv_joint_model()`, `nested_cv_joint_model()` and `cbe_survex_loss_ibs()`, divided each time's Brier score by the sum of the censoring weights and the integral by (max - min) of the evaluation times (0.2094 where `yardstick` gave 0.1699 on the same predictions). It now divides by the number of subjects and integrates as `yardstick`, `glmnet_IBS()` and `cv_coxnet()` do, so the IBS of all three joint-model rows changes for right-censored data too, and `cbe_survex_loss_ibs()` agrees with `cbe_survex_loss_brier()`.
* **Behaviour change (results): the status calibrator is fitted on out-of-fold predictions.** For `engine = "baguette"` and `"stacks"` it was fitted on the training rows' own bagged-tree probabilities (AUC 0.999 in sample against 0.51 out of sample on pure noise), so `.pred_status_calibrated` was pushed towards 0 and 1 and was worse than the raw probability out of sample (standard deviation 0.408 against 0.214 for the raw one). It now learns from `cv.glmnet(keep = TRUE)`'s prevalidated predictions at `lambda.min` (glmnet engine) or from an inner 5-fold cross-validation of the bagged trees, with folds grouped by subject for start/stop data. When out-of-fold predictions cannot be produced, or the calibrator cannot be fitted, `joint_model()` warns and returns the status probabilities uncalibrated; the failure used to be silent. For start/stop data the glmnet status and time models also choose their penalty on folds grouped by subject.
* **`cbe_explain_survival(joint_model)` and `cbe_explain()` work with factor and character predictors.** Their default `data` was the one-hot columns, while the coxnet model forges from the original ones, so the explainer failed on first use (`The required column "g" is missing.`), and user-supplied raw data failed in the status and time explainers instead. `joint_model()` now stores the raw predictor columns (`components$raw_predictors`), the explainers use them as the default `data`, and the status and time explainers apply the model's training encoding with `hardhat::forge()`, as `predict()` does, so all three explainers take the original columns.
* **An assessment subject with a missing predictor is left out of all three joint models' scores.** A factor level the analysis set never saw gave `NaN` for the coxnet IBS of the whole fold while the status and time models stayed finite. As in `cv_coxnet()`, the subject is dropped at every model with one warning that gives the count; a resample with no subject left is skipped, and an error says so if none could be scored.
* **`engine = "stacks"` is documented for what it does, and warns.** It fits a glmnet Cox meta-learner (the stacks package is not used) that `predict()` never uses, so its predictions are the `"baguette"` ones. `joint_model()` now says so with a warning, the docs say so, the package `stacks` is no longer required for it, and a failing meta-learner fit is a warning instead of a silent `NULL` (or a fixed-penalty fallback). Exposing the meta-learner in `predict()` is not done.
* **Documentation.** `?cv_joint_model` no longer says it mirrors `cv_coxnet()` where it did not, and states which row each model is scored from; `?nested_cv_joint_model` says the inner resamples are not used to fit anything; `?joint_model` says `penalty` and `...` reach the coxnet model only; `?predict.joint_model` says that start/stop rows are predicted one by one from time 0.

## Fixes from the pre-release review: pipeline graph (`R/compute_graph.R`)

* **A changed upstream file makes every report that depends on it stale.** Staleness now passes through helper scripts and through producers that do not render a document (`renders = FALSE`), so `get_render_plan()`, `as_igraph()` and `visualize_pipeline()` agree. A stale producer is named in a message ("Out of date, but not rendered here"), and when no report reads its outputs the plan says so instead of claiming everything is up to date.
* **Declared files that are not on disk are reported.** A mistyped path, or relative paths built from the wrong working directory, used to be dropped silently and the report was judged fresh. `get_render_plan()` now warns once, naming the files (including files named only inside a `dependencies` list), and still treats them as unchanged. Outputs that do not exist yet are not reported, since their producer will create them.
* **`get_render_plan()` accepts a single report** that is not wrapped in a list; it used to return an empty plan without a word.
* **`export_interactive_pipeline()` no longer deletes an existing `<stem>_files` folder** next to the target file; a folder it created itself is still removed.
* **`export_subgraph()` rejects a misspelled `focal_node` or an unknown `stage`** with an error (it exported the whole graph), and `stage =` now exports that stage only; a known stage used to export the whole graph as well.
* **`as_tbl_graph()` keeps tidygraph's behaviour for lists that are not pipelines.** The package's method no longer replaces tidygraph's own list method once the package is loaded.
* **`collapse_by_stage()` marks a stage stale when any one of its files is stale**, not only when its first node is.

* **One check for graphs, plans, summaries and plots.** `as_igraph()`, `get_render_plan()` and `visualize_pipeline()` (so also `pipeline_summary()`, `print_pipeline()`, the interactive views and `export_subgraph()`) now reject a top-level name used twice, with the message `get_render_plan()` always had (the first object used to vanish silently), warn once when two producers declare the same output, and warn once when one path appears under two names or one name stands for two paths. A name that reappears as a bare copy inside a `dependencies` or `output` list is still one node.
* **`create_qmd_renderer()` validates `name` and `path` like the constructors do**, so an `NA` or empty value is an error instead of an output called `NA.pdf`. **`output_format = "yaml"` gives the real extension** (`revealjs`, `ioslides`, `slidy` write `.html`, `beamer` and `typst` `.pdf`, `gfm` `.md`, and so on) instead of the format name, so such a report is no longer out of date on every run. `render()` and `zip_render()` read the same mapping, and a format with no known extension keeps its name and warns that the extension is a guess.
* **`join_pipelines()` no longer lets placeholders win.** A graph that only reads a file knows it as stage "Other", role "unspecified" and no description; those count as missing, so the real values win whichever graph comes first, `stale` and `renders` are true if true in either graph, a shared edge is kept once (joining a graph with itself no longer doubles its edges), and the joined graph has a `label`.

Not fixed yet: graph identity is still the node name rather than the path (a mismatch now warns).

# TempleCBE 0.4.0003.2026.09.30.01.11

## PDF -> DOCX Conversion: Defensive Fences & Test Coverage

* **`convert_pdfs_to_docx()` and `convert_pdf_to_docx()` fortified**:
  - Validated `backend` parameter against supported backends (`"auto"`, `"python"`, `"libreoffice"`, `"word_com"`), returning an informative error on unsupported backends.
  - Enforced required `src` and `dest` columns and input schema on `conversions` data frame; `NULL` input cleanly returns an empty data frame with logical `converted` column.
  - Validated source PDF file existence and non-zero byte size with strict mode error reporting (`strict = TRUE`) and non-strict warning/skipping (`strict = FALSE`).
  - Added self-collision guard preventing `src` and `dest` from resolving to the same file.
  - Added validated positive numeric `timeout` constraint.
  - Added comprehensive mocked unit test coverage in `tests/testthat/test-pdf_to_docx.R` (58/58 tests passing).

## SAS Macro Tokenization: Decimal Evaluation and Observation Times

* **`%cbe_counting_process` (`obs_times`) and `%cbe_brier_score` (`eval_times`) now correctly handle decimal/fractional numbers.**
  SAS's `%sysfunc(countw(...))` and `%scan(...)` use default delimiters that include the period (`.`), which caused numbers like `0.5 1.0 1.5` to be split into separate digits (`0`, `5`, `1`, `0`, `1`, `5`). This doubled array bounds and caused macro execution crashes. The delimiter is now explicitly restricted to blank (`%str( )`). Regression tests in `test-run_sas_script.R` verify delimiter enforcement.

## Multi-Row Excel Header Ingestion

* **`read_excel_multiple_headers()` fortified**:
  - Upper-tier merged cells are now horizontally forward-filled left-to-right (`fill_merged = TRUE`, default) so spanning categories (e.g. `"Demographics"`) properly qualify all subcolumns (e.g. `"Demographics | Age"`, `"Demographics | Sex"`).
  - Header extraction is strictly isolated to `col_types = "text"` so data-body `col_types` passed in `...` do not collide with header text parsing.
  - Added defensive validation (`file.exists(path)`, positive integer `n_header_rows`), unique column name repair via `vctrs::vec_as_names`, optional `clean_names = TRUE`, and explicit `sheet = 1` support.
  - Dedicated unit tests added in `tests/testthat/test-read_excel_multiple_headers.R` (19/19 passing).

## Computational Pipeline Dependency Graphs (`compute_graph.R`)

* **S4 Validity & DAG Integrity**:
  - Added formal `setValidity` methods and constructor assertions for `FilePath`, `FileUses`, and `FileOutputs` enforcing scalar types, non-empty paths/names, and rejecting circular self-dependencies (`x@name %in% dependencies`).
  - Added immediate `igraph::is_dag()` validation in `get_render_plan()` to detect and reject cyclic pipelines with a clear, descriptive error prior to topological sorting.
  - Guarded against duplicate node names in `get_render_plan()` to prevent silent map overwriting.
  - Fortified `.is_stale()` with numeric epoch timestamp comparisons to prevent timezone/POSIXct coercion issues.
  - Comprehensive unit test suite added in `tests/testthat/test-compute_graph.R` (32/32 passing).

## Bug Fix: SAS `%cbe_brier_score` Ignored the IPCW Event Weights

Found by running the macro in SAS 9.4 (TS1M8) on the full `survival::lung` cohort and comparing it with R.

* **G(T_i-) was always 1** (`inst/sas/cbe_brier_score.sas`): the left limit of the censoring survival was taken as `max(G)` over the grid times before `T_i`, but the reverse Kaplan-Meier `G` is non-increasing and the grid starts at `G(0) = 1`, so every event was weighted `1 / max(1, trunc) = 1` instead of `1 / G(T_i-)`. It is now `G` at the last grid time strictly before `T_i`, floored at `trunc`. On `survival::lung` (227 complete cases, PHREG Breslow `age + sex + ph_karno`, evaluation times 100-500, `trunc = 0.05`; 49 subjects censored by day 500) SAS printed Brier scores 0.11248 0.19918 0.22247 0.21612 0.18977 and IBS 0.15778; R (`add_graf_weights()` with `yardstick`) gives 0.11252 0.20093 0.22951 0.22692 0.20005 and IBS 0.16273, and the fixed macro now prints those. Data whose censoring times are all after the last evaluation time were not affected (the 31-subject benchmark's numbers are unchanged).
* **`out_brier` has one row per evaluation time** (it had one identical row per subject and evaluation time; the summary `PROC SQL` lacked a `GROUP BY`).
* **No unset-variable warning, exit status 0**: the calibration table was initialised with a column `obs_km_surv` but appended `obs_prop_surv`, which SAS reported as `WARNING: Variable obs_prop_surv was not found on BASE file` at every evaluation time (exit status 1, so `run_sas_script()` warned every time) and the column was dropped from `out_calib`. It is `obs_prop_surv` throughout (the share of the decile still at risk at the evaluation time, not censoring-adjusted). The logs still show `PROC LIFETEST`'s `WARNING: ODS graphics must be enabled ... PLOTS=` line, which does not change the exit status (0 for both benchmark programs).
* **New full-lung benchmark**: `inst/sas/benchmark_brier_lung_full.sas` (datalines are `survival::lung`), its committed log and listing, and the reference numbers `tests/testthat/reference/sas_brier_lung_full.csv` (SAS 9.4 TS1M8, run 2026-09-29). Unlike the 31-subject benchmark, it exercises the censoring weights. Tier 1 tests compare R with the reference (PHREG and Brier/IBS) and check that the program's datalines are `survival::lung`; the opt-in Tier 2 test (`TEMPLECBE_RUN_SAS_TESTS=true`) re-runs the program in SAS. The committed listings and logs of `benchmark_brier_lung.sas` were regenerated with the fixed macro; its numbers did not change. `expect_matches_sas()` now also fails on a row listed twice.
* **Documentation**: the table in `vignettes/sas_survival.Rmd` (section "Numerical Concordance Validation") listed R's numbers in its "SAS `%cbe_brier_score`" column and labelled them "Exact match", though the macro printed the different numbers above. Its SAS column now holds the numbers the fixed macro printed, with their source, and the status reads "Same to 5 decimals". The "exact numerical concordance" wording in the macro header and `benchmark_brier_lung.sas`, and the "machine precision"/"exact numerical parity"/"complete ... parity" summary sentences of the two SAS vignettes, were rewritten to say what is checked. The rendered vignette PDFs under `vignettes/` were not regenerated.

## Two-Tier SAS/Python Parity Tests and Guards

Code that needs SAS or Python now runs only on machines that have them, and everything else still passes and still checks the R side.

* **Tier 1 (always runs, needs no SAS)**: R is compared with numbers SAS produced once, committed under `tests/testthat/reference/` with the SAS release and run date (SAS 9.4 TS1M8, SAS/STAT 15.3, 2026-09-17), taken from the listings in `inst/sas/list/`. Covered: `PROC PHREG` on the counting-process tumor data (SAS/STAT Example 85.7, `cbe_cox_multi()`) and on the 31-subject lung benchmark of `inst/sas/benchmark_brier_lung.sas` (estimates, standard errors, chi-squares, hazard ratios, -2 log L, AIC, SBC, the three global tests), plus that benchmark's Brier scores and integrated Brier score. A further test checks that `tumor_wide()` is the data typed into the SAS program. The 31-subject Brier benchmark does not exercise the censoring weights (all three censoring times are after day 500, so every weight is 1); the full-lung benchmark described above does, and the weights are also covered by the hand-computed test in `test-surv_helpers.R`.
* **Tier 2 (opt-in, needs SAS)**: the same programs (the tumor example and both lung benchmarks) are run through `run_sas_script()` (log and listing in a temporary folder) and their listings are compared with the same reference files. Set `TEMPLECBE_RUN_SAS_TESTS=true` to run them; without it, or without SAS, they are skipped. Likewise `TEMPLECBE_RUN_PDF_TESTS=true` runs one real PDF to DOCX conversion per backend (Python with `pdf2docx`, LibreOffice, Word), each skipped when its backend is absent. Both variables are described in `?find_sas` and `?find_python`. The test helpers `skip_if_no_sas()`, `skip_if_no_python(module)`, `skip_if_no_soffice()` and `skip_if_no_word_com()` are in `tests/testthat/helper-external-tools.R`; none of them starts Python inside R.
* **`sas_available()`** (new; `!is.null(find_sas())`) replaces the three separate `has_sas <- ...` assignments in the SAS vignettes, whose SAS chunks now use `eval = TempleCBE::sas_available()`. The two chunks that run a bundled SAS program now write its log and listing to a temporary folder; by default they are created beside the script, which is inside the package library (and, when the package is loaded from a source checkout, could overwrite the tracked listings in `inst/sas/`).
* **`find_python(module = )`**: the module an interpreter must import (default `"pdf2docx"`, as before), so a guard can ask for any module. The reticulate vignette now requires an interpreter that can import `numpy` and pins reticulate to it; it no longer calls `reticulate::py_available(initialize = TRUE)`, which started whatever Python it found. `find_python()` also no longer asks `reticulate::py_exe()`, which can start Python, unless reticulate has already started it.

## Bug Fix: PDF -> DOCX Toolchain Discovery

* **`find_python(verify = TRUE)` never verified anything**: the internal exit-status helper dropped the exit code of a process run with `stdout = FALSE, stderr = FALSE`, so an interpreter that could not `import pdf2docx` was accepted (and `check_docx_toolchain()` reported the python backend as usable). It now reads the exit code. (The LibreOffice backend keeps judging success by the output file alone.)

## BEHAVIOR CHANGE: Deterministic Investigator Pseudonyms Now Require a Secret Key

* **`anonymize_pi()` (`method = "hmac_token"`, the default, and `"hmac_surname"`) and `generate_pseudonym_token(name = )` now stop when no secret key is available** (`key` empty and `TEMPLECBE_SECRET_KEY` unset). They used to fall back silently to the constant `"temple_cbe_default_salt"`, which is public, so anyone could recompute the tokens from a list of names. The error says how to set the key, for example in `~/.Renviron`. The check runs before the data is looked at, so an all-`NA` input without a key is now an error too (it used to return `NA`).
* **The old convenience behavior is still available, on purpose:** both functions gain `allow_default_key = FALSE` (last argument). `allow_default_key = TRUE` restores the fallback, with a warning that the output is NOT secret; the pseudonyms are identical to earlier versions. Existing code that relied on the silent fallback must either set a real key (which changes every pseudonym) or pass `allow_default_key = TRUE`.
* **`method = "token"` no longer derives tokens from the public default key.** With no key, names added to the mapping file now get random tokens (they used to be the public-key HMAC of the name, recomputable by anyone holding the names). Mapping files already on disk are read unchanged, and with a key set the tokens are unchanged; treat tokens issued earlier without a key as recomputable and re-issue them if that matters. Without a key this method now needs `jsonlite`. `method = "synthetic"` is unchanged and needs no key.
* **Mapping-file guard:** a `secrets_path` (or default location) inside a git repository is now refused before any directory is created, so a refused call no longer leaves empty directories behind in the repository. The refusal was already in place; it just used to happen after `validate_secrets_dir()` had created the directory. `scripts/pi_anonymizer.R` had a weaker guard (working directory only, `.git` directories only) that could write the mapping file into a repository; it now matches the package.
* **`scripts/pi_anonymizer.py` follows the package:** `anonymize_pi()` raises `ValueError` without a key and accepts `allow_default_key=True` (warns). An empty `TEMPLECBE_SECRET_KEY` now counts as unset. `scripts/pi_anonymizer.R` has no key or default salt (random tokens only), so it needed no key change. New `scripts/test_pi_anonymizer.py` shares its known-answer vectors with the R tests.
* **Docs:** `anonymize_pi()` documented `n_chars` as 8; the default has always been 16. Examples no longer show a literal `key = "study_salt"`; they set a throwaway key for the example only, which models keeping keys out of committed scripts. The help pages and the pkgdown section (now "Investigator Name Pseudonyms") say plainly that only investigator names are pseudonymized: dates, record numbers and free text are not touched, and this is not general de-identification.

## Breaking Changes: Review Extractor Reviewer Identities

* **`cbe_docx_review_extract()` no longer names its reviewers.** The reviewer columns, the default `fork_reviewers`, the fork workbook names and the document-level sign-off pair were hardcoded to a fixed set of people in `R/cbe_review_extract.R`. `review_config()` (added by PR #36, then lost when the PR #39 merge kept the older file) is restored with its `signoff_reviewers` and `documents_sheet_mode` arguments, so reviewer ids are project data: set them once with `review_config(fork_reviewers = , signoff_reviewers = )` or pass them per call. `fork_reviewers` now defaults to `review_config()$fork_reviewers`, the placeholders `reviewer_1`, `reviewer_2`, `reviewer_3`, and no document-level sign-off column is written unless `signoff_reviewers` is set.
* **What breaks:** with no configuration, a new tracker gets `reviewer_1`..`reviewer_3` columns and `review_tracker_reviewer_<n>.xlsx` fork workbooks, and no sign-off pair, instead of the previous per-person columns and fork files. A tracker written by an earlier version is not recognised as belonging to those reviewers until they are configured.
* **Keeping the old behaviour:** before the next run, call `review_config()` with the ids your existing tracker uses. They are the header names on its Comments sheet (each reviewer has an `<id>` and an `<id>_comment` column) and the `<id>` in each `review_tracker_<id>.xlsx` fork workbook. Reviewers with a fork workbook go in `fork_reviewers`; a reviewer who only has a Documents-sheet sign-off goes in `signoff_reviewers`: `review_config(fork_reviewers = c("<id_1>", "<id_2>", "<id_3>"), signoff_reviewers = "<sign_off_id>")`. Keep that call in the project (a script or `.Rprofile`), not in package code.
* **Existing workbooks stay readable:** columns are merged by name, so an unconfigured reviewer's column and values survive on the Comments and SuggestedChanges sheets. The extractor now warns, naming the ids, when an unconfigured reviewer has entries in the existing tracker (a sign-off or comment in the master, a Documents-sheet sign-off, or a fork workbook that has not been merged), because such a reviewer's fork workbook is not merged and their Documents-sheet sign-off is not rewritten until they are configured. Blank leftover columns do not warn.
* `extract_docx_stem()` also strips a configured reviewer id from the end of a file name (previously one specific id was built in), and the `TEMPLECBE_ANALYSIS_ROOT` fallback for locating the manifest and edits directory is kept.

## Maintenance

* **Study-specific names removed from tracked text files**: the extractor's test fixtures use generic reviewer names, an internal migration catalog under `completed_tasks/` was renamed and reworded, and two older entries in this file no longer name the downstream study.
* **New `test-denylist.R`**: scans every tracked file (names and contents, plus the extracted text of tracked PDFs) for terms listed in an untracked file, either the path in the `TEMPLECBE_DENYLIST` environment variable or `.git/info/denylist`, one case-insensitive term per line. It skips when neither exists, so the terms themselves never enter the repository, and it reports file paths only.

# TempleCBE 0.4.0002.2026.09.29.16.28

Independent validation of 0.4.0001 (full test suite and `R CMD check` clean), followed by a bug-fix pass. Every fix below has a regression test that fails on the 0.4.0001 code.

## Bug Fixes: Categorical Tests & Plots

* **Unused factor levels no longer demote a 2x2 test** (`cbe_test_categorical()`, `cbe_exact2x2()`, `cbe_four_quadrant_report()`, `cbe_contingency_plot()`): a binary variable with an empty extra level (e.g. after filtering) was tabulated as 3x2, which skipped the exact 2x2 path, ran a *random* Monte Carlo Fisher test (p-value changed between calls), gave `NaN` for `test = "chisq"`, and made the 2x2-only helpers refuse the data. All-zero rows/columns are now dropped before testing. A variable observed at only one level now returns `NA` ("Insufficient levels") instead of `p = 1`.
* **Fisher labels**: a 2x2 Fisher test on more than 500 observations is exact but was labelled "simulated"; `cbe_contingency_plot()` captions for larger tables now say "simulated" where the p-value is a Monte Carlo estimate.
* **`cbe_exact2x2()` works as a `gtsummary::add_p()` custom test**: it now absorbs the `group`, `type`, `test.args`, `adj.vars`, `tbl` and `continuous_variable` arguments gtsummary always passes (previously every p-value came back blank). The `(data, variable, by)` form is documented as by-name only.
* **`plot_categorical_associations(method = "p_value")`**: pairs are on one deterministic scale (exact 2x2 test, asymptotic Pearson for larger tables) instead of mixing exact p-values with Monte Carlo ones that can never fall below `1/2001` (a perfect 3x3 association plotted weaker than a moderate 2x2 one); an untestable pair is now `NA` rather than being drawn as the *strongest* association (`max(NA, 1e-16, na.rm = TRUE)`); and an all-insignificant matrix no longer crashes `corrplot`.
* **`...` now overrides corrplot defaults** in `correlation_plot()`, `plot_categorical_associations()` and `cbe_contingency_plot(type = "corrplot")` instead of failing with "formal argument matched by multiple actual arguments"; a stray `type` passed to `cbe_four_quadrant_report()` is ignored.
* **`correlation_plot()` / `correlation_plot_split()`** drop constant or all-missing columns with a warning naming them, instead of dying inside `corrplot`/`hclust` with an error that never mentions the column.

## Bug Fixes: Comparison, Tests & Summaries

* **`cbe_compare_df()`**: identical infinite values no longer abort the comparison (`Inf - Inf` was `NaN`); differences are taken in double, so large integers cannot overflow; rows sharing a key are paired in order of appearance with a warning (SAS `PROC COMPARE` behaviour) instead of a many-to-many join that reported a base with two identical rows and a compare with one as concordant; a key column named `variable`, `label`, ... is prefixed `key_` in `diffs` instead of being renamed `variable...1`.
* **`corr_test_all()`**: an unknown `method` or invalid `alternative`/`conf.level` is now an error, not a table full of `NA`.
* **`multiple_t_test()`**: the default variable list excludes `.class` and `.id` (a numeric 0/1 classifier was "tested" against itself); `.id` is an explicit argument.
* **`single_t_test()` (paired, by `.id`)**: a missing measurement drops that pair (as `t.test()` does) instead of aborting with a misleading "id missing" error; duplicated ids are a clear error instead of a list-column warning.
* **`min_max_norm()` / `range_norm()`**: a constant input maps to 0 (was `NaN`), an all-missing or all-infinite input to `NA` (was `Inf` plus a warning), infinities no longer define the range, and a non-numeric vector gets a clear error.
* **`z_norm()`**: `na.rm = FALSE` on data containing `NA` returns `NA` (it returned 0 for every non-missing value); a matrix is standardized column by column (it was standardized as one long vector), consistent with the data-frame method.
* **`flag_outliers()` / `detect_outliers()`**: values are flagged only when strictly beyond a fence, so a constant column no longer has every value flagged `EXTREME` (and the ordinary values in a zero-IQR column are no longer flagged); a data frame with no numeric columns returns the documented empty schema instead of erroring.
* **`features_percent_miss()`** keeps its class on a zero-row input.

## Bug Fixes: Cox Models

* **`cbe_cox_single()`** labelled the interval of a numeric predictor `95% CI` even when `conf_level` was not 0.95.
* **`cbe_cox_multi()`**: `converged` no longer compares the iteration count with the *default* `iter.max`, which reported "Converged: Yes" for a fit that had run out of a smaller `control = coxph.control(iter.max = )`. It also warns when coefficients (interactions, transformed terms such as `log(x)`) are in the model but not in the formatted `table`.

## Bug Fixes: Packaging & Reproducibility

* **`package_deliverables()`**: a relative `zip_path` no longer silently destroys the archive (`zip::zip(root = )` wrote it inside the staging folder, which was then deleted, while the call reported success); the path is returned as documented, absolute; deliverables sharing a file name are kept with a numeric suffix and a warning instead of overwriting each other while still being counted.
* **`cbe_loco_mp_coxnet(seed = )` and the sequential `missforest_sweep_mtry(seed = )`** restore the caller's RNG state on exit (as `simulate_cohort()` and the anonymizer helpers already did), via a small internal `local_seed()`.
* **`build_manual_versioned()`** checks that `devtools` is installed before using it.
* **`plot_cox_survival()` docs** no longer claim an `id_col` default of a study-specific column name; the default is `NULL` (row number). Study names were also removed from the compute-graph vignette and `MakeComputeGraph.R` comments.

## Maintenance

* Removed the stray `RoxygenNote: 7.3.3` from `DESCRIPTION`, which contradicted `Config/roxygen2/version: 8.1.0`; the docs were regenerated with roxygen2 8.1.0. This reverts the `man/` churn introduced by regenerating with 7.3.3 and reflows the (semantically identical) `importFrom` entries in `NAMESPACE`.
* The `nested_cv_joint_model()` test now uses 120 observations, so it runs without 12 glmnet warnings, and also asserts that every IBS is finite and within [0, 1].

# TempleCBE 0.4.0001.2026.09.29.21.51

## Test Suite Hardening & Interface Verification

* **Cross-Validation Test Coverage**: Added comprehensive unit test coverage for `nested_cv_joint_model()` verifying outer split evaluation across `coxnet`, `status_calibrated`, and `time_regression` models.
* **Recipe Step Coverage**: Added unit tests for `step_lencode_joint_model()` verifying prep, bake, tidy, and required package methods.
* **Categorical Visualizations**: Added dedicated test for `cbe_balloon_plot()` verifying ggplot return class alongside sibling contingency plots.
* **Infix Helpers**: Added test coverage for `%plike%` (Perl-compatible regex match) and `%!in%` alias.
* **Normalization Robustness**: Added tests for `min_max_norm()` NA preservation.
* **Pruned Redundant Tests**: Removed duplicate `sd.error` test in `test-summary.R` (delegated to dedicated `test-sd_error.R`) and duplicate S3 autoplot test in `test-plot_features_percent_miss.R`.

# TempleCBE 0.3.800009.2026.09.29.10.46

## Vignette Build Fixes & Release Polish

* **Vignette Engine Directives (`sas_survival.Rmd`)**: Formatted `%\VignetteIndexEntry`, `%\VignetteEngine`, and `%\VignetteEncoding` onto separate lines in YAML metadata, allowing `tools::buildVignettes()` to correctly detect the knitr vignette engine.
* **Namespace Resolution in Vignettes (`R_to_SAS.Rmd`)**: Explicitly called `dplyr::lag()` and `dplyr::first()` in the `compress_tumor_tidy` chunk to prevent namespace collisions with `stats::lag()` under `conflicted`.
* **Vignette Title Synchronization (`compute_graph.Rmd`, `nested_survival_cv.Rmd`)**: Synchronized YAML `title:` fields with `\VignetteIndexEntry{}` values ("03. Visualizing Computational Pipeline Dependencies" and "02. Penalized Cox Models and Nested Cross-Validation for Start/Stop Survival Data") to eliminate R CMD build title mismatch warnings.
* **Build Artifact Cleanup (`.Rbuildignore`, `vignettes/.gitignore`)**: Ignored generated vignette artifacts (`*.rmarkdown`, `*.xpt`, `*.sas7bdat`, `*.docx`) to keep package builds clean.

# TempleCBE 0.3.80002.2026.09.28.20.36

## Bug Fixes & Documentation

* **`step_lencode_coxnet()` fits with Breslow ties**, as `coxnet()` does, instead of glmnet's default. Under glmnet 5.1, whose default Cox ties method is changing, the step otherwise warned on every factor it encoded.

* **`cbe_explain_survival()`'s risk scores for parsnip models and workflows have the right sign.** The default risk function used `predict(type = "linear_pred")`, which tidymodels' censored regression returns increasing with survival time (the opposite sign of the engine's own linear predictor), so survex's risk-based performance and importances for `model_fit` and `workflow` explainers were computed with a reversed sign. The risk is now the negative of that linear predictor. A regression test compares it with `survival::coxph()` for the `survival`, `glmnet`, and `coxnet` engines, alone and in a workflow.

* **`as_database()` assigns default `N_j` vertex names** when an `igraph` has no `name` attribute, preventing downstream errors in `database_to_igraph()` round-trips. Added `@aliases as_database cbe_database` for cross-reference discoverability.

* **Documentation cross-references**: Fixed three `\link{cbe_database}` references to nonexistent topics in `cbe_compare_df()`, `database_setops()`, and `database_to_igraph()` by using `\link[=as_database]{cbe_database}` instead.

* **`.Rbuildignore`**: Added `^vignettes/.*\\.pdf$` and `^.*\\.rds$` patterns to prevent accidental bundling of compiled vignette PDFs and binary data files.

# TempleCBE 0.3.80001.2026.09.28.18.00

## Survival modeling fixes: `coxnet()` and its tidymodels tie-ins

A review of the penalized Cox code (glmnet's Cox models for start/stop data, and the recipes, parsnip, and survex layers built on it) found and fixed the following. Each fix has a regression test, except the `nested_cv_coxnet()` message and the README corrections.

* **`coxnet()` honors small penalties.** glmnet's default penalty path stops early once the deviance ratio saturates, and `predict()`/`coef()` return the smallest-penalty solution for anything below it without a message. A `penalty` below the path (including `0`, the unpenalized fit) made `predict()`, `tidy()`, and the parsnip `coxnet` engine silently use the wrong coefficients, so a tuning grid reaching down to small penalties evaluated one model many times. `coxnet(penalty = )` now continues the path down to `penalty`; `predict()`/`tidy()` at a penalty below the fitted path warn; and a `penalty` below a user-supplied `path` warns at fit time. At `penalty = 0` a start/stop fit reproduces `survival::coxph()`.
* **`cbe_loco_mp_coxnet()` pairs subjects with the right outcomes and weights.** Censoring weights and outcomes were indexed by order of first appearance but computed in sorted-subject order, so importances depended on how the rows of `data` were ordered (silently wrong unless subject ids were already sorted). Subjects now follow one canonical order.
* **`cbe_loco_mp_coxnet()` no longer uses future covariates.** For start/stop data it predicted each out-of-bag subject's survival from their last row's covariates applied from time 0. It now uses the same Breslow baseline and covariate-path prediction as `cv_coxnet()`, with a new `covariates = c("path", "baseline")` argument (identical results for right-censored data). Minipatches also keep at least two predictors, which `coxnet()` needs (a small `m_ratio` used to fail every patch), and the documentation of the default `penalty` now describes what it does.
* **`cbe_loco_mp_coxnet()` is faster.** The per-feature, per-subject loops over minipatches are now matrix products, with identical output on right-censored data.
* **`nested_cv_coxnet(importance = "loco_mp")`** passes `covariates` through and warns when LOCO-MP fails on an outer split instead of returning an empty `.importance` silently.
* **`step_lencode_coxnet()` encodes two-level factors.** glmnet needs two or more columns, so a binary factor's fit always errored, and the error was swallowed into an all-zero encoding. It now pads the design matrix, treats missing values as the reference level instead of dropping rows, warns when a fit fails (as `step_lencode_joint_model()` does), and checks columns at `bake()`. Its `tunable()` method is now registered with `generics::tunable()` so `tune` finds it without TempleCBE attached, and the `outcome` documentation no longer points to a nonexistent `recipes::vars()`.
* **survex tie-ins.** `cbe_survex_loss_brier()` looked up the censoring distribution with `summary.survfit(times = )`, which sorts `times`, so IPCW weights were attached to the wrong subjects; it is now the Graf IPCW Brier score, tested against a subject-by-subject reference and invariant to row order. `cbe_explain_survival()` scores `coxph` models on the new data (its risk function used to score the training rows), and refuses start/stop `y`, which survex reads as start times.
* **`tidy_tmerge_cox()`** gains `censor_types`, the `event_type` values that mark censoring times, so those subjects get `event = 0` (by default every subject with an `event_time` still gets `event = 1` there), and warns about zero-length intervals (`tstop <= tstart`) that survival models can't use.
* Documentation: the README's `joint_model()` example used an invalid engine and a `type` argument `predict.joint_model()` doesn't take; it and the gap table named a nonexistent `step_lencode_survival()`. The README now shows start/stop and parsnip use of `coxnet`.
* Documentation and site: five exported topics (`as_database`, `database_setops`, `database_to_igraph`, `graph_setops`, `clean_publish`) were missing from the pkgdown reference index, which fails `pkgdown::build_site()`; they are now listed. `cbe_compare_df()`'s help now documents the `by` and `edge_by` arguments of its `cbe_database` method (a codoc mismatch), and three `\link{cbe_database}` cross-references to a nonexistent topic are plain text.
* Tests: `skip_if_no_coxnet_deps(cv = FALSE)` skips only on packages that fitting and predicting need, so the plain `coxnet()` and parsnip-engine tests no longer skip when rsample or yardstick is missing.

## tidymodels integration

* **`coxnet` engine for `parsnip::proportional_hazards()`**: `coxnet()` is now registered as an engine of `proportional_hazards()` in `"censored regression"` mode, so it works in `parsnip::fit()`, workflows, `tune::tune_grid()`, and workflowsets (`set_engine("coxnet")`, with `penalty` and `mixture` tunable). Unlike censored's `"glmnet"` engine it accepts counting-process `Surv(start, stop, event)` outcomes. Predictions: `"linear_pred"` (identical to censored's `"glmnet"` engine), `"survival"` (Breslow baseline; within about 1e-3 of censored's), and `"time"`. It is registered when parsnip loads, in either load order. Each tuning candidate is fit separately; case weights and `strata()` are not supported. New exports `coxnet_train()` and `predict_coxnet_*()` are the functions parsnip calls; use `coxnet()` directly outside parsnip.
* **`predict.coxnet_model(type = "time")`**: restricted mean survival time, the area under the predicted survival curve up to the last training time. It is what static yardstick metrics such as `concordance_survival()` use.
* `censored` is now listed in `Suggests`. The `joint_model_brief` chunk of the SAS survival vignette is skipped when `glmnet`, `rsample`, `yardstick`, or `probably` isn't installed instead of failing the build.
* The five figures in the SAS survival vignette now have captions.

## Clinical Database & Multi-Table Management Methods

* **Multi-Table Metadata & Database Attributes (`get_dataset_info`, `cbe_labels`)**:
  * Added S3 method `get_dataset_info.list()` to summarize multi-table clinical databases (e.g. `list(inputs, ABG, Cardiovascular, Injury, Pulmonary, Surface, survival_data)`), returning consolidated data dictionaries with `database_name`, `dataset_name`, and `dataset_label`.
  * Extended `labelled` variable-level conventions to dataset-level and database-level metadata via `cbe_dataset_label()`, `cbe_database_name()`, `cbe_database_label()`, and batch helpers `cbe_set_dataset_labels()` / `cbe_get_dataset_labels()`.
* **Excel & Database Multi-Sheet I/O (`write_database_to_excel`, `read_database_from_excel`, `write_workbook`)**:
  * New `write_database_to_excel()` (and alias `write_workbook()`, `cbe_write_database()`) writes multi-table databases into named worksheets with automated metadata sheets (`"METADATA"`).
  * New `read_database_from_excel()` (and alias `cbe_read_database()`) reads multi-sheet workbooks and attaches the metadata sheet as an attribute.
  * New `write_database_metadata()` and `read_database_metadata()` support in-place metadata review and modification across both `.csv` and `.xlsx` formats.
  * New `apply_database_metadata()` synchronizes modified variable labels, dataset descriptions, and variable roles back into database data frames in-place.
* **Clinical Variable Role Assignment (`cbe_variable_roles`, `cbe_set_roles`)**:
  * New `cbe_variable_roles()` allows defining functional roles (predictors `X_var`, outcomes `Y_var`, identifiers `ID_var`, visit times `Time_var`, `strata`, `weight`, `ignore`) across individual tables or entire multi-table databases.
  * Role extractors `cbe_get_predictors()`, `cbe_get_outcomes()`, `cbe_get_id_cols()`, and `cbe_get_time_cols()` streamline downstream recipe and modeling pipelines.
* **Database Relational Linkage Analysis (`cbe_database_relationships`, `cbe_find_shared_keys`, `cbe_check_key_integrity`)**:
  * New `cbe_find_shared_keys()` detects candidate linkage identifiers across tables.
  * New `cbe_database_relationships()` analyzes cross-table record overlap, coverage percentages, and cardinality (`1:1`, `1:Many`, `Many:1`, `Many:Many`).
  * New `cbe_check_key_integrity()` audits cohort attrition and identifies orphan records relative to a master cohort dataset.

# TempleCBE 0.3.403

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
  * Added project-level pipeline orchestration script `MakeComputeGraph.R` (in `vignettes/` and `inst/scripts/`) simulating a multi-stage dependency graph architecture across all four analytical stages.
  * Organized vignette sequence into `01`–`04` numbered stages in documentation and articles navigation:
    * `01. eda_and_missingness.Rmd`
    * `02. nested_survival_cv.Rmd`
    * `03. compute_graph.Rmd`
    * `04. sas_survival.Rmd`

# TempleCBE 0.3.2

## Integrated Standard Biostatistical and Presentation Components

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
