# Package index

## Data Quality & Missingness Analysis

Functions for missing data auditing, summary tables, and visualizations.

- [`SumNa()`](https://jkylearmstrong.github.io/TempleCBE/reference/SumNa.md)
  : Count Total Missing (NA) Values
- [`features_percent_miss()`](https://jkylearmstrong.github.io/TempleCBE/reference/features_percent_miss.md)
  : Calculate Percentage of Missing Data Per Feature
- [`plot(`*`<features_percent_miss>`*`)`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_features_percent_miss.md)
  [`plot_features_percent_miss()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_features_percent_miss.md)
  : Plot method for features_percent_miss objects
- [`missmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/missmap.md)
  : Missingness Map
- [`my_summary_table()`](https://jkylearmstrong.github.io/TempleCBE/reference/my_summary_table.md)
  : Summary Table Function
- [`get_dataset_info()`](https://jkylearmstrong.github.io/TempleCBE/reference/get_dataset_info.md)
  [`proc_contents()`](https://jkylearmstrong.github.io/TempleCBE/reference/get_dataset_info.md)
  : Summarize a Data Frame's Columns

## Normalization & Outlier Detection

Functions for scaling numeric features and flagging statistical
outliers.

- [`min_max_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/min_max_norm.md)
  : Min-Max Data Normalization
- [`z_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/z_norm.md)
  : Z-Score Standard Normalization
- [`range_norm()`](https://jkylearmstrong.github.io/TempleCBE/reference/range_norm.md)
  : Range Normalization
- [`detect_outliers()`](https://jkylearmstrong.github.io/TempleCBE/reference/detect_outliers.md)
  : Detect Outliers Across a Data Frame's Numeric Columns
- [`calculate_fences()`](https://jkylearmstrong.github.io/TempleCBE/reference/calculate_fences.md)
  : Calculate Inner and Outer IQR Fences
- [`flag_outliers()`](https://jkylearmstrong.github.io/TempleCBE/reference/flag_outliers.md)
  : Flag and Classify Outliers

## Tidymodels Recipe Steps & Modeling Utilities

Recipe steps and modeling evaluation metrics.

- [`step_famd()`](https://jkylearmstrong.github.io/TempleCBE/reference/step_famd.md)
  : Factor Analysis of Mixed Data (FAMD) Recipe Step
- [`glmnet_IBS()`](https://jkylearmstrong.github.io/TempleCBE/reference/glmnet_IBS.md)
  : Integrated Brier Score (IBS) Evaluation for Regularized Survival
  Models

## Statistical Testing & EDA

Biostatistical test wrappers, correlation matrices, and distribution
tests.

- [`single_t_test()`](https://jkylearmstrong.github.io/TempleCBE/reference/single_t_test.md)
  : Single T-Test, Tidied
- [`multiple_t_test()`](https://jkylearmstrong.github.io/TempleCBE/reference/multiple_t_test.md)
  : Multiple T-Tests Against One Classifier
- [`one_vs_rest_t_test()`](https://jkylearmstrong.github.io/TempleCBE/reference/one_vs_rest_t_test.md)
  : One-vs-Rest T-Tests Across a Multi-Level Factor
- [`corr_test_all()`](https://jkylearmstrong.github.io/TempleCBE/reference/corr_test_all.md)
  : Pairwise Correlation Matrix and Significance Testing
- [`correlation_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot.md)
  : Correlation Plot
- [`find_correlation()`](https://jkylearmstrong.github.io/TempleCBE/reference/find_correlation.md)
  : Find Highly Correlated Columns
- [`distribution_test()`](https://jkylearmstrong.github.io/TempleCBE/reference/distribution_test.md)
  : Check a Vector or Data Frame's Distribution
- [`distribution_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/distribution_plot.md)
  : Distribution Plot
- [`is_normal()`](https://jkylearmstrong.github.io/TempleCBE/reference/is_normal.md)
  : Test Whether a Vector Looks Normally Distributed
- [`is_poisson()`](https://jkylearmstrong.github.io/TempleCBE/reference/is_poisson.md)
  : Test Whether a Vector Looks Poisson-Distributed
- [`is.int()`](https://jkylearmstrong.github.io/TempleCBE/reference/is.int.md)
  : Is a Vector Composed of Integer-Valued Numbers
- [`significance_stars()`](https://jkylearmstrong.github.io/TempleCBE/reference/significance_stars.md)
  : P-value Significance Stars

## PCA & Dimensionality Reduction

Principal Component Analysis utilities, loadings, and biplots.

- [`proc_pca()`](https://jkylearmstrong.github.io/TempleCBE/reference/proc_pca.md)
  : Process and Plot Principal Component Analysis (PCA)

- [`rotation_matrix()`](https://jkylearmstrong.github.io/TempleCBE/reference/rotation_matrix.md)
  [`pca_loadings()`](https://jkylearmstrong.github.io/TempleCBE/reference/rotation_matrix.md)
  : PCA Rotation Matrix (Loadings)

- [`pca_eqns()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_eqns.md)
  : PCA Equations

- [`pca_percent_var_explained()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_percent_var_explained.md)
  : Percent Variance Explained by Each Principal Component

- [`pca_feature_loading_heatmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_feature_loading_heatmap.md)
  : PCA Feature-Loading Heatmap

- [`plot_pca_bi()`](https://jkylearmstrong.github.io/TempleCBE/reference/plot_pca_bi.md)
  : PCA Biplot

- [`plot(`*`<prcomp>`*`)`](https://jkylearmstrong.github.io/TempleCBE/reference/plot.prcomp.md)
  :

  Generic Plot Method for `prcomp` Objects

## Visualization & Reporting

Manhattan/volcano plots, report generation, and document conversions.

- [`manhattan_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/manhattan_plot.md)
  : Manhattan Plot
- [`volcano_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/volcano_plot.md)
  : Volcano Plot
- [`create_report()`](https://jkylearmstrong.github.io/TempleCBE/reference/create_report.md)
  : Scaffold a New Report From a Template
- [`zip_render()`](https://jkylearmstrong.github.io/TempleCBE/reference/zip_render.md)
  : Render a Quarto Document and Zip It With Its Dependencies
- [`pdf_to_rtf()`](https://jkylearmstrong.github.io/TempleCBE/reference/pdf_to_rtf.md)
  : Convert a PDF to Rich Text Format (RTF)
- [`create_toc_from_sas_pdf()`](https://jkylearmstrong.github.io/TempleCBE/reference/create_toc_from_sas_pdf.md)
  : Build a Table of Contents from a SAS-Generated PDF
- [`read_excel_multiple_headers()`](https://jkylearmstrong.github.io/TempleCBE/reference/read_excel_multiple_headers.md)
  : Read Excel Data With Multi-Row Column Headers
- [`read_workbook()`](https://jkylearmstrong.github.io/TempleCBE/reference/read_workbook.md)
  : Read Every Sheet of an Excel Workbook

## Helper & Infix Operators

Clean column naming, string matching, and vector manipulation.

- [`clean_names()`](https://jkylearmstrong.github.io/TempleCBE/reference/clean_names.md)
  : Clean and Standardize Variable Names
- [`R_names()`](https://jkylearmstrong.github.io/TempleCBE/reference/R_names.md)
  : Clean Column Names, Preserving Originals as Labels
- [`make_excel_names()`](https://jkylearmstrong.github.io/TempleCBE/reference/make_excel_names.md)
  : Generate Excel-Compatible Column Names
- [`keep_only()`](https://jkylearmstrong.github.io/TempleCBE/reference/keep_only.md)
  : Keep Only Specified Objects in an Environment
- [`delete_nul_files()`](https://jkylearmstrong.github.io/TempleCBE/reference/delete_nul_files.md)
  : Delete Stray 'nul' Files
- [`find_code()`](https://jkylearmstrong.github.io/TempleCBE/reference/find_code.md)
  : Search for Code Patterns Across a Directory Tree
- [`sd.error()`](https://jkylearmstrong.github.io/TempleCBE/reference/sd.error.md)
  : Compute standard error
- [`install.packages.no_lock()`](https://jkylearmstrong.github.io/TempleCBE/reference/install.packages.no_lock.md)
  : Install a Package, Bypassing an Existing Lock
- [`like()`](https://jkylearmstrong.github.io/TempleCBE/reference/infix_helpers.md)
  [`` `%like%` ``](https://jkylearmstrong.github.io/TempleCBE/reference/infix_helpers.md)
  [`ilike()`](https://jkylearmstrong.github.io/TempleCBE/reference/infix_helpers.md)
  [`` `%ilike%` ``](https://jkylearmstrong.github.io/TempleCBE/reference/infix_helpers.md)
  [`flike()`](https://jkylearmstrong.github.io/TempleCBE/reference/infix_helpers.md)
  [`` `%flike%` ``](https://jkylearmstrong.github.io/TempleCBE/reference/infix_helpers.md)
  [`plike()`](https://jkylearmstrong.github.io/TempleCBE/reference/infix_helpers.md)
  [`` `%plike%` ``](https://jkylearmstrong.github.io/TempleCBE/reference/infix_helpers.md)
  [`notin()`](https://jkylearmstrong.github.io/TempleCBE/reference/infix_helpers.md)
  [`` `%!in%` ``](https://jkylearmstrong.github.io/TempleCBE/reference/infix_helpers.md)
  [`` `%notin%` ``](https://jkylearmstrong.github.io/TempleCBE/reference/infix_helpers.md)
  : Pattern Matching and Logical-Negation Infix Operators
