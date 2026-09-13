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
  : Integrated Brier Score of a Penalized Cox Model on Start/Stop
  Survival Data
- [`tune_over_alpha()`](https://jkylearmstrong.github.io/TempleCBE/reference/tune_over_alpha.md)
  : Tune a Penalized Cox Model Over a Grid of \`alpha\` Values
- [`summarize_tune_results()`](https://jkylearmstrong.github.io/TempleCBE/reference/summarize_tune_results.md)
  : Tune Over \`alpha\` for Every Split of a Resample
- [`get_model_parameters()`](https://jkylearmstrong.github.io/TempleCBE/reference/get_model_parameters.md)
  : Tuning Parameters of a Ranked Workflow in a Workflow Set
- [`fit_n_rank()`](https://jkylearmstrong.github.io/TempleCBE/reference/fit_n_rank.md)
  : Fit the Configuration Ranked \`.rank\` in a Workflow Set

## Missing Data Imputation

Per-column mtry sweeps for missForest and missRanger imputation.

- [`missforest_sweep_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missforest_sweep_mtry.md)
  : Impute a Data Frame by Sweeping \`missForest\` Over \`mtry\`
- [`missforest_oob_by_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missforest_oob_by_mtry.md)
  : Run \`missForest\` at a Single \`mtry\` and Report Variablewise OOB
  Error
- [`missforest_impute_by_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missforest_impute_by_mtry.md)
  : Assemble Imputed Columns From Their Best-\`mtry\` Runs
- [`missranger_sweep_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missranger_sweep_mtry.md)
  : Impute a Data Frame by Sweeping \`missRanger\` Over \`mtry\`
- [`missranger_oob_by_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missranger_oob_by_mtry.md)
  : Run \`missRanger\` at a Single \`mtry\` and Report Per-Column OOB
  Error
- [`missranger_max_mtry()`](https://jkylearmstrong.github.io/TempleCBE/reference/missranger_max_mtry.md)
  : Largest \`mtry\` \`missRanger\` Will Accept For a Data Set

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
  : Pairwise Correlation Tests Across All Numeric Columns
- [`correlation_plot()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot.md)
  : Correlation Plot
- [`correlation_plot_split()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_plot_split.md)
  : Correlation Plot, Split Into Legible Sub-Plots
- [`correlation_diff()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_diff.md)
  : Difference in Correlation Matrices Between Two Datasets
- [`correlation_diff_heatmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/correlation_diff_heatmap.md)
  : Heatmap of Correlation Differences Between Two Datasets
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

- [`pca_biplot()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_biplot.md)
  : PCA Loadings Biplot

- [`pca_loading_diff()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_loading_diff.md)
  : Difference in PCA Loadings Between Two Fits

- [`pca_loading_diff_heatmap()`](https://jkylearmstrong.github.io/TempleCBE/reference/pca_loading_diff_heatmap.md)
  : Heatmap of PCA Loading Differences Between Two Fits

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
- [`reexports`](https://jkylearmstrong.github.io/TempleCBE/reference/reexports.md)
  [`write_xlsx`](https://jkylearmstrong.github.io/TempleCBE/reference/reexports.md)
  : Objects exported from other packages
- [`km_summary_to_prism()`](https://jkylearmstrong.github.io/TempleCBE/reference/km_summary_to_prism.md)
  : Convert a Kaplan-Meier Summary Table to a GraphPad Prism Survival
  Table
- [`convert_pdf_to_docx()`](https://jkylearmstrong.github.io/TempleCBE/reference/convert_pdf_to_docx.md)
  : Convert a Single PDF to DOCX Using the Best Available Backend
- [`convert_pdfs_to_docx()`](https://jkylearmstrong.github.io/TempleCBE/reference/convert_pdfs_to_docx.md)
  : Convert PDFs to DOCX Using the Best Available Backend
- [`check_docx_toolchain()`](https://jkylearmstrong.github.io/TempleCBE/reference/check_docx_toolchain.md)
  : Report Which PDF -\> DOCX Backends Are Usable on This Machine
- [`find_python()`](https://jkylearmstrong.github.io/TempleCBE/reference/find_python.md)
  : Locate a Python Interpreter That Can Import pdf2docx
- [`find_soffice()`](https://jkylearmstrong.github.io/TempleCBE/reference/find_soffice.md)
  : Locate a LibreOffice Headless Binary
- [`run_sas_script()`](https://jkylearmstrong.github.io/TempleCBE/reference/run_sas_script.md)
  : Run a SAS Program in Batch Mode
- [`find_sas()`](https://jkylearmstrong.github.io/TempleCBE/reference/find_sas.md)
  : Locate a SAS Executable
- [`render_me()`](https://jkylearmstrong.github.io/TempleCBE/reference/render_me.md)
  : Render Quarto Documents to Multiple Formats With Timing
- [`zip_reports()`](https://jkylearmstrong.github.io/TempleCBE/reference/zip_reports.md)
  : Package Multiple Already-Rendered Reports Into an Indexed Zip
- [`scan_data_io()`](https://jkylearmstrong.github.io/TempleCBE/reference/scan_data_io.md)
  : Audit Data File Read/Write Calls Against a Project's Files on Disk
- [`read_search()`](https://jkylearmstrong.github.io/TempleCBE/reference/read_search.md)
  : Search a Directory Tree for File-Read Calls
- [`write_search()`](https://jkylearmstrong.github.io/TempleCBE/reference/write_search.md)
  : Search a Directory Tree for File-Write Calls

## Temple Brand

Temple University colors, ggplot2 scales and theme, and setup for the
quarto_temple_brand Quarto extension.

- [`temple_colors()`](https://jkylearmstrong.github.io/TempleCBE/reference/temple_colors.md)
  : Temple University Brand Colors
- [`temple_pal()`](https://jkylearmstrong.github.io/TempleCBE/reference/temple_pal.md)
  : Temple Color Palettes
- [`scale_colour_temple()`](https://jkylearmstrong.github.io/TempleCBE/reference/scale_colour_temple.md)
  [`scale_color_temple()`](https://jkylearmstrong.github.io/TempleCBE/reference/scale_colour_temple.md)
  [`scale_fill_temple()`](https://jkylearmstrong.github.io/TempleCBE/reference/scale_colour_temple.md)
  : Temple Color and Fill Scales for ggplot2
- [`theme_temple()`](https://jkylearmstrong.github.io/TempleCBE/reference/theme_temple.md)
  : Temple ggplot2 Theme
- [`temple_brand_path()`](https://jkylearmstrong.github.io/TempleCBE/reference/temple_brand_path.md)
  : Path to the Bundled Temple brand.yml
- [`use_temple_brand()`](https://jkylearmstrong.github.io/TempleCBE/reference/use_temple_brand.md)
  : Install the Temple Brand Quarto Extension Into a Project

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
- [`profvis_summary()`](https://jkylearmstrong.github.io/TempleCBE/reference/profvis_summary.md)
  : Summarize a \`profvis\` Profile
- [`normalize_safely()`](https://jkylearmstrong.github.io/TempleCBE/reference/normalize_safely.md)
  : Normalize File Paths Without Failing
- [`parse_here_call_vec()`](https://jkylearmstrong.github.io/TempleCBE/reference/parse_here_call_vec.md)
  : Resolve \`here::here()\` Calls Found in Code Text
- [`file_meta_fs()`](https://jkylearmstrong.github.io/TempleCBE/reference/file_meta_fs.md)
  : File Metadata as a Tibble
- [`extract_win_posix_paths()`](https://jkylearmstrong.github.io/TempleCBE/reference/extract_win_posix_paths.md)
  : Extract Full \`.xlsx\` Paths From Text
- [`extract_all_xlsx_tokens()`](https://jkylearmstrong.github.io/TempleCBE/reference/extract_all_xlsx_tokens.md)
  : Extract Every \`.xlsx\` File Name From Text
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
