# The deprecated wrappers announced for removal in 0.7.0 (72 names: 71 exports and the internal
# %notin% shim). Their replacements are in R/deprecated_tools.R, which find_deprecated_calls()
# and update_deprecated_calls() still use.
removed_in_0_7_0 <- c(
    "%like%",
    "%notin%",
    "cbe_check_key_integrity",
    "cbe_compare_df",
    "cbe_control_race",
    "cbe_cox_check",
    "cbe_cox_multi",
    "cbe_cox_single",
    "cbe_database_label",
    "cbe_database_label<-",
    "cbe_database_name",
    "cbe_database_name<-",
    "cbe_database_relationships",
    "cbe_dataset_label",
    "cbe_dataset_label<-",
    "cbe_docx_review_extract",
    "cbe_domain_children",
    "cbe_exact2x2",
    "cbe_explain",
    "cbe_explain_stack",
    "cbe_explain_status",
    "cbe_explain_survival",
    "cbe_explain_time",
    "cbe_factor_reference",
    "cbe_find_shared_keys",
    "cbe_get_dataset_labels",
    "cbe_get_id_cols",
    "cbe_get_outcomes",
    "cbe_get_predictors",
    "cbe_get_roles",
    "cbe_get_time_cols",
    "cbe_km_single",
    "cbe_loco_mp_coxnet",
    "cbe_nested_survival_run",
    "cbe_pairwise_combos",
    "cbe_predict_parts_shap",
    "cbe_read_database",
    "cbe_review_tracker",
    "cbe_sas_macro_dir",
    "cbe_sas_macro_path",
    "cbe_set_dataset_labels",
    "cbe_set_roles",
    "cbe_survex_loss_brier",
    "cbe_survex_loss_ibs",
    "cbe_test_categorical",
    "cbe_tune_race_survival",
    "cbe_variable_roles",
    "cbe_write_database",
    "clean_names",
    "controls_datasets",
    "extract_win_posix_paths",
    "factor_reference",
    "find_correlation",
    "fit_n_rank",
    "get_model_parameters",
    "install.packages.no_lock",
    "is.int",
    "keep_only",
    "like",
    "my_summary_table",
    "normalize_safely",
    "proc_contents",
    "proc_pca",
    "R_names",
    "read_search",
    "render",
    "render_me",
    "sd.error",
    "summarize_tune_results",
    "SumNa",
    "write_search",
    "write_xlsx"
)

test_that("the deprecated wrappers are neither exported nor defined", {
  expect_length(removed_in_0_7_0, 72L)
  ns <- asNamespace("TempleCBE")
  expect_identical(intersect(removed_in_0_7_0, getNamespaceExports("TempleCBE")), character())
  defined <- vapply(removed_in_0_7_0, exists, logical(1), envir = ns, inherits = FALSE)
  expect_identical(names(defined)[defined], character())
})

test_that("every replacement named in the migration table exists", {
  map <- TempleCBE:::templecbe_deprecated_map()
  ns <- asNamespace("TempleCBE")
  missing <- unname(map)[!vapply(unname(map), exists, logical(1), envir = ns, inherits = FALSE)]
  expect_identical(missing, character())
  # none of the old names in the table is a function any more
  still <- names(map)[vapply(names(map), exists, logical(1), envir = ns, inherits = FALSE)]
  expect_identical(still, character())
})

test_that("find_deprecated_calls() and update_deprecated_calls() still find and rewrite the removed names", {
  td <- withr::local_tempdir()
  script <- file.path(td, "analysis.R")
  writeLines(c(
    "n <- SumNa(df)",
    "s <- TempleCBE::sd.error(x)",
    "TempleCBE::write_xlsx(df, 'out.xlsx')",
    "janitor::clean_names(df)"
  ), script)

  found <- find_deprecated_calls(script)
  expect_identical(found$old_name, c("SumNa", "sd.error", "write_xlsx"))
  expect_identical(found$replacement, c("count_na", "std_error", "write_xlsx_file"))

  suppressMessages(update_deprecated_calls(script, dry_run = FALSE))
  expect_identical(
    readLines(script),
    c("n <- count_na(df)", "s <- TempleCBE::std_error(x)", "TempleCBE::write_xlsx_file(df, 'out.xlsx')", "janitor::clean_names(df)")
  )
  expect_identical(nrow(find_deprecated_calls(script)), 0L)
})
