#' Find and Update Deprecated Calls Across Project Code
#'
#' Scans source files (`.R`, `.Rmd`, `.qmd`) in a project directory or script
#' for calls to deprecated TempleCBE functions and replaces them with their
#' canonical names.
#'
#' @description
#' `find_deprecated_calls()` inspects code files and reports all occurrences of
#' deprecated TempleCBE functions scheduled for removal in TempleCBE 0.6.0.
#'
#' `update_deprecated_calls()` previews or applies in-place renames to update
#' both package-qualified calls (`TempleCBE::old()`, `TempleCBE:::old()`) and
#' unqualified calls (`old()`).
#'
#' @param path Path to a source file or directory of project scripts.
#' @param dry_run Logical; if `TRUE` (default), only previews the proposed
#'   renames without modifying files. If `FALSE`, rewrites files in place.
#' @return A `tibble` with columns:
#'   \item{file}{Path to the file where the deprecated call was detected.}
#'   \item{line}{1-based line number.}
#'   \item{old_name}{Name of the deprecated function.}
#'   \item{replacement}{Canonical replacement function name.}
#'   \item{call_type}{Whether the call was `"qualified"` or `"unqualified"`.}
#'   \item{context}{The original source code line.}
#' @export
#' @examples
#' \donttest{
#' tmp <- tempfile(fileext = ".R")
#' writeLines(c(
#'   "x <- TempleCBE::sd.error(c(1, 2, 3))",
#'   "y <- keep_only(c('a', 'b'))"
#' ), tmp)
#' find_deprecated_calls(tmp)
#' update_deprecated_calls(tmp, dry_run = TRUE)
#' update_deprecated_calls(tmp, dry_run = FALSE)
#' readLines(tmp)
#' unlink(tmp)
#' }
find_deprecated_calls <- function(path) {
  if (!file.exists(path)) {
    stop("`path` '", path, "' does not exist.", call. = FALSE)
  }

  files <- if (dir.exists(path)) {
    all_f <- list.files(
      path,
      pattern = "\\.[RrQqMm][a-zA-Z0-9]*$",
      recursive = TRUE,
      full.names = TRUE
    )
    # Exclude version control and build folders
    all_f[!grepl("[/\\\\](\\.(git|Rproj\\.user|system_generated)|renv)[/\\\\]", all_f)]
  } else {
    path
  }

  dep_map <- templecbe_deprecated_map()
  dep_names <- names(dep_map)

  # Sort old names by length descending so longer matching prefixes take precedence
  dep_names <- dep_names[order(nchar(dep_names), decreasing = TRUE)]

  results <- list()

  for (f in files) {
    lines <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
    if (length(lines) == 0L) next

    for (line_idx in seq_along(lines)) {
      ln <- lines[line_idx]
      trimmed <- trimws(ln)
      if (grepl("^#", trimmed)) next # Skip comment lines

      for (old_fn in dep_names) {
        new_fn <- dep_map[[old_fn]]
        escaped_old <- gsub("\\.", "\\\\.", old_fn)

        # Regex for qualified call: TempleCBE::old(...) or TempleCBE:::old(...)
        qual_pattern <- paste0("(?<![a-zA-Z0-9_.])(TempleCBE:::?)(", escaped_old, ")(?![a-zA-Z0-9_])\\s*\\(")
        # Regex for unqualified call: old(...)
        unqual_pattern <- paste0("(?<![a-zA-Z0-9_.])(?<!TempleCBE::)(?<!TempleCBE:::)(", escaped_old, ")(?![a-zA-Z0-9_])\\s*\\(")

        has_qual <- grepl(qual_pattern, ln, perl = TRUE)
        has_unqual <- grepl(unqual_pattern, ln, perl = TRUE)

        if (has_qual || has_unqual) {
          ctype <- if (has_qual && has_unqual) "both" else if (has_qual) "qualified" else "unqualified"
          results[[length(results) + 1L]] <- tibble::tibble(
            file = f,
            line = line_idx,
            old_name = old_fn,
            replacement = new_fn,
            call_type = ctype,
            context = trimmed
          )
        }
      }
    }
  }

  if (length(results) == 0L) {
    return(tibble::tibble(
      file = character(),
      line = integer(),
      old_name = character(),
      replacement = character(),
      call_type = character(),
      context = character()
    ))
  }

  out <- dplyr::bind_rows(results)
  dplyr::distinct(out, .data$file, .data$line, .data$old_name, .keep_all = TRUE)
}

#' @rdname find_deprecated_calls
#' @export
update_deprecated_calls <- function(path, dry_run = TRUE) {
  findings <- find_deprecated_calls(path)

  if (nrow(findings) == 0L) {
    message("No deprecated TempleCBE calls found in '", path, "'.")
    return(invisible(findings))
  }

  n_calls <- nrow(findings)
  n_files <- length(unique(findings$file))

  if (isTRUE(dry_run)) {
    message(sprintf(
      "Dry run: Found %d deprecated call(s) across %d file(s). Set dry_run = FALSE to apply replacements.",
      n_calls, n_files
    ))
    return(findings)
  }

  dep_map <- templecbe_deprecated_map()
  dep_names <- names(dep_map)
  dep_names <- dep_names[order(nchar(dep_names), decreasing = TRUE)]

  by_file <- split(findings, findings$file)

  for (fpath in names(by_file)) {
    lines <- readLines(fpath, warn = FALSE, encoding = "UTF-8")
    for (old_fn in dep_names) {
      if (!old_fn %in% by_file[[fpath]]$old_name) next
      new_fn <- dep_map[[old_fn]]
      escaped_old <- gsub("\\.", "\\\\.", old_fn)

      # 1. Qualified calls: TempleCBE:::old( or TempleCBE::old(
      lines <- gsub(
        paste0("(?<![a-zA-Z0-9_.])TempleCBE:::", escaped_old, "(?![a-zA-Z0-9_])(\\s*\\()"),
        paste0("TempleCBE:::", new_fn, "\\1"),
        lines,
        perl = TRUE
      )
      lines <- gsub(
        paste0("(?<![a-zA-Z0-9_.])TempleCBE::", escaped_old, "(?![a-zA-Z0-9_])(\\s*\\()"),
        paste0("TempleCBE::", new_fn, "\\1"),
        lines,
        perl = TRUE
      )

      # 2. Unqualified calls: old(
      lines <- gsub(
        paste0("(?<![a-zA-Z0-9_.:])", escaped_old, "(?![a-zA-Z0-9_])(\\s*\\()"),
        paste0(new_fn, "\\1"),
        lines,
        perl = TRUE
      )
    }
    writeLines(lines, fpath, useBytes = FALSE)
  }

  message(sprintf("Updated %d deprecated call(s) across %d file(s).", n_calls, n_files))
  invisible(findings)
}

templecbe_deprecated_map <- function() {
  c(
    "sd.error" = "std_error",
    "summarize_tune_results" = "tune_alpha_by_split",
    "write_xlsx" = "write_xlsx_file",
    "proc_contents" = "describe_columns",
    "normalize_safely" = "normalize_path_safely",
    "keep_only" = "rm_all_except",
    "fit_n_rank" = "workflow_set_rank_fit",
    "read_search" = "find_read_calls",
    "write_search" = "find_write_calls",
    "get_model_parameters" = "workflow_set_params",
    "R_names" = "clean_names_keep_labels",
    "render_me" = "render_pipeline",
    "SumNa" = "count_na",
    "cbe_exact2x2_ci" = "exact2x2_ci",
    "cbe_exact2x2" = "exact2x2",
    "cbe_compare_df" = "compare_df",
    "cbe_database_relationships" = "database_relationships",
    "cbe_find_shared_keys" = "find_shared_keys",
    "cbe_check_key_integrity" = "check_key_integrity",
    "cbe_test_categorical" = "test_categorical",
    "proc_pca" = "run_pca",
    "cbe_cox_check" = "cox_check",
    "cbe_cox_multi" = "cox_multi",
    "cbe_cox_single" = "cox_single",
    "cbe_km_single" = "km_single",
    "cbe_factor_reference" = "relevel_factor",
    "factor_reference" = "relevel_factor",
    "cbe_docx_review_extract" = "docx_review_extract",
    "cbe_review_tracker" = "review_tracker",
    "cbe_variable_roles" = "variable_roles",
    "cbe_set_roles" = "set_roles",
    "cbe_get_roles" = "get_roles",
    "cbe_get_predictors" = "get_predictors",
    "cbe_get_outcomes" = "get_outcomes",
    "cbe_get_id_cols" = "get_id_cols",
    "cbe_get_time_cols" = "get_time_cols",
    "clean_names" = "clean_names_keep_labels",
    "controls_datasets" = "produces_data",
    "find_correlation" = "drop_correlated_columns",
    "install.packages.no_lock" = "install_package_no_lock",
    "is.int" = "is_integer_valued",
    "cbe_explain_survival" = "explain_survival",
    "cbe_survex_loss_ibs" = "survex_loss_ibs",
    "cbe_survex_loss_brier" = "survex_loss_brier",
    "cbe_explain" = "explain_joint",
    "cbe_explain_status" = "explain_status",
    "cbe_explain_time" = "explain_time",
    "cbe_explain_stack" = "explain_stack",
    "cbe_predict_parts_shap" = "predict_parts_shap",
    "cbe_dataset_label" = "dataset_label",
    "cbe_database_name" = "database_name",
    "cbe_database_label" = "database_label",
    "cbe_set_dataset_labels" = "set_dataset_labels",
    "cbe_get_dataset_labels" = "get_dataset_labels",
    "like" = "match_like",
    "cbe_loco_mp_coxnet" = "loco_mp_coxnet",
    "my_summary_table" = "summary_table",
    "cbe_nested_survival_run" = "nested_survival_run",
    "cbe_domain_children" = "domain_children",
    "extract_win_posix_paths" = "extract_xlsx_paths",
    "cbe_pairwise_combos" = "pairwise_combos",
    "cbe_control_race" = "control_race_survival",
    "cbe_tune_race_survival" = "tune_race_survival",
    "render" = "render_pipeline",
    "cbe_sas_macro_dir" = "sas_macro_dir",
    "cbe_sas_macro_path" = "sas_macro_path",
    "cbe_write_database" = "write_database_to_excel",
    "cbe_read_database" = "read_database_from_excel"
  )
}
