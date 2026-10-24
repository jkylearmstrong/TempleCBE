#' Find and Update Deprecated Calls Across Project Code
#'
#' Scans source files (`.R`, `.Rmd`, `.qmd`) in a project directory or script
#' for calls to deprecated TempleCBE functions and replaces them with their
#' canonical names.
#'
#' @description
#' `find_deprecated_calls()` inspects code files and reports all occurrences of
#' deprecated TempleCBE functions removed in TempleCBE 0.7.0 (this tool finds them in your own code).
#'
#' `update_deprecated_calls()` previews or applies in-place renames to update
#' both package-qualified calls (`TempleCBE::old()`, `TempleCBE:::old()`) and
#' unqualified calls (`old()`).
#' Only code is read: string literals and comments are ignored, and in `.Rmd`/`.qmd` files only
#' the R chunks. Method calls (`obj$render()`) are ignored.
#'
#' @param path Path to a source file or directory of project scripts.
#' @param exclude_dirs Optional character vector of directory names (for example
#'   `c("ARCHIVE", "renv", "doc")`) to skip when `path` is a directory; a file is
#'   skipped if any folder on its path below `path` has one of these names.
#' @param dry_run Logical; if `TRUE` (default), only previews the proposed
#'   renames without modifying files. If `FALSE`, rewrites files in place.
#' @return A `tibble` with columns:
#'   \item{file}{Path to the file where the deprecated call was detected.}
#'   \item{line}{1-based line number.}
#'   \item{old_name}{Name of the deprecated function.}
#'   \item{replacement}{Canonical replacement function name.}
#'   \item{call_type}{`"qualified"` (`TempleCBE::old()`), `"unqualified"` (`old()`), or
#'     `"ambiguous"`: an unqualified `clean_names()`, `render()`, `like()` or `write_xlsx()` in a
#'     file that uses TempleCBE. Other packages export these names too, so
#'     `update_deprecated_calls()` never rewrites them.}
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
find_deprecated_calls <- function(path, exclude_dirs = NULL) {
  out <- .deprecated_matches(path, exclude_dirs)
  out[, c("file", "line", "old_name", "replacement", "call_type", "context")]
}

# `lines` with the text of string literals and comments blanked out (same length, so positions hold)
.mask_code <- function(lines) {
  vapply(lines, function(x) {
    ch <- strsplit(x, "", fixed = TRUE)[[1]]
    n <- length(ch)
    if (n == 0L) return("")
    q <- ""
    i <- 1L
    while (i <= n) {
      c1 <- ch[i]
      if (nzchar(q)) {
        if (c1 == "\\" && i < n) {
          ch[i] <- " "
          ch[i + 1L] <- " "
          i <- i + 2L
          next
        }
        if (c1 == q) q <- "" else ch[i] <- " "
      } else if (c1 == "#") {
        ch[i:n] <- " "
        break
      } else if (c1 == "\"" || c1 == "'") {
        q <- c1
      }
      i <- i + 1L
    }
    paste(ch, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

# TRUE for the lines inside an R code chunk of an Rmd/qmd file (the fences themselves are not code)
.in_r_chunk <- function(lines) {
  open <- grepl("^\\s*`{3,}\\s*\\{r[ ,}]", lines, perl = TRUE)
  close <- grepl("^\\s*`{3,}\\s*$", lines, perl = TRUE)
  inside <- logical(length(lines))
  state <- FALSE
  for (i in seq_along(lines)) {
    if (!state && open[i]) {
      state <- TRUE
    } else if (state && close[i]) {
      state <- FALSE
    } else {
      inside[i] <- state
    }
  }
  inside
}

.deprecated_matches <- function(path, exclude_dirs = NULL) {
  if (!file.exists(path)) {
    stop("`path` '", path, "' does not exist.", call. = FALSE)
  }

  files <- if (dir.exists(path)) {
    all_f <- list.files(
      path,
      pattern = "\\.(r|rmd|qmd)$",
      ignore.case = TRUE,
      recursive = TRUE,
      full.names = TRUE
    )
    # Exclude version control, build folders, renv
    all_f <- all_f[!grepl("[/\\\\](\\.(git|Rproj\\.user|system_generated)|renv)[/\\\\]", all_f)]
    if (length(exclude_dirs) && length(all_f)) {
      rel <- substring(all_f, nchar(path) + 1L)
      drop <- vapply(strsplit(rel, "[/\\\\]"), function(p) {
        any(utils::head(p, -1L) %in% exclude_dirs)
      }, logical(1))
      all_f <- all_f[!drop]
    }
    all_f
  } else {
    path
  }

  dep_map <- templecbe_deprecated_map()
  dep_names <- names(dep_map)
  dep_names <- dep_names[order(nchar(dep_names), decreasing = TRUE)]
  dep_pattern <- paste(gsub("\\.", "\\\\.", dep_names), collapse = "|")

  call_pattern <- paste0(
    "(?<![a-zA-Z0-9_.$@])",
    "(?:([a-zA-Z0-9_.]+):::?)?",
    "(", dep_pattern, ")",
    "(?![a-zA-Z0-9_])\\s*\\("
  )

  empty <- tibble::tibble(
    file = character(), line = integer(), old_name = character(), replacement = character(),
    call_type = character(), context = character(), name_start = integer(), name_len = integer()
  )
  results <- list()

  for (f in files) {
    if (grepl("\\.(rds|rda|rdata|rtf|pdf|docx|xlsx|png|jpe?g|gif|exe|dll|so|dylib)$", f, ignore.case = TRUE)) {
      message("Skipping binary or non-script file: '", f, "'")
      next
    }

    lines <- tryCatch({
      l <- readLines(f, warn = FALSE, encoding = "UTF-8")
      if (any(!validUTF8(l))) {
        l_conv <- iconv(l, from = "latin1", to = "UTF-8", sub = "byte")
        if (any(is.na(l_conv))) {
          message("Skipping file with invalid UTF-8 characters: '", f, "'")
          character()
        } else {
          l_conv
        }
      } else {
        l
      }
    }, error = function(e) {
      message("Skipping unreadable file: '", f, "' (", conditionMessage(e), ")")
      character()
    })

    if (length(lines) == 0L) next

    # Code only: string literals and comments are blanked; in Rmd/qmd only the R chunks count
    code <- .mask_code(lines)
    if (grepl("\\.(rmd|qmd)$", f, ignore.case = TRUE)) {
      code[!.in_r_chunk(lines)] <- ""
    }

    # Functions defined locally in this file
    local_fns <- character()
    def_matches <- regmatches(
      code,
      gregexpr("(?<![a-zA-Z0-9_.])([a-zA-Z0-9_.]+)\\s*(?:<-|=)\\s*(?:function|\\\\)", code, perl = TRUE)
    )
    for (m in unlist(def_matches)) {
      fn_name <- trimws(sub("\\s*(?:<-|=)\\s*(?:function|\\\\).*", "", m))
      if (nzchar(fn_name)) local_fns <- c(local_fns, fn_name)
    }
    local_fns <- unique(local_fns)

    attaches_templecbe <- any(grepl("(library|require|requireNamespace|loadNamespace)\\s*\\(\\s*[\"']?TempleCBE\\b", lines)) ||
      any(grepl("TempleCBE:::?", code))

    for (line_idx in which(grepl(dep_pattern, code, perl = TRUE))) {
      ln <- code[line_idx]
      m <- gregexpr(call_pattern, ln, perl = TRUE)
      if (m[[1]][1] == -1L) next

      capture_starts <- attr(m[[1]], "capture.start")
      capture_lengths <- attr(m[[1]], "capture.length")

      for (k in seq_along(m[[1]])) {
        pkg_start <- capture_starts[k, 1]
        pkg_len <- capture_lengths[k, 1]
        pkg <- if (pkg_start > 0 && pkg_len > 0) substr(ln, pkg_start, pkg_start + pkg_len - 1L) else ""

        fn_start <- capture_starts[k, 2]
        fn_len <- capture_lengths[k, 2]
        old_fn <- if (fn_start > 0 && fn_len > 0) substr(ln, fn_start, fn_start + fn_len - 1L) else ""

        if (!nzchar(old_fn) || !old_fn %in% dep_names) next

        # Ignore if qualified with another package (e.g. janitor::clean_names)
        if (nzchar(pkg) && pkg != "TempleCBE") next

        # Ignore if unqualified and defined locally in this script (e.g. local SumNa)
        if (!nzchar(pkg) && old_fn %in% local_fns) next

        ctype <- if (nzchar(pkg)) "qualified" else "unqualified"
        if (ctype == "unqualified" && old_fn %in% .ambiguous_deprecated_names) {
          # janitor, rmarkdown, data.table and writexl export these names too: it is TempleCBE's
          # only if the file uses TempleCBE at all, and even then a person has to decide
          if (!attaches_templecbe) next
          ctype <- "ambiguous"
        }

        results[[length(results) + 1L]] <- tibble::tibble(
          file = f,
          line = line_idx,
          old_name = old_fn,
          replacement = dep_map[[old_fn]],
          call_type = ctype,
          context = trimws(lines[line_idx]),
          name_start = as.integer(fn_start),
          name_len = as.integer(fn_len)
        )
      }
    }
  }

  if (length(results) == 0L) return(empty)
  dplyr::distinct(dplyr::bind_rows(results), .data$file, .data$line, .data$name_start, .keep_all = TRUE)
}

#' @rdname find_deprecated_calls
#' @export
update_deprecated_calls <- function(path, dry_run = TRUE, exclude_dirs = NULL) {
  matches <- .deprecated_matches(path, exclude_dirs)
  findings <- matches[, c("file", "line", "old_name", "replacement", "call_type", "context")]

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

  total_updated <- 0L
  modified_files_count <- 0L

  for (fpath in unique(matches$file)) {
    file_matches <- matches[matches$file == fpath, ]
    skipped <- file_matches[file_matches$call_type == "ambiguous", ]
    if (nrow(skipped)) {
      message(sprintf("  - %s: not rewritten, check by hand (the name is also a function of another package): %s",
                      basename(fpath),
                      paste0(skipped$old_name, " [line ", skipped$line, "]", collapse = ", ")))
    }
    file_matches <- file_matches[file_matches$call_type != "ambiguous", ]
    if (nrow(file_matches) == 0L) next

    lines <- readLines(fpath, warn = FALSE, encoding = "UTF-8")
    # right to left within a line, so that earlier positions stay valid
    file_matches <- file_matches[order(file_matches$line, -file_matches$name_start), ]
    for (i in seq_len(nrow(file_matches))) {
      l_i <- file_matches$line[i]
      s <- file_matches$name_start[i]
      lines[l_i] <- paste0(substr(lines[l_i], 1L, s - 1L), file_matches$replacement[i],
                           substr(lines[l_i], s + file_matches$name_len[i], nchar(lines[l_i])))
    }
    writeLines(lines, fpath, useBytes = FALSE)
    total_updated <- total_updated + nrow(file_matches)
    modified_files_count <- modified_files_count + 1L
    message(sprintf("  - %s: %d call(s) updated", basename(fpath), nrow(file_matches)))
  }

  message(sprintf("Updated %d deprecated call(s) across %d file(s).", total_updated, modified_files_count))
  invisible(findings)
}

.ambiguous_deprecated_names <- c("clean_names", "render", "like", "write_xlsx")

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
