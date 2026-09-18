#' Compare Two Data Frames (SAS PROC COMPARE Parity)
#'
#' Compares two data frames (or tibbles) at the dataset, variable, and observation levels,
#' providing an audit-ready, tidy alternative to SAS \code{PROC COMPARE} and legacy comparison
#' utilities (such as \code{arsenal::comparedf}). Evaluates variable presence, data types,
#' variable labels (from \pkg{labelled}), row counts, and value-level discrepancies within
#' a user-specified numerical tolerance.
#'
#' @param base The base data frame or tibble (equivalent to SAS \code{base=}).
#' @param compare The comparison data frame or tibble (equivalent to SAS \code{compare=}).
#' @param by Optional character vector of column names specifying key variables used to align
#'   observations across datasets (equivalent to SAS \code{id} or \code{by} statement). If \code{NULL}
#'   (default), observations are compared by row order.
#' @param tolerance Non-negative numeric threshold for numeric differences (default: \code{1e-7}).
#'   Differences with absolute magnitude less than or equal to \code{tolerance} are considered matches.
#' @param base_name Optional character string identifying the base dataset. If \code{NULL},
#'   deparsed from the \code{base} argument.
#' @param compare_name Optional character string identifying the comparison dataset. If \code{NULL},
#'   deparsed from the \code{compare} argument.
#' @param max_diffs Maximum number of discrepant rows to store per variable (default: 100).
#'
#' @return An S3 object of class \code{"cbe_compare_df"} containing:
#'   \item{meta}{List containing metadata on dataset names, dimensions, tolerance, and keys.}
#'   \item{variables}{List with elements \code{common}, \code{base_only}, and \code{compare_only}.}
#'   \item{summary}{Tibble summarizing comparison status for each variable (types, match status, difference counts, max difference, RMSE).}
#'   \item{observations}{List summarizing observation matching (counts, keys matched, unmatched).}
#'   \item{diffs}{Tibble containing detailed cell-by-cell discrepancies for all variables exceeding tolerance.}
#'   \item{is_concordant}{Logical indicating whether the datasets match completely within tolerance on all common variables and observations.}
#'
#' @export
#' @importFrom generics tidy
#' @examples
#' df1 <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5), y = c("a", "b", "c", "d", "e"))
#' df2 <- data.frame(id = 1:5, x = c(1, 2, 3.0001, 4, 5), y = c("a", "b", "c", "d", "f"))
#' cmp <- cbe_compare_df(df1, df2, by = "id", tolerance = 1e-3)
#' print(cmp)
#' generics::tidy(cmp)
cbe_compare_df <- function(base,
                           compare,
                           by = NULL,
                           tolerance = 1e-7,
                           base_name = NULL,
                           compare_name = NULL,
                           max_diffs = 100) {
  if (is.null(base_name)) {
    base_name <- deparse(substitute(base))
    if (length(base_name) > 1) base_name <- base_name[1]
  }
  if (is.null(compare_name)) {
    compare_name <- deparse(substitute(compare))
    if (length(compare_name) > 1) compare_name <- compare_name[1]
  }

  base_df <- as.data.frame(base)
  comp_df <- as.data.frame(compare)

  # Extract variable labels helper (shared with get_dataset_info.R, which
  # documents why var_label() is tried before the attr() fallback)
  get_var_label <- function(df, col) resolve_var_label(df[[col]], default = NA_character_)

  # 1. Variables Summary
  vars_base <- names(base_df)
  vars_comp <- names(comp_df)
  common_vars <- intersect(vars_base, vars_comp)
  base_only   <- setdiff(vars_base, vars_comp)
  comp_only   <- setdiff(vars_comp, vars_base)

  # Check 'by' variables
  if (!is.null(by)) {
    miss_base <- setdiff(by, vars_base)
    miss_comp <- setdiff(by, vars_comp)
    if (length(miss_base) > 0) {
      stop(sprintf("Key variable(s) not found in base dataset: %s", paste(miss_base, collapse = ", ")))
    }
    if (length(miss_comp) > 0) {
      stop(sprintf("Key variable(s) not found in compare dataset: %s", paste(miss_comp, collapse = ", ")))
    }
    compare_vars <- setdiff(common_vars, by)
  } else {
    compare_vars <- common_vars
  }

  # 2. Observations & Alignment
  n_base <- nrow(base_df)
  n_comp <- nrow(comp_df)

  if (!is.null(by)) {
    k_base <- base_df[, by, drop = FALSE]
    k_base$.row_base <- seq_len(n_base)
    k_comp <- comp_df[, by, drop = FALSE]
    k_comp$.row_comp <- seq_len(n_comp)

    matched <- dplyr::inner_join(k_base, k_comp, by = by, relationship = "many-to-many")
    anti_base <- dplyr::anti_join(k_base, k_comp, by = by)
    anti_comp <- dplyr::anti_join(k_comp, k_base, by = by)

    n_matched <- nrow(matched)
    row_idx_base <- matched$.row_base
    row_idx_comp <- matched$.row_comp
    matched_keys <- matched[, by, drop = FALSE]
  } else {
    n_min <- min(n_base, n_comp)
    n_matched <- n_min
    row_idx_base <- seq_len(n_min)
    row_idx_comp <- seq_len(n_min)
    anti_base <- if (n_base > n_min) seq(n_min + 1, n_base) else integer(0)
    anti_comp <- if (n_comp > n_min) seq(n_min + 1, n_comp) else integer(0)
    matched_keys <- NULL
  }

  # 3. Value-Level Comparisons
  summary_list <- list()
  diffs_list <- list()

  for (col in compare_vars) {
    v1 <- base_df[[col]][row_idx_base]
    v2 <- comp_df[[col]][row_idx_comp]

    cls1 <- paste(class(base_df[[col]]), collapse = "/")
    cls2 <- paste(class(comp_df[[col]]), collapse = "/")
    lbl1 <- get_var_label(base_df, col)

    types_match <- (cls1 == cls2)

    if (is.numeric(v1) && is.numeric(v2)) {
      d <- v1 - v2
      abs_d <- abs(d)

      is_na1 <- is.na(v1)
      is_na2 <- is.na(v2)

      # Mismatch if one NA and one not, or difference > tolerance
      is_diff <- (is_na1 != is_na2) | (!is_na1 & !is_na2 & (abs_d > tolerance))
      n_diff <- sum(is_diff)

      valid_d <- abs_d[!is.na(abs_d)]
      max_diff <- if (length(valid_d) > 0) max(valid_d, na.rm = TRUE) else NA_real_
      rmse <- if (length(valid_d) > 0) sqrt(mean(valid_d^2)) else NA_real_
    } else {
      c1 <- as.character(v1)
      c2 <- as.character(v2)
      is_na1 <- is.na(c1)
      is_na2 <- is.na(c2)

      is_diff <- (is_na1 != is_na2) | (!is_na1 & !is_na2 & (c1 != c2))
      n_diff <- sum(is_diff)
      max_diff <- NA_real_
      rmse <- NA_real_
      d <- rep(NA_real_, length(v1))
    }

    summary_list[[col]] <- tibble::tibble(
      variable = col,
      label = lbl1,
      type_base = cls1,
      type_compare = cls2,
      types_match = types_match,
      n_diff = n_diff,
      max_diff = max_diff,
      rmse = rmse
    )

    if (n_diff > 0) {
      diff_idx <- which(is_diff)
      if (!is.null(max_diffs) && length(diff_idx) > max_diffs) {
        diff_idx <- diff_idx[seq_len(max_diffs)]
      }

      diff_tbl <- tibble::tibble(
        row_base = row_idx_base[diff_idx],
        row_compare = row_idx_comp[diff_idx],
        variable = col,
        label = lbl1,
        base_value = as.character(v1[diff_idx]),
        compare_value = as.character(v2[diff_idx]),
        diff = if (is.numeric(v1) && is.numeric(v2)) d[diff_idx] else NA_real_
      )

      if (!is.null(matched_keys)) {
        diff_tbl <- dplyr::bind_cols(matched_keys[diff_idx, , drop = FALSE], diff_tbl)
      }
      diffs_list[[col]] <- diff_tbl
    }
  }

  summary_tbl <- dplyr::bind_rows(summary_list)
  all_diffs <- tibble::as_tibble(if (length(diffs_list) > 0) dplyr::bind_rows(diffs_list) else tibble::tibble())

  is_concordant <- (length(base_only) == 0 &&
                    length(comp_only) == 0 &&
                    (if (!is.null(by)) (nrow(anti_base) == 0 && nrow(anti_comp) == 0) else (n_base == n_comp)) &&
                    all(summary_tbl$types_match) &&
                    sum(summary_tbl$n_diff) == 0)

  res <- list(
    meta = list(
      base_name = base_name,
      compare_name = compare_name,
      by = by,
      tolerance = tolerance,
      n_base = n_base,
      n_compare = n_comp,
      p_base = length(vars_base),
      p_compare = length(vars_comp)
    ),
    variables = list(
      common = common_vars,
      base_only = base_only,
      compare_only = comp_only
    ),
    summary = summary_tbl,
    observations = list(
      n_matched = n_matched,
      unmatched_base = if (!is.null(by)) nrow(anti_base) else length(anti_base),
      unmatched_compare = if (!is.null(by)) nrow(anti_comp) else length(anti_comp)
    ),
    diffs = all_diffs,
    is_concordant = is_concordant
  )
  class(res) <- "cbe_compare_df"
  res
}

#' @export
print.cbe_compare_df <- function(x, max_print = 10, ...) {
  cli_line <- paste0(rep("-", 70), collapse = "")
  cat(cli_line, "\n")
  cat("TempleCBE Data Frame Comparison (SAS PROC COMPARE Parity)\n")
  cat(cli_line, "\n")
  cat(sprintf("Base Data:    %-25s (N = %d, P = %d)\n", x$meta$base_name, x$meta$n_base, x$meta$p_base))
  cat(sprintf("Compare Data: %-25s (N = %d, P = %d)\n", x$meta$compare_name, x$meta$n_compare, x$meta$p_compare))
  cat(sprintf("By Variables: %s\n", if (is.null(x$meta$by)) "(Row Order)" else paste(x$meta$by, collapse = ", ")))
  cat(sprintf("Tolerance:    %g\n", x$meta$tolerance))
  cat(cli_line, "\n\n")

  # Variable status
  cat("-- Variable Concordance ----------------------------------------------\n")
  cat(sprintf("Variables in Common:       %d\n", length(x$variables$common)))
  if (length(x$variables$base_only) > 0) {
    cat(sprintf("Variables in Base only:    %s\n", paste(x$variables$base_only, collapse = ", ")))
  }
  if (length(x$variables$compare_only) > 0) {
    cat(sprintf("Variables in Compare only: %s\n", paste(x$variables$compare_only, collapse = ", ")))
  }

  # Observation status
  cat("\n-- Observation Concordance -------------------------------------------\n")
  cat(sprintf("Matched Observations:      %d\n", x$observations$n_matched))
  if (x$observations$unmatched_base > 0) {
    cat(sprintf("Unmatched in Base:         %d\n", x$observations$unmatched_base))
  }
  if (x$observations$unmatched_compare > 0) {
    cat(sprintf("Unmatched in Compare:      %d\n", x$observations$unmatched_compare))
  }

  # Discrepancies
  cat("\n-- Discrepancies Summary ---------------------------------------------\n")
  n_diff_vars <- sum(x$summary$n_diff > 0)
  if (n_diff_vars == 0) {
    cat(sprintf("Result: All values match within tolerance %g.\n", x$meta$tolerance))
    if (x$is_concordant) {
      cat("Status: Data sets are completely CONCORDANT.\n")
    } else {
      cat("Status: Common variables match, but variables or observation counts differ.\n")
    }
  } else {
    cat(sprintf("Variables with Differences: %d / %d\n\n", n_diff_vars, nrow(x$summary)))
    diff_summary <- dplyr::filter(x$summary, .data$n_diff > 0)
    print(as.data.frame(diff_summary), row.names = FALSE)

    cat(sprintf("\nDiscrepant Values (showing up to %d rows):\n", max_print))
    print(as.data.frame(head(x$diffs, max_print)), row.names = FALSE)
    if (nrow(x$diffs) > max_print) {
      cat(sprintf("... and %d more discrepancies. Use broom::tidy() to view all.\n", nrow(x$diffs) - max_print))
    }
  }
  cat(cli_line, "\n")
  invisible(x)
}

#' @export
summary.cbe_compare_df <- function(object, ...) {
  object$summary
}

#' @exportS3Method generics::tidy
tidy.cbe_compare_df <- function(x, ...) {
  x$diffs
}
