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
#'   (default), observations are compared by row order. For the \code{cbe_database} method, this
#'   is the node key (passed through to the nodes comparison only).
#' @param edge_by \code{cbe_database} method only: key column(s) used to align edges (default
#'   \code{c("from", "to")}, which every \code{\link{as_database}} method produces).
#' @param tolerance Non-negative numeric threshold for numeric differences (default: \code{1e-7}).
#'   Differences with absolute magnitude less than or equal to \code{tolerance} are considered matches.
#' @param base_name Optional character string identifying the base dataset. If \code{NULL},
#'   deparsed from the \code{base} argument.
#' @param compare_name Optional character string identifying the comparison dataset. If \code{NULL},
#'   deparsed from the \code{compare} argument.
#' @param max_diffs Maximum number of discrepant rows to store per variable (default: 100).
#' @param ... Passed on to methods (and, for the \code{cbe_database} method,
#'   on to the per-table \code{cbe_compare_df()} calls -- e.g. \code{by}/\code{tolerance}).
#'
#' @return An S3 object of class \code{"cbe_compare_df"} containing (or, for a
#'   \code{\link{cbe_database}}/graph comparison, a \code{"cbe_compare_database"}
#'   list of two such objects, named \code{nodes} and \code{edges}):
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
cbe_compare_df <- function(base, compare, ...) {
  UseMethod("cbe_compare_df")
}

#' @rdname cbe_compare_df
#' @export
cbe_compare_df.default <- function(base,
                           compare,
                           by = NULL,
                           tolerance = 1e-7,
                           base_name = NULL,
                           compare_name = NULL,
                           max_diffs = 100,
                           ...) {
  # Support database list with character vector of table names
  if (is.list(base) && !is.data.frame(base)) {
    if (is.character(compare) && length(compare) == 2) {
      base_name <- compare[1]
      compare_name <- compare[2]
      comp_df <- as.data.frame(base[[compare_name]])
      base_df <- as.data.frame(base[[base_name]])
    } else if (is.character(base_name) && is.character(compare_name) &&
               base_name %in% names(base) && compare_name %in% names(base)) {
      comp_df <- as.data.frame(base[[compare_name]])
      base_df <- as.data.frame(base[[base_name]])
    } else {
      stop("When `base` is an R database list, `compare` must be a length-2 character vector of table names, or `base_name` and `compare_name` must specify valid table names.", call. = FALSE)
    }
  } else {
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
  }

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

  # bind_rows() on an empty list (no non-key variables left to compare --
  # e.g. two tables consisting only of `by` columns, like a bare edge list)
  # returns a tibble with NO columns at all, not even `n_diff`/`types_match`,
  # which then warns ("unknown or uninitialised column") every time those are
  # referenced below. Keep the schema so an empty comparison stays silent and
  # vacuously concordant on the (zero) shared non-key variables.
  summary_tbl <- if (length(summary_list) > 0) {
    dplyr::bind_rows(summary_list)
  } else {
    tibble::tibble(
      variable = character(0),
      label = character(0),
      type_base = character(0),
      type_compare = character(0),
      types_match = logical(0),
      n_diff = integer(0),
      max_diff = numeric(0),
      rmse = numeric(0)
    )
  }
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

#' @rdname cbe_compare_df
#' @export
#' @examples
#' g1 <- igraph::graph_from_data_frame(
#'   data.frame(from = c("a", "b"), to = c("b", "c")),
#'   vertices = data.frame(name = c("a", "b", "c"), stage = c("eda", "eda", "report"))
#' )
#' g2 <- igraph::graph_from_data_frame(
#'   data.frame(from = c("a", "b"), to = c("b", "c")),
#'   vertices = data.frame(name = c("a", "b", "c"), stage = c("eda", "analysis", "report"))
#' )
#' cbe_compare_df(g1, g2, by = "name")
cbe_compare_df.cbe_database <- function(base,
                                        compare,
                                        by = NULL,
                                        edge_by = c("from", "to"),
                                        ...,
                                        base_name = NULL,
                                        compare_name = NULL) {
  base_name <- base_name %||% deparse(substitute(base))[1]
  compare_name <- compare_name %||% deparse(substitute(compare))[1]

  res <- list(
    nodes = cbe_compare_df.default(
      base$nodes, compare$nodes, by = by, ...,
      base_name = paste0(base_name, "$nodes"), compare_name = paste0(compare_name, "$nodes")
    ),
    edges = cbe_compare_df.default(
      base$edges, compare$edges, by = edge_by, ...,
      base_name = paste0(base_name, "$edges"), compare_name = paste0(compare_name, "$edges")
    )
  )
  class(res) <- "cbe_compare_database"
  res
}

#' @rdname cbe_compare_df
#' @export
cbe_compare_df.igraph <- function(base, compare, ...) {
  cbe_compare_df(as_database(base), as_database(compare), ...)
}

#' @rdname cbe_compare_df
#' @export
cbe_compare_df.visNetwork <- function(base, compare, ...) {
  cbe_compare_df(as_database(base), as_database(compare), ...)
}

#' @export
print.cbe_compare_database <- function(x, ...) {
  cat("== Nodes ", strrep("=", 63), "\n", sep = "")
  print(x$nodes, ...)
  cat("\n== Edges ", strrep("=", 63), "\n", sep = "")
  print(x$edges, ...)
  invisible(x)
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

#' Autoplot Method for CBE Data Frame Comparison
#'
#' Generates ggplot2 visualizations of data frame comparisons:
#' 2-set Venn diagrams of observation/key overlap or variable concordance (leveraging
#' \pkg{ggVennDiagram} when installed), or discrepancy bar charts across variables.
#'
#' @param object A \code{\link{cbe_compare_df}} object.
#' @param type Character string specifying the plot type: \code{"observations"} (default; Venn of rows/keys),
#'   \code{"variables"} (Venn of common/unique columns), or \code{"discrepancies"} (bar chart of value differences).
#' @param ... Additional arguments passed to methods or \pkg{ggVennDiagram}.
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @exportS3Method ggplot2::autoplot
autoplot.cbe_compare_df <- function(object, type = c("observations", "variables", "discrepancies"), ...) {
  type <- match.arg(type)

  if (type == "observations") {
    b_name <- object$meta$base_name %||% "Base"
    c_name <- object$meta$compare_name %||% "Compare"

    set_list <- list()
    set_list[[b_name]] <- c(paste0("matched_", seq_len(object$observations$n_matched)),
                            paste0("base_only_", seq_len(object$observations$unmatched_base)))
    set_list[[c_name]] <- c(paste0("matched_", seq_len(object$observations$n_matched)),
                            paste0("compare_only_", seq_len(object$observations$unmatched_compare)))

    if (requireNamespace("ggVennDiagram", quietly = TRUE)) {
      p <- ggVennDiagram::ggVennDiagram(set_list, label_alpha = 0, ...) +
        ggplot2::scale_fill_gradient(low = "#e8f8f5", high = "#a41e35") +
        ggplot2::labs(
          title = paste0("Observation / Key Concordance: ", b_name, " vs ", c_name),
          subtitle = paste0("Total Matched: ", object$observations$n_matched,
                            " | Unmatched in Base: ", object$observations$unmatched_base,
                            " | Unmatched in Compare: ", object$observations$unmatched_compare)
        ) +
        theme_cbe()
      return(p)
    }

    df_labels <- tibble::tribble(
      ~x, ~y, ~label,
      -0.6, 0, paste0(b_name, "\nOnly\n(N = ", object$observations$unmatched_base, ")"),
      0.6, 0, paste0(c_name, "\nOnly\n(N = ", object$observations$unmatched_compare, ")"),
      0, 0, paste0("Matched\n(N = ", object$observations$n_matched, ")")
    )
    t <- seq(0, 2 * pi, length.out = 100)
    circle1 <- data.frame(x = cos(t) - 0.4, y = sin(t), set = b_name)
    circle2 <- data.frame(x = cos(t) + 0.4, y = sin(t), set = c_name)
    circles <- rbind(circle1, circle2)

    p <- ggplot2::ggplot() +
      ggplot2::geom_polygon(data = circles, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$set, group = .data$set),
                            alpha = 0.35, color = "#a41e35", linewidth = 1) +
      ggplot2::geom_text(data = df_labels, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
                         fontface = "bold", size = 4.5, color = "#2b2b2b") +
      ggplot2::scale_fill_manual(values = c("#005a70", "#a41e35")) +
      ggplot2::coord_fixed() +
      ggplot2::theme_void() +
      ggplot2::labs(
        title = paste0("Observation Concordance: ", b_name, " vs ", c_name),
        fill = "Dataset"
      )
    return(p)
  }

  if (type == "variables") {
    b_name <- object$meta$base_name %||% "Base"
    c_name <- object$meta$compare_name %||% "Compare"
    set_list <- list()
    set_list[[b_name]] <- c(object$variables$common, object$variables$base_only)
    set_list[[c_name]] <- c(object$variables$common, object$variables$compare_only)

    if (requireNamespace("ggVennDiagram", quietly = TRUE)) {
      p <- ggVennDiagram::ggVennDiagram(set_list, label_alpha = 0, ...) +
        ggplot2::scale_fill_gradient(low = "#e8f8f5", high = "#005a70") +
        ggplot2::labs(
          title = paste0("Variable Concordance: ", b_name, " vs ", c_name),
          subtitle = paste0("Common: ", length(object$variables$common),
                            " | Base Only: ", length(object$variables$base_only),
                            " | Compare Only: ", length(object$variables$compare_only))
        ) +
        theme_cbe()
      return(p)
    }

    df_labels <- tibble::tribble(
      ~x, ~y, ~label,
      -0.6, 0, paste0(b_name, "\nOnly\n(", length(object$variables$base_only), ")"),
      0.6, 0, paste0(c_name, "\nOnly\n(", length(object$variables$compare_only), ")"),
      0, 0, paste0("Common\n(", length(object$variables$common), ")")
    )
    t <- seq(0, 2 * pi, length.out = 100)
    circles <- rbind(
      data.frame(x = cos(t) - 0.4, y = sin(t), set = b_name),
      data.frame(x = cos(t) + 0.4, y = sin(t), set = c_name)
    )

    p <- ggplot2::ggplot() +
      ggplot2::geom_polygon(data = circles, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$set, group = .data$set),
                            alpha = 0.35, color = "#005a70", linewidth = 1) +
      ggplot2::geom_text(data = df_labels, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
                         fontface = "bold", size = 4.5, color = "#2b2b2b") +
      ggplot2::scale_fill_manual(values = c("#1fceb6", "#005a70")) +
      ggplot2::coord_fixed() +
      ggplot2::theme_void() +
      ggplot2::labs(title = paste0("Variable Concordance: ", b_name, " vs ", c_name), fill = "Dataset")
    return(p)
  }

  diff_tbl <- dplyr::filter(object$summary, .data$n_diff > 0)
  if (nrow(diff_tbl) == 0) {
    p <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 1, y = 1, label = "All common variables match within tolerance!",
                        size = 5, color = "#005a70", fontface = "bold") +
      ggplot2::theme_void() +
      ggplot2::labs(title = "Discrepancies Summary: Zero Differences")
    return(p)
  }

  diff_tbl <- dplyr::arrange(diff_tbl, .data$n_diff)
  diff_tbl$variable <- factor(diff_tbl$variable, levels = diff_tbl$variable)

  p <- ggplot2::ggplot(diff_tbl, ggplot2::aes(x = .data$n_diff, y = .data$variable)) +
    ggplot2::geom_col(fill = "#a41e35", width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$n_diff), hjust = -0.2, size = 3.5) +
    ggplot2::labs(
      title = "Value-Level Discrepancies by Variable",
      x = "Number of Discrepant Observations",
      y = "Variable"
    ) +
    theme_cbe()
  p
}
