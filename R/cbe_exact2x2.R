#' Exact Test for 2x2 Tables with Automatic Zero-Cell Mid-p Default
#'
#' Performs an exact test for 2x2 contingency tables using \pkg{exact2x2}.
#' If any cell count in the 2x2 table is zero (\code{min(tab) == 0}) and
#' \code{midp} is unspecified, the function defaults to the mid-p version of
#' Central Fisher's exact test (\code{midp = TRUE}). This prevents the extreme
#' conservatism and loss of power of standard conditional exact tests on boundary
#' tables. When all cells are non-zero, it defaults to the standard Central
#' Fisher's exact test (\code{midp = FALSE}), which guarantees central confidence
#' intervals that invert the two one-sided tests.
#'
#' The function accepts a 2x2 table/matrix, two categorical vectors \code{x} and
#' \code{y}, or \code{(data, variable, by)} arguments for direct compatibility as
#' a custom test in \pkg{gtsummary}'s \code{\link[gtsummary]{add_p}}.
#'
#' @param x A 2x2 numeric matrix, table, or a categorical vector. Can also be a
#'   data frame if \code{variable} and \code{by} are supplied.
#' @param y Optional second categorical vector when \code{x} is a vector.
#' @param data Optional data frame when used with \code{variable} and \code{by}.
#' @param variable Character string of the column name to test when called by
#'   \pkg{gtsummary}.
#' @param by Character string of the grouping/stratifying column when called by
#'   \pkg{gtsummary}.
#' @param midp Logical. If \code{NULL} (the default), automatically set to
#'   \code{TRUE} when any table cell is 0, and \code{FALSE} when all cells are
#'   positive. Can be explicitly set to \code{TRUE} or \code{FALSE} to override.
#' @param conf.level Confidence level for the returned confidence interval
#'   (default 0.95).
#' @param alternative Alternative hypothesis direction: \code{"two.sided"}
#'   (default), \code{"greater"}, or \code{"less"}.
#' @param tsmethod Two-sided method passed to \code{\link[exact2x2]{exact2x2}}:
#'   \code{"two.sided"} (Central Fisher's exact test, default), \code{"minlike"},
#'   or \code{"blaker"}.
#' @param ... Additional arguments passed to \code{\link[exact2x2]{exact2x2}}.
#'
#' @return A tibble with columns:
#'   \item{estimate}{Estimated odds ratio (conditional MLE or median unbiased estimate).}
#'   \item{p.value}{Two-sided or one-sided p-value (mid-p adjusted when \code{midp = TRUE}).}
#'   \item{conf.low}{Lower bound of the confidence interval.}
#'   \item{conf.high}{Upper bound of the confidence interval.}
#'   \item{statistic}{Point estimate of the odds ratio (for \pkg{gtsummary} compatibility).}
#'   \item{method}{Descriptive name of the exact test performed.}
#'   \item{alternative}{Alternative hypothesis.}
#'   \item{midp}{Logical flag indicating whether mid-p adjustment was used.}
#'   \item{has_zero}{Logical flag indicating whether any cell in the table was zero.}
#'
#' @export
#' @examples
#' # 2x2 table with a zero cell: automatically triggers mid-p adjustment
#' tab_zero <- matrix(c(0, 10, 5, 15), nrow = 2,
#'                    dimnames = list(c("Treated", "Control"), c("Event", "No Event")))
#' cbe_exact2x2(tab_zero)
#'
#' # 2x2 table without zero cells: standard Central Fisher's exact test
#' tab_nonzero <- matrix(c(12, 8, 5, 15), nrow = 2)
#' cbe_exact2x2(tab_nonzero)
cbe_exact2x2 <- function(x, y = NULL, data = NULL, variable = NULL, by = NULL,
                         midp = NULL, conf.level = 0.95,
                         alternative = "two.sided", tsmethod = "central", ...) {
  # Handle gtsummary signature: cbe_exact2x2(data, variable, by, ...)
  if (!is.null(data) && !is.null(variable) && !is.null(by)) {
    tab <- table(data[[variable]], data[[by]])
  } else if (is.data.frame(x) && !is.null(variable) && !is.null(by)) {
    tab <- table(x[[variable]], x[[by]])
  } else if (is.matrix(x) || is.table(x)) {
    tab <- x
  } else if (!is.null(y)) {
    tab <- table(x, y)
  } else {
    stop("cbe_exact2x2() requires a 2x2 table/matrix, two vectors (x, y), or (data, variable, by).",
         call. = FALSE)
  }

  dims <- dim(tab)
  if (!identical(as.integer(dims), c(2L, 2L))) {
    stop("cbe_exact2x2() requires a 2x2 contingency table; received dimensions ",
         paste(dims, collapse = "x"), ". For general RxC tables, use cbe_test_categorical().",
         call. = FALSE)
  }

  has_zero <- any(tab == 0L)
  use_midp <- if (is.null(midp)) has_zero else isTRUE(midp)

  # exact2x2 requires tsmethod in c("central", "minlike", "blaker")
  tsm <- if (is.null(tsmethod) || identical(tsmethod, "two.sided")) {
    "central"
  } else {
    match.arg(tsmethod, c("central", "minlike", "blaker"))
  }

  test_res <- exact2x2::exact2x2(
    tab,
    midp = use_midp,
    conf.level = conf.level,
    alternative = alternative,
    tsmethod = tsm,
    ...
  )

  est <- unname(test_res$estimate)
  ci <- unname(test_res$conf.int)
  pval <- unname(test_res$p.value)

  tibble::tibble(
    estimate = est,
    p.value = pval,
    conf.low = ci[1],
    conf.high = ci[2],
    statistic = est,
    method = test_res$method,
    alternative = test_res$alternative,
    midp = use_midp,
    has_zero = has_zero
  )
}

#' Format Exact 2x2 Odds Ratio and Confidence Interval
#'
#' Computes the odds ratio and confidence interval from \code{\link{cbe_exact2x2}}
#' and returns a formatted character string (e.g. \code{"0.8 (0.3, 2.1)"}).
#' Useful for inline text reporting or table presentation.
#'
#' @param data A data frame or a 2x2 table/matrix.
#' @param variable Character string column name (if \code{data} is a data frame).
#' @param by Character string grouping column name (if \code{data} is a data frame).
#' @param digits Integer number of decimal places for rounding (default 1).
#' @param conf.level Confidence level (default 0.95).
#' @param midp Logical or \code{NULL} (default). If \code{NULL}, automatically
#'   defaults to \code{TRUE} if any cell count is zero.
#' @param ... Additional arguments passed to \code{\link{cbe_exact2x2}}.
#'
#' @return A character string with the formatted odds ratio and confidence interval.
#' @export
#' @examples
#' tab <- matrix(c(0, 10, 5, 15), nrow = 2)
#' cbe_exact2x2_ci(tab)
cbe_exact2x2_ci <- function(data, variable = NULL, by = NULL, digits = 1,
                            conf.level = 0.95, midp = NULL, ...) {
  res <- if (is.matrix(data) || is.table(data)) {
    cbe_exact2x2(data, conf.level = conf.level, midp = midp, ...)
  } else {
    cbe_exact2x2(data = data, variable = variable, by = by,
                 conf.level = conf.level, midp = midp, ...)
  }

  fmt <- paste0("%.", digits, "f")
  est_str <- sprintf(fmt, res$estimate)
  low_str <- sprintf(fmt, res$conf.low)
  high_str <- sprintf(fmt, res$conf.high)

  paste0(est_str, " (", low_str, ", ", high_str, ")")
}

#' Format p-values for Biostatistical and Clinical Reporting
#'
#' Formats numeric p-values into clean, publication-ready strings (e.g.
#' \code{"p = 0.024"}, \code{"p < 0.001"}, or \code{"0.024"}).
#'
#' @param p Numeric p-value or vector of p-values.
#' @param accuracy Numeric precision threshold passed to \code{\link[scales]{label_pvalue}} (default 0.001).
#' @param add_p Logical; if \code{TRUE} (the default), prepends \code{"p = "}, \code{"p < "}, or \code{"p > "}.
#'   If \code{FALSE}, returns only the formatted numeric string.
#' @param digits Optional integer number of decimal places (overrides \code{accuracy} as \code{10^(-digits)}).
#'
#' @return A character vector of formatted p-values.
#' @export
#' @examples
#' pformat(0.0241)
#' pformat(0.0001)
#' pformat(0.852, add_p = FALSE)
pformat <- function(p, accuracy = 0.001, add_p = TRUE, digits = NULL) {
  if (is.null(p) || length(p) == 0) return(character(0))
  if (is.character(p)) return(p)
  if (!is.null(digits) && is.numeric(digits)) {
    accuracy <- 10^(-digits)
  }
  prefix <- if (isTRUE(add_p)) c("p < ", "p = ", "p > ") else c("< ", "", "> ")
  scales::label_pvalue(accuracy = accuracy, prefix = prefix, add_p = isTRUE(add_p))(p)
}

#' @rdname pformat
#' @export
cbe_pformat <- pformat

#' Institutional Categorical Hypothesis Test for gtsummary and Batch Testing
#'
#' Standard categorical hypothesis testing engine conforming to the CBE statistical
#' protocol. Operates as a drop-in custom test for \code{\link[gtsummary]{add_p}}
#' or as a standalone batch testing engine with optional \pkg{furrr} parallelization.
#'
#' The function follows a rigorous 4-rule hierarchy:
#' \enumerate{
#'   \item \strong{2x2 table with zero cell}: Uses Central Fisher's exact test with
#'     mid-p adjustment (\code{\link[exact2x2]{exact2x2}} with \code{midp = TRUE}) to
#'     avoid extreme conservatism.
#'   \item \strong{2x2 table without zero cell}: Uses Central Fisher's exact test
#'     (\code{\link[exact2x2]{exact2x2}} with \code{midp = FALSE}).
#'   \item \strong{RxC table with sparse counts}: If any expected cell count is
#'     less than 5, runs Fisher's exact test with Monte Carlo simulation
#'     (\code{\link[stats]{fisher.test}} with \code{simulate.p.value = TRUE}).
#'   \item \strong{RxC table with adequate counts}: If all expected cell counts are
#'     at least 5, runs Pearson's Chi-squared test without continuity correction
#'     (\code{\link[stats]{chisq.test}} with \code{correct = FALSE}).
#' }
#'
#' @param data Data frame supplied directly or by \pkg{gtsummary}.
#' @param variable Character string column name for the feature being tested,
#'   a character vector of variable names for batch testing, or a two-sided
#'   \code{\link[stats]{formula}} of the form \code{response ~ grouping_var} or
#'   \code{var1 + var2 ~ grouping_var}. If \code{NULL}, all categorical/factor
#'   columns in \code{data} (excluding \code{by}) are tested.
#' @param by Character string column name for the stratifying/grouping variable.
#'   Can be omitted when \code{variable} is a formula.
#' @param test Hypothesis test engine: \code{"auto"} (default CBE hierarchy),
#'   \code{"exact"} (force Central Fisher exact test for 2x2 or simulated Fisher for RxC),
#'   \code{"chisq"} (force Pearson's Chi-squared test via \code{\link[stats]{chisq.test}}),
#'   or \code{"fisher"} (force \code{\link[stats]{fisher.test}}).
#' @param correct Logical; whether to apply continuity correction when \code{test = "chisq"}
#'   (default \code{FALSE} following CBE standard protocol).
#' @param B Integer; number of Monte Carlo replicates when Fisher's exact test with
#'   simulation is triggered (default \code{2000L}).
#' @param simulate.p.value Optional logical; if specified, explicitly enables or disables
#'   Monte Carlo simulation for Fisher's test. If \code{NULL} (default), auto-triggers
#'   simulation when table total exceeds 500 or any dimension exceeds 2.
#' @param parallel Logical; whether to use \pkg{furrr} for parallel execution
#'   (for batch testing across multiple variables or chunking high-\code{B} simulations).
#'   Requires setting a \code{\link[future]{plan}} beforehand.
#' @param n_chunks Integer; number of chunks to partition \code{B} into when running
#'   parallel simulations with \code{B >= 10000} (default 4L).
#' @param ... Additional arguments passed to \code{\link[exact2x2]{exact2x2}} or
#'   \code{\link[stats]{fisher.test}}.
#'
#' @return When \code{variable} is a single string (such as when called by
#'   \pkg{gtsummary}), returns a one-row tibble compliant with \pkg{gtsummary}'s
#'   custom test specification (\code{p.value}, \code{statistic}, \code{parameter},
#'   \code{method}). When \code{variable} is a vector or \code{NULL}, returns a
#'   tidy tibble of test results across all evaluated variables.
#' @export
#' @examples
#' library(gtsummary)
#' trial |>
#'   tbl_summary(by = trt, include = c(response, death, grade)) |>
#'   add_p(test = all_categorical() ~ cbe_test_categorical) |>
#'   separate_p_footnotes()
#'
#' # Standalone batch testing across multiple features:
#' cbe_test_categorical(trial, variable = c("response", "death", "grade"), by = "trt")
#'
#' # Formula interface:
#' cbe_test_categorical(trial, response ~ trt)
cbe_test_categorical <- function(data,
                                 variable = NULL,
                                 by = NULL,
                                 test = c("auto", "exact", "chisq", "fisher"),
                                 correct = FALSE,
                                 B = 2000L,
                                 simulate.p.value = NULL,
                                 parallel = FALSE,
                                 n_chunks = 4L,
                                 ...) {
  test <- match.arg(test)

  # Support formula syntax: response ~ by or var1 + var2 ~ by
  if (inherits(variable, "formula")) {
    lhs_vars <- all.vars(variable[[2L]])
    rhs_vars <- all.vars(variable[[3L]])
    if (length(rhs_vars) != 1L) {
      stop("Formula interface requires exactly one grouping variable on the right-hand side (e.g. outcome ~ group).",
           call. = FALSE)
    }
    by <- rhs_vars
    variable <- lhs_vars
  }

  # Check grouping variable 'by'
  if (is.null(by) || !is.character(by) || length(by) != 1L || !by %in% names(data)) {
    stop("cbe_test_categorical() requires a single grouping column 'by' present in 'data'.",
         call. = FALSE)
  }

  # Batch mode across multiple variables or auto-discovered categorical features
  if (is.null(variable) || length(variable) > 1L) {
    if (is.null(variable)) {
      candidate_vars <- setdiff(names(data), by)
      is_cat <- vapply(data[candidate_vars], function(col) {
        is.factor(col) || is.character(col) || is.logical(col) ||
          (is.atomic(col) && length(unique(stats::na.omit(col))) <= 10L)
      }, logical(1L))
      vars_to_test <- candidate_vars[is_cat]
      if (length(vars_to_test) == 0L) {
        stop("No categorical variables found in 'data' to test against 'by'.", call. = FALSE)
      }
    } else {
      vars_to_test <- variable
    }

    test_single <- function(v) {
      out <- cbe_test_categorical(
        data = data,
        variable = v,
        by = by,
        test = test,
        correct = correct,
        B = B,
        simulate.p.value = simulate.p.value,
        parallel = FALSE,
        ...
      )
      tibble::tibble(
        variable = v,
        by = by,
        p.value = out$p.value,
        p.formatted = pformat(out$p.value),
        statistic = if ("statistic" %in% names(out)) out$statistic else NA_real_,
        parameter = if ("parameter" %in% names(out)) out$parameter else NA_real_,
        method = out$method
      )
    }

    if (isTRUE(parallel)) {
      rlang::check_installed(c("furrr", "future"), reason = "to evaluate categorical tests in parallel.")
      return(furrr::future_map_dfr(vars_to_test, test_single, .options = furrr::furrr_options(seed = TRUE)))
    } else {
      return(purrr::map_dfr(vars_to_test, test_single))
    }
  }

  # Single-variable mode
  if (!variable %in% names(data)) {
    stop(sprintf("Variable '%s' not found in data.", variable), call. = FALSE)
  }

  d <- data[!is.na(data[[variable]]) & !is.na(data[[by]]), , drop = FALSE]
  tab <- table(d[[variable]], d[[by]])
  dims <- dim(tab)

  # Check if empty or 1-dimensional
  if (length(dims) != 2L || any(dims < 2L)) {
    return(tibble::tibble(
      p.value = NA_real_,
      method = "Insufficient levels for hypothesis test"
    ))
  }

  # Helper for simulated Fisher test (with optional parallel chunking for large B)
  run_sim_fisher <- function(tab_m, rep_b, ...) {
    rep_b <- as.integer(rep_b)
    if (is.na(rep_b) || rep_b < 1L) rep_b <- 2000L

    # Base R stats::fisher.test ignores simulation for 2x2 tables
    if (identical(as.integer(dim(tab_m)), c(2L, 2L))) {
      ft <- stats::fisher.test(tab_m, ...)
      return(ft$p.value)
    }

    if (isTRUE(parallel) && rep_b >= 10000L && requireNamespace("furrr", quietly = TRUE)) {
      n_c <- min(max(1L, as.integer(n_chunks)), rep_b)
      if (n_c > 1L) {
        chunk_size <- rep_b %/% n_c
        rem <- rep_b %% n_c
        b_vec <- rep(chunk_size, n_c)
        if (rem > 0L) b_vec[seq_len(rem)] <- b_vec[seq_len(rem)] + 1L

        k_vec <- furrr::future_map_dbl(b_vec, function(b_sub) {
          ft_sub <- stats::fisher.test(tab_m, simulate.p.value = TRUE, B = b_sub, ...)
          max(0L, round(ft_sub$p.value * (b_sub + 1) - 1))
        }, .options = furrr::furrr_options(seed = TRUE))

        return((1 + sum(k_vec)) / (rep_b + 1))
      }
    }

    ft <- stats::fisher.test(tab_m, simulate.p.value = TRUE, B = rep_b, ...)
    ft$p.value
  }

  # Explicit test = "chisq"
  if (test == "chisq") {
    cs <- stats::chisq.test(tab, correct = correct, ...)
    return(tibble::tibble(
      p.value = cs$p.value,
      statistic = unname(cs$statistic),
      parameter = unname(cs$parameter),
      method = "Pearson's Chi-squared test"
    ))
  }

  # Explicit test = "fisher"
  if (test == "fisher") {
    sim <- if (!is.null(simulate.p.value)) isTRUE(simulate.p.value) else (sum(tab) > 500L || any(dims > 2L))
    if (sim) {
      pval <- run_sim_fisher(tab, rep_b = B, ...)
      return(tibble::tibble(
        p.value = pval,
        method = "Fisher's exact test (simulated)"
      ))
    } else {
      ft <- stats::fisher.test(tab, ...)
      return(tibble::tibble(
        p.value = ft$p.value,
        method = "Fisher's exact test"
      ))
    }
  }

  # Explicit test = "exact" for 2x2
  if (test == "exact" && identical(as.integer(dims), c(2L, 2L))) {
    has_zero <- any(tab == 0L)
    res <- cbe_exact2x2(tab, midp = has_zero, ...)
    return(tibble::tibble(
      p.value = res$p.value,
      statistic = res$estimate,
      method = res$method
    ))
  }

  # test == "auto" (default CBE hierarchy)
  # Rule 1 & 2: 2x2 Tables
  if (identical(as.integer(dims), c(2L, 2L))) {
    has_zero <- any(tab == 0L)
    res <- cbe_exact2x2(tab, midp = has_zero, ...)
    return(tibble::tibble(
      p.value = res$p.value,
      statistic = res$estimate,
      method = res$method
    ))
  }

  # Rule 3 & 4: General RxC Tables
  exp_counts <- tryCatch({
    suppressWarnings(stats::chisq.test(tab, correct = FALSE)$expected)
  }, error = function(e) {
    matrix(0, nrow = dims[1], ncol = dims[2])
  })

  if (test == "exact" || any(exp_counts < 5, na.rm = TRUE)) {
    # Rule 3: Sparse cells -> Fisher's exact test
    sim <- if (!is.null(simulate.p.value)) isTRUE(simulate.p.value) else (sum(tab) > 500L || any(dims > 2L))
    if (sim) {
      pval <- run_sim_fisher(tab, rep_b = B, ...)
      method_name <- if (any(exp_counts < 5, na.rm = TRUE)) {
        "Fisher's exact test (simulated, expected counts < 5)"
      } else {
        "Fisher's exact test (simulated)"
      }
    } else {
      ft <- stats::fisher.test(tab, ...)
      pval <- ft$p.value
      method_name <- if (any(exp_counts < 5, na.rm = TRUE)) {
        "Fisher's exact test (expected counts < 5)"
      } else {
        "Fisher's exact test"
      }
    }
    return(tibble::tibble(
      p.value = pval,
      method = method_name
    ))
  } else {
    # Rule 4: Adequate counts -> Pearson Chi-squared test
    cs <- stats::chisq.test(tab, correct = correct, ...)
    return(tibble::tibble(
      p.value = cs$p.value,
      statistic = unname(cs$statistic),
      parameter = unname(cs$parameter),
      method = "Pearson's Chi-squared test"
    ))
  }
}

