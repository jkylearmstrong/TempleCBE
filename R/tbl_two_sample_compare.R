#' Two-Group Comparison Table for Continuous Variables
#'
#' One row per variable comparing two groups, ready for a manuscript: group sizes, mean (SD) in each
#' group, the difference with its confidence interval, and a p-value.
#'
#' @section Difference and tests:
#' The difference is **first level minus second level** of `group_col` (the factor order, or sorted
#' values if it is not a factor). `"welch"` (default) is Welch's t-test (unequal variances) and
#' `"student"` the pooled-variance t-test; both give the mean difference and its confidence interval.
#' `"wilcoxon"` is the Wilcoxon rank-sum test; its difference is the Hodges-Lehmann estimate (a
#' shift in location, not a difference in means) with its confidence interval. The p-value is kept
#' when the estimate or interval cannot be computed (those are then blank), and it is computed even when
#' both groups are constant at different values. Each group's
#' `N` counts the non-missing values of that variable in that group. A variable with fewer than 2
#' values in a group, or constant in both, gets `NA` for the test.
#'
#' @param data A data frame.
#' @param vars Character vector of numeric columns.
#' @param group_col Name of the grouping column; it must have exactly two non-missing values.
#' @param test `"welch"`, `"student"` or `"wilcoxon"`.
#' @param conf_level Confidence level for the difference (default 0.95).
#' @param digits Decimals for means, SDs and the difference.
#' @param p_digits Decimals for the p-value (values below the last digit print as `<0.001`).
#' @return A tibble with `Variable`, `N (<group 1>)`, `Mean (SD) <group 1>`, `N (<group 2>)`,
#'   `Mean (SD) <group 2>`, `Difference [95\% CI]` and `p-value` (all text), and attributes `groups` (the two
#'   levels), `test` and `numeric` (the unformatted results as a tibble).
#' @seealso [table_two_by_two()], [compare_domain_datasets()]
#' @export
#' @examples
#' tbl_two_sample_compare(mtcars, c("mpg", "hp", "wt"), "am")
tbl_two_sample_compare <- function(data,
                                   vars,
                                   group_col,
                                   test = c("welch", "student", "wilcoxon"),
                                   conf_level = 0.95,
                                   digits = 2,
                                   p_digits = 3) {
  test <- match.arg(test)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.character(group_col) || length(group_col) != 1L || !group_col %in% names(data)) {
    stop("`group_col` must name one column of `data`.", call. = FALSE)
  }
  vars <- as.character(vars)
  if (length(vars) == 0L) stop("`vars` must name at least one column.", call. = FALSE)
  gone <- setdiff(vars, names(data))
  if (length(gone)) stop("Column(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  bad <- vars[!vapply(data[vars], is.numeric, logical(1))]
  if (length(bad)) stop("`vars` must be numeric: ", paste(bad, collapse = ", "), ".", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be between 0 and 1.", call. = FALSE)
  }

  g <- data[[group_col]]
  lv <- if (is.factor(g)) levels(droplevels(g[!is.na(g)])) else as.character(sort(unique(g[!is.na(g)])))
  if (length(lv) != 2L) {
    stop("`group_col` must have exactly two groups; it has ", length(lv), ".", call. = FALSE)
  }
  g <- as.character(g)

  fmt <- function(x) formatC(x, format = "f", digits = digits)
  fmt_p_val <- function(p) {
    if (is.na(p)) return("")
    lim <- 10^-p_digits
    if (p < lim) paste0("<", formatC(lim, format = "f", digits = p_digits)) else formatC(p, format = "f", digits = p_digits)
  }

  rows <- lapply(vars, function(v) {
    x1 <- data[[v]][!is.na(g) & g == lv[1] & !is.na(data[[v]])]
    x2 <- data[[v]][!is.na(g) & g == lv[2] & !is.na(data[[v]])]
    est <- lo <- hi <- p <- NA_real_
    enough <- length(x1) >= 2L && length(x2) >= 2L
    if (enough && test == "wilcoxon") {
      # the p-value does not need the interval, so it is computed on its own and kept if the interval fails
      if (length(unique(c(x1, x2))) > 1L) {
        w <- tryCatch(suppressWarnings(stats::wilcox.test(x1, x2)), error = function(e) NULL)
        if (!is.null(w)) p <- w$p.value
        wc <- tryCatch(suppressWarnings(stats::wilcox.test(x1, x2, conf.int = TRUE, conf.level = conf_level)),
                       error = function(e) NULL)
        if (!is.null(wc) && !is.null(wc$conf.int)) {
          est <- unname(wc$estimate)
          lo <- wc$conf.int[1]
          hi <- wc$conf.int[2]
        }
      }
    } else if (enough && !(stats::sd(x1) == 0 && stats::sd(x2) == 0)) {
      tt <- tryCatch(
        suppressWarnings(stats::t.test(x1, x2, var.equal = identical(test, "student"), conf.level = conf_level)),
        error = function(e) NULL
      )
      if (!is.null(tt)) {
        p <- tt$p.value
        est <- unname(tt$estimate[1] - tt$estimate[2])
        lo <- tt$conf.int[1]
        hi <- tt$conf.int[2]
      }
    }
    list(
      v = v, n1 = length(x1), m1 = mean(x1), s1 = if (length(x1) > 1L) stats::sd(x1) else NA_real_,
      n2 = length(x2), m2 = mean(x2), s2 = if (length(x2) > 1L) stats::sd(x2) else NA_real_,
      diff = est, lo = lo, hi = hi, p = p
    )
  })

  ms <- function(m, s, n) if (n == 0L) "" else if (is.na(s)) fmt(m) else sprintf("%s (%s)", fmt(m), fmt(s))
  ci_lbl <- sprintf("%s%%", format(100 * conf_level))
  num <- tibble::tibble(
    variable = vapply(rows, `[[`, "", "v"),
    n1 = vapply(rows, `[[`, 1L, "n1"), mean1 = vapply(rows, `[[`, 1, "m1"), sd1 = vapply(rows, `[[`, 1, "s1"),
    n2 = vapply(rows, `[[`, 1L, "n2"), mean2 = vapply(rows, `[[`, 1, "m2"), sd2 = vapply(rows, `[[`, 1, "s2"),
    difference = vapply(rows, `[[`, 1, "diff"), conf.low = vapply(rows, `[[`, 1, "lo"),
    conf.high = vapply(rows, `[[`, 1, "hi"), p.value = vapply(rows, `[[`, 1, "p")
  )
  out <- tibble::tibble(
    Variable = num$variable,
    N1 = as.character(num$n1),
    M1 = mapply(ms, num$mean1, num$sd1, num$n1),
    N2 = as.character(num$n2),
    M2 = mapply(ms, num$mean2, num$sd2, num$n2),
    D = ifelse(is.na(num$difference), "",
               ifelse(is.na(num$conf.low), fmt(num$difference),
                      sprintf("%s [%s, %s]", fmt(num$difference), fmt(num$conf.low), fmt(num$conf.high)))),
    P = vapply(num$p.value, fmt_p_val, "")
  )
  names(out) <- c("Variable", paste0("N (", lv[1], ")"), paste0("Mean (SD) ", lv[1]),
                  paste0("N (", lv[2], ")"), paste0("Mean (SD) ", lv[2]),
                  paste0("Difference [", ci_lbl, " CI]"), "p-value")
  attr(out, "groups") <- lv
  attr(out, "test") <- test
  attr(out, "numeric") <- num
  out
}
