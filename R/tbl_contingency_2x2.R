#' Publication-Ready 2x2 Contingency Table with Tests and Risk Measures
#'
#' Cross-tabulates two binary variables as `n (percent)` cells with marginal totals, an exact
#' and/or chi-squared test, the odds ratio and relative risk with confidence intervals, and the
#' standard table footnote. For the institutional layout with a conditional-MLE odds ratio and
#' mid-p handling use [table_two_by_two()].
#'
#' @section Risk measures:
#' The first level of `row_var` is the exposed group and the first level of `col_var` is the
#' outcome. The odds ratio is `(a * d) / (b * c)` and the relative risk is
#' `(a / (a + b)) / (c / (c + d))`, with Wald (log-scale) intervals. If any cell is zero, 0.5 is
#' added to every cell for these two measures only (the counts and percentages are unchanged).
#' Factor levels set the order; otherwise levels are sorted.
#'
#' @param data A data frame.
#' @param row_var,col_var Names of the row and column variables. Each must have exactly two
#'   observed levels after rows with a missing value in either are dropped.
#' @param percent Denominator of the cell percentages: `"row"`, `"col"` or `"total"`.
#' @param test `"fisher"`, `"chisq"` (Pearson, no continuity correction) or `"both"`.
#' @param include_risk Add the odds ratio and relative risk.
#' @param include_total Add the marginal totals.
#' @param digits Decimals of the percentages.
#' @param p_digits Decimals of the p-values (smaller values print as `<0.001`).
#' @param note Build the footnote text.
#' @param conf.level Confidence level of the risk intervals.
#' @return A list with `table` (tibble of formatted cells), `counts` (the 2x2 `table`),
#'   `tests` (tibble: `test`, `p_value`, `p_formatted`), `risk` (tibble: `measure`, `estimate`,
#'   `lower`, `upper`; `NULL` if `include_risk = FALSE`), `note` (string or `NULL`) and `n`
#'   (rows used).
#' @seealso [table_two_by_two()], [tbl_table_one()]
#' @export
#' @examples
#' res <- tbl_contingency_2x2(mtcars, "am", "vs", test = "both")
#' res$table
#' res$risk
#' res$note
tbl_contingency_2x2 <- function(data, row_var, col_var,
                                percent = c("row", "col", "total"),
                                test = c("fisher", "chisq", "both"),
                                include_risk = TRUE, include_total = TRUE,
                                digits = 1, p_digits = 3, note = TRUE,
                                conf.level = 0.95) {
  percent <- match.arg(percent)
  test <- match.arg(test)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (v in list(row_var, col_var)) {
    if (length(v) != 1L || !is.character(v) || !v %in% names(data)) {
      stop("`", paste(v, collapse = ", "), "` is not a column of `data`.", call. = FALSE)
    }
  }
  if (!is.numeric(conf.level) || length(conf.level) != 1L || conf.level <= 0 || conf.level >= 1) {
    stop("`conf.level` must be a single number between 0 and 1.", call. = FALSE)
  }
  keep <- stats::complete.cases(data[, c(row_var, col_var)])
  r <- droplevels(as.factor(data[[row_var]][keep]))
  cl <- droplevels(as.factor(data[[col_var]][keep]))
  if (nlevels(r) != 2L || nlevels(cl) != 2L) {
    stop("`row_var` and `col_var` must each have exactly two observed levels (found ",
         nlevels(r), " and ", nlevels(cl), ").", call. = FALSE)
  }
  tab <- table(r, cl, dnn = c(row_var, col_var))
  n <- sum(tab)

  cell <- function(k, d) sprintf("%d (%.*f%%)", k, digits, if (d > 0) 100 * k / d else NA_real_)
  den_of <- function(i, j) switch(percent, row = sum(tab[i, ]), col = sum(tab[, j]), total = n)
  out <- tibble::tibble(!!row_var := rownames(tab))
  for (j in 1:2) {
    out[[colnames(tab)[j]]] <- vapply(1:2, function(i) cell(tab[i, j], den_of(i, j)), "")
  }
  if (include_total) {
    out$Total <- vapply(1:2, function(i) cell(sum(tab[i, ]), if (percent == "col") n else sum(tab[i, ])), "")
    tot <- c(
      "Total",
      vapply(1:2, function(j) cell(sum(tab[, j]), if (percent == "row") n else sum(tab[, j])), ""),
      cell(n, n)
    )
    out <- rbind(out, stats::setNames(as.list(tot), names(out)))
  }

  fmt_p <- function(p) {
    if (is.na(p)) return(NA_character_)
    if (p < 10^-p_digits) paste0("<", formatC(10^-p_digits, format = "f", digits = p_digits)) else formatC(p, format = "f", digits = p_digits)
  }
  tests <- tibble::tibble(test = character(), p_value = numeric())
  if (test %in% c("fisher", "both")) {
    tests <- rbind(tests, tibble::tibble(test = "Fisher's exact test", p_value = stats::fisher.test(tab)$p.value))
  }
  if (test %in% c("chisq", "both")) {
    p <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$p.value)
    tests <- rbind(tests, tibble::tibble(test = "Pearson's chi-squared test", p_value = p))
  }
  tests$p_formatted <- vapply(tests$p_value, fmt_p, "")

  risk <- NULL
  if (include_risk) {
    t2 <- if (any(tab == 0)) tab + 0.5 else tab
    z <- stats::qnorm(1 - (1 - conf.level) / 2)
    a <- t2[1, 1]; b <- t2[1, 2]; c <- t2[2, 1]; d <- t2[2, 2]
    lor <- log(a * d / (b * c)); se_or <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
    lrr <- log((a / (a + b)) / (c / (c + d))); se_rr <- sqrt(1 / a - 1 / (a + b) + 1 / c - 1 / (c + d))
    risk <- tibble::tibble(
      measure = c("Odds ratio", "Relative risk"),
      estimate = exp(c(lor, lrr)),
      lower = exp(c(lor, lrr) - z * c(se_or, se_rr)),
      upper = exp(c(lor, lrr) + z * c(se_or, se_rr))
    )
  }

  txt <- NULL
  if (note) {
    pct_word <- switch(percent, row = "row %", col = "column %", total = "total %")
    tests_txt <- paste(sprintf("%s, p = %s", tests$test, tests$p_formatted), collapse = "; ")
    txt <- sprintf("n = %d. Cells are n (%s). %s.", n, pct_word, tests_txt)
  }
  list(table = out, counts = tab, tests = tests, risk = risk, note = txt, n = n)
}
