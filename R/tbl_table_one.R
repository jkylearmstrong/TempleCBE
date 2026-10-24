#' Stratified Baseline "Table 1"
#'
#' A manuscript baseline table with an overall column, one column per stratum and an omnibus test
#' p-value, built on [gtsummary::tbl_summary()] so the result can be styled and exported like any
#' other `gtsummary` table (`gtsummary::as_flex_table()`, `as_gt()`, ...).
#' * categorical variables: `n (%)`;
#' * continuous variables: `mean (SD)`, or `median [Q1, Q3]` for those in `non_normal_vars`;
#' * p-values: Welch's t-test or Welch's one-way ANOVA for the mean (SD) variables, Wilcoxon or
#'   Kruskal-Wallis for the median variables, chi-squared or Fisher's exact test for categorical ones
#'   (`gtsummary` chooses by the expected counts).
#'
#' @param data A data frame.
#' @param strata Name of the grouping column (at least two levels in use). `NULL` gives an
#'   unstratified table without p-values.
#' @param vars Columns to describe; default all columns except `strata`.
#' @param non_normal_vars Continuous variables shown as median [Q1, Q3] and tested with a rank test.
#' @param test Add the p-value column (default `TRUE`; ignored without `strata`).
#' @param overall Add the overall column (default `TRUE`).
#' @param digits Decimals for continuous statistics.
#' @param p_digits Decimals for p-values.
#' @param missing Passed to [gtsummary::tbl_summary()] (`"ifany"` default).
#' @return A `gtsummary` table.
#' @seealso [tbl_two_sample_compare()], [summary_table()]
#' @export
#' @examples
#' d <- mtcars
#' d$cyl <- factor(d$cyl)
#' tbl_table_one(d, strata = "cyl", vars = c("mpg", "hp", "am"), non_normal_vars = "hp")
tbl_table_one <- function(data,
                          strata = NULL,
                          vars = NULL,
                          non_normal_vars = NULL,
                          test = TRUE,
                          overall = TRUE,
                          digits = 2,
                          p_digits = 3,
                          missing = "ifany") {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.null(strata) && (!is.character(strata) || length(strata) != 1L || !strata %in% names(data))) {
    stop("`strata` must name one column of `data`.", call. = FALSE)
  }
  if (is.null(vars)) vars <- setdiff(names(data), strata)
  vars <- setdiff(as.character(vars), strata)
  gone <- setdiff(c(vars, non_normal_vars), names(data))
  if (length(gone)) stop("Column(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  if (length(vars) == 0L) stop("No variables to describe.", call. = FALSE)
  if (!is.null(strata)) {
    data <- data[!is.na(data[[strata]]), , drop = FALSE]
    data[[strata]] <- droplevels(as.factor(data[[strata]]))
    if (nlevels(data[[strata]]) < 2L) stop("`strata` needs at least two levels with data.", call. = FALSE)
  }
  data <- data[unique(c(vars, strata))]

  is_cont <- vapply(data[vars], function(x) is.numeric(x) && length(unique(stats::na.omit(x))) > 2L, logical(1))
  nn <- intersect(non_normal_vars, vars)
  stat <- list(gtsummary::all_categorical() ~ "{n} ({p}%)")
  stat <- c(list(gtsummary::all_continuous() ~ "{mean} ({sd})"),
            if (length(nn)) list(stats::as.formula(paste0("c(", paste(sprintf("`%s`", nn), collapse = ", "), ") ~ \"{median} [{p25}, {p75}]\""))),
            stat)

  tbl <- gtsummary::tbl_summary(
    data, by = if (is.null(strata)) NULL else dplyr::all_of(strata), include = dplyr::all_of(vars),
    type = if (any(!is_cont & vapply(data[vars], is.numeric, logical(1)))) {
      stats::setNames(rep(list("categorical"), sum(!is_cont & vapply(data[vars], is.numeric, logical(1)))),
                      names(which(!is_cont & vapply(data[vars], is.numeric, logical(1)))))
    } else NULL,
    statistic = stat, digits = gtsummary::all_continuous() ~ digits, missing = missing
  )
  if (is.null(strata)) return(tbl)
  if (isTRUE(overall)) tbl <- gtsummary::add_overall(tbl)
  if (isTRUE(test)) {
    two <- nlevels(data[[strata]]) == 2L
    cont <- names(which(is_cont))
    tests <- stats::setNames(
      lapply(cont, function(v) if (v %in% nn) { if (two) "wilcox.test" else "kruskal.test" } else { if (two) "t.test" else "oneway.test" }),
      cont
    )
    tbl <- gtsummary::add_p(tbl, test = tests, pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = p_digits))
  }
  tbl
}
