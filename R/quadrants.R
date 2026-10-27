#' Assign Observations to Bivariate Quadrants
#'
#' Splits observations by a cut on each of two columns. With the default labels, `"I"` is
#' x and y both at or above their cuts, `"II"` x below and y at or above, `"III"` both below
#' and `"IV"` x at or above and y below (the mathematical numbering). A value exactly on a cut
#' belongs to the "at or above" side.
#'
#' @param data A data frame.
#' @param x_col,y_col Names of the two numeric columns.
#' @param x_cut,y_cut Cut values (default 0).
#' @param labels Four labels in the order above.
#' @param quadrant_col Name of the new factor column. Rows with a missing x or y get `NA`.
#' @return `data` as a tibble with the added factor column (all four levels kept).
#' @seealso [tbl_quadrant_distribution()], [quadrant_n()]
#' @export
#' @examples
#' d <- classify_quadrants(mtcars, "wt", "mpg", x_cut = 3.2, y_cut = 20)
#' table(d$quadrant)
classify_quadrants <- function(data, x_col, y_col, x_cut = 0, y_cut = 0,
                               labels = c("I", "II", "III", "IV"), quadrant_col = "quadrant") {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (v in list(x_col, y_col)) {
    if (length(v) != 1L || !is.character(v) || !v %in% names(data)) {
      stop("`", paste(v, collapse = ", "), "` is not a column of `data`.", call. = FALSE)
    }
    if (!is.numeric(data[[v]])) stop("`", v, "` must be numeric.", call. = FALSE)
  }
  for (cut in list(x_cut, y_cut)) {
    if (!is.numeric(cut) || length(cut) != 1L || is.na(cut)) stop("Cuts must be single numbers.", call. = FALSE)
  }
  if (length(labels) != 4L || anyDuplicated(labels) || anyNA(labels)) {
    stop("`labels` must be four distinct labels.", call. = FALSE)
  }
  if (!is.character(quadrant_col) || length(quadrant_col) != 1L) stop("`quadrant_col` must be a single name.", call. = FALSE)
  xh <- data[[x_col]] >= x_cut
  yh <- data[[y_col]] >= y_cut
  q <- ifelse(xh & yh, 1L, ifelse(!xh & yh, 2L, ifelse(!xh & !yh, 3L, 4L)))
  out <- tibble::as_tibble(data)
  out[[quadrant_col]] <- factor(labels[q], levels = labels)
  out
}

#' Distribution of Clinical Strata Across Quadrants
#'
#' Counts of each level of the stratifying variables within each quadrant.
#'
#' @param data A data frame with a quadrant column (see [classify_quadrants()]).
#' @param quadrant_col Name of the quadrant column.
#' @param strat_vars Names of the categorical (or logical) variables to count.
#' @param include_total Add a `Total` pseudo-variable with the number of observations per quadrant.
#' @return A long tibble with `quadrant`, `variable`, `level`, `n` and `pct` (percent of the
#'   quadrant's observations with a non-missing quadrant; missing values of a stratifying
#'   variable count in the denominator but have no row). Rows with a missing quadrant are dropped.
#' @seealso [classify_quadrants()], [quadrant_n()]
#' @export
#' @examples
#' d <- classify_quadrants(mtcars, "wt", "mpg", x_cut = 3.2, y_cut = 20)
#' tbl_quadrant_distribution(d, strat_vars = c("am", "vs"))
tbl_quadrant_distribution <- function(data, quadrant_col = "quadrant", strat_vars,
                                      include_total = TRUE) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (missing(strat_vars) || length(strat_vars) == 0L) stop("`strat_vars` must name at least one column.", call. = FALSE)
  gone <- setdiff(c(quadrant_col, strat_vars), names(data))
  if (length(gone)) stop("Column(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  q <- as.factor(data[[quadrant_col]])
  ok <- !is.na(q)
  qn <- as.integer(table(q[ok]))
  names(qn) <- levels(q)
  rows <- lapply(strat_vars, function(v) {
    x <- data[[v]][ok]
    f <- if (is.factor(x)) x else factor(x)
    tab <- table(quadrant = q[ok], level = f)
    df <- as.data.frame(tab, stringsAsFactors = FALSE)
    tibble::tibble(quadrant = df$quadrant, variable = v, level = df$level, n = as.integer(df$Freq))
  })
  if (include_total) {
    rows <- c(rows, list(tibble::tibble(quadrant = names(qn), variable = "Total", level = "Total", n = unname(qn))))
  }
  out <- dplyr::bind_rows(rows)
  out$pct <- ifelse(qn[out$quadrant] > 0, 100 * out$n / qn[out$quadrant], NA_real_)
  out$quadrant <- factor(out$quadrant, levels = levels(q))
  out[order(out$quadrant, match(out$variable, c(strat_vars, "Total"))), , drop = FALSE]
}

#' One Quadrant Count for Inline Prose
#'
#' @param tbl_quad Result of [tbl_quadrant_distribution()].
#' @param quadrant Quadrant label.
#' @param col A `level` of the table (for example `"Died"`) or `"Total"`. A level that occurs in
#'   more than one variable is an error.
#' @return The count as an integer (`0` if the level exists but not in that quadrant).
#' @seealso [tbl_quadrant_distribution()]
#' @export
#' @examples
#' d <- classify_quadrants(mtcars, "wt", "mpg", x_cut = 3.2, y_cut = 20)
#' quadrant_n(tbl_quadrant_distribution(d, strat_vars = "am"), "I", "Total")
quadrant_n <- function(tbl_quad, quadrant, col) {
  if (!is.data.frame(tbl_quad) || !all(c("quadrant", "variable", "level", "n") %in% names(tbl_quad))) {
    stop("`tbl_quad` must come from tbl_quadrant_distribution().", call. = FALSE)
  }
  if (length(quadrant) != 1L || !as.character(quadrant) %in% as.character(tbl_quad$quadrant)) {
    stop("Unknown quadrant: ", paste(quadrant, collapse = ", "), ".", call. = FALSE)
  }
  if (length(col) != 1L || !col %in% tbl_quad$level) {
    stop("`col` must be one of: ", paste(unique(tbl_quad$level), collapse = ", "), ".", call. = FALSE)
  }
  vars <- unique(tbl_quad$variable[tbl_quad$level == col])
  if (length(vars) > 1L) {
    stop("`col` = \"", col, "\" occurs in several variables (", paste(vars, collapse = ", "), ").", call. = FALSE)
  }
  as.integer(sum(tbl_quad$n[as.character(tbl_quad$quadrant) == as.character(quadrant) & tbl_quad$level == col]))
}
