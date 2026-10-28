#' Missingness Patterns Table
#'
#' Counts the distinct combinations of missing and observed values across columns, one row per
#' pattern, most common first. It is the tabular companion to [plot_comissingness_matrix()] and a
#' pre-flight check before multiple imputation (see [pool_imputations()]): it shows which columns
#' tend to be missing together and how many rows are complete.
#'
#' @param data A data frame.
#' @param vars Columns to use (default all).
#' @return A tibble with one logical column per variable (`TRUE` = missing), then `n_missing_vars`,
#'   `n` (rows with the pattern) and `percent` (of all rows).
#' @seealso [plot_comissingness_matrix()], [features_percent_miss()]
#' @export
#' @examples
#' d <- data.frame(a = c(1, NA, 3, NA, 5, 6), b = c(1, NA, 3, NA, 5, NA), c = c(NA, 2, 3, 4, 5, 6))
#' tbl_missingness_patterns(d)
tbl_missingness_patterns <- function(data, vars = NULL) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (is.null(vars)) vars <- names(data)
  gone <- setdiff(vars, names(data))
  if (length(gone)) stop("Column(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  if (length(vars) == 0L) stop("No columns to summarise.", call. = FALSE)
  if (nrow(data) == 0L) stop("`data` has no rows.", call. = FALSE)
  miss <- as.data.frame(lapply(data[vars], is.na), stringsAsFactors = FALSE)
  names(miss) <- vars
  key <- do.call(paste, c(lapply(miss, as.integer), sep = ""))
  counts <- table(key)
  first_row <- match(names(counts), key)
  out <- tibble::as_tibble(miss[first_row, , drop = FALSE])
  out$n_missing_vars <- as.integer(rowSums(miss[first_row, , drop = FALSE]))
  out$n <- as.integer(counts)
  out$percent <- 100 * out$n / nrow(data)
  out[order(-out$n, out$n_missing_vars), , drop = FALSE]
}
