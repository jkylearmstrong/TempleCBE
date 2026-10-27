#' Baseline-Anchored Longitudinal Composite Index
#'
#' Builds a composite score for every subject and time point. Each variable is standardised with
#' the **baseline** mean and SD (at `baseline_time`, across subjects), multiplied by its polarity
#' (`alpha`, +1 if a higher value means worse/more, -1 if lower does), and the score is the mean
#' of the available standardised variables:
#' `index = mean_i( alpha_i * (x_i - mean0_i) / sd0_i )`. At baseline each variable has mean 0,
#' so the index has mean 0 at baseline and a change reads as standard deviations of the baseline
#' distribution. For one time point (cross-sectional data) use [compute_composite_index()].
#'
#' @param data A data frame in long format.
#' @param variables Numeric columns that contribute.
#' @param index_map Optional data frame with columns `variable` and `alpha` (+1 or -1); variables
#'   not listed get +1.
#' @param id_col,time_col Subject and time columns.
#' @param baseline_time Time that defines the baseline rows.
#' @param index_name Name of the new column.
#' @param min_vars Smallest number of non-missing standardised variables a row needs to get a
#'   score (default 1); otherwise `NA`.
#' @return `data` as a tibble with the index column added. The attribute `baseline` has one row
#'   per variable (`variable`, `alpha`, `mean0`, `sd0`, `n0`). A variable whose baseline SD is 0
#'   or undefined (fewer than two baseline values) is left out with a warning.
#' @seealso [compute_composite_index()]
#' @export
#' @examples
#' set.seed(1)
#' d <- expand.grid(id = 1:20, time = c(0, 5, 15))
#' d$a <- rnorm(60, 10 + d$time / 5, 2)
#' d$b <- rnorm(60, 50 - d$time, 5)
#' out <- compute_longitudinal_composite_index(d, c("a", "b"), data.frame(variable = "b", alpha = -1),
#'                                             id_col = "id")
#' tapply(out$composite_index, out$time, mean)
compute_longitudinal_composite_index <- function(data, variables, index_map = NULL, id_col = "subject_id",
                                                 time_col = "time", baseline_time = 0,
                                                 index_name = "composite_index", min_vars = 1L) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (v in c(id_col, time_col, variables)) {
    if (!v %in% names(data)) stop("`", v, "` is not a column of `data`.", call. = FALSE)
  }
  nonnum <- variables[!vapply(data[variables], is.numeric, logical(1))]
  if (length(nonnum) || !is.numeric(data[[time_col]])) stop("`variables` and `time_col` must be numeric.", call. = FALSE)
  if (length(variables) == 0L) stop("`variables` must name at least one column.", call. = FALSE)
  if (!is.character(index_name) || length(index_name) != 1L) stop("`index_name` must be a single name.", call. = FALSE)
  alpha <- stats::setNames(rep(1, length(variables)), variables)
  if (!is.null(index_map)) {
    if (!is.data.frame(index_map) || !all(c("variable", "alpha") %in% names(index_map))) {
      stop("`index_map` needs columns `variable` and `alpha`.", call. = FALSE)
    }
    if (!all(index_map$alpha %in% c(-1, 1))) stop("`alpha` must be +1 or -1.", call. = FALSE)
    unknown <- setdiff(index_map$variable, variables)
    if (length(unknown)) stop("`index_map` names variable(s) not in `variables`: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
    alpha[index_map$variable] <- index_map$alpha
  }
  base <- data[!is.na(data[[time_col]]) & data[[time_col]] == baseline_time, , drop = FALSE]
  if (nrow(base) == 0L) stop("No rows at `baseline_time` = ", baseline_time, ".", call. = FALSE)
  if (anyDuplicated(base[[id_col]])) {
    stop("Some subjects have more than one baseline row; average them first.", call. = FALSE)
  }

  stats0 <- tibble::tibble(
    variable = variables, alpha = unname(alpha[variables]),
    mean0 = unname(vapply(variables, function(v) mean(base[[v]], na.rm = TRUE), numeric(1))),
    sd0 = unname(vapply(variables, function(v) stats::sd(base[[v]], na.rm = TRUE), numeric(1))),
    n0 = unname(vapply(variables, function(v) sum(!is.na(base[[v]])), integer(1)))
  )
  usable <- is.finite(stats0$sd0) & stats0$sd0 > 0
  if (any(!usable)) {
    warning("Left out (baseline SD is 0 or undefined): ", paste(stats0$variable[!usable], collapse = ", "), call. = FALSE)
  }
  stats0 <- stats0[usable, , drop = FALSE]
  if (nrow(stats0) == 0L) stop("No variable has a usable baseline SD.", call. = FALSE)

  z <- vapply(seq_len(nrow(stats0)), function(i) {
    stats0$alpha[i] * (data[[stats0$variable[i]]] - stats0$mean0[i]) / stats0$sd0[i]
  }, numeric(nrow(data)))
  z <- matrix(z, nrow = nrow(data))
  n_ok <- rowSums(!is.na(z))
  idx <- rowSums(z, na.rm = TRUE) / pmax(n_ok, 1L)
  idx[n_ok < max(1L, min_vars)] <- NA_real_
  out <- tibble::as_tibble(data)
  out[[index_name]] <- idx
  attr(out, "baseline") <- stats0
  out
}
