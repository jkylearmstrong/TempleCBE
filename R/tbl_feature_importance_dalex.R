#' Table of DALEX Permutation Feature Importance
#'
#' Summarises the dropout loss of each variable over the permutations of a
#' [DALEX::feature_importance()] (or `model_parts()`) result: minimum, first quartile, median,
#' mean, third quartile and maximum, ranked by the chosen statistic. Replaces the hand-rolled
#' aggregation that was repeated in the feature-selection child documents.
#'
#' @param x A `feature_importance_explainer` object or a data frame with the columns `variable`,
#'   `permutation` and `dropout_loss` (and optionally `label`).
#' @param rank_by Statistic to rank by, one of `"mean"` (default), `"median"`, `"min"`, `"q1"`,
#'   `"q3"`, `"max"`. Larger dropout loss means more important.
#' @param top_n Keep only the `top_n` highest ranked variables (default all).
#' @param keep_reference Keep the `_baseline_` and `_full_model_` rows (default `FALSE`).
#' @param id Optional named list of constant columns put in front, e.g. `list(index = "A")`, to
#'   stack the tables of several models with [dplyr::bind_rows()].
#' @return A tibble with `rank`, `variable`, `label` (if present), `dropout_loss` (the
#'   unpermuted, `permutation == 0` value, if present) and the six summary columns.
#' @seealso [calc_grouped_summary()]
#' @export
#' @examples
#' fi <- data.frame(
#'   variable = rep(c("_full_model_", "a", "b", "_baseline_"), each = 4),
#'   permutation = rep(0:3, 4),
#'   dropout_loss = c(rep(0.2, 4), 0.30, 0.31, 0.29, 0.33, 0.22, 0.21, 0.23, 0.22, rep(0.5, 4)),
#'   label = "model"
#' )
#' tbl_feature_importance_dalex(fi)
tbl_feature_importance_dalex <- function(x,
                                         rank_by = c("mean", "median", "min", "q1", "q3", "max"),
                                         top_n = NULL,
                                         keep_reference = FALSE,
                                         id = NULL) {
  rank_by <- match.arg(rank_by)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  need <- setdiff(c("variable", "permutation", "dropout_loss"), names(x))
  if (length(need)) stop("`x` lacks column(s): ", paste(need, collapse = ", "), ".", call. = FALSE)
  if (!is.null(top_n) && (!is.numeric(top_n) || length(top_n) != 1L || top_n < 1)) {
    stop("`top_n` must be a single positive number.", call. = FALSE)
  }
  if (!is.null(id) && (!is.list(id) || is.null(names(id)) || any(!nzchar(names(id))))) {
    stop("`id` must be a named list.", call. = FALSE)
  }
  x$variable <- as.character(x$variable)
  if (!isTRUE(keep_reference)) x <- x[!x$variable %in% c("_baseline_", "_full_model_"), , drop = FALSE]
  if (nrow(x) == 0L) stop("No variables left in `x`.", call. = FALSE)

  vars <- unique(x$variable)
  rows <- lapply(vars, function(v) {
    d <- x[x$variable == v, , drop = FALSE]
    l <- d$dropout_loss
    allna <- all(is.na(l))
    q <- stats::quantile(l, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
    ref <- d$dropout_loss[d$permutation == 0]
    out <- data.frame(variable = v, stringsAsFactors = FALSE)
    if ("label" %in% names(d)) out$label <- as.character(d$label[1L])
    if (length(ref)) out$dropout_loss <- ref[1L]
    out$min <- if (allna) NA_real_ else min(l, na.rm = TRUE)
    out$q1 <- q[1L]
    out$median <- stats::median(l, na.rm = TRUE)
    out$mean <- mean(l, na.rm = TRUE)
    out$q3 <- q[2L]
    out$max <- if (allna) NA_real_ else max(l, na.rm = TRUE)
    out
  })
  res <- dplyr::bind_rows(rows)
  res <- res[order(-res[[rank_by]], res$variable), , drop = FALSE]
  res$rank <- seq_len(nrow(res))
  res <- res[c("rank", setdiff(names(res), "rank"))]
  if (!is.null(top_n)) res <- res[res$rank <= top_n, , drop = FALSE]
  res <- tibble::as_tibble(res)
  if (!is.null(id)) res <- dplyr::bind_cols(tibble::as_tibble(id), res)
  res
}
