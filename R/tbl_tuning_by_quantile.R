#' Tuning Results Summarised by Performance Quantile Tier
#'
#' Bins tuning candidates into tiers by a performance metric (tier 1 is the best) and summarises
#' each tier: the number of candidates, the metric range, the mean penalty (`lambda`) and mixing
#' parameter (`alpha`), or how often each model term occurs in the candidates of the tier.
#'
#' @param tuning_results A data frame of candidates, or anything [as_tuning_results()] accepts.
#'   Needs the `metric` column; `lambda`, `alpha` and `formula` are used when present.
#' @param metric Name of the performance column (default `"IBS"`).
#' @param n_tiles Number of tiers (quantile groups of equal size, up to ties).
#' @param lower_is_better `TRUE` when a smaller metric is better (IBS, error); `FALSE` otherwise.
#' @param what `"summary"` (one row per tier) or `"terms"` (one row per tier and model term, with
#'   the number and share of the tier's candidates that contain it; needs a `formula` column).
#' @return A tibble. `"summary"`: `tier`, `n_models`, `metric_min`, `metric_mean`, `metric_max`,
#'   `mean_lambda`, `mean_alpha` (the last two `NA` when absent). `"terms"`: `tier`, `term`,
#'   `n_with_term`, `n_models`, `retention`.
#' @seealso [as_tuning_results()], [compare_tuning_runs()], [select_candidate_models()]
#' @export
#' @examples
#' set.seed(1)
#' tr <- data.frame(IBS = runif(50, 0.1, 0.3), alpha = runif(50), lambda = 10^runif(50, -3, 0),
#'                  formula = sample(c("~ a + b", "~ a + c", "~ b + c"), 50, TRUE))
#' tbl_tuning_by_quantile(tr, n_tiles = 4)
#' tbl_tuning_by_quantile(tr, n_tiles = 4, what = "terms")
tbl_tuning_by_quantile <- function(tuning_results, metric = "IBS", n_tiles = 5,
                                   lower_is_better = TRUE, what = c("summary", "terms")) {
  what <- match.arg(what)
  if (!is.numeric(n_tiles) || length(n_tiles) != 1L || n_tiles < 2 || n_tiles != round(n_tiles)) {
    stop("`n_tiles` must be a whole number of at least 2.", call. = FALSE)
  }
  x <- tuning_results
  if (!(is.data.frame(x) && metric %in% names(x))) x <- as_tuning_results(x)
  if (!metric %in% names(x)) stop("`", metric, "` is not a column of `tuning_results`.", call. = FALSE)
  x <- tibble::as_tibble(x)
  x <- x[!is.na(x[[metric]]), , drop = FALSE]
  if (nrow(x) < n_tiles) stop("Fewer candidates (", nrow(x), ") than tiers (", n_tiles, ").", call. = FALSE)
  # rank 1 = best; tiers of (almost) equal size
  rk <- rank(if (lower_is_better) x[[metric]] else -x[[metric]], ties.method = "first")
  x$tier <- as.integer(ceiling(rk * n_tiles / nrow(x)))

  if (what == "terms") {
    if (!"formula" %in% names(x)) stop("`what = \"terms\"` needs a `formula` column.", call. = FALSE)
    labs <- lapply(as.character(x$formula), function(f) {
      attr(stats::terms(stats::as.formula(if (grepl("~", f, fixed = TRUE)) f else paste("~", f))), "term.labels")
    })
    n_by_tier <- table(factor(x$tier, levels = seq_len(n_tiles)))
    long <- tibble::tibble(tier = rep(x$tier, lengths(labs)), term = unlist(labs))
    if (nrow(long) == 0L) return(tibble::tibble(tier = integer(), term = character(), n_with_term = integer(),
                                                n_models = integer(), retention = numeric()))
    out <- dplyr::count(long, .data$tier, .data$term, name = "n_with_term")
    out$n_models <- as.integer(n_by_tier[as.character(out$tier)])
    out$retention <- out$n_with_term / out$n_models
    return(out[order(out$tier, -out$retention, out$term), , drop = FALSE])
  }

  col_mean <- function(v, i) if (v %in% names(x)) mean(x[[v]][i], na.rm = TRUE) else NA_real_
  rows <- lapply(seq_len(n_tiles), function(t) {
    i <- which(x$tier == t)
    tibble::tibble(
      tier = t, n_models = length(i),
      metric_min = min(x[[metric]][i]), metric_mean = mean(x[[metric]][i]), metric_max = max(x[[metric]][i]),
      mean_lambda = col_mean("lambda", i), mean_alpha = col_mean("alpha", i)
    )
  })
  dplyr::bind_rows(rows)
}
