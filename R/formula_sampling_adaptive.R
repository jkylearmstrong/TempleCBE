#' Sample Candidate Formulas Conditioned on Earlier Tuning Results
#'
#' The second stage of a two-stage formula search. Stage one is [sample_candidate_formulas()] scored
#' by a tuning run; this function learns from that run and draws new formulas:
#' 1. the best models are kept (`top_quantile` of the rows by `metric`);
#' 2. the number of terms of those models gives the size range of the new formulas (the 25th
#'    percentile to the median of the winners' sizes);
#' 3. terms that appear more often among the winners are drawn more often (their share among the winners,
#'    plus a small floor so that no term is shut out);
#' 4. optionally, terms whose effect points the wrong way are dropped (see `effects`).
#'
#' Term and size choices come from the rows given, so pass results from the **training**
#' folds only if the new formulas are to be assessed on held-out data (see the data-boundary note of
#' [sample_candidate_formulas()]).
#'
#' @param prior_results_df A data frame of earlier results with a formula column (text or formula
#'   objects) and a `metric` column, such as the output of [as_tuning_results()].
#' @param metric Name of the performance column (default `"IBS"`).
#' @param lower_is_better `TRUE` (default; IBS, Brier, AIC) or `FALSE` (concordance, AUC).
#' @param top_quantile Share of rows kept as winners, between 0 and 1 (default 0.25).
#' @param filter_direction If `TRUE` (default) and `effects` is given, terms with an unfavourable
#'   effect are dropped from the pool. Without `effects` there is nothing to filter on and this is ignored.
#' @param effects Optional data frame with columns `term` and `estimate`: the univariate effect of
#'   each term (for example from [cox_single()] or [glm_single()]).
#' @param effect_scale `"ratio"` (default; hazard or odds ratios, favourable when above 1) or
#'   `"coef"` (coefficients, favourable when above 0).
#' @param n_formulas Number of distinct formulas wanted.
#' @param formula_col Name of the formula column (default `"formula"`).
#' @param response Left-hand side as text; by default the one of the first formula.
#' @param required Terms that are in every formula.
#' @param seed Optional integer; see [sample_candidate_formulas()].
#' @param env Environment of the formulas.
#' @param data Optional data frame (training rows): winning terms that are zero-variance columns
#'   of it are dropped from the pool unless `required`; see [sample_candidate_formulas()].
#' @return The tibble of [sample_candidate_formulas()]; the pool, size range and weights used are in
#'   the attribute `"adaptive"`.
#' @seealso [sample_candidate_formulas()], [as_tuning_results()]
#' @export
#' @examples
#' prior <- data.frame(
#'   formula = c("y ~ a + b", "y ~ a + c", "y ~ a + b + c", "y ~ d", "y ~ d + e", "y ~ e"),
#'   IBS = c(0.10, 0.11, 0.12, 0.20, 0.22, 0.25)
#' )
#' sample_candidate_formulas_adaptive(prior, top_quantile = 0.5, n_formulas = 3, seed = 1)
sample_candidate_formulas_adaptive <- function(prior_results_df,
                                               metric = "IBS",
                                               lower_is_better = TRUE,
                                               top_quantile = 0.25,
                                               filter_direction = TRUE,
                                               effects = NULL,
                                               effect_scale = c("ratio", "coef"),
                                               n_formulas = 5L,
                                               formula_col = "formula",
                                               response = NULL,
                                               required = character(),
                                               seed = NULL,
                                               env = parent.frame(),
                                               data = NULL) {
  effect_scale <- match.arg(effect_scale)
  if (!is.data.frame(prior_results_df) || nrow(prior_results_df) == 0L) {
    stop("`prior_results_df` must be a data frame with at least one row.", call. = FALSE)
  }
  for (col in c(metric, formula_col)) {
    if (!is.character(col) || length(col) != 1L || !col %in% names(prior_results_df)) {
      stop("Column `", col, "` is not in `prior_results_df`.", call. = FALSE)
    }
  }
  if (!is.numeric(top_quantile) || length(top_quantile) != 1L || is.na(top_quantile) ||
      top_quantile <= 0 || top_quantile > 1) {
    stop("`top_quantile` must be a number above 0 and at most 1.", call. = FALSE)
  }
  score <- suppressWarnings(as.numeric(prior_results_df[[metric]]))
  fml <- prior_results_df[[formula_col]]
  ok <- !is.na(score) & vapply(fml, function(f) inherits(f, "formula") || (length(f) > 0L && !all(is.na(f))), logical(1))
  if (!any(ok)) stop("No row has both a `", metric, "` value and a formula.", call. = FALSE)
  score <- score[ok]
  fml <- fml[ok]

  if (!lower_is_better) score <- -score
  k <- max(1L, ceiling(top_quantile * length(score)))
  keep <- order(score)[seq_len(k)]
  # ties at the cut-off stay in
  keep <- which(score <= score[keep[k]])

  parsed <- lapply(fml[keep], function(f) {
    f <- if (inherits(f, "formula")) f else stats::as.formula(as.character(f)[1L], env = env)
    list(terms = attr(stats::terms(f), "term.labels"), lhs = if (length(f) == 3L) deparse(f[[2L]], width.cutoff = 500L) else NULL)
  })
  win_terms <- lapply(parsed, `[[`, "terms")
  sizes <- lengths(win_terms)
  if (is.null(response)) response <- parsed[[1L]]$lhs
  if (all(sizes == 0L)) stop("The winning formulas have no terms.", call. = FALSE)

  min_size <- max(1L, as.integer(floor(stats::quantile(sizes, 0.25, names = FALSE))))
  max_size <- max(min_size, as.integer(ceiling(stats::median(sizes))))

  pool <- unique(unlist(win_terms))
  counts <- vapply(pool, function(tm) sum(vapply(win_terms, function(w) tm %in% w, logical(1))), numeric(1))
  share <- counts / length(win_terms)

  if (isTRUE(filter_direction) && !is.null(effects)) {
    if (!is.data.frame(effects) || !all(c("term", "estimate") %in% names(effects))) {
      stop("`effects` must be a data frame with columns `term` and `estimate`.", call. = FALSE)
    }
    cut <- if (effect_scale == "ratio") 1 else 0
    bad <- effects$term[!is.na(effects$estimate) & effects$estimate <= cut]
    drop <- setdiff(intersect(pool, bad), required)
    if (length(drop) == length(setdiff(pool, required))) {
      warning("Every candidate term has an unfavourable effect; the direction filter was not applied.", call. = FALSE)
    } else {
      share <- share[!pool %in% drop]
      pool <- pool[!pool %in% drop]
    }
  }

  if (!is.null(data)) {
    zero <- setdiff(.zero_variance_terms(data, pool), required)
    if (length(zero) && length(zero) < length(setdiff(pool, required))) {
      message("Zero-variance terms kept out of the candidates: ", paste(zero, collapse = ", "), ".")
      share <- share[!pool %in% zero]
      pool <- pool[!pool %in% zero]
    }
  }

  pool <- union(pool, required)
  w <- stats::setNames(rep(mean(share), length(pool)), pool)
  w[names(share)] <- share
  w <- pmax(w, 0.05)
  min_size <- min(max(min_size, length(required)), length(pool))
  max_size <- min(max(max_size, min_size), length(pool))

  out <- sample_candidate_formulas(
    pool, response = response, required = required, n = n_formulas,
    min_size = min_size, max_size = max_size, weights = w, seed = seed, env = env
  )
  attr(out, "adaptive") <- list(pool = pool, min_size = min_size, max_size = max_size, weights = w, n_winners = length(keep))
  out
}
