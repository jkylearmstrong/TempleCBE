#' Candidate Formulas Weighted by Feature Importance
#'
#' Draws random subsets of features, each feature chosen with probability related to its
#' weight (for example the absolute log hazard ratio of a univariate screen), as candidate model
#' formulas. A thin front end to [sample_candidate_formulas()], which has more constraints
#' (required terms, exclusions, hierarchy).
#'
#' Compute weights from the **training** data of the current fold only; weights from all rows
#' leak held-out information into the candidate set.
#'
#' @param feature_weights Named numeric vector of non-negative weights (names = features). A
#'   feature with weight 0 is never drawn, except in the full formula.
#' @param n_samples Number of distinct formulas.
#' @param min_terms,max_terms Smallest and largest number of features per formula.
#' @param include_full Also include the formula with every feature (added if the draw did not
#'   produce it, so the result can have `n_samples + 1` rows).
#' @param response Optional left-hand side as text, for example `"Surv(time, status)"`.
#' @param seed Optional integer; the caller's random-number state is restored.
#' @return The tibble of [sample_candidate_formulas()] (`id`, `formula`, `terms`, `n_terms`).
#' @seealso [adaptive_cox_formulas()], [expand_feature_formulas()], [calculate_feature_weights()]
#' @export
#' @examples
#' w <- c(age = 0.8, bmi = 0.2, lactate = 1.1, hr = 0.5)
#' sample_weighted_formulas(w, n_samples = 4, min_terms = 2, max_terms = 3, seed = 1)
sample_weighted_formulas <- function(feature_weights, n_samples = 25, min_terms = 2,
                                     max_terms = length(feature_weights), include_full = TRUE,
                                     response = NULL, seed = NULL) {
  if (!is.numeric(feature_weights) || is.null(names(feature_weights)) || anyNA(names(feature_weights)) ||
      anyDuplicated(names(feature_weights))) {
    stop("`feature_weights` must be a numeric vector with unique names.", call. = FALSE)
  }
  if (anyNA(feature_weights) || any(feature_weights < 0)) stop("`feature_weights` must be non-negative and not missing.", call. = FALSE)
  feats <- names(feature_weights)
  positive <- sum(feature_weights > 0)
  min_terms <- max(1L, as.integer(min_terms))
  max_terms <- min(as.integer(max_terms), length(feats))
  if (min_terms > max_terms) stop("`min_terms` is larger than `max_terms`.", call. = FALSE)
  if (positive < min_terms) stop("Fewer features with a positive weight (", positive, ") than `min_terms`.", call. = FALSE)
  cand <- sample_candidate_formulas(feats, response = response, n = n_samples, min_size = min_terms,
                                    max_size = max_terms, weights = unname(feature_weights), seed = seed)
  if (include_full) {
    has_full <- any(vapply(cand$terms, function(t) setequal(t, feats), logical(1)))
    if (!has_full) {
      full <- .formula_rows(list(feats), response, parent.frame())
      full$id <- max(cand$id) + 1L
      cand <- rbind(cand, full[names(cand)])
    }
  }
  cand
}

#' Adaptive Candidate Formulas from Top Models
#'
#' Keeps the predictors whose odds (or hazard) ratio is above a threshold, learns the size and
#' term frequencies of the formulas of the best models, and draws new candidate formulas from
#' them. A front end to [sample_candidate_formulas_adaptive()] for a table of predictor ratios
#' and a table of already selected (low-error) models.
#'
#' @param summary_df Data frame with `predictor` and `odds_ratio` (ratios; use `exp(coef)` for
#'   Cox models). Predictors are matched to the terms of the formulas by name.
#' @param low_ib_models Data frame of the best models with a `formula` column (text or formula
#'   objects). All rows count as winners; rank and filter them first.
#' @param n_formulas Number of distinct formulas wanted.
#' @param or_threshold Keep predictors with a ratio **above** this value (1 = harmful or
#'   associated with more events).
#' @param seed Optional integer; see [sample_candidate_formulas()].
#' @return The tibble of [sample_candidate_formulas()]; details in the attribute `"adaptive"`.
#' @seealso [sample_candidate_formulas_adaptive()], [sample_weighted_formulas()]
#' @export
#' @examples
#' ratios <- data.frame(predictor = c("a", "b", "c", "d"), odds_ratio = c(1.8, 0.6, 1.2, 2.5))
#' top <- data.frame(formula = c("y ~ a + c", "y ~ a + d", "y ~ a + c + d"))
#' adaptive_cox_formulas(ratios, top, n_formulas = 3, seed = 1)
adaptive_cox_formulas <- function(summary_df, low_ib_models, n_formulas = 5, or_threshold = 1.0, seed = NULL) {
  if (!is.data.frame(summary_df) || !all(c("predictor", "odds_ratio") %in% names(summary_df))) {
    stop("`summary_df` needs columns `predictor` and `odds_ratio`.", call. = FALSE)
  }
  if (!is.data.frame(low_ib_models) || !"formula" %in% names(low_ib_models) || nrow(low_ib_models) == 0L) {
    stop("`low_ib_models` needs a non-empty `formula` column.", call. = FALSE)
  }
  if (!is.numeric(or_threshold) || length(or_threshold) != 1L || is.na(or_threshold) || or_threshold <= 0) {
    stop("`or_threshold` must be a single positive number.", call. = FALSE)
  }
  prior <- data.frame(formula = low_ib_models$formula, .rank = seq_len(nrow(low_ib_models)))
  prior$formula <- vapply(prior$formula, function(f) if (is.character(f)) f[1L] else paste(deparse(f), collapse = " "), "")
  eff <- data.frame(term = as.character(summary_df$predictor), estimate = summary_df$odds_ratio / or_threshold)
  sample_candidate_formulas_adaptive(prior, metric = ".rank", top_quantile = 1, effects = eff,
                                     effect_scale = "ratio", n_formulas = n_formulas, seed = seed)
}

#' All Formulas with k Terms
#'
#' Exhaustively expands the combinations of `features`. The number of formulas is
#' `choose(n, k)` summed over the sizes, which grows quickly (20 features, sizes 2 to 20, is
#' over a million), so the call stops above `max_formulas`.
#'
#' @param features Character vector of terms.
#' @param k One size, or `NULL` for the range `min_terms` to `max_terms`.
#' @param min_terms,max_terms Size range when `k` is `NULL`.
#' @param response Optional left-hand side as text, for example `"Surv(time, status)"` or
#'   `"outcome"`; a trailing `~` is accepted.
#' @param max_formulas Safety limit on the number of formulas.
#' @return A tibble with `id`, `formula` (list of formulas), `terms` (list of character vectors)
#'   and `n_terms`, ordered by size and then by the order of `features`.
#' @seealso [sample_weighted_formulas()], [sample_candidate_formulas()]
#' @export
#' @examples
#' expand_feature_formulas(c("a", "b", "c"), k = 2, response = "y ~")
#' expand_feature_formulas(c("a", "b", "c"))
expand_feature_formulas <- function(features, k = NULL, min_terms = 2, max_terms = length(features),
                                    response = NULL, max_formulas = 1e5) {
  if (!is.character(features) || length(features) == 0L || anyNA(features) || anyDuplicated(features)) {
    stop("`features` must be unique, non-missing strings.", call. = FALSE)
  }
  n <- length(features)
  sizes <- if (is.null(k)) seq.int(max(1L, as.integer(min_terms)), min(as.integer(max_terms), n)) else as.integer(k)
  if (length(sizes) == 0L || anyNA(sizes) || any(sizes < 1L | sizes > n)) {
    stop("Term counts must be between 1 and ", n, ".", call. = FALSE)
  }
  total <- sum(choose(n, sizes))
  if (total > max_formulas) {
    stop("That would create ", format(total, big.mark = ","), " formulas (limit ", format(max_formulas, big.mark = ","),
         "); narrow `k` or raise `max_formulas`.", call. = FALSE)
  }
  sets <- unlist(lapply(sizes, function(s) utils::combn(features, s, simplify = FALSE)), recursive = FALSE)
  resp <- if (is.null(response)) NULL else trimws(sub("~\\s*$", "", response))
  .formula_rows(sets, resp, parent.frame())
}

.formula_rows <- function(sets, response, env) {
  fm <- lapply(sets, function(t) {
    stats::as.formula(paste(if (is.null(response)) "" else response, "~", paste(t, collapse = " + ")), env = env)
  })
  tibble::tibble(id = seq_along(sets), formula = fm, terms = sets, n_terms = lengths(sets))
}
