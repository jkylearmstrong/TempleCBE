#' Correlations Within and Between Feature Domains
#'
#' Lists pairs of features whose absolute correlation is at least `cutoff`, labelled by the
#' domain of each feature (for example blood gas, pulmonary, cardiovascular) and by whether the
#' pair lies within one domain or between two. See [tbl_correlated_pairs()] for a table without
#' domains.
#'
#' @param data A data frame.
#' @param domain_map A data frame with columns `variable` (or `columns`) and `domain`, or a named
#'   vector (names = columns, values = domains). Only mapped numeric columns of `data` are used;
#'   a mapped column that is not in `data` is an error.
#' @param cutoff Absolute correlation threshold in (0, 1]; a pair at exactly the cutoff is listed.
#' @param cross_domain_only Keep only pairs from different domains.
#' @param method `"pearson"` or `"spearman"` (pairwise complete observations).
#' @return A tibble with `feature_1`, `domain_1`, `feature_2`, `domain_2`, `scope` (`"within"` or
#'   `"between"`), `correlation` and `n` (pairs of observations used), sorted by decreasing absolute
#'   correlation. Pairs whose correlation cannot be computed are not listed. The attribute
#'   `domain_summary` counts the listed pairs per domain pair.
#' @seealso [tbl_correlated_pairs()], [corr_test_all()]
#' @export
#' @examples
#' map <- c(mpg = "engine", disp = "engine", hp = "engine", wt = "body", qsec = "body", drat = "body")
#' tbl_domain_correlations(mtcars, map, cutoff = 0.7)
#' tbl_domain_correlations(mtcars, map, cutoff = 0.7, cross_domain_only = TRUE)
tbl_domain_correlations <- function(data, domain_map, cutoff = 0.5, cross_domain_only = FALSE,
                                    method = c("pearson", "spearman")) {
  method <- match.arg(method)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.numeric(cutoff) || length(cutoff) != 1L || is.na(cutoff) || cutoff <= 0 || cutoff > 1) {
    stop("`cutoff` must be a single number in (0, 1].", call. = FALSE)
  }
  if (is.data.frame(domain_map)) {
    vcol <- intersect(c("variable", "columns"), names(domain_map))[1L]
    if (is.na(vcol) || !"domain" %in% names(domain_map)) {
      stop("`domain_map` needs columns `variable` (or `columns`) and `domain`.", call. = FALSE)
    }
    vars <- as.character(domain_map[[vcol]])
    doms <- as.character(domain_map$domain)
  } else if (is.atomic(domain_map) && !is.null(names(domain_map))) {
    vars <- names(domain_map)
    doms <- as.character(domain_map)
  } else {
    stop("`domain_map` must be a data frame or a named vector.", call. = FALSE)
  }
  if (anyDuplicated(vars)) stop("`domain_map` maps a column to more than one domain.", call. = FALSE)
  gone <- setdiff(vars, names(data))
  if (length(gone)) stop("Mapped column(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  notnum <- vars[!vapply(data[vars], is.numeric, logical(1))]
  if (length(notnum)) stop("Non-numeric mapped column(s): ", paste(notnum, collapse = ", "), ".", call. = FALSE)
  if (length(vars) < 2L) stop("Need at least two mapped columns.", call. = FALSE)

  m <- suppressWarnings(stats::cor(data[vars], use = "pairwise.complete.obs", method = method))
  ix <- which(upper.tri(m), arr.ind = TRUE)
  r <- m[ix]
  hit <- !is.na(r) & abs(r) >= cutoff
  ix <- ix[hit, , drop = FALSE]
  out <- tibble::tibble(
    feature_1 = vars[ix[, 1]], domain_1 = doms[ix[, 1]],
    feature_2 = vars[ix[, 2]], domain_2 = doms[ix[, 2]],
    correlation = r[hit]
  )
  out$scope <- ifelse(out$domain_1 == out$domain_2, "within", "between")
  out$n <- vapply(seq_len(nrow(out)), function(i) {
    sum(!is.na(data[[out$feature_1[i]]]) & !is.na(data[[out$feature_2[i]]]))
  }, integer(1))
  if (cross_domain_only) out <- out[out$scope == "between", , drop = FALSE]
  out <- out[order(-abs(out$correlation)), c("feature_1", "domain_1", "feature_2", "domain_2", "scope", "correlation", "n"), drop = FALSE]
  summ <- if (nrow(out)) {
    a <- pmin(out$domain_1, out$domain_2)
    b <- pmax(out$domain_1, out$domain_2)
    stats::aggregate(n_pairs ~ domain_a + domain_b, data = data.frame(domain_a = a, domain_b = b, n_pairs = 1L), FUN = sum)
  } else {
    data.frame(domain_a = character(), domain_b = character(), n_pairs = integer())
  }
  attr(out, "domain_summary") <- tibble::as_tibble(summ)
  out
}
