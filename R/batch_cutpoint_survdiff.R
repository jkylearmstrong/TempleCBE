#' Scan Cutpoints of Several Features with the Log-Rank Test
#'
#' For every feature, dichotomises the cohort at each candidate cutpoint (`<= cutpoint` is
#' "low", `> cutpoint` "high"), runs [survival::survdiff()] and returns the p-values with a
#' significance bin. Candidate cutpoints are the midpoints between consecutive distinct values or
#' quantiles. The univariate Cox coefficient is returned too, which
#' [first_significant_cutpoint()] uses to pick the first significant cutpoint.
#'
#' @param data A data frame.
#' @param time_col,status_col Names of the follow-up time and event (1 = event) columns.
#' @param feature_cols Names of the numeric features to scan.
#' @param cutpoints `"midpoints"` (default) or `"quantiles"`.
#' @param probs Quantile probabilities for `cutpoints = "quantiles"`.
#' @param min_n Minimum number of subjects in each of the two groups (default 1).
#' @param p_breaks Increasing p-value breaks for the significance bin
#'   (default `c(0.001, 0.05, 0.1)`).
#' @return A tibble with `feature`, `cutpoint`, `n_low`, `n_high`, `chisq`, `p`, `p_type` (ordered
#'   factor: `"p >= 0.1"`, `"p < 0.1"`, `"p < 0.05"`, `"p < 0.001"` for the default breaks),
#'   `cox_coef` and `cox_p`. A feature without a usable cutpoint contributes no rows.
#' @seealso [first_significant_cutpoint()], [km_cutpoint_search()]
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(time = rexp(80), status = rbinom(80, 1, 0.7), x = rnorm(80), z = rnorm(80))
#' res <- batch_cutpoint_survdiff(d, "time", "status", c("x", "z"), cutpoints = "quantiles")
#' head(res)
batch_cutpoint_survdiff <- function(data,
                                    time_col,
                                    status_col,
                                    feature_cols,
                                    cutpoints = c("midpoints", "quantiles"),
                                    probs = seq(0.1, 0.9, by = 0.1),
                                    min_n = 1,
                                    p_breaks = c(0.001, 0.05, 0.1)) {
  cutpoints <- match.arg(cutpoints)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  gone <- setdiff(c(time_col, status_col, feature_cols), names(data))
  if (length(gone)) stop("Column(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  if (length(feature_cols) == 0L) stop("`feature_cols` is empty.", call. = FALSE)
  notnum <- feature_cols[!vapply(data[feature_cols], is.numeric, logical(1))]
  if (length(notnum)) stop("Non-numeric feature(s): ", paste(notnum, collapse = ", "), ".", call. = FALSE)
  if (is.unsorted(p_breaks, strictly = TRUE)) stop("`p_breaks` must be strictly increasing.", call. = FALSE)
  if (cutpoints == "quantiles" && (length(probs) == 0L || any(probs <= 0 | probs >= 1))) {
    stop("`probs` must lie strictly between 0 and 1.", call. = FALSE)
  }

  nb <- length(p_breaks)
  lev <- c(paste0("p >= ", p_breaks[nb]), paste0("p < ", rev(p_breaks)))
  bin <- function(p) {
    out <- rep(lev[1L], length(p))
    # p below break i (largest first) gets progressively stricter labels
    for (i in nb:1L) out[!is.na(p) & p < p_breaks[i]] <- lev[nb - i + 2L]
    out[is.na(p)] <- NA_character_
    factor(out, levels = lev, ordered = TRUE)
  }

  empty <- tibble::tibble(feature = character(), cutpoint = numeric(), n_low = integer(),
                          n_high = integer(), chisq = numeric(), p = numeric(),
                          cox_coef = numeric(), cox_p = numeric())

  one <- function(feat) {
    d <- data[stats::complete.cases(data[c(time_col, status_col, feat)]), , drop = FALSE]
    x <- d[[feat]]
    if (length(unique(x)) < 2L) return(empty)
    cand <- if (cutpoints == "midpoints") {
      v <- sort(unique(x))
      (v[-1L] + v[-length(v)]) / 2
    } else {
      unique(stats::quantile(x, probs, names = FALSE))
    }
    sv <- survival::Surv(d[[time_col]], d[[status_col]])
    na2 <- c(NA_real_, NA_real_)
    cox <- tryCatch(suppressWarnings(summary(survival::coxph(sv ~ x))$coefficients[1L, c("coef", "Pr(>|z|)")]),
                    error = function(e) na2)
    rows <- lapply(cand, function(cp) {
      high <- x > cp
      nh <- sum(high)
      nl <- sum(!high)
      if (nh < max(min_n, 1) || nl < max(min_n, 1)) return(NULL)
      sdf <- tryCatch(survival::survdiff(sv ~ high), error = function(e) NULL)
      if (is.null(sdf)) return(NULL)
      tibble::tibble(feature = feat, cutpoint = cp, n_low = nl, n_high = nh,
                     chisq = unname(sdf$chisq), p = unname(sdf$pvalue),
                     cox_coef = unname(cox[1L]), cox_p = unname(cox[2L]))
    })
    rows <- dplyr::bind_rows(rows)
    if (nrow(rows) == 0L) empty else rows
  }

  res <- dplyr::bind_rows(lapply(feature_cols, one))
  res$p_type <- bin(res$p)
  res[c("feature", "cutpoint", "n_low", "n_high", "chisq", "p", "p_type", "cox_coef", "cox_p")]
}

#' First Significant Cutpoint per Feature
#'
#' From the result of [batch_cutpoint_survdiff()], picks for each feature and each `alpha` the
#' first cutpoint with `p < alpha`, scanning from the lowest cutpoint when the univariate Cox
#' coefficient is positive and from the highest otherwise (the most inclusive split on the risk
#' side).
#'
#' @param x Result of [batch_cutpoint_survdiff()].
#' @param alpha Significance levels (default `c(0.1, 0.05, 0.001)`).
#' @return A tibble with one row per feature and `alpha` that has a significant cutpoint (the
#'   columns of `x` plus `alpha`).
#' @seealso [batch_cutpoint_survdiff()]
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(time = rexp(80), status = rbinom(80, 1, 0.7), x = rnorm(80))
#' first_significant_cutpoint(batch_cutpoint_survdiff(d, "time", "status", "x"), alpha = 0.5)
first_significant_cutpoint <- function(x, alpha = c(0.1, 0.05, 0.001)) {
  need <- setdiff(c("feature", "cutpoint", "p", "cox_coef"), names(x))
  if (length(need)) stop("`x` lacks column(s): ", paste(need, collapse = ", "), ".", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) == 0L || any(alpha <= 0 | alpha > 1)) {
    stop("`alpha` must be in (0, 1].", call. = FALSE)
  }
  rows <- list()
  for (f in unique(x$feature)) {
    d <- x[x$feature == f, , drop = FALSE]
    for (a in alpha) {
      s <- d[!is.na(d$p) & d$p < a, , drop = FALSE]
      if (nrow(s) == 0L) next
      pos <- isTRUE(d$cox_coef[1L] > 0)
      pick <- s[if (pos) which.min(s$cutpoint) else which.max(s$cutpoint), , drop = FALSE]
      pick$alpha <- a
      rows[[length(rows) + 1L]] <- pick
    }
  }
  if (length(rows) == 0L) {
    out <- x[0L, , drop = FALSE]
    out$alpha <- numeric()
    return(tibble::as_tibble(out))
  }
  tibble::as_tibble(dplyr::bind_rows(rows))
}
