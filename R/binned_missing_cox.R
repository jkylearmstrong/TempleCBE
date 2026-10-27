#' Quantile Bins with an Explicit Missing Level
#'
#' Cuts a numeric variable into equal-count bins and gives missing values their own level, so a
#' survival model can ask whether being unmeasured carries risk of its own. The reference level
#' comes first (what `coxph()` and `lm()` treat as the baseline) and the missing level last.
#'
#' @param x Numeric vector.
#' @param n_bins Number of bins (at least 2).
#' @param labels Bin labels, lowest to highest; default `Low`/`Med`/`High` for 3 bins, `Q1`... otherwise.
#' @param missing_label Label of the missing level; no such level is added if nothing is missing.
#' @param reference Label of the reference level, one of `labels`.
#' @param subset_mask Optional logical vector (length of `x`): compute the quantile breaks on
#'   these rows only (for example events only, or the training rows); all rows are then binned
#'   with those breaks.
#' @return A factor. The breaks are in the attribute `breaks` (inner cutpoints).
#' @seealso [tbl_cox_binned_missing()]
#' @export
#' @examples
#' x <- c(1:9, NA, NA)
#' table(cut_with_missing_category(x))
cut_with_missing_category <- function(x, n_bins = 3, labels = NULL, missing_label = "Missing",
                                      reference = NULL, subset_mask = NULL) {
  if (!is.numeric(x)) stop("`x` must be numeric.", call. = FALSE)
  if (!is.numeric(n_bins) || length(n_bins) != 1L || n_bins < 2 || n_bins != round(n_bins)) {
    stop("`n_bins` must be a whole number of at least 2.", call. = FALSE)
  }
  if (is.null(labels)) labels <- if (n_bins == 3) c("Low", "Med", "High") else paste0("Q", seq_len(n_bins))
  if (length(labels) != n_bins || anyDuplicated(labels)) stop("`labels` must be ", n_bins, " distinct labels.", call. = FALSE)
  if (missing_label %in% labels) stop("`missing_label` must differ from the bin labels.", call. = FALSE)
  if (is.null(reference)) reference <- labels[1L]
  if (!reference %in% labels) stop("`reference` must be one of `labels`.", call. = FALSE)
  if (!is.null(subset_mask) && (!is.logical(subset_mask) || length(subset_mask) != length(x))) {
    stop("`subset_mask` must be a logical vector the length of `x`.", call. = FALSE)
  }
  basis <- x[if (is.null(subset_mask)) rep(TRUE, length(x)) else subset_mask %in% TRUE]
  basis <- basis[!is.na(basis)]
  if (length(basis) < n_bins) stop("Fewer observed values than bins.", call. = FALSE)
  inner <- unique(unname(stats::quantile(basis, seq_len(n_bins - 1L) / n_bins, type = 7)))
  if (length(inner) < n_bins - 1L) {
    stop("The quantile breaks are not distinct (too many tied values) for ", n_bins, " bins.", call. = FALSE)
  }
  f <- cut(x, breaks = c(-Inf, inner, Inf), labels = labels, right = TRUE)
  lev <- c(reference, setdiff(labels, reference))
  f <- as.character(f)
  if (anyNA(x)) {
    f[is.na(x)] <- missing_label
    lev <- c(lev, missing_label)
  }
  structure(factor(f, levels = lev), breaks = inner)
}

#' Cox Model on Quantile Bins with an Explicit Missing Level
#'
#' Fits `Surv(time, status) ~ bins` where the bins come from [cut_with_missing_category()]
#' and reports each level against the reference (the lowest bin).
#'
#' @param data A data frame.
#' @param time_col,status_col Survival time and event indicator.
#' @param feature Numeric feature column.
#' @param n_bins,labels,missing_label,reference,subset_mask See [cut_with_missing_category()].
#' @param conf_level Confidence level.
#' @return A tibble with one row per level: `level`, `n`, `events`, `hr` (1 for the reference),
#'   `conf.low`, `conf.high`, `p.value` (`NA` for the reference) and `reference`. The Cox fit is in
#'   the attribute `fit`.
#' @seealso [cut_with_missing_category()], [cox_single()]
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(time = rexp(200, 0.1), status = rbinom(200, 1, 0.6), x = rnorm(200))
#' d$x[sample(200, 60)] <- NA
#' tbl_cox_binned_missing(d, "time", "status", "x")
tbl_cox_binned_missing <- function(data, time_col, status_col, feature, n_bins = 3, labels = NULL,
                                   missing_label = "Missing", reference = NULL, subset_mask = NULL,
                                   conf_level = 0.95) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (v in c(time_col, status_col, feature)) {
    if (length(v) != 1L || !v %in% names(data)) stop("`", paste(v, collapse = ", "), "` is not a column of `data`.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be between 0 and 1.", call. = FALSE)
  }
  d <- data.frame(.time = data[[time_col]], .status = as.numeric(data[[status_col]]))
  d$.bin <- cut_with_missing_category(data[[feature]], n_bins, labels, missing_label, reference, subset_mask)
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (sum(d$.status == 1) < 2L) stop("Fewer than two events.", call. = FALSE)
  fit <- survival::coxph(survival::Surv(.time, .status) ~ .bin, data = d)
  s <- summary(fit, conf.int = conf_level)
  lev <- levels(d$.bin)
  rows <- lapply(lev, function(l) {
    i <- d$.bin == l
    base <- tibble::tibble(level = l, n = sum(i), events = sum(d$.status[i] == 1))
    nm <- paste0(".bin", l)
    if (l == lev[1L]) {
      cbind(base, hr = 1, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_, reference = TRUE)
    } else if (nm %in% rownames(s$conf.int)) {
      cbind(base, hr = s$conf.int[nm, "exp(coef)"], conf.low = s$conf.int[nm, 3], conf.high = s$conf.int[nm, 4],
            p.value = s$coefficients[nm, "Pr(>|z|)"], reference = FALSE)
    } else {
      cbind(base, hr = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_, reference = FALSE)
    }
  })
  out <- tibble::as_tibble(do.call(rbind, rows))
  attr(out, "fit") <- fit
  out
}
