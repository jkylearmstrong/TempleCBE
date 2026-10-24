#' Univariate GLM and Mixed-Model Screens, With Coefficient Forest Plots and Tables
#'
#' `glm_single()` and `lmm_single()` fit one model per call with a single predictor, once on the
#' predictor as it is and once rescaled to the range 0 to 1, so that effects of predictors measured in
#' different units can be put side by side. `plot_coef_forest()` and `tbl_coef_forest()` display
#' the stacked results with significance stars.
#'
#' @section Scales:
#' `scale = "raw"` is the change in the outcome (or in the log odds, log rate, ...) per **one unit** of the
#' predictor. `scale = "normalized"` is the change across the **whole observed range** of the predictor
#' (the predictor is min-max scaled to 0..1 with [min_max_norm()]), which makes coefficients of
#' different predictors comparable in size. A non-numeric predictor has only the raw scale (its
#' coefficients are the contrasts with the reference level).
#'
#' @section Guards:
#' A predictor with fewer than two distinct non-missing values, or an outcome with none, returns
#' one row with `status` saying so instead of an error, so a screen over many predictors does not stop.
#' A model that fails to fit also returns a row with the message; convergence or singular-fit warnings
#' are kept in `warning`.
#'
#' @section Intervals:
#' For `glm_single()` with a family whose coefficient table is t-based (gaussian, quasi), the limits use the
#' t quantile with the residual degrees of freedom, so the interval excludes 0 exactly when the p-value is
#' below `1 - conf_level`; for z-based families (binomial, poisson) they use the normal quantile.
#'
#' @section Mixed models:
#' `lmm_single()` fits `outcome ~ feature + (1 | id_col)` with [lme4::lmer()] (maximum likelihood is not
#' used for the fixed effects; REML). Confidence limits and p-values are **Wald** (normal
#' approximation): `estimate +/- qnorm(1 - alpha / 2) * std.error` and `2 * pnorm(-abs(statistic))`.
#' They are optimistic for small numbers of subjects; use `lmerTest` or a bootstrap ([boot_lmer()]) for
#' small data. Rows with a missing outcome, predictor or id are dropped.
#'
#' @param data A data frame.
#' @param outcome,feature Names of the outcome and the single predictor.
#' @param family A [stats::family] object (default [stats::gaussian()]).
#' @param id_col Name of the grouping (subject) column for the random intercept.
#' @param normalize If `TRUE` (default) also fit on the 0..1 scaled predictor.
#' @param conf_level Confidence level (default 0.95).
#' @return A tibble of class `cbe_coef_screen` with `feature`, `var_label`, `scale`, `term`, `estimate`,
#'   `std.error`, `statistic`, `p.value`, `conf.low`, `conf.high`, `n`, `status` (`"ok"` or a reason) and
#'   `warning`. One row per coefficient of the predictor (the intercept is omitted).
#' @seealso [cox_single()], [batch_cox_single()], [plot_coef_forest()]
#' @name coef_single
#' @examples
#' glm_single(mtcars, "mpg", "wt")
#' glm_single(mtcars, "am", "wt", family = binomial())
NULL

.coef_screen_empty <- function(feature, var_label, scale, n, status, warning = "") {
  tibble::tibble(
    feature = feature, var_label = var_label, scale = scale, term = NA_character_, estimate = NA_real_,
    std.error = NA_real_, statistic = NA_real_, p.value = NA_real_, conf.low = NA_real_,
    conf.high = NA_real_, n = as.integer(n), status = status, warning = warning
  )
}

.coef_screen_label <- function(x, feature) {
  lbl <- attr(x, "label", exact = TRUE)
  if (is.null(lbl) || length(lbl) != 1L || is.na(lbl) || !nzchar(lbl)) feature else as.character(lbl)
}

# fits one version of the model; `fit_fun(df)` returns a fitted model, `extract(fit)` a coefficient matrix
.coef_screen_fit <- function(df, feature, var_label, scale, conf_level, fit_fun, coef_fun) {
  warns <- character()
  fit <- tryCatch(
    withCallingHandlers(
      fit_fun(df),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) conditionMessage(e)
  )
  if (is.character(fit)) return(.coef_screen_empty(feature, var_label, scale, nrow(df), fit))
  cm <- coef_fun(fit)
  cm <- cm[rownames(cm) != "(Intercept)", , drop = FALSE]
  if (nrow(cm) == 0L) return(.coef_screen_empty(feature, var_label, scale, nrow(df), "no coefficient estimated"))
  # a t-based coefficient table (Pr(>|t|)) gets t limits so that the interval and the p-value agree
  dfres <- tryCatch(fit$df.residual, error = function(e) NULL)
  z <- if (ncol(cm) >= 4L && identical(colnames(cm)[4], "Pr(>|t|)") && is.numeric(dfres) && length(dfres) == 1L && dfres > 0) {
    stats::qt(1 - (1 - conf_level) / 2, dfres)
  } else {
    stats::qnorm(1 - (1 - conf_level) / 2)
  }
  est <- cm[, 1]
  se <- cm[, 2]
  stat <- cm[, 3]
  p <- if (ncol(cm) >= 4L) cm[, 4] else 2 * stats::pnorm(-abs(stat))
  tibble::tibble(
    feature = feature, var_label = var_label, scale = scale,
    term = rownames(cm), estimate = unname(est), std.error = unname(se), statistic = unname(stat),
    p.value = unname(p), conf.low = unname(est - z * se), conf.high = unname(est + z * se),
    n = nrow(df), status = ifelse(is.na(est), "not estimable", "ok"),
    warning = paste(unique(warns), collapse = "; ")
  )
}

.coef_screen <- function(data, outcome, feature, id_col, normalize, conf_level, fit_builder, coef_fun, need_id) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (a in c("outcome", "feature")) {
    v <- get(a)
    if (!is.character(v) || length(v) != 1L || is.na(v) || !v %in% names(data)) {
      stop("`", a, "` must name one column of `data`.", call. = FALSE)
    }
  }
  if (need_id && (!is.character(id_col) || length(id_col) != 1L || is.na(id_col) || !id_col %in% names(data))) {
    stop("`id_col` must name one column of `data`.", call. = FALSE)
  }
  if (!is.logical(normalize) || length(normalize) != 1L || is.na(normalize)) {
    stop("`normalize` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be between 0 and 1.", call. = FALSE)
  }
  var_label <- .coef_screen_label(data[[feature]], feature)
  keep_cols <- c(outcome, feature, if (need_id) id_col)
  df <- data[stats::complete.cases(data[, keep_cols, drop = FALSE]), keep_cols, drop = FALSE]
  names(df)[1:2] <- c(".y", ".x")
  if (need_id) names(df)[3] <- ".id"
  attr(df$.x, "label") <- NULL

  if (nrow(df) == 0L) return(.coef_screen_empty(feature, var_label, "raw", 0L, "no complete cases"))
  if (length(unique(df$.x)) < 2L) {
    return(.coef_screen_empty(feature, var_label, "raw", nrow(df), "predictor has fewer than two distinct values"))
  }
  if (length(unique(df$.y)) < 2L) {
    return(.coef_screen_empty(feature, var_label, "raw", nrow(df), "outcome has fewer than two distinct values"))
  }
  raw <- .coef_screen_fit(df, feature, var_label, "raw", conf_level, fit_builder, coef_fun)
  out <- raw
  if (isTRUE(normalize) && is.numeric(df$.x)) {
    dn <- df
    dn$.x <- min_max_norm(dn$.x)
    out <- dplyr::bind_rows(raw, .coef_screen_fit(dn, feature, var_label, "normalized", conf_level, fit_builder, coef_fun))
  }
  # label the coefficients with the original feature name
  out$term <- ifelse(is.na(out$term), NA_character_, sub("^\\.x", feature, out$term))
  class(out) <- c("cbe_coef_screen", class(out))
  out
}

#' @rdname coef_single
#' @export
glm_single <- function(data, outcome, feature, family = stats::gaussian(), normalize = TRUE, conf_level = 0.95) {
  if (is.character(family)) family <- get(family, mode = "function", envir = parent.frame())
  if (is.function(family)) family <- family()
  if (!inherits(family, "family")) stop("`family` must be a family object such as gaussian() or binomial().", call. = FALSE)
  .coef_screen(
    data, outcome, feature, id_col = NULL, normalize = normalize, conf_level = conf_level, need_id = FALSE,
    fit_builder = function(df) stats::glm(.y ~ .x, data = df, family = family),
    coef_fun = function(fit) summary(fit)$coefficients
  )
}

#' @rdname coef_single
#' @export
lmm_single <- function(data, outcome, feature, id_col, normalize = TRUE, conf_level = 0.95) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package \"lme4\" is required for lmm_single().", call. = FALSE)
  }
  .coef_screen(
    data, outcome, feature, id_col = id_col, normalize = normalize, conf_level = conf_level, need_id = TRUE,
    fit_builder = function(df) lme4::lmer(.y ~ .x + (1 | .id), data = df, REML = TRUE),
    coef_fun = function(fit) stats::coef(summary(fit))
  )
}

# significance label for each p: the label of the first break the p-value is strictly below
.coef_stars <- function(p, p_breaks, p_labels) {
  idx <- vapply(p, function(x) if (is.na(x)) NA_integer_ else sum(p_breaks <= x) + 1L, integer(1))
  ifelse(is.na(idx), "", p_labels[idx])
}

.check_star_scheme <- function(p_breaks, p_labels) {
  if (!is.numeric(p_breaks) || anyNA(p_breaks) || is.unsorted(p_breaks, strictly = TRUE) ||
      any(p_breaks <= 0 | p_breaks >= 1)) {
    stop("`p_breaks` must be increasing numbers between 0 and 1.", call. = FALSE)
  }
  if (!is.character(p_labels) || length(p_labels) != length(p_breaks) + 1L) {
    stop("`p_labels` must have one more element than `p_breaks` (the last is for p at or above the last break).",
         call. = FALSE)
  }
  invisible(TRUE)
}

.coef_cols <- function(results_df, term_col, estimate_col, low_col, high_col, p_col) {
  if (!is.data.frame(results_df)) stop("`results_df` must be a data frame.", call. = FALSE)
  need <- c(term_col, estimate_col, low_col, high_col, p_col)
  miss <- setdiff(need, names(results_df))
  if (length(miss)) stop("`results_df` lacks column(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  invisible(TRUE)
}

#' @rdname coef_single
#' @param results_df Results of [glm_single()], [lmm_single()] (stacked with `rbind()`/`dplyr::bind_rows()`),
#'   or any data frame with the columns named below.
#' @param term_col,estimate_col,low_col,high_col,p_col Column names (defaults match `glm_single()`).
#' @param p_breaks Increasing p-value cut-offs (default `c(0.001, 0.01, 0.05, 0.1, 0.2)`).
#' @param p_labels Labels, one more than `p_breaks` (default `c("****", "***", "**", "*", ".", "ns")`).
#'   A p-value gets the label of the first break it is **strictly below**; p at or above the last break
#'   gets the last label; a missing p-value gets `""`.
#' @param facet_by Optional column to facet by (for example `"scale"`).
#' @param order_by `"input"` (default) keeps the row order, `"estimate"` and `"p"` sort.
#' @param x_label Axis label (default: "Coefficient estimate with confidence interval").
#' @export
plot_coef_forest <- function(results_df,
                             term_col = "var_label",
                             estimate_col = "estimate",
                             low_col = "conf.low",
                             high_col = "conf.high",
                             p_col = "p.value",
                             p_breaks = c(0.001, 0.01, 0.05, 0.1, 0.2),
                             p_labels = c("****", "***", "**", "*", ".", "ns"),
                             facet_by = NULL,
                             order_by = c("input", "estimate", "p"),
                             x_label = NULL) {
  order_by <- match.arg(order_by)
  .coef_cols(results_df, term_col, estimate_col, low_col, high_col, p_col)
  .check_star_scheme(p_breaks, p_labels)
  if (!is.null(facet_by) && !facet_by %in% names(results_df)) stop("`facet_by` is not a column of `results_df`.", call. = FALSE)
  df <- as.data.frame(results_df)
  df <- df[!is.na(df[[estimate_col]]), , drop = FALSE]
  if (nrow(df) == 0L) stop("No estimable coefficients to plot.", call. = FALSE)
  df$.est <- df[[estimate_col]]
  df$.lo <- df[[low_col]]
  df$.hi <- df[[high_col]]
  df$.stars <- .coef_stars(df[[p_col]], p_breaks, p_labels)
  lab <- as.character(df[[term_col]])
  # contrasts of one factor, or the raw and normalized rows of one predictor, share a label: tell them apart
  dup <- lab %in% lab[duplicated(lab)]
  if (any(dup) && "term" %in% names(df) && !identical(term_col, "term")) {
    lab[dup] <- paste0(lab[dup], " [", df$term[dup], "]")
  }
  dup <- lab %in% lab[duplicated(lab)]
  if (any(dup) && "scale" %in% names(df) && is.null(facet_by)) {
    lab[dup] <- paste0(lab[dup], " (", df$scale[dup], ")")
  }
  if (is.null(facet_by)) lab <- make.unique(lab, sep = " #")
  ord <- switch(order_by, input = seq_len(nrow(df)), estimate = order(df$.est), p = order(-df[[p_col]], na.last = FALSE))
  df <- df[ord, , drop = FALSE]
  lab <- lab[ord]
  df$.label <- factor(lab, levels = rev(unique(lab)))
  grp <- if (is.null(facet_by)) rep("all", nrow(df)) else as.character(df[[facet_by]])
  df$.xstar <- stats::ave(seq_len(nrow(df)), grp, FUN = function(i) {
    rng <- range(c(df$.lo[i], df$.hi[i], df$.est[i]), na.rm = TRUE)
    rng[2] + 0.04 * diff(rng)
  })
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$.est, y = .data$.label)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$.lo, xmax = .data$.hi), width = 0.25, color = "#9D2235", orientation = "y") +
    ggplot2::geom_point(size = 3, color = "#9D2235") +
    ggplot2::geom_text(ggplot2::aes(x = .data$.xstar, label = .data$.stars), hjust = 0, size = 3.5) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15))) +
    cbe_theme_survival() +
    ggplot2::labs(x = x_label %||% "Coefficient estimate with confidence interval", y = NULL,
                  caption = paste0("Significance: ", paste0(p_labels[-length(p_labels)], " p < ", p_breaks, collapse = ", "),
                                   ", ", p_labels[length(p_labels)], " otherwise"))
  if (!is.null(facet_by)) p <- p + ggplot2::facet_wrap(stats::as.formula(paste0("~", facet_by)), scales = "free_x")
  p
}

#' @rdname coef_single
#' @param digits Decimals for the estimate and its interval.
#' @export
tbl_coef_forest <- function(results_df,
                            term_col = "var_label",
                            estimate_col = "estimate",
                            low_col = "conf.low",
                            high_col = "conf.high",
                            p_col = "p.value",
                            p_breaks = c(0.001, 0.01, 0.05, 0.1, 0.2),
                            p_labels = c("****", "***", "**", "*", ".", "ns"),
                            digits = 2) {
  .coef_cols(results_df, term_col, estimate_col, low_col, high_col, p_col)
  .check_star_scheme(p_breaks, p_labels)
  f <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
  df <- as.data.frame(results_df)
  p <- df[[p_col]]
  out <- tibble::tibble(
    Term = as.character(df[[term_col]]),
    `Estimate [95% CI]` = ifelse(is.na(df[[estimate_col]]), "",
      sprintf("%s [%s, %s]", f(df[[estimate_col]]), f(df[[low_col]]), f(df[[high_col]]))),
    `p-value` = ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3))),
    Significance = .coef_stars(p, p_breaks, p_labels)
  )
  if ("scale" %in% names(df)) out$Scale <- as.character(df$scale)
  out
}
