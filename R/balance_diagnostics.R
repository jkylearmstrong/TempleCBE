# Covariate balance between groups: standardized mean differences and the Love plot.

# weighted mean and (reliability-weighted) variance; equal weights give mean() and var()
#' @noRd
balance_wstats <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  sw <- sum(w)
  if (length(x) == 0L || sw <= 0) return(c(mean = NA_real_, var = NA_real_, n = 0))
  m <- sum(w * x) / sw
  denom <- sw - sum(w^2) / sw
  v <- if (length(x) < 2L || denom <= 0) NA_real_ else sum(w * (x - m)^2) / denom
  c(mean = m, var = v, n = length(x))
}

# one covariate: SMD of the compared group against the reference. The means (and the variance
# ratio) use the weights `w`; the standard deviation in the denominator always comes from the
# unweighted sample, so adjusted and unadjusted SMDs are on the same scale (Stuart 2013).
#' @noRd
balance_one <- function(x, in_ref, in_cmp, w, denominator) {
  a <- balance_wstats(x[in_ref], w[in_ref])
  b <- balance_wstats(x[in_cmp], w[in_cmp])
  a0 <- balance_wstats(x[in_ref], rep(1, sum(in_ref)))
  b0 <- balance_wstats(x[in_cmp], rep(1, sum(in_cmp)))
  sd_den <- switch(
    denominator,
    pooled = sqrt((a0[["var"]] + b0[["var"]]) / 2),
    reference = sqrt(a0[["var"]])
  )
  smd <- if (is.na(sd_den) || sd_den == 0) NA_real_ else (b[["mean"]] - a[["mean"]]) / sd_den
  vr <- if (is.na(a[["var"]]) || a[["var"]] == 0) NA_real_ else b[["var"]] / a[["var"]]
  list(mean_ref = a[["mean"]], mean_cmp = b[["mean"]], smd = smd, var_ratio = vr)
}

#' Standardized Mean Differences for Covariate Balance
#'
#' Computes the standardized mean difference (SMD) of each covariate between a group and a
#' reference group, before and, when `weights` are given, after weighting (inverse
#' probability of treatment, overlap or matching weights). It is the table behind a Love plot,
#' [plot_balance_love()], and the check that comes with a propensity-score analysis or a
#' table-one of two cohorts.
#'
#' For a numeric covariate the SMD is the difference in means over the pooled standard
#' deviation, `sqrt((s_ref^2 + s_cmp^2) / 2)`. A logical or 0/1 covariate is treated as
#' numeric, so its SMD is the difference in proportions over `sqrt((v_ref + v_cmp) / 2)`
#' with the sample variance. A factor or character covariate becomes one 0/1 indicator
#' per level (named `variable = level`). Variances are the unbiased (sample) ones. With
#' weights, the means in the numerator are weighted but the standard deviation in the
#' denominator is that of the unweighted sample, so the adjusted and unadjusted SMDs are on
#' the same scale (as in `cobalt` and `MatchIt`); `var_ratio` uses the weighted variances.
#' Missing values are dropped covariate by covariate. A common rule of thumb calls `abs(smd) < 0.1` balanced.
#'
#' @param data A data frame.
#' @param group Name of the grouping column: two or more levels.
#' @param vars Covariates (column names). Default: every column except `group` and the
#'   `weights` column.
#' @param weights `NULL` (default), the name of a numeric column of `data`, or a numeric vector
#'   with one weight per row. When given, the table has an `"Adjusted"` row besides the
#'   `"Unadjusted"` one for each covariate.
#' @param ref The reference level of `group`. Default: the first level (the first value, for
#'   a non-factor).
#' @param denominator `"pooled"` (default) or `"reference"` (the standard deviation of the
#'   reference group, as for the average treatment effect on the treated).
#' @param threshold The `abs(smd)` below which a covariate is called balanced (default `0.1`).
#' @return A tibble of class `balance_smd` with one row per comparison, covariate and
#'   sample: `comparison` (`"level vs ref"`), `variable`, `sample` (`"Unadjusted"` /
#'   `"Adjusted"`), `mean_ref`, `mean_cmp`, `smd`, `abs_smd`, `var_ratio` (variance in the
#'   compared group over the reference group) and `balanced`. The reference level and the
#'   threshold are in `attr(x, "ref")` and `attr(x, "threshold")`.
#' @seealso [plot_balance_love()]. The `cobalt` package has a fuller set of balance tools
#'   (balance after matching and weighting, many weighting estimands); this function
#'   needs no extra package and covers the table and the Love plot.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   treated = rep(c("no", "yes"), each = 100),
#'   age = c(rnorm(100, 60, 10), rnorm(100, 66, 10)),
#'   male = c(rbinom(100, 1, 0.45), rbinom(100, 1, 0.6)),
#'   site = sample(c("A", "B", "C"), 200, replace = TRUE)
#' )
#' balance_smd(d, "treated")
balance_smd <- function(data,
                        group,
                        vars = NULL,
                        weights = NULL,
                        ref = NULL,
                        denominator = c("pooled", "reference"),
                        threshold = 0.1) {
  denominator <- match.arg(denominator)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.character(group) || length(group) != 1L || !group %in% names(data)) {
    stop("`group` must be the name of a column of `data`.", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) || threshold < 0) {
    stop("`threshold` must be a single non-negative number.", call. = FALSE)
  }
  weight_col <- NULL
  if (is.character(weights) && length(weights) == 1L) {
    if (!weights %in% names(data)) stop("`weights` column not found: ", weights, ".", call. = FALSE)
    weight_col <- weights
    weights <- data[[weights]]
  }
  if (is.null(weights)) {
    w_adj <- NULL
  } else {
    if (!is.numeric(weights) || length(weights) != nrow(data)) {
      stop("`weights` must be a numeric column name or a numeric vector with one value per row.", call. = FALSE)
    }
    if (any(weights < 0, na.rm = TRUE)) stop("`weights` must not be negative.", call. = FALSE)
    w_adj <- weights
  }
  if (is.null(vars)) vars <- setdiff(names(data), c(group, weight_col))
  gone <- setdiff(vars, names(data))
  if (length(gone)) stop("`vars` not found in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  if (group %in% vars) stop("`group` cannot be one of `vars`.", call. = FALSE)
  if (length(vars) == 0L) stop("There is no covariate to compare.", call. = FALSE)

  g <- data[[group]]
  keep_row <- !is.na(g)
  lev <- if (is.factor(g)) levels(droplevels(g[keep_row])) else as.character(sort(unique(g[keep_row])))
  if (length(lev) < 2L) stop("`group` must have at least two levels.", call. = FALSE)
  g <- as.character(g)
  if (is.null(ref)) ref <- lev[1L]
  ref <- as.character(ref)
  if (!ref %in% lev) stop("`ref` is not a level of `group`: ", ref, ".", call. = FALSE)
  others <- setdiff(lev, ref)

  # expand factors and characters into indicators
  cols <- list()
  for (v in vars) {
    x <- data[[v]]
    if (is.logical(x)) {
      cols[[v]] <- as.numeric(x)
    } else if (is.numeric(x)) {
      cols[[v]] <- as.numeric(x)
    } else if (is.factor(x) || is.character(x)) {
      lv <- if (is.factor(x)) levels(x) else sort(unique(x[!is.na(x)]))
      for (l in lv) cols[[paste0(v, " = ", l)]] <- ifelse(is.na(x), NA_real_, as.numeric(x == l))
    } else {
      stop("Covariate `", v, "` must be numeric, logical, factor or character.", call. = FALSE)
    }
  }

  samples <- c("Unadjusted", if (!is.null(w_adj)) "Adjusted")
  unit <- rep(1, nrow(data))
  rows <- list()
  for (o in others) {
    in_ref <- keep_row & g == ref
    in_cmp <- keep_row & g == o
    for (nm in names(cols)) {
      for (smp in samples) {
        w <- if (smp == "Adjusted") w_adj else unit
        r <- balance_one(cols[[nm]], in_ref, in_cmp, w, denominator)
        rows[[length(rows) + 1L]] <- tibble::tibble(
          comparison = paste(o, "vs", ref),
          variable = nm,
          sample = smp,
          mean_ref = r$mean_ref,
          mean_cmp = r$mean_cmp,
          smd = r$smd,
          abs_smd = abs(r$smd),
          var_ratio = r$var_ratio,
          balanced = abs(r$smd) < threshold
        )
      }
    }
  }
  out <- dplyr::bind_rows(rows)
  attr(out, "ref") <- ref
  attr(out, "threshold") <- threshold
  class(out) <- c("balance_smd", class(out))
  out
}

#' @rdname templecbe_methods
#' @export
print.balance_smd <- function(x, ...) {
  thr <- attr(x, "threshold") %||% 0.1
  n_bad <- sum(!x$balanced & x$sample == utils::tail(unique(x$sample), 1L), na.rm = TRUE)
  cat(sprintf(
    "Covariate balance (reference: %s); %d covariate row(s) with |SMD| >= %s in the %s sample\n",
    attr(x, "ref") %||% "?", n_bad, format(thr), tolower(utils::tail(unique(x$sample), 1L))
  ))
  print(tibble::as_tibble(unclass_balance(x)), ...)
  invisible(x)
}

#' @noRd
unclass_balance <- function(x) {
  class(x) <- setdiff(class(x), "balance_smd")
  attr(x, "ref") <- NULL
  attr(x, "threshold") <- NULL
  x
}

#' Love Plot of Covariate Balance
#'
#' Plots the absolute standardized mean difference of each covariate, one dot per sample
#' (unadjusted and adjusted), with dashed lines at the threshold. Covariates are ordered by
#' their unadjusted imbalance, the largest at the top.
#'
#' @param x A [balance_smd()] result, or a data frame; in the second case the other
#'   arguments of [balance_smd()] are taken from `...`.
#' @param threshold Where to draw the dashed lines. Default: the threshold stored in `x`
#'   (`0.1`).
#' @param absolute Plot `abs(smd)` (default `TRUE`, the usual Love plot) or the signed SMD
#'   with lines at plus and minus the threshold.
#' @param comparison For a group with more than two levels, the comparison to show (default:
#'   all, in facets).
#' @param ... Passed to [balance_smd()] when `x` is a data frame (`group` is required).
#' @return A `ggplot2` object.
#' @seealso [balance_smd()]
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   treated = rep(c("no", "yes"), each = 100),
#'   age = c(rnorm(100, 60, 10), rnorm(100, 66, 10)),
#'   male = c(rbinom(100, 1, 0.45), rbinom(100, 1, 0.6))
#' )
#' plot_balance_love(balance_smd(d, "treated"))
plot_balance_love <- function(x, threshold = NULL, absolute = TRUE, comparison = NULL, ...) {
  if (!inherits(x, "balance_smd")) {
    if (!is.data.frame(x)) stop("`x` must be a balance_smd() result or a data frame.", call. = FALSE)
    x <- balance_smd(x, ...)
  }
  if (is.null(threshold)) threshold <- attr(x, "threshold") %||% 0.1
  df <- tibble::as_tibble(unclass_balance(x))
  if (!is.null(comparison)) {
    df <- df[df$comparison %in% comparison, , drop = FALSE]
    if (nrow(df) == 0L) stop("No such `comparison`.", call. = FALSE)
  }
  df <- df[!is.na(df$smd), , drop = FALSE]
  if (nrow(df) == 0L) stop("No covariate has a defined standardized mean difference.", call. = FALSE)
  df$value <- if (absolute) df$abs_smd else df$smd
  base <- df[df$sample == "Unadjusted", , drop = FALSE]
  ord <- tapply(base$abs_smd, base$variable, max, na.rm = TRUE)
  df$variable <- factor(df$variable, levels = names(sort(ord, decreasing = FALSE)))
  df$sample <- factor(df$sample, levels = c("Unadjusted", "Adjusted"))
  lines <- if (absolute) threshold else c(-threshold, threshold)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$value, y = .data$variable, color = .data$sample, shape = .data$sample)) +
    ggplot2::geom_vline(xintercept = lines, linetype = "dashed", color = "grey50") +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_color_manual(values = c(Unadjusted = "#2B2B2B", Adjusted = "#9D2235"), drop = TRUE) +
    ggplot2::scale_shape_manual(values = c(Unadjusted = 16, Adjusted = 17), drop = TRUE) +
    theme_cbe() +
    ggplot2::labs(
      title = "Covariate balance",
      x = if (absolute) "Absolute standardized mean difference" else "Standardized mean difference",
      y = NULL, color = "Sample", shape = "Sample"
    )
  if (!absolute) p <- p + ggplot2::geom_vline(xintercept = 0, color = "grey70")
  if (length(unique(df$comparison)) > 1L) p <- p + ggplot2::facet_wrap(~ .data$comparison)
  p
}
