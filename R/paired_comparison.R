#' Paired Comparison of Two Versions of a Measurement
#'
#' Tools for comparing two versions of the same quantity measured on the same rows, such as an index
#' computed from the original data and from imputed data. The difference is always
#' `second - first` (for example imputed minus original).
#'
#' * `compare_distributions_ks()`: empirical distribution functions and densities of two samples
#'   with the two-sample Kolmogorov-Smirnov test.
#' * `compare_paired_correlation()`: scatter of the second version against the first with the line
#'   of identity and a fitted regression line (a linear model, or a random-intercept mixed model
#'   when `id_col` is given).
#' * `paired_diff_tests()`: paired t-test and Wilcoxon signed-rank test, overall or per group.
#' * `paired_diff_histogram()`, `paired_diff_ribbon_by_group()`, `paired_diff_ridgeline()`:
#'   the distribution of the paired differences, overall, along an ordered group (time) and as
#'   ridgelines per group.
#'
#' @section Reading the tests:
#' The Kolmogorov-Smirnov test treats the two samples as independent. When they are two versions
#' of the same rows they are not, and the test is then conservative for a shift; use it as a
#' description of how far apart the distributions are and `paired_diff_tests()` for the paired
#' question. With ties (rounded values) the KS p-value is approximate. With repeated measurements
#' per subject, the paired tests treat the rows as independent; pass one row per subject, or use
#' `compare_paired_correlation(id_col = )` for a model that accounts for the subject.
#'
#' @param data A data frame with one row per paired observation.
#' @param first,second Names of the two numeric columns (for example original and imputed).
#' @param labels Display names of the two versions, in the order `first`, `second`.
#' @param group_col Optional grouping column (a time point, a cohort). Required for the ribbon and
#'   ridgeline plots.
#' @return See each function.
#' @name paired_comparison
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:30, 3), time = rep(c(0, 6, 12), each = 30), original = rnorm(90, 50, 8))
#' d$imputed <- d$original + rnorm(90, 0.5, 2)
#' paired_diff_tests(d, "original", "imputed", group_col = "time")
#' paired_diff_histogram(d, "original", "imputed")
#' paired_diff_ribbon_by_group(d, "original", "imputed", group_col = "time")
#' compare_paired_correlation(d, "original", "imputed")
NULL

.paired_frame <- function(data, first, second, group_col = NULL, id_col = NULL) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (a in list(first, second)) {
    if (!is.character(a) || length(a) != 1L) stop("`first` and `second` must each be one column name.", call. = FALSE)
  }
  gone <- setdiff(c(first, second, group_col, id_col), names(data))
  if (length(gone)) stop("Column(s) not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  if (!is.numeric(data[[first]]) || !is.numeric(data[[second]])) {
    stop("`", first, "` and `", second, "` must be numeric.", call. = FALSE)
  }
  out <- data.frame(first = data[[first]], second = data[[second]])
  if (!is.null(group_col)) out$group <- data[[group_col]]
  if (!is.null(id_col)) out$id <- data[[id_col]]
  out <- out[stats::complete.cases(out), , drop = FALSE]
  if (nrow(out) == 0L) stop("No complete pairs in `data`.", call. = FALSE)
  out$diff <- out$second - out$first
  out
}

.check_pair_labels <- function(labels) {
  if (!is.character(labels) || length(labels) != 2L || anyNA(labels) || labels[1] == labels[2]) {
    stop("`labels` must be two different strings.", call. = FALSE)
  }
  labels
}

.fmt_p_short <- function(p) format_p_value(p, digits = 3, prefix = TRUE, na = "p = NA")

#' @rdname paired_comparison
#' @param x,y Numeric vectors, the two samples (missing values are dropped). They need not have
#'   the same length.
#' @param title Optional title.
#' @return `compare_distributions_ks()`: a `patchwork` of the ECDF and density panels (needs the
#'   \pkg{patchwork} package), with the [stats::ks.test()] result in `attr(, "ks")`.
#' @export
compare_distributions_ks <- function(x, y, labels = c("Original", "Imputed"), title = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package \"patchwork\" is required for compare_distributions_ks().", call. = FALSE)
  }
  labels <- .check_pair_labels(labels)
  if (!is.numeric(x) || !is.numeric(y)) stop("`x` and `y` must be numeric.", call. = FALSE)
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  if (length(x) < 2L || length(y) < 2L) stop("`x` and `y` each need at least two non-missing values.", call. = FALSE)
  ks <- suppressWarnings(stats::ks.test(x, y))
  df <- data.frame(value = c(x, y), version = factor(rep(labels, c(length(x), length(y))), levels = labels))
  cols <- stats::setNames(c("#2B2B2B", "#A41E35"), labels)
  note <- sprintf("Two-sample Kolmogorov-Smirnov: D = %.3f, %s (n = %d and %d)", unname(ks$statistic),
                  .fmt_p_short(ks$p.value), length(x), length(y))
  ecdf <- ggplot2::ggplot(df, ggplot2::aes(x = .data$value, color = .data$version)) +
    ggplot2::stat_ecdf(linewidth = 1) +
    ggplot2::scale_color_manual(values = cols) +
    theme_cbe() +
    ggplot2::labs(x = "Value", y = "Cumulative proportion", color = NULL)
  dens <- ggplot2::ggplot(df, ggplot2::aes(x = .data$value, color = .data$version, fill = .data$version)) +
    ggplot2::geom_density(alpha = 0.25) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_fill_manual(values = cols) +
    theme_cbe() +
    ggplot2::labs(x = "Value", y = "Density", color = NULL, fill = NULL)
  out <- patchwork::wrap_plots(ecdf, dens, ncol = 2, guides = "collect") +
    patchwork::plot_annotation(title = title, caption = note)
  attr(out, "ks") <- ks
  out
}

#' @rdname paired_comparison
#' @param id_col Optional subject column. When given, the fitted line is the fixed-effect line of
#'   `second ~ first + (1 | id)` ([lme4::lmer()]); a model that fails or is singular falls back to
#'   the linear model, with a message.
#' @param digits Decimals in the caption.
#' @return `compare_paired_correlation()`: a `ggplot2` object; the fitted model is in
#'   `attr(, "model")` and the Pearson test in `attr(, "cor_test")`.
#' @export
compare_paired_correlation <- function(data, first, second, labels = c("Original", "Imputed"), id_col = NULL,
                                       digits = 3, title = NULL) {
  labels <- .check_pair_labels(labels)
  df <- .paired_frame(data, first, second, id_col = id_col)
  if (nrow(df) < 3L || length(unique(df$first)) < 2L) {
    stop("At least three pairs and two distinct values of `", first, "` are needed.", call. = FALSE)
  }
  ct <- stats::cor.test(df$first, df$second)
  fit <- NULL
  kind <- "linear model"
  if (!is.null(id_col)) {
    if (!requireNamespace("lme4", quietly = TRUE)) stop("Package \"lme4\" is required when `id_col` is given.", call. = FALSE)
    fit <- tryCatch({
      m <- suppressMessages(suppressWarnings(lme4::lmer(second ~ first + (1 | id), data = df)))
      if (lme4::isSingular(m)) NULL else m
    }, error = function(e) NULL)
    if (is.null(fit)) {
      message("The mixed model failed or was singular; showing the linear model.")
    } else {
      kind <- "mixed model, random intercept"
    }
  }
  if (is.null(fit)) fit <- stats::lm(second ~ first, data = df)
  cf <- if (inherits(fit, "merMod")) lme4::fixef(fit) else stats::coef(fit)
  f <- function(v) formatC(v, format = "f", digits = digits)
  caption <- sprintf("Pearson r = %s, %s; %s: %s = %s %s %s \u00d7 %s (n = %d)",
                     f(unname(ct$estimate)), .fmt_p_short(ct$p.value), kind, labels[2], f(cf[[1]]),
                     if (cf[[2]] >= 0) "+" else "\u2212", f(abs(cf[[2]])), labels[1], nrow(df))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$first, y = .data$second)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_point(alpha = 0.6, color = "#2B2B2B") +
    ggplot2::geom_abline(slope = cf[[2]], intercept = cf[[1]], color = "#A41E35", linewidth = 1) +
    theme_cbe() +
    ggplot2::labs(title = title, x = labels[1], y = labels[2], caption = paste0(caption, ". Dashed: identity."))
  attr(p, "model") <- fit
  attr(p, "cor_test") <- ct
  p
}

#' @rdname paired_comparison
#' @param conf_level Confidence level of the interval of the mean difference (default 0.95).
#' @return `paired_diff_tests()`: a tibble with one row overall or per group: `group` (if grouped),
#'   `n`, `mean_first`, `mean_second`, `mean_diff`, `sd_diff`, `conf.low`, `conf.high`, `t_statistic`,
#'   `p_t`, `median_diff` and `p_wilcoxon`. A group with fewer than two pairs, or with identical
#'   differences, has missing test results.
#' @export
paired_diff_tests <- function(data, first, second, group_col = NULL, conf_level = 0.95) {
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single number between 0 and 1.", call. = FALSE)
  }
  df <- .paired_frame(data, first, second, group_col = group_col)
  one <- function(d) {
    n <- nrow(d)
    tt <- if (n >= 2L && stats::sd(d$diff) > 0) {
      tryCatch(stats::t.test(d$diff, conf.level = conf_level), error = function(e) NULL)
    }
    wt <- if (n >= 2L && any(d$diff != 0)) {
      tryCatch(suppressWarnings(stats::wilcox.test(d$diff)), error = function(e) NULL)
    }
    tibble::tibble(
      n = n,
      mean_first = mean(d$first),
      mean_second = mean(d$second),
      mean_diff = mean(d$diff),
      sd_diff = if (n >= 2L) stats::sd(d$diff) else NA_real_,
      conf.low = if (is.null(tt)) NA_real_ else tt$conf.int[1],
      conf.high = if (is.null(tt)) NA_real_ else tt$conf.int[2],
      t_statistic = if (is.null(tt)) NA_real_ else unname(tt$statistic),
      p_t = if (is.null(tt)) NA_real_ else tt$p.value,
      median_diff = stats::median(d$diff),
      p_wilcoxon = if (is.null(wt)) NA_real_ else wt$p.value
    )
  }
  if (is.null(group_col)) return(one(df))
  keys <- if (is.factor(df$group)) levels(droplevels(df$group)) else sort(unique(df$group))
  rows <- lapply(keys, function(k) {
    r <- one(df[df$group == k, , drop = FALSE])
    tibble::add_column(r, group = k, .before = 1L)
  })
  dplyr::bind_rows(rows)
}

#' @rdname paired_comparison
#' @param bins Number of histogram bins (default 30).
#' @return `paired_diff_histogram()`, `paired_diff_ribbon_by_group()`, `paired_diff_ridgeline()`:
#'   a `ggplot2` object.
#' @export
paired_diff_histogram <- function(data, first, second, labels = c("Original", "Imputed"), group_col = NULL,
                                  bins = 30, title = NULL) {
  labels <- .check_pair_labels(labels)
  df <- .paired_frame(data, first, second, group_col = group_col)
  m <- mean(df$diff)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$diff)) +
    ggplot2::geom_histogram(bins = bins, fill = "#D9D9D9", color = "#2B2B2B") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    theme_cbe() +
    ggplot2::labs(title = title, x = paste0("Difference (", labels[2], " \u2212 ", labels[1], ")"), y = "Count")
  if (is.null(group_col)) {
    p <- p + ggplot2::geom_vline(xintercept = m, color = "#A41E35", linewidth = 1) +
      ggplot2::labs(caption = sprintf("Mean difference %s (solid line), SD %s, n = %d",
                                      formatC(m, digits = 3, format = "g"),
                                      formatC(stats::sd(df$diff), digits = 3, format = "g"), nrow(df)))
  } else {
    p <- p + ggplot2::facet_wrap(~group)
  }
  p
}

#' @rdname paired_comparison
#' @param ribbon What the band around the mean difference shows: `"ci"` (default; the t confidence
#'   interval of the mean at `conf_level`), `"sd"` (mean plus or minus one standard deviation) or
#'   `"quantile"` (the 2.5th to 97.5th percentile of the differences, around the median line).
#' @export
paired_diff_ribbon_by_group <- function(data, first, second, group_col, labels = c("Original", "Imputed"),
                                        ribbon = c("ci", "sd", "quantile"), conf_level = 0.95, title = NULL) {
  ribbon <- match.arg(ribbon)
  labels <- .check_pair_labels(labels)
  if (missing(group_col) || is.null(group_col)) stop("`group_col` is required.", call. = FALSE)
  df <- .paired_frame(data, first, second, group_col = group_col)
  keys <- if (is.factor(df$group)) levels(droplevels(df$group)) else sort(unique(df$group))
  s <- dplyr::bind_rows(lapply(keys, function(k) {
    v <- df$diff[df$group == k]
    n <- length(v)
    se <- if (n >= 2L) stats::sd(v) / sqrt(n) else NA_real_
    half <- switch(ribbon,
      ci = if (n >= 2L) stats::qt(1 - (1 - conf_level) / 2, n - 1) * se else NA_real_,
      sd = if (n >= 2L) stats::sd(v) else NA_real_,
      quantile = NA_real_
    )
    q <- stats::quantile(v, c(0.025, 0.975), names = FALSE)
    tibble::tibble(
      n = n,
      center = if (ribbon == "quantile") stats::median(v) else mean(v),
      lo = if (ribbon == "quantile") q[1] else mean(v) - half,
      hi = if (ribbon == "quantile") q[2] else mean(v) + half
    )
  }))
  numeric_x <- is.numeric(df$group)
  s$x <- if (numeric_x) keys else seq_along(keys)
  band <- switch(ribbon,
    ci = sprintf("mean, %s%% CI", format(100 * conf_level)),
    sd = "mean \u00b1 SD",
    quantile = "median, 2.5th to 97.5th percentile"
  )
  p <- ggplot2::ggplot(s, ggplot2::aes(x = .data$x, y = .data$center)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo, ymax = .data$hi), fill = "#A41E35", alpha = 0.2) +
    ggplot2::geom_line(color = "#A41E35", linewidth = 1) +
    ggplot2::geom_point(color = "#A41E35", size = 2) +
    theme_cbe() +
    ggplot2::labs(title = title, x = group_col,
                  y = paste0("Difference (", labels[2], " \u2212 ", labels[1], "): ", band))
  if (!numeric_x) p <- p + ggplot2::scale_x_continuous(breaks = seq_along(keys), labels = as.character(keys))
  p
}

#' @rdname paired_comparison
#' @export
paired_diff_ridgeline <- function(data, first, second, group_col, labels = c("Original", "Imputed"), title = NULL) {
  labels <- .check_pair_labels(labels)
  if (missing(group_col) || is.null(group_col)) stop("`group_col` is required.", call. = FALSE)
  df <- .paired_frame(data, first, second, group_col = group_col)
  df$group <- if (is.factor(df$group)) droplevels(df$group) else factor(df$group, levels = sort(unique(df$group)))
  ggplot2::ggplot(df, ggplot2::aes(x = .data$diff, y = .data$group)) +
    ggridges::geom_density_ridges(fill = "#A41E35", alpha = 0.5, color = "white", scale = 1.2) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    theme_cbe() +
    ggplot2::labs(title = title, x = paste0("Difference (", labels[2], " \u2212 ", labels[1], ")"), y = group_col)
}
