# Grid-level tuning diagnostics: coefficient estimate against IBS, the four-panel grid overview,
# IBS densities per resample and the mixed model of IBS on alpha and lambda. Companions of the
# functions in tune_diagnostics.R; they read the same one-row-per-coefficient tuning table.

.tune_split_col <- function(data) {
  if ("inner_resamples_splits" %in% names(data)) {
    "inner_resamples_splits"
  } else if ("inner_resamples" %in% names(data)) {
    "inner_resamples"
  } else if ("split" %in% names(data)) {
    "split"
  } else {
    NULL
  }
}

.tune_need <- function(data, cols) {
  if (!is.data.frame(data) || nrow(data) == 0L) stop("`data` must be a non-empty data frame.", call. = FALSE)
  miss <- setdiff(cols, names(data))
  if (length(miss)) stop("`data` is missing required column(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  invisible(TRUE)
}

# one row per fitted model (the tuning table repeats alpha/lambda/IBS for every coefficient)
.tune_fits <- function(data, cols = c("alpha", "lambda", "IBS")) {
  keep <- intersect(c(.tune_split_col(data), cols), names(data))
  df <- unique(as.data.frame(data)[, keep, drop = FALSE])
  df[stats::complete.cases(df[intersect(cols, names(df))]), , drop = FALSE]
}

# non-intercept coefficients with a finite estimate and IBS, terms relabelled
.tune_coefs <- function(data, terms, label_map, style) {
  df <- as.data.frame(data)
  df <- df[!is.na(df$term) & df$term != "(Intercept)" & !is.na(df$estimate) & !is.na(df$IBS), , drop = FALSE]
  if (!is.null(terms)) {
    gone <- setdiff(terms, df$term)
    if (length(gone)) stop("`terms` not in `data`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
    df <- df[df$term %in% terms, , drop = FALSE]
  }
  if (nrow(df) == 0L) stop("No feature coefficients found in `data`.", call. = FALSE)
  raw <- unique(df$term)
  lab <- make.unique(resolve_cbe_labels(raw, label_map = label_map, style = style), sep = " ")
  df$term <- factor(unname(stats::setNames(lab, raw)[df$term]), levels = lab)
  df
}

#' Coefficient Estimates Against the Integrated Brier Score
#'
#' Scatter of each feature's penalized coefficient against the IBS of the model it came from, over
#' the whole tuning grid and, in a second panel, over the best-performing share of the grid (lowest
#' IBS). Coefficients that stay away from zero among the best models are the stable ones.
#'
#' @param data A tuning table with one row per coefficient and model: `term`, `estimate` and `IBS`
#'   (as returned by [tune_alpha_by_split()] / `glmnet_IBS()` summaries).
#' @param top_frac Share of the models, by lowest IBS, shown in the second panel (default `0.2`, the
#'   top quintile). The cut is the `top_frac` quantile of IBS over distinct models, and models at the
#'   cut are included. `NULL` shows the full grid only.
#' @param terms Optional character vector of terms to show (raw names, before `label_map`).
#' @param drop_zero If `TRUE`, coefficients that are exactly zero (not selected) are left out.
#'   Default `FALSE`.
#' @param label_map Optional label mapping (see [cbe_labels()]).
#' @param style Optional label style (`"short"` or `"full"`).
#' @return A `ggplot2` object.
#' @seealso [plot_tune_grid()], [plot_tune_coef_boxplot()], [tbl_tune_coef()]
#' @export
#' @examples
#' set.seed(1)
#' grid <- expand.grid(split = paste0("Fold", 1:3), alpha = c(0.1, 0.5, 1), lambda = c(0.01, 0.1))
#' grid$IBS <- 0.15 + 0.03 * grid$alpha + stats::rnorm(nrow(grid), 0, 0.01)
#' tune <- merge(grid, data.frame(term = c("age", "bmi")))
#' tune$estimate <- stats::rnorm(nrow(tune), 0.3, 0.1)
#' plot_tune_coefficients(tune)
plot_tune_coefficients <- function(data, top_frac = 0.2, terms = NULL, drop_zero = FALSE,
                                   label_map = NULL, style = NULL) {
  .tune_need(data, c("term", "estimate", "IBS"))
  if (!is.null(top_frac) && (!is.numeric(top_frac) || length(top_frac) != 1L || is.na(top_frac) ||
                             top_frac <= 0 || top_frac >= 1)) {
    stop("`top_frac` must be a single number between 0 and 1, or NULL.", call. = FALSE)
  }
  df <- .tune_coefs(data, terms, label_map, style)
  if (isTRUE(drop_zero)) df <- df[df$estimate != 0, , drop = FALSE]
  if (nrow(df) == 0L) stop("No non-zero coefficients to plot.", call. = FALSE)
  df$.set <- "All models"
  if (!is.null(top_frac)) {
    # The cut is the quantile over distinct fitted models (one IBS per model),
    # not over distinct IBS values: tied IBS values repeated across models
    # would otherwise make the "best 20% of models" panel hold a different
    # share of models.
    model_ibs <- .tune_fits(df)$IBS
    cut <- stats::quantile(if (length(model_ibs)) model_ibs else unique(df$IBS), top_frac, names = FALSE)
    best <- df[df$IBS <= cut, , drop = FALSE]
    best_lab <- sprintf("Best %s%% of models (IBS \u2264 %s)", format(100 * top_frac), formatC(cut, digits = 3, format = "g"))
    best$.set <- best_lab
    df <- rbind(df, best)
    df$.set <- factor(df$.set, levels = c("All models", best_lab))
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$IBS, y = .data$estimate, color = .data$term)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_point(alpha = 0.6, size = 1.8) +
    theme_cbe() +
    ggplot2::labs(title = "Coefficient Estimates Against Model Performance",
                  x = "Integrated Brier Score (IBS)", y = "Penalized coefficient estimate", color = "Term")
  if (!is.null(top_frac)) p <- p + ggplot2::facet_wrap(~.set, scales = "free_x")
  p
}

#' Four-Panel Overview of a Penalized Tuning Grid
#'
#' One figure with (A) `alpha` against IBS, (B) `log10(IBS)` against the coefficient estimates,
#' (C) `alpha` against `log10(lambda)` and (D) `log10(IBS)` against `log10(lambda)`, replacing the
#' patchwork block that was repeated in the nested cross-validation reports.
#'
#' @param data A tuning table with `alpha`, `lambda`, `IBS`, `term` and `estimate` (one row per
#'   coefficient and model). Models with a missing or non-positive `lambda` or `IBS` are dropped,
#'   because the panels take logarithms.
#' @param terms Optional character vector of terms for panel B (raw names).
#' @param title Optional overall title.
#' @param label_map Optional label mapping (see [cbe_labels()]).
#' @param style Optional label style (`"short"` or `"full"`).
#' @return A `patchwork` object (requires the \pkg{patchwork} package).
#' @seealso [plot_tune_coefficients()], [plot_tune_alpha_ibs()], [plot_tune_lambda_ibs()]
#' @export
#' @examples
#' if (requireNamespace("patchwork", quietly = TRUE)) {
#'   set.seed(1)
#'   grid <- expand.grid(split = paste0("Fold", 1:3), alpha = c(0.1, 0.5, 1), lambda = c(0.01, 0.1))
#'   grid$IBS <- 0.15 + 0.03 * grid$alpha + stats::rnorm(nrow(grid), 0, 0.01)
#'   tune <- merge(grid, data.frame(term = c("age", "bmi")))
#'   tune$estimate <- stats::rnorm(nrow(tune), 0.3, 0.1)
#'   plot_tune_grid(tune)
#' }
plot_tune_grid <- function(data, terms = NULL, title = NULL, label_map = NULL, style = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package \"patchwork\" is required for plot_tune_grid().", call. = FALSE)
  }
  .tune_need(data, c("alpha", "lambda", "IBS", "term", "estimate"))
  ok <- !is.na(data$lambda) & !is.na(data$IBS) & data$lambda > 0 & data$IBS > 0
  data <- data[ok, , drop = FALSE]
  if (nrow(data) == 0L) stop("No models with a positive `lambda` and `IBS`.", call. = FALSE)
  fits <- .tune_fits(data)
  fits$log_lambda <- log10(fits$lambda)
  fits$log_ibs <- log10(fits$IBS)
  coefs <- .tune_coefs(data, terms, label_map, style)
  coefs$log_ibs <- log10(coefs$IBS)

  base <- function(mapping) ggplot2::ggplot(fits, mapping) + ggplot2::geom_point(alpha = 0.6, color = "#A41E35") + theme_cbe()
  a <- base(ggplot2::aes(x = .data$alpha, y = .data$IBS)) +
    ggplot2::labs(x = expression(alpha), y = "IBS")
  b <- ggplot2::ggplot(coefs, ggplot2::aes(x = .data$log_ibs, y = .data$estimate, color = .data$term)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_point(alpha = 0.6) +
    theme_cbe() +
    ggplot2::labs(x = expression(log[10](IBS)), y = "Estimate", color = "Term")
  c_ <- base(ggplot2::aes(x = .data$alpha, y = .data$log_lambda)) +
    ggplot2::labs(x = expression(alpha), y = expression(log[10](lambda)))
  d <- base(ggplot2::aes(x = .data$log_ibs, y = .data$log_lambda)) +
    ggplot2::labs(x = expression(log[10](IBS)), y = expression(log[10](lambda)))
  patchwork::wrap_plots(a, b, c_, d, ncol = 2) +
    patchwork::plot_annotation(title = title, tag_levels = "A")
}

#' Density of the Integrated Brier Score per Resample
#'
#' Density curves of the IBS over the tuning grid, one per inner resample, either overlaid or in
#' facets. The box-plot view of the same data is [plot_tune_ibs_by_resample()].
#'
#' @param data A tuning table with `IBS` and a resample column (`inner_resamples_splits`,
#'   `inner_resamples` or `split`). Without a resample column one overall density is drawn.
#' @param facet If `TRUE`, one panel per resample; default `FALSE` overlays the densities.
#' @param log_ibs If `TRUE`, the x-axis is `log10(IBS)`. Default `FALSE`.
#' @param label_map Optional label mapping (see [cbe_labels()]).
#' @param style Optional label style (`"short"` or `"full"`).
#' @return A `ggplot2` object.
#' @seealso [plot_tune_ibs_by_resample()], [tbl_tune_alpha()]
#' @export
#' @examples
#' set.seed(1)
#' tune <- data.frame(split = rep(paste0("Fold", 1:3), each = 20), alpha = 0.5,
#'                    lambda = rep(seq(0.01, 0.2, length.out = 20), 3))
#' tune$IBS <- 0.15 + tune$lambda / 5 + stats::rnorm(60, 0, 0.01)
#' plot_tune_ibs_density(tune)
plot_tune_ibs_density <- function(data, facet = FALSE, log_ibs = FALSE, label_map = NULL, style = NULL) {
  .tune_need(data, "IBS")
  split_col <- .tune_split_col(data)
  df <- .tune_fits(data, cols = intersect(c("alpha", "lambda", "IBS"), names(data)))
  if (isTRUE(log_ibs)) {
    df <- df[df$IBS > 0, , drop = FALSE]
    df$.x <- log10(df$IBS)
  } else {
    df$.x <- df$IBS
  }
  if (nrow(df) < 2L) stop("At least two models with an IBS are needed for a density.", call. = FALSE)
  x_lab <- if (isTRUE(log_ibs)) expression(log[10](IBS)) else "Integrated Brier Score (IBS)"
  if (is.null(split_col)) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$.x)) +
      ggplot2::geom_density(fill = "#A41E35", alpha = 0.4, color = "#A41E35")
  } else {
    df$.split <- factor(resolve_cbe_labels(as.character(df[[split_col]]), label_map = label_map, style = style))
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$.x, fill = .data$.split, color = .data$.split)) +
      ggplot2::geom_density(alpha = 0.25)
  }
  p <- p + theme_cbe() +
    ggplot2::labs(title = "IBS Distribution Across Inner Resamples", x = x_lab, y = "Density",
                  fill = "Resample", color = "Resample")
  if (!is.null(split_col) && isTRUE(facet)) {
    p <- p + ggplot2::facet_wrap(~.split) + ggplot2::theme(legend.position = "none")
  }
  p
}

#' Mixed Model of the Integrated Brier Score on Alpha and Lambda
#'
#' Fits `IBS ~ alpha * log10(lambda) + (1 | resample)` to a tuning grid with [lme4::lmer()] and
#' returns the fixed effects as a table, so the report does not have to rebuild the model and its
#' `gtsummary` call in every chapter.
#'
#' @section Inference:
#' The tibble reports Wald intervals and p-values from the normal approximation
#' (`estimate / std.error`) for the mixed model, which is reasonable here because a tuning grid has many models per
#' resample; they are optimistic when there are few resamples. The models of one grid are fitted to
#' the same data, so treat the p-values as a description of the response surface, not as tests.
#' When there is a single resample, \pkg{lme4} is not installed, or the mixed model fails, an
#' ordinary linear model is fitted instead and `attr(, "method")` is `"lm"`; for that
#' fallback the intervals and p-values use the t-distribution with the model's residual
#' degrees of freedom, which is the correct small-sample reference distribution for `lm`.
#'
#' @param data A tuning table with `alpha`, `lambda`, `IBS` and a resample column
#'   (`inner_resamples_splits`, `inner_resamples` or `split`). Rows are reduced to one per model.
#' @param log_ibs If `TRUE`, the response is `log10(IBS)`. Default `FALSE`.
#' @param random Optional names of further grouping columns that get a random intercept each (for
#'   example the outer fold), in addition to the resample column.
#' @param interaction If `TRUE` (default) the model has the `alpha` by `log10(lambda)` interaction.
#' @param conf_level Confidence level (default 0.95).
#' @param format `"tibble"` (default) or `"gtsummary"`, a [gtsummary::tbl_regression()] table
#'   (needs the \pkg{broom.helpers} package, and \pkg{broom.mixed} for a mixed model).
#' @return A tibble with `term`, `estimate`, `std.error`, `statistic`, `conf.low`, `conf.high` and
#'   `p.value`, with the fitted model in `attr(, "model")`, the formula in `attr(, "formula")` and
#'   `attr(, "method")` (`"lmer"` or `"lm"`); or a `gtsummary` table.
#' @seealso [fit_ibs_mixed_model()], [tbl_tune_alpha()], [plot_tune_grid()]
#' @export
#' @examples
#' set.seed(1)
#' tune <- expand.grid(split = paste0("Fold", 1:4), alpha = c(0.1, 0.5, 1),
#'                     lambda = c(0.01, 0.05, 0.1))
#' tune$IBS <- 0.15 + 0.03 * tune$alpha + 0.1 * tune$lambda + stats::rnorm(nrow(tune), 0, 0.01)
#' tbl_tune_mixed_model(tune)
tbl_tune_mixed_model <- function(data, log_ibs = FALSE, random = NULL, interaction = TRUE,
                                 conf_level = 0.95, format = c("tibble", "gtsummary")) {
  format <- match.arg(format)
  .tune_need(data, c("alpha", "lambda", "IBS", random))
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single number between 0 and 1.", call. = FALSE)
  }
  split_col <- .tune_split_col(data)
  groups <- unique(c(split_col, random))
  keep <- c(groups, "alpha", "lambda", "IBS")
  df <- unique(as.data.frame(data)[, keep, drop = FALSE])
  df <- df[stats::complete.cases(df) & df$lambda > 0, , drop = FALSE]
  if (isTRUE(log_ibs)) df <- df[df$IBS > 0, , drop = FALSE]
  if (nrow(df) < 5L) stop("Too few models (", nrow(df), ") to fit the IBS model.", call. = FALSE)
  if (length(unique(df$alpha)) < 2L || length(unique(df$lambda)) < 2L) {
    stop("`alpha` and `lambda` must each take at least two values.", call. = FALSE)
  }
  df$log10_lambda <- log10(df$lambda)
  df$.y <- if (isTRUE(log_ibs)) log10(df$IBS) else df$IBS
  resp <- if (isTRUE(log_ibs)) "log10_IBS" else "IBS"
  names(df)[names(df) == ".y"] <- resp
  if (isTRUE(log_ibs)) df$IBS <- NULL
  fixed <- paste(resp, "~", if (isTRUE(interaction)) "alpha * log10_lambda" else "alpha + log10_lambda")
  groups <- groups[vapply(groups, function(g) length(unique(df[[g]])) > 1L, logical(1))]

  fit <- NULL
  form <- stats::as.formula(fixed)
  if (length(groups) && requireNamespace("lme4", quietly = TRUE)) {
    mixed <- stats::as.formula(paste(fixed, "+", paste0("(1 | ", groups, ")", collapse = " + ")))
    fit <- tryCatch(suppressMessages(lme4::lmer(mixed, data = df)), error = function(e) NULL)
    if (!is.null(fit)) form <- mixed
  }
  method <- if (is.null(fit)) "lm" else "lmer"
  if (is.null(fit)) fit <- stats::lm(form, data = df)

  if (format == "gtsummary") {
    need <- c("broom.helpers", if (method == "lmer") "broom.mixed")
    miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
    if (length(miss)) {
      stop("`format = \"gtsummary\"` needs package(s): ", paste(miss, collapse = ", "),
           ". Use `format = \"tibble\"` without them.", call. = FALSE)
    }
    return(gtsummary::tbl_regression(fit, conf.level = conf_level))
  }

  if (method == "lmer") {
    est <- lme4::fixef(fit)
    se <- sqrt(diag(as.matrix(stats::vcov(fit))))
  } else {
    est <- stats::coef(fit)
    se <- sqrt(diag(stats::vcov(fit)))[names(est)]
  }
  if (method == "lm") {
    df_resid <- stats::df.residual(fit)
    crit <- stats::qt(1 - (1 - conf_level) / 2, df = df_resid)
    stat <- unname(est / se)
    pval <- unname(2 * stats::pt(-abs(stat), df = df_resid))
  } else {
    crit <- stats::qnorm(1 - (1 - conf_level) / 2)
    stat <- unname(est / se)
    pval <- unname(2 * stats::pnorm(-abs(stat)))
  }
  out <- tibble::tibble(
    term = names(est),
    estimate = unname(est),
    std.error = unname(se),
    statistic = stat,
    conf.low = unname(est - crit * se),
    conf.high = unname(est + crit * se),
    p.value = pval
  )
  attr(out, "model") <- fit
  attr(out, "formula") <- form
  attr(out, "method") <- method
  out
}
