#' Cox Proportional Hazards Diagnostic and Reporting Visualizations
#'
#' Functions for creating hazard ratio forest plots, model-predicted survival curves,
#' and continuous predictor vs. event probability diagnostic plots.
#'
#' @importFrom rlang %||% .data
#' @keywords internal
#' @name plot_cox
NULL

#' Forest Plot of Hazard Ratios with Confidence Intervals
#'
#' Generates a publication/deck-ready horizontal forest plot of hazard ratios.
#'
#' @param data Data frame containing tidy Cox regression results (e.g. from \code{broom::tidy} or multi-model runs).
#' @param label_col Column name for labels / terms (character string or unquoted).
#' @param hr_col Column name for hazard ratios, always on the HR (not log) scale (default: "estimate").
#' @param low_col Column name for lower CI bounds, on the HR scale (default: "conf.low").
#' @param high_col Column name for upper CI bounds, on the HR scale (default: "conf.high").
#' @param p_col Optional column name for p-values. Required for \code{color_by = "significance"}
#'   and \code{order_by = "pvalue"}.
#' @param color Primary color for points and error bars (default: Temple Cherry \code{"#9D2235"}).
#' @param scale One of \code{"hr"} (default; plots hazard ratios with a reference line at 1) or
#'   \code{"log_hr"} (log-transforms \code{hr_col}/\code{low_col}/\code{high_col} for plotting, with
#'   a reference line at 0).
#' @param color_by One of \code{"none"} (default) or \code{"significance"}, which colors points/error
#'   bars by whether \code{p_col} is below 0.05 and adds a legend.
#' @param order_by One of \code{"none"} (default; original row order), \code{"magnitude"} (sorts by
#'   \code{abs(log(estimate))} descending), or \code{"pvalue"} (sorts by \code{p_col} ascending).
#' @param x_limits Optional 2-element numeric vector for x-axis limits.
#' @param x_breaks Optional numeric vector of axis break points.
#' @param title Optional plot title.
#' @param caption Optional plot caption.
#' @param base_size Base font size (default: 13).
#' @return A ggplot2 object.
#' @seealso [plot_cox_forest_multi()]
#' @export
plot_cox_forest <- function(data,
                            label_col = "index_label",
                            hr_col = "estimate",
                            low_col = "conf.low",
                            high_col = "conf.high",
                            p_col = NULL,
                            color = "#9D2235",
                            scale = c("hr", "log_hr"),
                            color_by = c("none", "significance"),
                            order_by = c("none", "magnitude", "pvalue"),
                            x_limits = NULL,
                            x_breaks = NULL,
                            title = NULL,
                            caption = NULL,
                            base_size = 13) {
  scale <- match.arg(scale)
  color_by <- match.arg(color_by)
  order_by <- match.arg(order_by)

  if (inherits(data, "cbe_cox")) {
    conf_level <- data$conf_level %||% 0.95
    td <- broom::tidy(data$model, exponentiate = TRUE, conf.int = TRUE, conf.level = conf_level)
    if (nrow(td) == 1) {
      td$index_label <- data$var_label
    } else {
      lvl <- stringr::str_remove(td$term, stringr::fixed(data$var_name))
      td$index_label <- paste0(data$var_label, ": ", lvl)
    }
    if (conf_level != 0.95) {
      attr(td, "ci_label") <- sprintf("%d%% CI", round(conf_level * 100))
    }
    data <- td
  }

  forest_plot_engine(
    df = data, label_col = label_col, hr_col = hr_col, low_col = low_col, high_col = high_col,
    p_col = p_col, color = color, x_limits = x_limits, x_breaks = x_breaks,
    title = title, caption = caption, base_size = base_size,
    scale = scale, color_by = color_by, order_by = order_by
  )
}

#' Forest Plot of Hazard Ratios for a Multivariable Cox Model
#'
#' Generates a forest plot from a [cbe_cox_multi()] result, grouping terms into
#' per-variable blocks (facets) so factor levels stay visually attached to their
#' parent variable.
#'
#' @param x A \code{cbe_cox_multi} object from [cbe_cox_multi()].
#' @param color Primary color for points and error bars (default: Temple Cherry \code{"#9D2235"}).
#' @param scale One of \code{"hr"} (default) or \code{"log_hr"}. See [plot_cox_forest()].
#' @param color_by One of \code{"none"} (default) or \code{"significance"}. See [plot_cox_forest()].
#' @param order_by One of \code{"none"} (default), \code{"magnitude"}, or \code{"pvalue"}, applied
#'   within each variable's block. See [plot_cox_forest()].
#' @param x_limits Optional 2-element numeric vector for x-axis limits.
#' @param x_breaks Optional numeric vector of axis break points.
#' @param title Optional plot title.
#' @param caption Optional plot caption.
#' @param base_size Base font size (default: 13).
#' @return A ggplot2 object.
#' @seealso [cbe_cox_multi()], [plot_cox_forest()]
#' @export
plot_cox_forest_multi <- function(x,
                                  color = "#9D2235",
                                  scale = c("hr", "log_hr"),
                                  color_by = c("none", "significance"),
                                  order_by = c("none", "magnitude", "pvalue"),
                                  x_limits = NULL,
                                  x_breaks = NULL,
                                  title = NULL,
                                  caption = NULL,
                                  base_size = 13) {
  if (!inherits(x, "cbe_cox_multi")) {
    stop("`x` must be a `cbe_cox_multi` object (see `cbe_cox_multi()`).", call. = FALSE)
  }
  scale <- match.arg(scale)
  color_by <- match.arg(color_by)
  order_by <- match.arg(order_by)

  df <- cox_multi_plot_frame(x)

  forest_plot_engine(
    df = df, label_col = "Level", hr_col = "estimate", low_col = "conf.low", high_col = "conf.high",
    p_col = "p.value", color = color, x_limits = x_limits, x_breaks = x_breaks,
    title = title, caption = caption, base_size = base_size,
    scale = scale, color_by = color_by, order_by = order_by, facet_col = "Variable"
  )
}

# Rebuilds a numeric (non-rounded, non-string) tidy frame of a cbe_cox_multi
# model's non-reference terms, for forest plotting. `x$table` is presentation-
# formatted (rounded HR, CI as a single string), so this re-derives estimate/
# conf.low/conf.high straight from broom::tidy(x$model).
cox_multi_plot_frame <- function(x) {
  td <- broom::tidy(x$model, conf.int = TRUE, exponentiate = TRUE)

  feat_by_length <- x$features[order(-nchar(x$features))]
  matched_feature <- vapply(td$term, function(term) {
    hit <- feat_by_length[startsWith(term, feat_by_length)]
    if (length(hit) == 0) NA_character_ else hit[1]
  }, character(1))

  td$Variable <- unname(x$var_labels[matched_feature])
  td$Level <- ifelse(
    td$term == matched_feature,
    "1-unit increase",
    substring(td$term, nchar(matched_feature) + 1)
  )
  td
}

# Shared rendering engine behind plot_cox_forest() and plot_cox_forest_multi():
# handles the scale (hr/log_hr), color_by (none/significance), order_by
# (none/magnitude/pvalue), and optional per-Variable facet_grid() blocks.
forest_plot_engine <- function(df,
                               label_col, hr_col, low_col, high_col, p_col,
                               color, x_limits, x_breaks, title, caption, base_size,
                               scale = "hr", color_by = "none", order_by = "none",
                               facet_col = NULL) {
  if (color_by == "significance" && is.null(p_col)) {
    stop("`color_by = \"significance\"` requires `p_col`.", call. = FALSE)
  }
  if (order_by == "pvalue" && is.null(p_col)) {
    stop("`order_by = \"pvalue\"` requires `p_col`.", call. = FALSE)
  }

  df <- as.data.frame(df)
  df$.label <- as.character(df[[label_col]])
  df$.hr  <- df[[hr_col]]
  df$.low <- df[[low_col]]
  df$.hi  <- df[[high_col]]
  df$.p   <- if (!is.null(p_col)) suppressWarnings(as.numeric(df[[p_col]])) else NA_real_
  if (!is.null(facet_col)) df$.facet <- as.character(df[[facet_col]])

  if (order_by == "magnitude") {
    ord <- if (!is.null(facet_col)) order(df$.facet, -abs(log(df$.hr))) else order(-abs(log(df$.hr)))
    df <- df[ord, , drop = FALSE]
  } else if (order_by == "pvalue") {
    ord <- if (!is.null(facet_col)) order(df$.facet, df$.p) else order(df$.p)
    df <- df[ord, , drop = FALSE]
  } else if (!is.null(facet_col)) {
    df <- df[order(df$.facet), , drop = FALSE]
  }

  df$.y <- factor(df$.label, levels = rev(unique(df$.label)))

  if (scale == "log_hr") {
    df$.x    <- log(df$.hr)
    df$.xmin <- log(df$.low)
    df$.xmax <- log(df$.hi)
    x_intercept <- 0
    x_lab <- "log(Hazard Ratio)"
  } else {
    df$.x    <- df$.hr
    df$.xmin <- df$.low
    df$.xmax <- df$.hi
    x_intercept <- 1
    ci_lbl <- attr(df, "ci_label") %||% "95% CI"
    x_lab <- paste0("Hazard Ratio (", ci_lbl, ")")
  }

  if (color_by == "significance") {
    df$.sig <- ifelse(df$.p < 0.05, "Significant (p < 0.05)", "Not significant")
    color_values <- stats::setNames(c(color, "grey60"), c("Significant (p < 0.05)", "Not significant"))
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$.x, y = .data$.y, color = .data$.sig)) +
      ggplot2::geom_vline(xintercept = x_intercept, linetype = "dashed", color = "grey50") +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$.xmin, xmax = .data$.xmax), width = 0.25, linewidth = 0.8) +
      ggplot2::geom_point(size = 3.5) +
      ggplot2::scale_color_manual(values = color_values, name = "Significance")
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$.x, y = .data$.y)) +
      ggplot2::geom_vline(xintercept = x_intercept, linetype = "dashed", color = "grey50") +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$.xmin, xmax = .data$.xmax), width = 0.25, linewidth = 0.8, color = color) +
      ggplot2::geom_point(size = 3.5, color = color)
  }

  p <- p +
    cbe_theme_survival(base_size = base_size) +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 18, 5.5, 5.5)) +
    ggplot2::labs(title = title, x = x_lab, y = NULL, caption = caption)

  if (!is.null(x_limits)) {
    p <- p + ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks %||% scales::breaks_pretty(n = 5))
  }

  if (!is.null(facet_col)) {
    p <- p + ggplot2::facet_grid(rows = ggplot2::vars(.data$.facet), scales = "free_y", space = "free_y")
  }

  p
}

#' Model-Predicted Survival Curves Stratified by Predictor
#'
#' Generates stratified predicted survival curves from a Cox model. For continuous predictors,
#' values are automatically binned into quantile strata (e.g. quartiles).
#'
#' @param fit A fitted \code{survival::coxph} model or a \code{cbe_cox} object.
#' @param data Data frame used for fitting the model.
#' @param feature Character name of the predictor column.
#' @param id_col Character name of subject ID column (default: "arl_number" or row number).
#' @param n_tiles Number of quantile bins for continuous predictors (default: 4).
#' @param label_endpoints Logical; if TRUE, repels labels for stratum values at the final time point.
#' @param overlay_km Logical (default: \code{FALSE}). If \code{TRUE}, overlays observed
#'   Kaplan-Meier step curves (dashed) for each stratum on top of the Cox-predicted curves
#'   (solid), sharing color by stratum, with a linetype legend distinguishing
#'   "Cox-predicted" from "KM observed". The event time/status used are those the model
#'   was fit with (\code{fit$y}).
#' @param base_size Base font size (default: 12).
#' @return A ggplot2 object.
#' @export
plot_cox_survival <- function(fit,
                              data,
                              feature = NULL,
                              id_col = NULL,
                              n_tiles = 4,
                              label_endpoints = TRUE,
                              overlay_km = FALSE,
                              base_size = 12) {
  if (inherits(fit, "cbe_cox")) {
    feature <- fit$feature
    fit <- fit$model
  }

  if (is.null(feature)) {
    stop("Must specify 'feature' name if 'fit' is a standard coxph object.")
  }

  df <- data
  var_vals <- df[[feature]]
  var_lbl <- if (requireNamespace("labelled", quietly = TRUE)) {
    labelled::var_label(var_vals) %||% feature
  } else {
    feature
  }

  is_num <- is.numeric(var_vals)

  # Prepare subject ID
  if (!is.null(id_col) && id_col %in% names(df)) {
    df$subject_id <- as.character(df[[id_col]])
  } else {
    df$subject_id <- as.character(seq_len(nrow(df)))
  }

  # Build stratum column
  if (is_num) {
    df_binned <- df |>
      dplyr::mutate(tile = dplyr::ntile(.data[[feature]], n_tiles)) |>
      dplyr::group_by(tile) |>
      dplyr::mutate(
        min_v = min(.data[[feature]], na.rm = TRUE),
        max_v = max(.data[[feature]], na.rm = TRUE),
        strata = sprintf("[%s \u2013 %s]", fmt_num(min_v, 1), fmt_num(max_v, 1))
      ) |>
      dplyr::ungroup()
  } else {
    df_binned <- df |>
      dplyr::mutate(strata = as.factor(.data[[feature]]))
  }

  # Predict curves from coxph
  sf <- survival::survfit(fit, newdata = df)
  td_sf <- broom::tidy(sf)

  # Check if tidy format has columns by index
  if (any(grepl("^estimate\\.", names(td_sf)))) {
    est_df <- td_sf |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("estimate."),
        names_to = "subject_id",
        names_prefix = "estimate.",
        values_to = "estimate"
      ) |>
      dplyr::select(time, subject_id, estimate)
  } else {
    est_df <- td_sf |>
      dplyr::mutate(subject_id = "1") |>
      dplyr::select(time, subject_id, estimate)
  }

  # Only bring over identifying/stratifying columns from df_binned: it still carries the
  # original survival `time`/`status` columns from `data`, which would otherwise collide
  # with est_df's predicted-curve `time` column and get silently suffixed by the join.
  plot_data <- est_df |>
    dplyr::inner_join(dplyr::select(df_binned, subject_id, strata), by = "subject_id")

  p <- ggplot2::ggplot()

  if (overlay_km) {
    plot_data$.curve_type <- "Cox-predicted"

    y_resp <- fit$y
    n_ycol <- ncol(y_resp)
    km_df <- df_binned
    km_df$.time   <- if (n_ycol >= 3) y_resp[, 2] else y_resp[, 1]
    km_df$.status <- y_resp[, n_ycol]

    km_fit <- survival::survfit(survival::Surv(.time, .status) ~ strata, data = km_df)
    km_tidy <- broom::tidy(km_fit)
    km_tidy$strata <- sub("^strata=", "", km_tidy$strata)
    km_tidy$.curve_type <- "KM observed"

    p <- p +
      ggplot2::geom_step(
        data = km_tidy,
        ggplot2::aes(x = time, y = estimate, color = strata, group = strata, linetype = .data$.curve_type),
        linewidth = 0.8
      )
  }

  cox_line_aes <- if (overlay_km) {
    ggplot2::aes(x = time, y = estimate, color = strata, group = subject_id, linetype = .data$.curve_type)
  } else {
    ggplot2::aes(x = time, y = estimate, color = strata, group = subject_id)
  }

  p <- p +
    ggplot2::geom_line(data = plot_data, cox_line_aes, alpha = 0.7, linewidth = 0.8) +
    ggplot2::geom_point(data = plot_data, ggplot2::aes(x = time, y = estimate, color = strata), size = 1.2, alpha = 0.7) +
    cbe_theme_survival(base_size = base_size) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::scale_color_manual(values = unname(cbe_palette)) +
    ggplot2::labs(
      title = sprintf("Cox Proportional Hazards Model: %s", var_lbl),
      subtitle = if (overlay_km) {
        "Cox-Predicted (solid) vs. Kaplan-Meier Observed (dashed) Survival Curves"
      } else {
        "Estimated Survival Probability Curves"
      },
      x = "Follow-up Time",
      y = "Survival Probability",
      color = if (is_num) sprintf("%s (Quantiles)", var_lbl) else var_lbl,
      linetype = if (overlay_km) "Curve Type" else NULL
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = min(n_tiles, 4)))

  if (overlay_km) {
    p <- p + ggplot2::scale_linetype_manual(values = c("Cox-predicted" = "solid", "KM observed" = "dashed"))
  }

  p
}

#' Marginal Event Probability / Relative Hazard Diagnostic Plot
#'
#' Plots a continuous predictor against a model-derived curve, with a 95% CI band and,
#' for \code{scale = "prob"}, overlaid actual binary event observations.
#'
#' @param fit A fitted \code{survival::coxph} model or a \code{cbe_cox} object.
#' @param data Data frame containing survival inputs.
#' @param feature Character name of continuous predictor column.
#' @param status_col Character name of status/event column (0 = censored, 1 = event; default: "status").
#'   Only used when \code{scale = "prob"}.
#' @param scale One of \code{"prob"} (default; predicted probability of the event),
#'   \code{"hr"} (relative hazard, i.e. \code{exp()} of the centered linear predictor), or
#'   \code{"log_hr"} (the centered linear predictor itself).
#' @param color Primary color for the fitted curve and CI band (default: Temple Cherry \code{"#9D2235"}).
#' @param base_size Base font size (default: 12).
#' @return A ggplot2 object.
#' @export
plot_cox_marginal <- function(fit,
                              data,
                              feature = NULL,
                              status_col = "status",
                              scale = c("prob", "hr", "log_hr"),
                              color = "#9D2235",
                              base_size = 12) {
  scale <- match.arg(scale)

  if (inherits(fit, "cbe_cox")) {
    feature <- fit$feature
    fit <- fit$model
  }

  if (is.null(feature)) {
    stop("Must specify 'feature' name if 'fit' is a standard coxph object.")
  }

  var_vals <- data[[feature]]
  var_lbl <- if (requireNamespace("labelled", quietly = TRUE)) {
    labelled::var_label(var_vals) %||% feature
  } else {
    feature
  }

  if (scale == "prob") {
    aug <- broom::augment(fit, data = data, type.predict = "survival")
    if (!".fitted" %in% names(aug)) {
      stop("Failed to extract .fitted survival probabilities from model augment.")
    }
    se_fit <- if (".se.fit" %in% names(aug)) aug[[".se.fit"]] else rep(0, nrow(aug))

    x_raw      <- aug[[feature]]
    center_raw <- 1 - aug[[".fitted"]]
    lower_raw  <- pmax(0, center_raw - 1.96 * se_fit)
    upper_raw  <- pmin(1, center_raw + 1.96 * se_fit)

    y_lab <- "Predicted Event Probability"
    subtitle <- "Fitted risk curve (solid) with 95% CI band; circles show observed status"
    title <- sprintf("Predicted Probability of Event vs. %s", var_lbl)
  } else {
    lp <- stats::predict(fit, newdata = data, type = "lp", se.fit = TRUE)
    centered <- lp$fit - mean(lp$fit, na.rm = TRUE)

    x_raw <- data[[feature]]
    if (scale == "hr") {
      center_raw <- exp(centered)
      lower_raw  <- exp(centered - 1.96 * lp$se.fit)
      upper_raw  <- exp(centered + 1.96 * lp$se.fit)
      y_lab <- "Relative Hazard (centered)"
      title <- sprintf("Predicted Relative Hazard vs. %s", var_lbl)
    } else {
      center_raw <- centered
      lower_raw  <- centered - 1.96 * lp$se.fit
      upper_raw  <- centered + 1.96 * lp$se.fit
      y_lab <- "log(Relative Hazard) (centered)"
      title <- sprintf("Predicted log(Relative Hazard) vs. %s", var_lbl)
    }
    subtitle <- "Relative hazard curve (solid) from the centered linear predictor, with 95% CI band"
  }

  ok <- stats::complete.cases(data.frame(x_raw, center_raw, lower_raw, upper_raw))
  x_raw <- x_raw[ok]; center_raw <- center_raw[ok]; lower_raw <- lower_raw[ok]; upper_raw <- upper_raw[ok]

  x_grid <- seq(min(x_raw), max(x_raw), length.out = 100)
  smooth_curve <- function(y) {
    fit_loess <- tryCatch(
      stats::loess(y ~ x, data = data.frame(x = x_raw, y = y), span = 0.9),
      error = function(e) NULL
    )
    if (is.null(fit_loess)) return(rep(NA_real_, length(x_grid)))
    as.numeric(stats::predict(fit_loess, newdata = data.frame(x = x_grid)))
  }

  band <- data.frame(
    x = x_grid,
    center = smooth_curve(center_raw),
    lower  = smooth_curve(lower_raw),
    upper  = smooth_curve(upper_raw)
  )
  if (scale == "prob") {
    band$lower <- pmax(0, band$lower)
    band$upper <- pmin(1, band$upper)
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = band, ggplot2::aes(x = x, ymin = lower, ymax = upper), fill = color, alpha = 0.15) +
    ggplot2::geom_line(data = band, ggplot2::aes(x = x, y = center), color = color, linewidth = 1.1)

  if (scale == "prob") {
    status_vals <- if (status_col %in% names(data)) data[[status_col]] else rep(NA_real_, nrow(data))
    point_df <- data.frame(x = data[[feature]], y = status_vals)
    p <- p + ggplot2::geom_point(data = point_df, ggplot2::aes(x = x, y = y), shape = 1, size = 2, color = "grey30", alpha = 0.6)
  } else {
    p <- p + ggplot2::geom_hline(yintercept = if (scale == "hr") 1 else 0, linetype = "dashed", color = "grey50")
  }

  p +
    cbe_theme_survival(base_size = base_size) +
    { if (scale == "prob") ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) } +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = var_lbl,
      y = y_lab
    )
}

utils::globalVariables(c(
  "tile", "min_v", "max_v", "time", "subject_id",
  "estimate", "strata", "x", "y", "lower", "upper", "center"
))
