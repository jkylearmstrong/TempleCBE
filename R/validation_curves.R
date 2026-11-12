# Calibration and decision curves for predicted risks, binary or time-to-event.

# Normalizes `truth` (0/1, logical, two-level factor or right-censored Surv) and `pred`
# (a vector, a named list or a data frame of predicted risks) into one complete-case frame.
#' @noRd
validation_inputs <- function(truth, pred, horizon, event_level) {
  is_surv <- inherits(truth, "Surv")
  if (is_surv) {
    rlang::check_installed("survival", reason = "for time-to-event calibration and decision curves.")
    if (!identical(attr(truth, "type"), "right")) {
      stop("`truth` must be a right-censored Surv(time, event); for start/stop data use surv_subject_truth() first.", call. = FALSE)
    }
    if (!is.numeric(horizon) || length(horizon) != 1L || is.na(horizon) || horizon <= 0) {
      stop("`horizon` (a single positive time) is required when `truth` is a Surv object.", call. = FALSE)
    }
    m <- unclass(truth)
    y <- list(time = unname(m[, "time"]), event = unname(m[, "status"]))
    n <- length(y$time)
  } else {
    if (!is.null(horizon)) stop("`horizon` applies to Surv outcomes only.", call. = FALSE)
    if (is.factor(truth) || is.character(truth)) {
      truth <- factor(truth)
      if (nlevels(truth) != 2L) stop("A factor `truth` must have two levels.", call. = FALSE)
      yy <- as.numeric(truth == levels(truth)[if (event_level == "second") 2L else 1L])
    } else if (is.logical(truth)) {
      yy <- as.numeric(truth)
    } else if (is.numeric(truth) && all(truth %in% c(0, 1, NA))) {
      yy <- as.numeric(truth)
    } else {
      stop("`truth` must be 0/1, logical, a two-level factor or a Surv object.", call. = FALSE)
    }
    y <- list(y = yy)
    n <- length(yy)
  }

  if (is.numeric(pred) && is.null(dim(pred))) {
    preds <- list(model = as.numeric(pred))
  } else if (is.data.frame(pred) || is.list(pred)) {
    preds <- lapply(pred, as.numeric)
    if (is.null(names(preds)) || any(!nzchar(names(preds)))) {
      names(preds) <- paste0("model", seq_along(preds))
    }
  } else {
    stop("`pred` must be a numeric vector of predicted risks, or a named list or data frame of them.", call. = FALSE)
  }
  if (any(lengths(preds) != n)) stop("`pred` and `truth` must have the same length.", call. = FALSE)
  all_pred <- unlist(preds, use.names = FALSE)
  if (any(all_pred < 0 | all_pred > 1, na.rm = TRUE)) {
    stop("Predicted risks must be probabilities between 0 and 1.", call. = FALSE)
  }
  ok <- Reduce(`&`, lapply(preds, function(p) !is.na(p)))
  ok <- ok & if (is_surv) !is.na(y$time) & !is.na(y$event) else !is.na(y$y)
  if (sum(ok) < 2L) stop("Fewer than two complete observations.", call. = FALSE)
  y <- lapply(y, function(v) v[ok])
  preds <- lapply(preds, function(p) p[ok])
  list(y = y, preds = preds, is_surv = is_surv, horizon = horizon, n = sum(ok))
}

# Kaplan-Meier risk (1 - S) at `horizon` with a log-log confidence interval; NA when the
# follow-up of the group ends before `horizon`
#' @noRd
km_risk_at <- function(time, event, horizon, conf_level) {
  if (length(time) == 0L) return(c(risk = NA_real_, lower = NA_real_, upper = NA_real_))
  sf <- survival::survfit(survival::Surv(time, event) ~ 1, conf.type = "log-log", conf.int = conf_level)
  s <- summary(sf, times = horizon, extend = FALSE)
  if (length(s$surv) == 0L) {
    # follow-up of the group ends before the horizon
    return(c(risk = NA_real_, lower = NA_real_, upper = NA_real_))
  }
  lo <- s$lower
  hi <- s$upper
  c(risk = 1 - s$surv, lower = if (is.null(lo) || is.na(lo)) NA_real_ else 1 - hi, upper = if (is.null(hi) || is.na(hi)) NA_real_ else 1 - lo)
}

# Wilson interval for a proportion
#' @noRd
wilson_interval <- function(k, n, conf_level) {
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  p <- k / n
  center <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(lower = max(0, center - half), upper = min(1, center + half))
}

#' Calibration Table of Predicted Risks
#'
#' Groups the predicted risks into bins and compares the mean predicted risk of each bin
#' with the observed event rate. A well-calibrated model lies on the diagonal. Works for a
#' binary outcome and, with `horizon`, for a right-censored time-to-event outcome, where
#' the observed risk of a bin is one minus its Kaplan-Meier survival at `horizon`.
#' Competing risks are not accounted for: a competing event is treated as censoring in the
#' Kaplan-Meier estimate, which overstates the observed risk when competing events are
#' common.
#'
#' For a binary outcome the table also has the calibration intercept (calibration-in-the-large,
#' with the logit of the predicted risk as an offset) and slope (the coefficient of the
#' logit of the predicted risk), and the Brier score, in `attr(x, "metrics")`. A slope below 1
#' means the predictions are too extreme (overfitting). These are `NA` for Surv outcomes,
#' and also `NA` when the binary outcome has a single class (the logistic fits are then
#' degenerate: separation makes the intercept/slope essentially arbitrary).
#'
#' @param truth The outcome: 0/1, logical, a two-level factor, or a right-censored
#'   [survival::Surv()] object.
#' @param pred Predicted risks (probabilities of the event, by `horizon` for Surv outcomes):
#'   a numeric vector, or a named list or data frame with one element per model.
#' @param horizon For a Surv `truth`, the time at which the predicted risks apply.
#' @param bins Number of bins (default `10`).
#' @param strategy `"quantile"` (default; bins of equal size) or `"uniform"` (equal-width bins
#'   of the predicted risk).
#' @param event_level For a factor `truth`: `"second"` (default, as `glm()`) or `"first"` level
#'   is the event.
#' @param conf_level Level of the confidence intervals (default `0.95`): Wilson for binary,
#'   log-log Kaplan-Meier for Surv outcomes.
#' @return A tibble with one row per model and bin: `model`, `bin`, `n`, `mean_pred`, `observed`,
#'   `conf_low`, `conf_high`. `attr(x, "metrics")` is a tibble with `model`, `n`, `intercept`,
#'   `slope` and `brier`.
#' @seealso [plot_calibration()], [decision_curve()]
#' @export
#' @examples
#' set.seed(1)
#' p <- runif(500)
#' y <- rbinom(500, 1, p)
#' calibration_table(y, p, bins = 5)
#' attr(calibration_table(y, p), "metrics")
calibration_table <- function(truth,
                              pred,
                              horizon = NULL,
                              bins = 10,
                              strategy = c("quantile", "uniform"),
                              event_level = c("second", "first"),
                              conf_level = 0.95) {
  strategy <- match.arg(strategy)
  event_level <- match.arg(event_level)
  if (!is.numeric(bins) || length(bins) != 1L || is.na(bins) || bins < 2) stop("`bins` must be a number of at least 2.", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a number between 0 and 1.", call. = FALSE)
  }
  inp <- validation_inputs(truth, pred, horizon, event_level)

  tabs <- list()
  mets <- list()
  for (nm in names(inp$preds)) {
    p <- inp$preds[[nm]]
    brk <- if (strategy == "quantile") {
      unique(stats::quantile(p, probs = seq(0, 1, length.out = bins + 1L), names = FALSE, type = 7))
    } else {
      seq(min(p), max(p), length.out = bins + 1L)
    }
    if (length(brk) < 2L) brk <- c(min(p), max(p) + 1e-9)
    bin <- findInterval(p, brk, rightmost.closed = TRUE, all.inside = TRUE)
    rows <- lapply(sort(unique(bin)), function(b) {
      idx <- bin == b
      n_b <- sum(idx)
      if (inp$is_surv) {
        r <- km_risk_at(inp$y$time[idx], inp$y$event[idx], inp$horizon, conf_level)
        obs <- r[["risk"]]; lo <- r[["lower"]]; hi <- r[["upper"]]
      } else {
        k <- sum(inp$y$y[idx])
        obs <- k / n_b
        ci <- wilson_interval(k, n_b, conf_level)
        lo <- ci[["lower"]]; hi <- ci[["upper"]]
      }
      tibble::tibble(model = nm, bin = b, n = n_b, mean_pred = mean(p[idx]), observed = obs, conf_low = lo, conf_high = hi)
    })
    tabs[[nm]] <- dplyr::bind_rows(rows)

    if (inp$is_surv) {
      mets[[nm]] <- tibble::tibble(model = nm, n = inp$n, intercept = NA_real_, slope = NA_real_, brier = NA_real_)
    } else {
      yy <- inp$y$y
      if (length(unique(yy)) < 2L) {
        # A single-class outcome: both logistic fits are degenerate (the
        # slope/intercept are driven by separation, not by calibration), so
        # report them as NA rather than e.g. intercept -28 / slope ~0.
        slope <- NA_real_
        icpt <- NA_real_
      } else {
        pc <- pmin(pmax(p, 1e-8), 1 - 1e-8)
        lp <- stats::qlogis(pc)
        slope <- tryCatch(unname(stats::coef(suppressWarnings(stats::glm(yy ~ lp, family = stats::binomial())))[2L]), error = function(e) NA_real_)
        icpt <- tryCatch(unname(stats::coef(suppressWarnings(stats::glm(yy ~ 1 + offset(lp), family = stats::binomial())))[1L]), error = function(e) NA_real_)
      }
      mets[[nm]] <- tibble::tibble(model = nm, n = inp$n, intercept = icpt, slope = slope, brier = mean((p - yy)^2))
    }
  }
  out <- dplyr::bind_rows(tabs)
  attr(out, "metrics") <- dplyr::bind_rows(mets)
  attr(out, "horizon") <- inp$horizon
  out
}

#' Calibration Plot of Predicted Risks
#'
#' Plots the mean predicted risk of each bin against the observed event rate, with a
#' confidence interval, the diagonal of perfect calibration, and the calibration slope
#' and intercept of each model in the subtitle (binary outcomes). See [calibration_table()]
#' for the definitions, the Surv outcomes and the handling of competing events.
#'
#' @inheritParams calibration_table
#' @param smooth Also draw a loess smooth of the observed event on the predicted risk, for a
#'   binary outcome (default `FALSE`).
#' @return A `ggplot2` object.
#' @seealso [calibration_table()], [decision_curve()]
#' @export
#' @examples
#' set.seed(1)
#' p <- runif(500)
#' y <- rbinom(500, 1, p)
#' plot_calibration(y, list(correct = p, overconfident = plogis(2 * qlogis(pmin(pmax(p, .01), .99)))))
plot_calibration <- function(truth,
                             pred,
                             horizon = NULL,
                             bins = 10,
                             strategy = c("quantile", "uniform"),
                             event_level = c("second", "first"),
                             conf_level = 0.95,
                             smooth = FALSE) {
  tab <- calibration_table(truth, pred, horizon = horizon, bins = bins, strategy = match.arg(strategy),
                           event_level = match.arg(event_level), conf_level = conf_level)
  mets <- attr(tab, "metrics")
  multi <- length(unique(tab$model)) > 1L
  p <- ggplot2::ggplot(tab, ggplot2::aes(x = .data$mean_pred, y = .data$observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50")
  if (multi) {
    p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$conf_low, ymax = .data$conf_high, color = .data$model), width = 0, alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(color = .data$model)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$model), size = 2)
  } else {
    p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$conf_low, ymax = .data$conf_high), width = 0, color = "#9D2235", alpha = 0.6) +
      ggplot2::geom_line(color = "#9D2235") +
      ggplot2::geom_point(color = "#9D2235", size = 2)
  }
  if (isTRUE(smooth) && is.null(horizon)) {
    inp <- validation_inputs(truth, pred, NULL, match.arg(event_level))
    sm <- dplyr::bind_rows(lapply(names(inp$preds), function(nm) {
      data.frame(model = nm, pred = inp$preds[[nm]], y = inp$y$y)
    }))
    p <- p + ggplot2::geom_smooth(
      data = sm, ggplot2::aes(x = .data$pred, y = .data$y, color = if (multi) .data$model else NULL),
      method = "loess", formula = y ~ x, se = FALSE, linewidth = 0.6, linetype = "dotted", inherit.aes = FALSE
    )
  }
  sub <- if (!all(is.na(mets$slope))) {
    paste(sprintf("%s: slope %.2f, intercept %.2f", mets$model, mets$slope, mets$intercept), collapse = "; ")
  } else NULL
  p +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_cbe() +
    ggplot2::labs(
      title = "Calibration", subtitle = sub,
      x = "Mean predicted risk", y = if (is.null(horizon)) "Observed event rate" else paste0("Observed risk by time ", format(horizon)),
      color = "Model"
    )
}

#' Decision Curve Analysis: Net Benefit of a Risk Model
#'
#' Net benefit of acting on patients whose predicted risk is at or above a threshold
#' probability, `TP/n - FP/n * pt / (1 - pt)`, over a range of thresholds `pt`, against the
#' strategies "treat all" and "treat none" (net benefit 0) (Vickers and Elkin, 2006). A model is
#' useful at the thresholds where its curve lies above both. For a right-censored outcome
#' (`horizon`), the event rate in the group above the threshold is the Kaplan-Meier risk at the
#' horizon, as in Vickers et al. (2008); competing events are treated as censoring.
#'
#' @inheritParams calibration_table
#' @param thresholds Threshold probabilities, strictly between 0 and 1 (default
#'   `seq(0.01, 0.99, by = 0.01)`).
#' @return A tibble of class `decision_curve` with one row per model and threshold: `model`,
#'   `threshold`, `net_benefit`, `treat_all` (net benefit of treating everyone) and
#'   `prop_above` (the share of patients at or above the threshold). Treat none is 0.
#' @references Vickers AJ, Elkin EB (2006). Decision curve analysis: a novel method for
#'   evaluating prediction models. Medical Decision Making 26:565-574. Vickers AJ, Cronin AM,
#'   Elkin EB, Gonen M (2008). Extensions to decision curve analysis, a novel method for
#'   evaluating diagnostic tests, prediction models and molecular markers. BMC Medical
#'   Informatics and Decision Making 8:53.
#' @seealso [plot_decision_curve()], [calibration_table()]
#' @export
#' @examples
#' set.seed(1)
#' x <- rnorm(400)
#' y <- rbinom(400, 1, plogis(-1 + x))
#' fit <- glm(y ~ x, family = binomial())
#' head(decision_curve(y, fitted(fit)))
decision_curve <- function(truth,
                           pred,
                           horizon = NULL,
                           thresholds = seq(0.01, 0.99, by = 0.01),
                           event_level = c("second", "first")) {
  event_level <- match.arg(event_level)
  if (!is.numeric(thresholds) || length(thresholds) == 0L || anyNA(thresholds) || any(thresholds <= 0 | thresholds >= 1)) {
    stop("`thresholds` must be probabilities strictly between 0 and 1.", call. = FALSE)
  }
  thresholds <- sort(unique(thresholds))
  inp <- validation_inputs(truth, pred, horizon, event_level)
  odds <- thresholds / (1 - thresholds)

  beyond_followup <- inp$is_surv && isTRUE(inp$horizon > max(inp$y$time, na.rm = TRUE))
  if (beyond_followup) {
    warning(
      "`horizon` (", inp$horizon, ") is beyond the maximum follow-up time (", max(inp$y$time, na.rm = TRUE),
      "): the Kaplan-Meier risk at the horizon is NA, so `treat_all` and most `net_benefit` values are NA. ",
      "Choose a horizon within the observed follow-up.",
      call. = FALSE
    )
  }
  all_risk <- if (inp$is_surv) km_risk_at(inp$y$time, inp$y$event, inp$horizon, 0.95)[["risk"]] else mean(inp$y$y)
  # the beyond-follow-up case is already reported above
  if (inp$is_surv && is.na(all_risk) && !beyond_followup) {
    warning("The Kaplan-Meier risk at `horizon` is NA (follow-up ends before the horizon); `treat_all` is NA.", call. = FALSE)
  }
  treat_all <- all_risk - (1 - all_risk) * odds

  out <- lapply(names(inp$preds), function(nm) {
    p <- inp$preds[[nm]]
    res <- vapply(seq_along(thresholds), function(i) {
      above <- p >= thresholds[i]
      prop <- mean(above)
      if (prop == 0) return(c(0, 0))
      r <- if (inp$is_surv) {
        km_risk_at(inp$y$time[above], inp$y$event[above], inp$horizon, 0.95)[["risk"]]
      } else {
        mean(inp$y$y[above])
      }
      if (is.na(r)) return(c(NA_real_, prop))
      c(prop * r - prop * (1 - r) * odds[i], prop)
    }, numeric(2))
    tibble::tibble(model = nm, threshold = thresholds, net_benefit = res[1L, ], treat_all = treat_all, prop_above = res[2L, ])
  })
  out <- dplyr::bind_rows(out)
  attr(out, "horizon") <- inp$horizon
  class(out) <- c("decision_curve", class(out))
  out
}

#' Plot a Decision Curve
#'
#' Net benefit against the threshold probability for each model, with the "treat all" and
#' "treat none" strategies. The vertical range is cut below at a small negative net benefit, so
#' that a poor "treat all" at high thresholds does not flatten the plot.
#'
#' @param x A [decision_curve()] result, or the `truth` for it, in which case `pred` and the
#'   other arguments of [decision_curve()] are taken from `...`.
#' @param ... Passed to [decision_curve()] when `x` is not a `decision_curve` result.
#' @param ymin Lower limit of the net benefit axis. Default: `-0.05` times the largest net benefit.
#' @return A `ggplot2` object.
#' @seealso [decision_curve()]
#' @export
#' @examples
#' set.seed(1)
#' x <- rnorm(400)
#' y <- rbinom(400, 1, plogis(-1 + x))
#' plot_decision_curve(y, pred = fitted(glm(y ~ x, family = binomial())))
plot_decision_curve <- function(x, ..., ymin = NULL) {
  dc <- if (inherits(x, "decision_curve")) x else decision_curve(x, ...)
  df <- tibble::as_tibble(unclass(dc))
  top <- max(df$net_benefit, df$treat_all, 0, na.rm = TRUE)
  if (is.null(ymin)) ymin <- -0.05 * top
  ref <- df[df$model == df$model[1L], , drop = FALSE]
  ggplot2::ggplot(df, ggplot2::aes(x = .data$threshold, y = .data$net_benefit, color = .data$model)) +
    ggplot2::geom_line(data = ref, ggplot2::aes(y = .data$treat_all, color = "Treat all"), linewidth = 0.7) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0, color = "Treat none"), linewidth = 0.7) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = c(
      stats::setNames(
        rep_len(c("#9D2235", "#2B6CB0", "#2F855A", "#B7791F", "#6B46C1", "#C05621", "#2C7A7B"), length(unique(df$model))),
        unique(df$model)
      ),
      "Treat all" = "grey55", "Treat none" = "grey20"
    )) +
    ggplot2::coord_cartesian(ylim = c(ymin, top * 1.05)) +
    theme_cbe() +
    ggplot2::labs(title = "Decision curve", x = "Threshold probability", y = "Net benefit", color = NULL)
}
