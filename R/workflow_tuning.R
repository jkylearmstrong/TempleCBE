#' Tidymodels Workflow Tuning and Feature Importance Summaries
#'
#' Orchestrates automated hyperparameter tuning across elastic net workflows,
#' model explanation extraction, and permutation variable importance summarization.
#'
#' @name workflow_tuning
NULL

#' Select and Finalize Best Penalized Cox Workflow
#'
#' Runs cross-validation tuning across a grid of elastic net mixture and penalty
#' parameters and finalizes the workflow with the best-performing hyperparameters.
#'
#' @param formula_or_recipe A formula with `Surv()` outcome or a `recipes::recipe`.
#' @param data Data frame containing predictors and survival outcomes.
#' @param resamples An `rsample` resample object (e.g. `vfold_cv` or `group_vfold_cv`).
#' @param mixture Numeric vector of candidate alpha mixture values (default `c(0.2, 0.5, 0.8, 1.0)`).
#' @param rule Hyperparameter selection rule: `"min"` (default) or `"1se"`.
#' @param checkpoint_dir Optional directory path where per-task checkpoints are stored.
#' @param shard Optional shard specification (e.g. `c(1, 2)`) for distributed computation.
#' @param checkpoint_mismatch Action on mismatch: `"recompute"`, `"overwrite"`, or `"error"`.
#' @param ... Additional arguments passed to [cv_coxnet()].
#' @return A list containing `best_mixture`, `best_penalty`, `model` (fitted on full data), and `cv_results`.
#' @export
tune_best_glmnet_workflow <- function(formula_or_recipe,
                                      data,
                                      resamples = NULL,
                                      mixture = c(0.2, 0.5, 0.8, 1.0),
                                      rule = c("min", "1se"),
                                      checkpoint_dir = NULL,
                                      shard = NULL,
                                      checkpoint_mismatch = c("recompute", "overwrite", "error"),
                                      ...) {
  rule <- match.arg(rule)
  checkpoint_mismatch <- resolve_checkpoint_mismatch(checkpoint_mismatch)

  if (is.null(resamples)) {
    if (!requireNamespace("rsample", quietly = TRUE)) {
      stop("Package \"rsample\" is required when `resamples` is not provided.", call. = FALSE)
    }
    resamples <- rsample::vfold_cv(data, v = 5)
  }

  fit <- cv_coxnet(
    formula_or_recipe,
    data = data,
    mixture = mixture,
    resamples = resamples,
    checkpoint_dir = checkpoint_dir,
    shard = shard,
    checkpoint_mismatch = checkpoint_mismatch,
    ...
  )

  best_mix <- fit$mixture %||% fit$best_mixture %||% mixture[1]
  best_pen <- if (rule == "1se") (fit$lambda_1se %||% fit$lambda.1se) else (fit$lambda_min %||% fit$lambda.min)

  final_fit <- coxnet(
    formula_or_recipe,
    data = data,
    mixture = best_mix,
    penalty = best_pen,
    ...
  )

  list(
    best_mixture = best_mix,
    best_penalty = best_pen,
    model = final_fit,
    cv_results = fit
  )
}

#' Extract and Summarize Permutation Variable Importance
#'
#' Computes permutation feature importance from a fitted model or DALEX explainer,
#' returning a tidy table of variable importance rankings and dropout losses.
#'
#' @param explainer A DALEX explainer object, a ready-made permutation importance result
#'   ([DALEX::model_parts()] / `feature_importance()`, or `survex::model_parts()` for a survival
#'   model, or a data frame with `variable`, `permutation` and `dropout_loss`), or a fitted model with
#'   coefficients. For a result object the summary is taken over its permutations; `data`, `y`, `B`
#'   and `...` are not used.
#' @param data Optional test dataset for computing importance.
#' @param y Optional outcome vector if creating an explainer on the fly.
#' @param B Number of permutation rounds (default 10).
#' @param ... Additional arguments passed to [DALEX::model_parts()].
#' @return A tibble with columns `variable`, `dropout_loss`, `importance`, and `rank`. For a
#'   permutation importance result it also has the distribution of the dropout loss over the
#'   permutations (`min`, `q1`, `median`, `mean`, `q3`, `max`), and the per-permutation values are
#'   kept in `attr(, "permutations")` for [plot_permutation_importance()].
#' @section Survival models:
#' `survex::model_parts()` returns the loss at every evaluation time, in one column per variable.
#' The summary averages each permutation over the evaluation times, so one number per variable and
#' permutation remains.
#' @seealso [plot_permutation_importance()], [tbl_permutation_importance()],
#'   [tbl_feature_importance_dalex()], [adapter_tidymodels_survex()]
#' @export
summarize_dalex_importance <- function(explainer, data = NULL, y = NULL, B = 10, ...) {
  perm <- .permutation_long(explainer)
  if (!is.null(perm)) return(.summarize_permutations(perm))
  if (inherits(explainer, "explainer")) {
    if (!requireNamespace("DALEX", quietly = TRUE)) {
      stop("Package \"DALEX\" is required for summarize_dalex_importance().", call. = FALSE)
    }
    mp <- DALEX::model_parts(explainer, B = B, ...)
    df <- as.data.frame(mp)
    # Extract baseline loss
    full_loss <- df$dropout_loss[df$variable == "_full_model_"][1]
    if (is.na(full_loss)) full_loss <- 0

    var_df <- df[!df$variable %in% c("_full_model_", "_baseline_"), , drop = FALSE]
    res <- dplyr::group_by(var_df, .data$variable)
    res <- dplyr::summarize(res, dropout_loss = mean(.data$dropout_loss), .groups = "drop")
    res$importance <- pmax(0, res$dropout_loss - full_loss)
    res <- dplyr::arrange(res, dplyr::desc(.data$importance))
    res$rank <- seq_len(nrow(res))
    return(tibble::as_tibble(res))
  }

  # Fallback for coefficient-based or custom models
  if (!is.null(explainer$coefficients)) {
    cf <- explainer$coefficients
    cf <- cf[names(cf) != "(Intercept)"]
    res <- tibble::tibble(
      variable = names(cf),
      dropout_loss = abs(unname(cf)),
      importance = abs(unname(cf)),
      rank = rank(-abs(unname(cf)), ties.method = "first")
    )
    return(dplyr::arrange(res, .data$rank))
  }

  stop("`explainer` must be a DALEX explainer or a model object with extractable coefficients.", call. = FALSE)
}

# Per-permutation dropout loss (variable, permutation, dropout_loss) from a DALEX / survex
# model_parts result or an equivalent data frame; NULL when `x` is not one.
.permutation_long <- function(x) {
  if (!is.data.frame(x) && is.list(x) && is.data.frame(x$result) && "_permutation_" %in% names(x$result)) {
    res <- as.data.frame(x$result)
    meta <- c("_times_", "_permutation_", "label", "_full_model_", "_baseline_")
    vars <- setdiff(names(res), meta)
    if (length(vars) == 0L) return(NULL)
    perm <- res[["_permutation_"]]
    # permutation 0 is the average over the permutations; keep the individual ones when there are any
    keep <- if (any(perm != 0)) perm != 0 else rep(TRUE, length(perm))
    long <- do.call(rbind, lapply(vars, function(v) {
      val <- tapply(res[[v]][keep], perm[keep], mean, na.rm = TRUE)
      data.frame(variable = v, permutation = as.numeric(names(val)), dropout_loss = as.numeric(val),
                 stringsAsFactors = FALSE)
    }))
    attr(long, "full_loss") <- if ("_full_model_" %in% names(res)) mean(res[["_full_model_"]][keep], na.rm = TRUE) else NA_real_
    return(long)
  }
  if (is.data.frame(x) && all(c("variable", "permutation", "dropout_loss") %in% names(x))) {
    df <- as.data.frame(x)
    df$variable <- as.character(df$variable)
    full <- df$dropout_loss[df$variable == "_full_model_"]
    if (any(df$permutation != 0)) df <- df[df$permutation != 0, , drop = FALSE]
    long <- df[!df$variable %in% c("_full_model_", "_baseline_"), c("variable", "permutation", "dropout_loss"), drop = FALSE]
    attr(long, "full_loss") <- if (length(full)) mean(full, na.rm = TRUE) else NA_real_
    return(long)
  }
  NULL
}

.summarize_permutations <- function(long) {
  full_loss <- attr(long, "full_loss")
  if (nrow(long) == 0L) stop("No variables in the permutation importance result.", call. = FALSE)
  rows <- lapply(split(long$dropout_loss, long$variable), function(l) {
    q <- stats::quantile(l, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
    data.frame(dropout_loss = mean(l, na.rm = TRUE), min = min(l, na.rm = TRUE), q1 = q[1],
               median = stats::median(l, na.rm = TRUE), mean = mean(l, na.rm = TRUE), q3 = q[2],
               max = max(l, na.rm = TRUE))
  })
  res <- cbind(data.frame(variable = names(rows), stringsAsFactors = FALSE), do.call(rbind, rows))
  res$importance <- if (is.na(full_loss)) res$dropout_loss else pmax(0, res$dropout_loss - full_loss)
  res <- res[order(-res$importance, res$variable), , drop = FALSE]
  res$rank <- seq_len(nrow(res))
  res <- tibble::as_tibble(res[c("variable", "dropout_loss", "importance", "rank", "min", "q1", "median", "mean", "q3", "max")])
  attr(res, "permutations") <- tibble::as_tibble(as.data.frame(long))
  attr(res, "full_loss") <- full_loss
  res
}

#' Plot Permutation Variable Importance
#'
#' Generates a horizontal bar chart of the top features ranked by permutation importance, or a
#' box plot of the dropout loss over the permutations.
#'
#' @param data A data frame from [summarize_dalex_importance()] containing `variable` and `importance`,
#'   or a permutation importance result itself ([DALEX::model_parts()], `survex::model_parts()`).
#' @param type `"bar"` (default; mean importance, the loss above the full model) or `"boxplot"`
#'   (the dropout loss of each permutation, with a dashed line at the full-model loss). The box plot
#'   needs the per-permutation values: a `model_parts` result, or a [summarize_dalex_importance()]
#'   summary of one.
#' @param top_n Optional integer limit on the number of features to plot (default 20).
#' @param fill_color Bar fill color (default Temple Cherry `"#9D2235"`).
#' @param title Optional plot title.
#' @param base_size Base font size (default 12).
#' @param label_map Optional label mapping (see [cbe_labels()]).
#' @param style Optional label style (`"short"` or `"full"`).
#' @return A `ggplot2` object.
#' @seealso [summarize_dalex_importance()], [tbl_permutation_importance()]
#' @export
plot_permutation_importance <- function(data,
                                        top_n = 20,
                                        fill_color = "#9D2235",
                                        title = "Feature Permutation Importance",
                                        base_size = 12,
                                        label_map = NULL,
                                        style = NULL,
                                        type = c("bar", "boxplot")) {
  type <- match.arg(type)
  perm <- .permutation_long(data)
  if (!is.null(perm)) data <- .summarize_permutations(perm)
  if (!all(c("variable", "importance") %in% names(data))) {
    stop("`data` must contain 'variable' and 'importance' columns.", call. = FALSE)
  }
  if (type == "boxplot") {
    long <- attr(data, "permutations")
    if (is.null(long)) {
      stop("`type = \"boxplot\"` needs the per-permutation losses: pass a model_parts() result or its ",
           "summarize_dalex_importance() summary.", call. = FALSE)
    }
    ord <- dplyr::arrange(data, dplyr::desc(.data$importance))
    if (!is.null(top_n) && top_n < nrow(ord)) ord <- utils::head(ord, top_n)
    long <- as.data.frame(long)
    long <- long[long$variable %in% ord$variable, , drop = FALSE]
    lab <- make.unique(resolve_cbe_labels(ord$variable, label_map = label_map, style = style), sep = " ")
    long$variable <- factor(unname(stats::setNames(lab, ord$variable)[long$variable]), levels = rev(lab))
    full_loss <- attr(data, "full_loss")
    p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$dropout_loss, y = .data$variable))
    if (!is.null(full_loss) && !is.na(full_loss)) {
      p <- p + ggplot2::geom_vline(xintercept = full_loss, linetype = "dashed", color = "grey40")
    }
    return(p +
      ggplot2::geom_boxplot(fill = fill_color, alpha = 0.6, color = "#2B2B2B", outlier.size = 0.8) +
      theme_cbe(base_size = base_size) +
      ggplot2::labs(title = title, x = "Dropout loss over permutations (dashed: full model)", y = "Feature"))
  }

  df <- dplyr::arrange(data, dplyr::desc(.data$importance))
  if (!is.null(top_n) && top_n < nrow(df)) {
    df <- utils::head(df, top_n)
  }

  mapped_vars <- make.unique(resolve_cbe_labels(df$variable, label_map = label_map, style = style), sep = " ")
  df$variable <- factor(mapped_vars, levels = rev(mapped_vars))

  ggplot2::ggplot(df, ggplot2::aes(x = .data$importance, y = .data$variable)) +
    ggplot2::geom_col(fill = fill_color, width = 0.7) +
    theme_cbe(base_size = base_size) +
    ggplot2::labs(
      title = title,
      x = "Permutation Importance (Loss Delta)",
      y = "Feature"
    )
}

#' Summary Table of Permutation Variable Importance
#'
#' Returns a formatted table of feature importance rankings and dropout losses.
#'
#' @param data A data frame from [summarize_dalex_importance()].
#' @param top_n Optional integer limit on the number of features to return.
#' @param label_map Optional label mapping (see [cbe_labels()]).
#' @param style Optional label style (`"short"` or `"full"`).
#' @return A tibble with `rank`, `variable`, `importance`, and `dropout_loss`.
#' @export
tbl_permutation_importance <- function(data, top_n = NULL, label_map = NULL, style = NULL) {
  if (!all(c("variable", "importance") %in% names(data))) {
    stop("`data` must contain 'variable' and 'importance' columns.", call. = FALSE)
  }
  df <- dplyr::arrange(data, dplyr::desc(.data$importance))
  if (!"rank" %in% names(df)) {
    df$rank <- seq_len(nrow(df))
  }
  if (!is.null(top_n) && top_n < nrow(df)) {
    df <- utils::head(df, top_n)
  }
  df$variable <- resolve_cbe_labels(df$variable, label_map = label_map, style = style)
  tibble::as_tibble(df[, c("rank", "variable", "importance", intersect(c("dropout_loss"), names(df)))])
}

