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
#' @param ... Additional arguments passed to [cv_coxnet()].
#' @return A list containing `best_mixture`, `best_penalty`, `model` (fitted on full data), and `cv_results`.
#' @export
tune_best_glmnet_workflow <- function(formula_or_recipe,
                                      data,
                                      resamples = NULL,
                                      mixture = c(0.2, 0.5, 0.8, 1.0),
                                      rule = c("min", "1se"),
                                      ...) {
  rule <- match.arg(rule)

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
#' @param explainer A DALEX explainer object, or a fitted model / function supported by DALEX.
#' @param data Optional test dataset for computing importance.
#' @param y Optional outcome vector if creating an explainer on the fly.
#' @param B Number of permutation rounds (default 10).
#' @param ... Additional arguments passed to [DALEX::model_parts()].
#' @return A tibble with columns `variable`, `dropout_loss`, `importance`, and `rank`.
#' @seealso [plot_permutation_importance()], [tbl_permutation_importance()]
#' @export
summarize_dalex_importance <- function(explainer, data = NULL, y = NULL, B = 10, ...) {
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

#' Plot Permutation Variable Importance
#'
#' Generates a horizontal bar chart of the top features ranked by permutation importance.
#'
#' @param data A data frame from [summarize_dalex_importance()] containing `variable` and `importance`.
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
                                        style = NULL) {
  if (!all(c("variable", "importance") %in% names(data))) {
    stop("`data` must contain 'variable' and 'importance' columns.", call. = FALSE)
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

