#' Observed Versus Imputed Values, per Cross-Validation Fold
#'
#' For a validation of an imputation method on cells whose truth is known: for each fold, the
#' observed (true) values and the imputed values side by side as boxplots and/or overlaid densities.
#' `type = "both"` needs the `patchwork` package.
#'
#' @param predictions_df A data frame with one row per held-out cell.
#' @param observed_col,imputed_col,fold_col Column names.
#' @param type `"both"` (default), `"boxplot"` or `"density"`.
#' @param title Optional title.
#' @return A ggplot (`"boxplot"`, `"density"`) or a patchwork of the two (`"both"`). Rows with a
#'   missing observed or imputed value are dropped; the long data are in the plot's `data`.
#' @seealso [compare_imputation_rse()], [test_imputed_distributions()]
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(Fold = rep(c("F1", "F2"), each = 50), observed = rnorm(100))
#' d$imputed <- d$observed + rnorm(100, 0, 0.5)
#' plot_imputed_vs_observed(d, type = "density")
plot_imputed_vs_observed <- function(predictions_df,
                                     observed_col = "observed",
                                     imputed_col = "imputed",
                                     fold_col = "Fold",
                                     type = c("both", "boxplot", "density"),
                                     title = NULL) {
  type <- match.arg(type)
  if (!is.data.frame(predictions_df)) stop("`predictions_df` must be a data frame.", call. = FALSE)
  need <- c(observed_col, imputed_col, fold_col)
  gone <- setdiff(need, names(predictions_df))
  if (length(gone)) stop("Column(s) not in `predictions_df`: ", paste(gone, collapse = ", "), ".", call. = FALSE)
  if (!is.numeric(predictions_df[[observed_col]]) || !is.numeric(predictions_df[[imputed_col]])) {
    stop("`observed_col` and `imputed_col` must be numeric.", call. = FALSE)
  }
  if (type == "both" && !requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package \"patchwork\" is required for type = \"both\".", call. = FALSE)
  }
  d <- predictions_df[stats::complete.cases(predictions_df[need]), need]
  if (nrow(d) == 0L) stop("No row has both an observed and an imputed value.", call. = FALSE)
  long <- rbind(
    data.frame(fold = as.character(d[[fold_col]]), source = "Observed", value = d[[observed_col]]),
    data.frame(fold = as.character(d[[fold_col]]), source = "Imputed", value = d[[imputed_col]])
  )
  long$fold <- factor(long$fold, levels = unique(as.character(d[[fold_col]])))
  long$source <- factor(long$source, levels = c("Observed", "Imputed"))
  cols <- c(Observed = "#2C3E50", Imputed = "#A41E35")

  box <- ggplot2::ggplot(long, ggplot2::aes(.data$fold, .data$value, fill = .data$source)) +
    ggplot2::geom_boxplot(outlier.size = 0.8, alpha = 0.7) +
    ggplot2::scale_fill_manual(values = cols, name = NULL) +
    ggplot2::labs(x = "Fold", y = "Value") +
    ggplot2::theme_minimal()
  dens <- ggplot2::ggplot(long, ggplot2::aes(.data$value, fill = .data$source, colour = .data$source)) +
    ggplot2::geom_density(alpha = 0.35) +
    ggplot2::facet_wrap(ggplot2::vars(.data$fold), scales = "free_y") +
    ggplot2::scale_fill_manual(values = cols, name = NULL) +
    ggplot2::scale_colour_manual(values = cols, name = NULL) +
    ggplot2::labs(x = "Value", y = "Density") +
    ggplot2::theme_minimal()

  p <- switch(type,
    boxplot = box,
    density = dens,
    both = patchwork::wrap_plots(box, dens, ncol = 1)
  )
  if (!is.null(title)) {
    p <- if (type == "both") p + patchwork::plot_annotation(title = title) else p + ggplot2::ggtitle(title)
  }
  p
}
