#' Compare Bootstrap Hazard Ratios Between Complete and Imputed Data
#'
#' Density overlays, one panel per feature, of the hazard-ratio estimates from Cox models fitted
#' to bootstrap resamples of the complete cases and to imputed datasets. If imputation distorts
#' an effect, the two densities separate.
#'
#' @param boot_coefs_original Data frame of coefficients from the complete-case bootstrap.
#' @param imputed_coefs Data frame of coefficients across the imputed datasets.
#' @param feature_col Name of the feature column in both.
#' @param estimate_col Name of the coefficient column in both (log hazard ratios by default).
#' @param exponentiate Plot `exp(estimate)` (hazard ratios, on a log axis); `FALSE` plots the
#'   estimates as they are.
#' @param filter_fences Drop extreme values with Tukey's outer fences (`k` times the IQR beyond
#'   the quartiles, computed per feature and source) before plotting. Fitting failures that give
#'   huge coefficients otherwise stretch the axis.
#' @param k Fence multiplier: 1.5 for the inner and 3 (default) for the outer fences.
#' @return A [ggplot2::ggplot()]. The data after filtering are in `$data`, and the number of
#'   removed values per feature and source is in the attribute `n_removed` of the plot.
#' @seealso [km_cutpoints()], [batch_cox_single()]
#' @export
#' @examples
#' set.seed(1)
#' orig <- data.frame(term = rep(c("age", "bmi"), each = 200), estimate = rnorm(400, 0.2, 0.1))
#' imp <- data.frame(term = rep(c("age", "bmi"), each = 200), estimate = rnorm(400, 0.25, 0.1))
#' plot_imputation_hazard_stability(orig, imp)
plot_imputation_hazard_stability <- function(boot_coefs_original, imputed_coefs,
                                             feature_col = "term", estimate_col = "estimate",
                                             exponentiate = TRUE, filter_fences = TRUE, k = 3) {
  for (nm in c("boot_coefs_original", "imputed_coefs")) {
    x <- get(nm)
    if (!is.data.frame(x) || !all(c(feature_col, estimate_col) %in% names(x))) {
      stop("`", nm, "` must be a data frame with columns `", feature_col, "` and `", estimate_col, "`.", call. = FALSE)
    }
  }
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k <= 0) stop("`k` must be a positive number.", call. = FALSE)
  both <- rbind(
    data.frame(feature = as.character(boot_coefs_original[[feature_col]]),
               estimate = as.numeric(boot_coefs_original[[estimate_col]]),
               source = "Complete cases (bootstrap)"),
    data.frame(feature = as.character(imputed_coefs[[feature_col]]),
               estimate = as.numeric(imputed_coefs[[estimate_col]]),
               source = "Imputed")
  )
  both <- both[is.finite(both$estimate), , drop = FALSE]
  if (nrow(both) == 0L) stop("No finite estimates to plot.", call. = FALSE)

  grp <- interaction(both$feature, both$source, drop = TRUE)
  keep <- rep(TRUE, nrow(both))
  if (filter_fences) {
    for (g in levels(grp)) {
      i <- which(grp == g)
      q <- stats::quantile(both$estimate[i], c(0.25, 0.75), names = FALSE)
      iqr <- q[2L] - q[1L]
      keep[i] <- both$estimate[i] >= q[1L] - k * iqr & both$estimate[i] <= q[2L] + k * iqr
    }
  }
  removed <- both[!keep, , drop = FALSE]
  d <- both[keep, , drop = FALSE]
  d$value <- if (exponentiate) exp(d$estimate) else d$estimate

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$value, fill = .data$source, colour = .data$source)) +
    ggplot2::geom_density(alpha = 0.35, adjust = 1) +
    ggplot2::geom_vline(xintercept = if (exponentiate) 1 else 0, linetype = "dashed", colour = "grey40") +
    ggplot2::facet_wrap(ggplot2::vars(.data$feature), scales = "free") +
    ggplot2::labs(x = if (exponentiate) "Hazard ratio" else "Coefficient", y = "Density", fill = NULL, colour = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  if (exponentiate) p <- p + ggplot2::scale_x_log10()
  attr(p, "n_removed") <- if (nrow(removed)) {
    stats::aggregate(estimate ~ feature + source, data = removed, FUN = length)
  } else {
    data.frame(feature = character(), source = character(), estimate = integer())
  }
  p
}
