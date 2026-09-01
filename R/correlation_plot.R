#' Correlation Plot
#'
#' Plots a correlation matrix across all numeric columns of \code{data}
#' using \pkg{corrplot}.
#'
#' @param data A data frame or tibble.
#' @param cor.use Passed to \code{\link[stats]{cor}}: \code{"everything"}
#'   (default), \code{"all.obs"}, \code{"complete.obs"},
#'   \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}.
#' @param cor.method Passed to \code{\link[stats]{cor}}: \code{"pearson"}
#'   (default), \code{"kendall"}, or \code{"spearman"}.
#' @param method Visualization method passed to
#'   \code{\link[corrplot]{corrplot}} (default \code{"ellipse"}).
#' @param type \code{"upper"} (default), \code{"full"}, or \code{"lower"}.
#' @param order Ordering method for the correlation matrix (default
#'   \code{"FPC"}, first principal component order).
#' @param title Plot title.
#' @param na_omit Logical (default \code{TRUE}); drop rows with any \code{NA}
#'   among the numeric columns before computing correlations.
#' @param tl.cex,number.cex,tl.srt Label/number sizing and rotation, passed
#'   to \code{\link[corrplot]{corrplot}}.
#' @param mar Plot margin, passed to \code{\link[corrplot]{corrplot}} in the
#'   standard \code{par("mar")} form \code{c(bottom, left, top, right)}
#'   (default \code{c(0, 0, 2, 0)}). \code{corrplot()} does not otherwise
#'   reserve any extra space above the matrix for \code{title}, so with the
#'   default (zero) margin the title text collides with the 45-degree
#'   diagonal column labels sitting just below it; the default here adds two
#'   lines of top margin so the title clears the labels. Increase further if
#'   using a long title or a larger \code{tl.cex}.
#' @param show_coef Logical (default \code{TRUE}); whether to draw the
#'   correlation coefficient inside each cell. On a small matrix the numbers
#'   add useful precision on top of the visual encoding, but on a large
#'   matrix (many variables) they quickly overlap each other and the
#'   diagonal labels. Set to \code{FALSE} to omit them entirely -- this is
#'   the clean way to declutter a large matrix; shrinking \code{tl.cex} and
#'   \code{number.cex} down to near-zero to visually "hide" the numbers (as
#'   earlier callers did) also erases the variable name labels and should be
#'   avoided. See \code{\link{correlation_plot_split}} for an alternative
#'   that keeps coefficients readable by splitting a large matrix into
#'   several smaller plots instead of hiding them.
#' @param ... Additional arguments passed to \code{\link[corrplot]{corrplot}}.
#' @return Invisibly, the correlation matrix (called for its plot side effect).
#' @export
#' @examples
#' correlation_plot(mtcars, tl.cex = .7)
#' correlation_plot(mtcars, tl.cex = .7, show_coef = FALSE)
correlation_plot <- function(data,
                              cor.use = "everything",
                              cor.method = "pearson",
                              method = "ellipse",
                              type = "upper",
                              order = "FPC",
                              title = "Correlation Coefficient Plot",
                              na_omit = TRUE,
                              tl.cex = .5,
                              number.cex = .75,
                              tl.srt = 45,
                              mar = c(0, 0, 2, 0),
                              show_coef = TRUE,
                              ...) {
  palette <- grDevices::colorRampPalette(c("blue", "white", "red"))

  num_data <- dplyr::select(data, dplyr::where(is.numeric))
  if (isTRUE(na_omit)) num_data <- stats::na.omit(num_data)

  cor_mat <- stats::cor(num_data, use = cor.use, method = cor.method)

  coef_args <- if (isTRUE(show_coef)) {
    list(addCoef.col = "black", number.cex = number.cex)
  } else {
    list()
  }

  do.call(corrplot::corrplot, c(
    list(
      cor_mat,
      col = palette(200),
      tl.col = "black",
      method = method,
      order = order,
      type = type,
      title = title,
      tl.cex = tl.cex,
      tl.srt = tl.srt,
      mar = mar
    ),
    coef_args,
    list(...)
  ))
  invisible(cor_mat)
}

#' Correlation Plot, Split Into Legible Sub-Plots
#'
#' Draws the same style of correlation matrix as \code{\link{correlation_plot}},
#' but for matrices with too many variables to stay legible in a single plot
#' (for example, ~40 clinical parameters), it first groups variables via
#' hierarchical clustering on their correlation structure and then draws one
#' \code{\link{correlation_plot}}-style plot per group, showing only the
#' within-group correlations.
#'
#' Variables are clustered on \code{as.dist(1 - abs(cor_mat))} -- the same
#' correlation-based distance \code{corrplot}'s own \code{order = "hclust"}
#' uses internally -- so that variables which move together end up in the
#' same sub-plot instead of being split arbitrarily (e.g. alphabetically).
#' The resulting dendrogram is cut into \code{ceiling(n_vars / group_size)}
#' contiguous groups via \code{\link[stats]{cutree}}.
#'
#' The default \code{group_size} of 12 is chosen to match
#' \code{\link{correlation_plot}}'s own defaults: at the default
#' \code{tl.cex}/\code{number.cex}, a matrix of roughly a dozen variables is
#' about as many as can fit one legible diagonal label and coefficient per
#' cell without crowding -- the same order of magnitude as the small examples
#' (like \code{mtcars}'s 11 numeric columns) \code{correlation_plot()} was
#' originally tuned against. Larger or smaller groups can be requested via
#' \code{group_size} depending on label length and plot size.
#'
#' @param data A data frame or tibble.
#' @param cor.use,cor.method,na_omit See \code{\link{correlation_plot}}.
#' @param group_size Target number of variables per sub-plot (default 12).
#'   The number of groups is \code{ceiling(n_vars / group_size)}; groups may
#'   end up somewhat smaller or larger than this target since
#'   \code{\link[stats]{cutree}} produces clusters of whatever sizes the
#'   dendrogram structure dictates, not exactly equal-sized groups.
#' @param title Base plot title; each sub-plot's title has
#'   \code{" (Group i of n)"} appended so the sub-plots can be told apart.
#' @param ... Additional arguments passed on to \code{\link{correlation_plot}}
#'   for each sub-plot (e.g. \code{tl.cex}, \code{show_coef}, \code{method}).
#' @return Invisibly, a named list of per-group correlation matrices (one
#'   matrix per sub-plot, named \code{"Group 1"}, \code{"Group 2"}, ...).
#' @export
#' @examples
#' # A data frame with more numeric columns than fit legibly in one plot.
#' wide_data <- cbind(mtcars, iris[seq_len(nrow(mtcars)), sapply(iris, is.numeric)])
#' correlation_plot_split(wide_data, group_size = 6)
correlation_plot_split <- function(data,
                                    cor.use = "everything",
                                    cor.method = "pearson",
                                    na_omit = TRUE,
                                    group_size = 12,
                                    title = "Correlation Coefficient Plot",
                                    ...) {
  num_data <- dplyr::select(data, dplyr::where(is.numeric))
  if (isTRUE(na_omit)) num_data <- stats::na.omit(num_data)

  cor_mat <- stats::cor(num_data, use = cor.use, method = cor.method)

  n_vars <- ncol(cor_mat)
  n_groups <- max(1, ceiling(n_vars / group_size))

  if (n_groups == 1) {
    groups <- stats::setNames(list(colnames(cor_mat)), "1")
  } else {
    dist_mat <- stats::as.dist(1 - abs(cor_mat))
    hc <- stats::hclust(dist_mat)
    clusters <- stats::cutree(hc, k = n_groups)
    groups <- split(names(clusters), clusters)
  }

  n_groups <- length(groups)
  result <- vector("list", n_groups)
  names(result) <- paste("Group", seq_len(n_groups))

  for (i in seq_len(n_groups)) {
    group_vars <- groups[[i]]
    group_data <- num_data[, group_vars, drop = FALSE]
    group_title <- paste0(title, " (Group ", i, " of ", n_groups, ")")

    result[[i]] <- correlation_plot(
      group_data,
      cor.use = cor.use,
      cor.method = cor.method,
      na_omit = FALSE,
      title = group_title,
      ...
    )
  }

  invisible(result)
}

#' Difference in Correlation Matrices Between Two Datasets
#'
#' Compares the correlation matrix of \code{comparison_data} against that of
#' \code{baseline_data} (e.g. a later timepoint vs. baseline, or a treatment
#' group vs. a reference group), matching numeric variables by column name.
#'
#' Unlike PCA loadings (see \code{\link{pca_loading_diff}}), correlation
#' coefficients have no sign-ambiguity to correct for -- a correlation matrix
#' is uniquely determined by the data, so the difference is simply
#' (comparison correlation) minus (baseline correlation), with no
#' sign-alignment step needed.
#'
#' Variables are matched by name (numeric column names shared by both
#' datasets). If the two datasets' numeric columns differ, only the
#' intersection is used; no error is raised. Because a correlation matrix is
#' symmetric, only one triangle is returned (no duplicate \code{var1}/\code{var2}
#' vs. \code{var2}/\code{var1} rows). The diagonal is dropped: a variable's
#' correlation with itself is always 1 in both datasets, so its difference is
#' always 0 and carries no information.
#'
#' @param baseline_data A data frame or tibble treated as the reference.
#' @param comparison_data A data frame or tibble to compare against
#'   \code{baseline_data}.
#' @param cor.use,cor.method,na_omit See \code{\link{correlation_plot}}.
#' @return A tibble with one row per shared variable pair, with columns
#'   \code{var1}, \code{var2}, and \code{diff} (comparison correlation minus
#'   baseline correlation).
#' @export
#' @examples
#' set.seed(1)
#' baseline <- mtcars
#' comparison <- mtcars[sample(nrow(mtcars), replace = TRUE), ]
#' correlation_diff(baseline, comparison)
correlation_diff <- function(baseline_data,
                              comparison_data,
                              cor.use = "everything",
                              cor.method = "pearson",
                              na_omit = TRUE) {
  num_baseline <- dplyr::select(baseline_data, dplyr::where(is.numeric))
  num_comparison <- dplyr::select(comparison_data, dplyr::where(is.numeric))

  shared_vars <- intersect(names(num_baseline), names(num_comparison))
  num_baseline <- num_baseline[, shared_vars, drop = FALSE]
  num_comparison <- num_comparison[, shared_vars, drop = FALSE]

  if (isTRUE(na_omit)) {
    num_baseline <- stats::na.omit(num_baseline)
    num_comparison <- stats::na.omit(num_comparison)
  }

  cor_baseline <- stats::cor(num_baseline, use = cor.use, method = cor.method)
  cor_comparison <- stats::cor(num_comparison, use = cor.use, method = cor.method)

  diff_mat <- cor_comparison - cor_baseline

  upper_idx <- upper.tri(diff_mat, diag = FALSE)

  tibble::tibble(
    var1 = rownames(diff_mat)[row(diff_mat)[upper_idx]],
    var2 = colnames(diff_mat)[col(diff_mat)[upper_idx]],
    diff = diff_mat[upper_idx]
  )
}

#' Heatmap of Correlation Differences Between Two Datasets
#'
#' Renders the output of \code{\link{correlation_diff}} as a heatmap
#' (variable by variable), using the same visual language as
#' \code{\link{pca_loading_diff_heatmap}}: a diverging fill scale centered at
#' zero, so variable pairs with little change are white and larger
#' correlation changes in either direction stand out in blue or red.
#'
#' @inheritParams correlation_diff
#' @return A ggplot object.
#' @export
#' @examples
#' set.seed(1)
#' baseline <- mtcars
#' comparison <- mtcars[sample(nrow(mtcars), replace = TRUE), ]
#' correlation_diff_heatmap(baseline, comparison)
correlation_diff_heatmap <- function(baseline_data,
                                      comparison_data,
                                      cor.use = "everything",
                                      cor.method = "pearson",
                                      na_omit = TRUE) {
  diff_df <- correlation_diff(baseline_data, comparison_data,
                               cor.use = cor.use, cor.method = cor.method,
                               na_omit = na_omit)

  ggplot2::ggplot(diff_df, ggplot2::aes(x = .data$var1, y = .data$var2, fill = .data$diff)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    ggplot2::labs(x = NULL, y = NULL, fill = "Correlation\ndifference",
                  title = "Correlation Differences Between Datasets") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
