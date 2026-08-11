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
#' @param ... Additional arguments passed to \code{\link[corrplot]{corrplot}}.
#' @return Invisibly, the correlation matrix (called for its plot side effect).
#' @export
#' @examples
#' correlation_plot(mtcars, tl.cex = .7)
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
                              ...) {
  palette <- grDevices::colorRampPalette(c("blue", "white", "red"))

  num_data <- dplyr::select(data, dplyr::where(is.numeric))
  if (isTRUE(na_omit)) num_data <- stats::na.omit(num_data)

  cor_mat <- stats::cor(num_data, use = cor.use, method = cor.method)

  corrplot::corrplot(
    cor_mat,
    col = palette(200),
    tl.col = "black",
    method = method,
    order = order,
    type = type,
    title = title,
    addCoef.col = "black",
    number.cex = number.cex,
    tl.cex = tl.cex,
    tl.srt = tl.srt,
    ...
  )
  invisible(cor_mat)
}
