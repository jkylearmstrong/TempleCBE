#' Process and Plot Principal Component Analysis (PCA)
#'
#' Helper functions for extracting component variances, feature loadings, and generating biplots.
#'
#' @param pca_obj A `prcomp` object.
#' @param data Data frame used for PCA.
#' @return Summary statistics or ggplot biplot object.
#' @export
proc_pca <- function(pca_obj, data = NULL) {
  if (!inherits(pca_obj, "prcomp")) {
    stop("Input 'pca_obj' must be a 'prcomp' object.")
  }
  
  vars <- pca_obj$sdev^2
  pct_var <- vars / sum(vars) * 100
  cum_pct <- cumsum(pct_var)
  
  tibble::tibble(
    component = paste0("PC", seq_along(vars)),
    eigenvalue = vars,
    variance_pct = pct_var,
    cum_variance_pct = cum_pct
  )
}
