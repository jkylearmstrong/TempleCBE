#' PCA Feature-Loading Heatmap
#'
#' Heatmap of each original feature's loading onto each principal component.
#'
#' @param pca_model A \code{\link[stats]{prcomp}} object.
#' @return A ggplot object.
#' @export
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' pca_feature_loading_heatmap(pca_model)
pca_feature_loading_heatmap <- function(pca_model) {
  rot <- pca_model$rotation
  long <- tibble::as_tibble(rot, rownames = "feature") |>
    tidyr::pivot_longer(-"feature", names_to = "PC", values_to = "value") |>
    dplyr::mutate(PC = as.numeric(sub("^PC", "", .data$PC)))

  num_comp <- ncol(rot)

  ggplot2::ggplot(long, ggplot2::aes(x = .data$PC, y = .data$feature, fill = .data$value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_x_continuous(breaks = seq_len(num_comp)) +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red")
}

#' PCA Biplot
#'
#' Biplot of feature loading vectors against two principal components.
#'
#' @param pca_model A \code{\link[stats]{prcomp}} object.
#' @param newdata Data to project onto \code{pca_model}.
#' @param column Column in \code{newdata} to use as point labels.
#' @param x,y Which principal components to plot on the x/y axes (default 1, 2).
#' @return A ggplot object.
#' @export
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' mtcars2 <- tibble::rownames_to_column(mtcars, "model")
#' plot_pca_bi(pca_model, mtcars2, column = "model")
plot_pca_bi <- function(pca_model, newdata, column, x = 1, y = 2) {
  if (!column %in% colnames(newdata)) {
    message("Column '", column, "' not found in newdata; using the first column instead.")
    column <- colnames(newdata)[1]
  }

  projection <- stats::predict(pca_model, newdata)
  n_components <- ncol(projection)

  if (x > n_components) { message("x exceeds available components; using 1."); x <- 1 }
  if (y > n_components) { message("y exceeds available components; using 1."); y <- 1 }
  if (x == y) {
    y <- if (x == n_components) 1 else x + 1
    message("x and y were equal; using y = ", y, ".")
  }

  plot_df <- dplyr::bind_cols(
    tibble::as_tibble(projection),
    dplyr::select(newdata, dplyr::all_of(column))
  )

  x_col <- paste0("PC", x)
  y_col <- paste0("PC", y)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
    ggplot2::geom_segment(xend = 0, yend = 0,
                           arrow = grid::arrow(angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt"))) +
    ggplot2::geom_text(ggplot2::aes(label = .data[[column]]), hjust = 1, nudge_x = -0.02, color = "#904C2F") +
    ggplot2::coord_fixed()
}

#' PCA Rotation Matrix (Loadings)
#'
#' @param PC_mod A \code{\link[stats]{prcomp}} object.
#' @return A tibble of feature loadings onto each principal component, with
#'   a \code{feature_num} column (\code{"f1"}, \code{"f2"}, ...) for compact
#'   labeling in \code{\link{pca_eqns}}.
#' @export
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' rotation_matrix(pca_model)
rotation_matrix <- function(PC_mod) {
  tibble::as_tibble(PC_mod$rotation, rownames = "feature") |>
    dplyr::mutate(feature_num = paste0("f", dplyr::row_number()))
}

#' @rdname rotation_matrix
#' @export
pca_loadings <- rotation_matrix

#' PCA Equations
#'
#' Writes out each principal component as a linear equation in the original
#' features (abbreviated \code{f1}, \code{f2}, ... — see the returned
#' \code{labels} table for what each abbreviation means).
#'
#' @param PC_mod A \code{\link[stats]{prcomp}} object.
#' @param precision Digits to round loadings to (default 3).
#' @return A list with two tibbles: \code{eqns} (one row per component, with
#'   its equation as text) and \code{labels} (feature-number-to-name key).
#' @export
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' pca_eqns(pca_model)$eqns
#' pca_eqns(pca_model)$labels
pca_eqns <- function(PC_mod, precision = 3) {
  pc_pc <- pca_loadings(PC_mod)
  pc_cols <- names(dplyr::select(pc_pc, dplyr::starts_with("PC")))

  eqns <- pc_pc |>
    dplyr::mutate(dplyr::across(dplyr::all_of(pc_cols), \(x) round(x, precision))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(pc_cols), \(x) ifelse(x >= 0, paste0("+", x), paste0(x)))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(pc_cols), \(x) paste0(x, "*(", .data$feature_num, ")"))) |>
    dplyr::select(-"feature", -"feature_num") |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "PC", values_to = "fct") |>
    dplyr::mutate(PC_num = as.numeric(sub("^PC", "", .data$PC))) |>
    dplyr::group_by(.data$PC_num, .data$PC) |>
    dplyr::summarise(rhs = paste(.data$fct, collapse = " "), .groups = "drop") |>
    dplyr::arrange(.data$PC_num) |>
    dplyr::mutate(PC = paste0(.data$PC, "= ")) |>
    dplyr::select("PC", "rhs")

  labels <- pc_pc |>
    dplyr::mutate(fi = paste0(.data$feature_num, " = ")) |>
    dplyr::select("fi", "feature")

  list(eqns = eqns, labels = labels)
}

#' Percent Variance Explained by Each Principal Component
#'
#' @param pca_model A \code{\link[stats]{prcomp}} object.
#' @return A ggplot object showing per-component and cumulative variance explained.
#' @export
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' pca_percent_var_explained(pca_model)
pca_percent_var_explained <- function(pca_model) {
  eig <- broom::tidy(pca_model, matrix = "eigenvalues")
  n_comp <- max(eig$PC)

  eig |>
    dplyr::select("PC", "percent", "cumulative") |>
    tidyr::pivot_longer(cols = c("percent", "cumulative"), names_to = "variance", values_to = "percent") |>
    dplyr::mutate(variance = factor(.data$variance, levels = c("cumulative", "percent"))) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$PC, y = .data$percent, fill = .data$variance, alpha = .data$variance)) +
    ggplot2::geom_bar(stat = "identity", position = "identity") +
    ggplot2::scale_fill_manual(values = c(cumulative = "#56B4E9", percent = "black")) +
    ggplot2::scale_x_continuous(breaks = seq_len(n_comp)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format()) +
    ggplot2::scale_alpha_manual(values = c(.75, 1)) +
    ggplot2::labs(x = "Principal Component", y = "Percent of Variance Explained",
                  title = "Variance Explained by Principal Component")
}

#' Generic Plot Method for \code{prcomp} Objects
#'
#' @param x A \code{\link[stats]{prcomp}} object.
#' @param type One of \code{"variance"} (\code{\link{pca_percent_var_explained}}),
#'   \code{"heatmap"} (\code{\link{pca_feature_loading_heatmap}}), or
#'   \code{"bi"} (\code{\link{plot_pca_bi}}).
#' @param ... Passed on to the underlying plot function (needed for
#'   \code{type = "bi"}, which requires \code{newdata} and \code{column}).
#' @return A ggplot object.
#' @exportS3Method base::plot
#' @examples
#' pca_model <- prcomp(mtcars, center = TRUE, scale. = TRUE)
#' plot(pca_model, type = "variance")
plot.prcomp <- function(x, type = c("variance", "heatmap", "bi"), ...) {
  type <- match.arg(type)
  switch(type,
    variance = pca_percent_var_explained(x),
    heatmap = pca_feature_loading_heatmap(x),
    bi = plot_pca_bi(x, ...)
  )
}
