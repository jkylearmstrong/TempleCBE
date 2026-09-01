#' Missingness Map
#'
#' Visualizes missingness across a data frame as a heatmap, either per
#' row/column or aggregated by a grouping column.
#'
#' @param df A data frame or tibble.
#' @param by_column Optional unquoted column name to aggregate missingness by
#'   (e.g. a site or subject id) instead of plotting every row individually.
#'   Missingness is summed per feature within each group. When \code{row_order
#'   = FALSE}, both the groups (x-axis) and the features (y-axis) are ordered
#'   by descending total missingness, exactly as in the per-row/column view.
#' @param na_list Optional vector of additional values to treat as missing
#'   (beyond actual \code{NA}).
#' @param row_order Logical; if \code{FALSE} (default) rows/features (or, in
#'   \code{by_column} mode, groups/features) are ordered by how much
#'   missingness they have.
#' @param fill In \code{by_column} mode, how to color the tiles: \code{"auto"}
#'   (default) picks a discrete two-level "Missing"/"Present" fill (matching
#'   the per-row/column view) when every group has at most one contributing
#'   row -- in which case the summed missingness count is always 0 or 1 and a
#'   count scale would be misleading -- and otherwise falls back to a
#'   continuous "# missing" gradient. \code{"binary"} and \code{"count"} force
#'   one of those two behaviors regardless of group size. Ignored outside
#'   \code{by_column} mode.
#' @return A ggplot object.
#' @importFrom rlang enquo quo_is_null
#' @export
#' @examples
#' df <- data.frame(
#'   a = c(1, NA, 3, 4, NA),
#'   b = c(NA, NA, 3, 4, 5),
#'   site = c("A", "A", "B", "B", "B")
#' )
#' missmap(df)
#' missmap(df, by_column = site)
missmap <- function(df, by_column = NULL, na_list = NULL, row_order = FALSE,
                     fill = c("auto", "binary", "count")) {
  fill <- match.arg(fill)

  df <- tryCatch(
    tibble::as_tibble(df),
    error = function(e) stop("object cannot be coerced into a tibble")
  )

  is_missing_cell <- function(col) {
    if (is.null(na_list)) is.na(col) else (is.na(col) | col %in% na_list)
  }

  by_col_quo <- rlang::enquo(by_column)

  if (!rlang::quo_is_null(by_col_quo)) {
    grouped <- df |>
      dplyr::group_by(!!by_col_quo)

    group_sizes <- grouped |>
      dplyr::summarise(.group_n = dplyr::n(), .groups = "drop")

    use_binary <- switch(fill,
      auto = all(group_sizes$.group_n <= 1),
      binary = TRUE,
      count = FALSE
    )

    long <- grouped |>
      dplyr::summarise(dplyr::across(dplyr::everything(), \(x) sum(is_missing_cell(x))), .groups = "drop") |>
      tidyr::pivot_longer(cols = -!!by_col_quo, names_to = "variable", values_to = "sum_na")

    if (!row_order) {
      group_order_vals <- long |>
        dplyr::group_by(!!by_col_quo) |>
        dplyr::summarise(groupsum_na = sum(.data$sum_na), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(.data$groupsum_na)) |>
        dplyr::pull(!!by_col_quo)

      var_order_vals <- long |>
        dplyr::group_by(.data$variable) |>
        dplyr::summarise(varsum_na = sum(.data$sum_na), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(.data$varsum_na)) |>
        dplyr::pull("variable")

      long <- long |>
        dplyr::mutate(!!by_col_quo := factor(!!by_col_quo, levels = group_order_vals)) |>
        dplyr::mutate(variable = factor(.data$variable, levels = var_order_vals))
    }

    if (use_binary) {
      p <- long |>
        dplyr::mutate(Missing = factor(ifelse(.data$sum_na > 0, "Missing", "Present"), levels = c("Missing", "Present"))) |>
        ggplot2::ggplot(ggplot2::aes(x = !!by_col_quo, y = .data$variable, fill = .data$Missing)) +
        ggplot2::geom_tile() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 6, angle = 45, vjust = 0.5, hjust = 1),
                        axis.text.y = ggplot2::element_text(size = 6)) +
        ggplot2::labs(fill = "Data Status")
    } else {
      p <- long |>
        ggplot2::ggplot(ggplot2::aes(x = !!by_col_quo, y = .data$variable, fill = .data$sum_na)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient(low = "black", high = "red") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 6, angle = 45, vjust = 0.5, hjust = 1),
                        axis.text.y = ggplot2::element_text(size = 6)) +
        ggplot2::labs(fill = "# missing")
    }
    return(p)
  }

  missmat <- df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), is_missing_cell)) |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    tidyr::pivot_longer(-".row", names_to = "feature", values_to = "is_na")

  if (!row_order) {
    row_order_vals <- missmat |>
      dplyr::group_by(.data$.row) |>
      dplyr::summarise(rowsum_na = sum(.data$is_na), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$rowsum_na)) |>
      dplyr::pull(".row")

    col_order_vals <- missmat |>
      dplyr::group_by(.data$feature) |>
      dplyr::summarise(colsum_na = sum(.data$is_na), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$colsum_na)) |>
      dplyr::pull("feature")

    missmat <- missmat |>
      dplyr::mutate(.row = factor(.data$.row, levels = row_order_vals)) |>
      dplyr::mutate(feature = factor(.data$feature, levels = col_order_vals))
  }

  missmat |>
    dplyr::mutate(Missing = factor(ifelse(.data$is_na, "Missing", "Present"), levels = c("Missing", "Present"))) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$.row, y = .data$feature, fill = .data$Missing)) +
    ggplot2::geom_tile() +
    ggplot2::labs(x = "Row", y = "Column", fill = "Data Status") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 6))
}
