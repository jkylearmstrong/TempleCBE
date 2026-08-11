#' Single T-Test, Tidied
#'
#' Runs a t-test comparing \code{.var} between the two levels of \code{.class},
#' tidied into one row with group sizes/SDs and fold-change added.
#'
#' @param .data A data frame or tibble.
#' @param .var Name (string) of the continuous column to test.
#' @param .class Name (string) of a binary (2-level) classification column.
#' @param alternative One of \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#' @param conf.level Confidence level for the interval.
#' @param paired Logical; paired t-test.
#' @param ... Additional arguments passed to \code{\link[stats]{t.test}}.
#' @return A one-row tibble.
#' @importFrom rlang :=
#' @export
#' @examples
#' mtcars |>
#'   dplyr::mutate(am = factor(am)) |>
#'   single_t_test("mpg", "am")
single_t_test <- function(.data, .var, .class, alternative = "two.sided",
                           conf.level = 0.95, paired = FALSE, ...) {
  sym_var <- rlang::sym(.var)
  sym_class <- rlang::sym(.class)

  class_vec <- dplyr::pull(.data, !!sym_class)
  if (!is.factor(class_vec)) class_vec <- factor(class_vec)
  levels_vec <- levels(class_vec)
  if (length(levels_vec) != 2) {
    stop(paste0("grouping factor ", .class, " must have exactly 2 levels, has ", length(levels_vec)))
  }

  x_vec <- dplyr::filter(.data, !!sym_class == levels_vec[1]) |> dplyr::pull(!!sym_var)
  y_vec <- dplyr::filter(.data, !!sym_class == levels_vec[2]) |> dplyr::pull(!!sym_var)

  t_test_result <- tryCatch(
    stats::t.test(x = x_vec, y = y_vec, alternative = alternative, conf.level = conf.level, paired = paired, ...),
    error = function(e) e
  )

  group_stats <- .data |>
    dplyr::select(!!sym_var, !!sym_class) |>
    dplyr::group_by(!!sym_class) |>
    dplyr::summarise(n = sum(!is.na(!!sym_var)), sd = stats::sd(!!sym_var, na.rm = TRUE), .groups = "drop")

  n_per_group <- paste(paste0(group_stats[[.class]], " (n = ", group_stats$n, ")"), collapse = "; ")
  sd_per_group <- paste(paste0(group_stats[[.class]], " (sd = ", group_stats$sd, ")"), collapse = "; ")

  if (inherits(t_test_result, "error")) {
    return(tibble::tibble(var = .var, method = t_test_result$message,
                           n_per_group = n_per_group, sd_per_group = sd_per_group))
  }

  broom::tidy(t_test_result) |>
    dplyr::mutate(
      var = .var,
      group1 = paste0("mean in group ", levels_vec[1]),
      group2 = paste0("mean in group ", levels_vec[2]),
      n_per_group = n_per_group,
      sd_per_group = sd_per_group,
      log_p = -log10(.data$p.value),
      fold_change = .data$estimate2 / .data$estimate1,
      log2_fold_change = log2(abs(.data$fold_change)) * sign(.data$fold_change)
    )
}

#' Multiple T-Tests Against One Classifier
#'
#' Runs \code{\link{single_t_test}} for every (or a chosen set of) numeric
#' column against a single binary classifier.
#'
#' @param .data A data frame or tibble.
#' @param .var_list Character vector of column names to test (default: every
#'   numeric column in \code{.data}).
#' @param .class Name (string) of a binary classification column.
#' @param alternative One of \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#' @param conf.level Confidence level for the interval.
#' @param ... Additional arguments passed to \code{\link[stats]{t.test}}.
#' @return A tibble with one row per tested variable.
#' @export
#' @examples
#' mtcars |>
#'   dplyr::mutate(am = factor(am)) |>
#'   multiple_t_test(.class = "am")
multiple_t_test <- function(.data,
                             .var_list = names(dplyr::select(.data, dplyr::where(is.numeric))),
                             .class,
                             alternative = "two.sided",
                             conf.level = 0.95,
                             ...) {
  purrr::map_dfr(.var_list, \(v) {
    single_t_test(.data, .var = v, .class = .class, alternative = alternative, conf.level = conf.level, ...)
  })
}

#' One-vs-Rest T-Tests Across a Multi-Level Factor
#'
#' For a classifier with more than two levels, dichotomizes each level
#' against all others in turn and runs \code{\link{single_t_test}}.
#'
#' @param .data A data frame or tibble.
#' @param .var Name (string) of the continuous column to test.
#' @param .class Name (string) of a classification column (2+ levels).
#' @param paired Logical; paired t-test.
#' @param alternative One of \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#' @param conf.level Confidence level for the interval.
#' @return A tibble with one row per level of \code{.class}.
#' @export
#' @examples
#' one_vs_rest_t_test(iris, "Sepal.Length", "Species")
one_vs_rest_t_test <- function(.data, .var, .class, paired = FALSE,
                                alternative = "two.sided", conf.level = 0.95) {
  sym_class <- rlang::sym(.class)
  .data <- dplyr::mutate(.data, !!sym_class := factor(!!sym_class))

  levels_vec <- levels(dplyr::pull(.data, !!sym_class))
  names(levels_vec) <- levels_vec

  purrr::map_dfr(levels_vec, \(lvl) {
    data2 <- dplyr::mutate(.data, !!sym_class := factor(
      ifelse(as.character(!!sym_class) == lvl, lvl, ".rest"),
      levels = c(lvl, ".rest")
    ))
    single_t_test(data2, .var, .class, paired = paired, alternative = alternative, conf.level = conf.level)
  })
}
