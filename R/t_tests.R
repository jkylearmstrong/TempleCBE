#' Single T-Test, Tidied
#'
#' Runs a t-test comparing \code{.var} between the two levels of \code{.class},
#' tidied into one row with group sizes/SDs and fold-change added.
#'
#' @param .data A data frame or tibble.
#' @param .var Name (string) of the continuous column to test.
#' @param .class Name (string) of a binary (2-level) classification column.
#' @param .id Name (string, optional) of a subject/record identifier column.
#'   When \code{paired = TRUE}, pass this to pair observations by matching
#'   \code{.id} across the two groups rather than by row position — every id
#'   must have exactly one observation in each group. If \code{paired = TRUE}
#'   and \code{.id} is omitted, observations are paired by row order within
#'   each group, which silently produces meaningless results if the two
#'   groups aren't already sorted into corresponding order.
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
single_t_test <- function(.data, .var, .class, .id = NULL, alternative = "two.sided",
                           conf.level = 0.95, paired = FALSE, ...) {
  sym_var <- rlang::sym(.var)
  sym_class <- rlang::sym(.class)

  class_vec <- dplyr::pull(.data, !!sym_class)
  if (!is.factor(class_vec)) class_vec <- factor(class_vec)
  levels_vec <- levels(class_vec)
  if (length(levels_vec) != 2) {
    stop(paste0("grouping factor ", .class, " must have exactly 2 levels, has ", length(levels_vec)))
  }

  if (isTRUE(paired) && !is.null(.id)) {
    sym_id <- rlang::sym(.id)
    wide <- .data |>
      dplyr::select(!!sym_id, !!sym_class, !!sym_var) |>
      tidyr::pivot_wider(names_from = !!sym_class, values_from = !!sym_var)

    if (anyNA(wide[[levels_vec[1]]]) || anyNA(wide[[levels_vec[2]]])) {
      stop(paste0(
        "single_t_test(): paired = TRUE with `.id` requires every id to have ",
        "exactly one observation in both '", levels_vec[1], "' and '", levels_vec[2],
        "'; found id(s) missing from one group."
      ))
    }

    x_vec <- wide[[levels_vec[1]]]
    y_vec <- wide[[levels_vec[2]]]
  } else {
    if (isTRUE(paired)) {
      message(
        "single_t_test(): paired = TRUE without `.id` pairs observations by row ",
        "order within each group. Pass `.id` to pair explicitly by a subject/record identifier."
      )
    }
    x_vec <- dplyr::filter(.data, !!sym_class == levels_vec[1]) |> dplyr::pull(!!sym_var)
    y_vec <- dplyr::filter(.data, !!sym_class == levels_vec[2]) |> dplyr::pull(!!sym_var)
  }

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

  # Computed directly from x_vec/y_vec (not from broom::tidy()'s
  # estimate1/estimate2) because a *paired* t-test only returns a single
  # `estimate` column (the mean difference) -- estimate1/estimate2 don't
  # exist in that case, which previously made single_t_test() error out
  # unconditionally whenever paired = TRUE.
  mean_x <- mean(x_vec, na.rm = TRUE)
  mean_y <- mean(y_vec, na.rm = TRUE)
  fold_change <- mean_y / mean_x

  broom::tidy(t_test_result) |>
    dplyr::mutate(
      var = .var,
      group1 = paste0("mean in group ", levels_vec[1]),
      group2 = paste0("mean in group ", levels_vec[2]),
      n_per_group = n_per_group,
      sd_per_group = sd_per_group,
      log_p = -log10(.data$p.value),
      fold_change = fold_change,
      log2_fold_change = log2(abs(fold_change)) * sign(fold_change)
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
#' @param ... Additional arguments passed to \code{\link{single_t_test}} (and
#'   on to \code{\link[stats]{t.test}}) — e.g. \code{paired} and \code{.id}.
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
#' @param .id Name (string, optional) of a subject/record identifier column;
#'   see \code{\link{single_t_test}}. Only meaningful when \code{paired = TRUE}.
#' @param paired Logical; paired t-test.
#' @param alternative One of \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#' @param conf.level Confidence level for the interval.
#' @return A tibble with one row per level of \code{.class}.
#' @export
#' @examples
#' one_vs_rest_t_test(iris, "Sepal.Length", "Species")
one_vs_rest_t_test <- function(.data, .var, .class, .id = NULL, paired = FALSE,
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
    single_t_test(data2, .var, .class, .id = .id, paired = paired,
                   alternative = alternative, conf.level = conf.level)
  })
}
