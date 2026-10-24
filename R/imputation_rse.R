#' Relative Squared Error of Imputation Methods Against a Null Baseline
#'
#' Scores imputation methods on cells whose true value is known (values you masked, then imputed).
#' For each variable and method it computes the root mean squared error, the RMSE of a **null
#' baseline** that predicts every cell with one overall value, and the **relative squared error**
#' \deqn{\mathrm{RSE} = (\mathrm{RMSE}_{\mathrm{method}} / \mathrm{RMSE}_{\mathrm{null}})^2.}
#' An RSE below 1 beats the null baseline, 1 matches it, above 1 is worse.
#'
#' @section Null baseline:
#' By default the baseline value is the mean (or median) of the held-out `truth` values of that variable
#' (every row of `results_df` for the variable counts once), one value per variable shared by all methods. This is an **oracle** baseline: it uses the very values
#' being predicted, so it is the best constant for the mean (it minimises the RMSE) and a method that
#' imputes the training mean scores an RSE slightly above 1. (The median minimises the absolute error,
#' not the RMSE.) Note that [tune_impute_numeric()] defines its RSE differently, with the mean of the
#' unmasked training values as the baseline, so its `mean` method scores exactly 1. To use the training
#' data's mean or median here as well, pass `baseline`: a named numeric vector (names are variables) or a
#' data frame with `variable` and `value`. If the null RMSE is 0 (all truths equal the baseline) the RSE
#' is `NA`. RSEs of different methods are only comparable when they were scored on the same cells; the
#' function warns when the cell counts differ.
#'
#' @param results_df A data frame with one row per scored cell, with the columns named by
#'   `variable_col`, `method_col`, `truth_col` and `imputed_col`. Cells with a missing truth or
#'   imputed value are dropped and counted in `n_dropped`. Alternatively, a table that already has
#'   `rmse` and `null_rmse` columns per variable and method.
#' @param null_method `"mean"` (default) or `"median"`.
#' @param baseline Optional baseline values; see Null baseline.
#' @param variable_col,method_col,truth_col,imputed_col Column names in `results_df`.
#' @return A tibble of class `cbe_rse` with `variable`, `method`, `n`, `n_dropped`, `rmse`,
#'   `null_rmse`, `rse`, ordered by variable and RSE. Extra columns named `pct_miss` are kept.
#' @seealso [tbl_best_imputation_method()], [plot_rse_by_column()], [plot_rse_vs_missingness()],
#'   [build_best_imputed_data()], [tune_impute_numeric()]
#' @export
#' @examples
#' set.seed(1)
#' cells <- data.frame(
#'   variable = "x", truth = rnorm(40, 10, 2)
#' )
#' res <- rbind(
#'   transform(cells, method = "mean", imputed = 10),
#'   transform(cells, method = "good", imputed = truth + rnorm(40, 0, 0.5))
#' )
#' compare_imputation_rse(res)
compare_imputation_rse <- function(results_df,
                                   null_method = c("mean", "median"),
                                   baseline = NULL,
                                   variable_col = "variable",
                                   method_col = "method",
                                   truth_col = "truth",
                                   imputed_col = "imputed") {
  null_method <- match.arg(null_method)
  if (!is.data.frame(results_df)) stop("`results_df` must be a data frame.", call. = FALSE)
  need <- c(variable_col, method_col)
  miss <- setdiff(need, names(results_df))
  if (length(miss)) stop("`results_df` lacks column(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)

  if (all(c("rmse", "null_rmse") %in% names(results_df))) {
    out <- tibble::tibble(
      variable = as.character(results_df[[variable_col]]),
      method = as.character(results_df[[method_col]]),
      n = if ("n" %in% names(results_df)) results_df$n else NA_integer_,
      n_dropped = if ("n_dropped" %in% names(results_df)) results_df$n_dropped else NA_integer_,
      rmse = as.numeric(results_df$rmse),
      null_rmse = as.numeric(results_df$null_rmse)
    )
    if ("pct_miss" %in% names(results_df)) out$pct_miss <- results_df$pct_miss
    return(.finish_rse(out))
  }

  miss <- setdiff(c(truth_col, imputed_col), names(results_df))
  if (length(miss)) {
    stop("`results_df` lacks column(s): ", paste(miss, collapse = ", "),
         " (or give `rmse` and `null_rmse`).", call. = FALSE)
  }
  if (!is.numeric(results_df[[truth_col]]) || !is.numeric(results_df[[imputed_col]])) {
    stop("`", truth_col, "` and `", imputed_col, "` must be numeric.", call. = FALSE)
  }

  df <- tibble::tibble(
    variable = as.character(results_df[[variable_col]]),
    method = as.character(results_df[[method_col]]),
    truth = results_df[[truth_col]],
    imputed = results_df[[imputed_col]]
  )
  if (anyNA(df$variable) || anyNA(df$method)) stop("`variable` and `method` must not be missing.", call. = FALSE)
  if ("pct_miss" %in% names(results_df)) df$pct_miss <- results_df$pct_miss
  base_fun <- if (null_method == "mean") mean else stats::median

  base_val <- NULL
  if (!is.null(baseline)) {
    if (is.data.frame(baseline)) {
      if (!all(c("variable", "value") %in% names(baseline))) {
        stop("`baseline` as a data frame needs columns `variable` and `value`.", call. = FALSE)
      }
      base_val <- stats::setNames(baseline$value, baseline$variable)
    } else if (is.numeric(baseline) && !is.null(names(baseline))) {
      base_val <- baseline
    } else {
      stop("`baseline` must be a named numeric vector or a data frame with `variable` and `value`.", call. = FALSE)
    }
    gap <- setdiff(unique(df$variable), names(base_val))
    if (length(gap)) stop("`baseline` has no value for: ", paste(gap, collapse = ", "), ".", call. = FALSE)
  }

  # one baseline value per variable, from every non-missing truth of that variable, so that all methods
  # are compared with the same constant
  var_base <- vapply(split(df$truth, df$variable), function(x) {
    x <- x[!is.na(x)]
    if (length(x)) base_fun(x) else NA_real_
  }, numeric(1))

  pieces <- lapply(split(df, list(df$variable, df$method), drop = TRUE), function(g) {
    ok <- !is.na(g$truth) & !is.na(g$imputed)
    gv <- g$variable[1]
    b <- if (is.null(base_val)) var_base[[gv]] else base_val[[gv]]
    tibble::tibble(
      variable = gv, method = g$method[1],
      n = sum(ok), n_dropped = sum(!ok),
      rmse = if (any(ok)) sqrt(mean((g$truth[ok] - g$imputed[ok])^2)) else NA_real_,
      null_rmse = if (any(ok)) sqrt(mean((g$truth[ok] - b)^2)) else NA_real_,
      pct_miss = if ("pct_miss" %in% names(g)) g$pct_miss[1] else NULL
    )
  })
  res <- dplyr::bind_rows(pieces)
  uneven <- unique(res$variable[ave(res$n, res$variable, FUN = function(z) length(unique(z))) > 1L])
  if (length(uneven)) {
    warning("The methods were scored on different numbers of cells for: ", paste(uneven, collapse = ", "),
            ". Their RSEs are only comparable on the same cells.", call. = FALSE)
  }
  .finish_rse(res)
}

.finish_rse <- function(out) {
  out$rse <- ifelse(is.na(out$null_rmse) | out$null_rmse == 0, NA_real_, (out$rmse / out$null_rmse)^2)
  out <- out[order(out$variable, out$rse, out$method), , drop = FALSE]
  class(out) <- c("cbe_rse", class(out))
  out
}

.check_rse_df <- function(rse_df, arg = "rse_df") {
  if (!is.data.frame(rse_df) || !all(c("variable", "method", "rse") %in% names(rse_df))) {
    stop("`", arg, "` must have columns `variable`, `method` and `rse` (see `compare_imputation_rse()`).", call. = FALSE)
  }
  rse_df
}

#' Best Imputation Method per Column
#'
#' Picks, for each variable, the method with the lowest relative squared error.
#'
#' Ties go to the method that comes first in alphabetical order; methods with a missing RSE
#' are never chosen (a variable where all are missing gets `NA`). `beats_null` says whether the winner's
#' RSE is below 1.
#'
#' @param rse_df A [compare_imputation_rse()] result, or [tune_impute_numeric()] output
#'   (anything with `variable`, `method` and `rse`).
#' @return A tibble with one row per variable: `variable`, `best_method`, `rse`, `runner_up`,
#'   `runner_up_rse`, `beats_null`, `n_methods`.
#' @export
#' @examples
#' rse <- data.frame(variable = c("x", "x", "y", "y"),
#'                   method = c("mean", "knn", "mean", "knn"), rse = c(1, 0.4, 0.9, 1.2))
#' tbl_best_imputation_method(rse)
tbl_best_imputation_method <- function(rse_df) {
  .check_rse_df(rse_df)
  df <- tibble::as_tibble(as.data.frame(rse_df))
  df$variable <- as.character(df$variable)
  df$method <- as.character(df$method)
  rows <- lapply(unique(df$variable), function(v) {
    g <- df[df$variable == v, , drop = FALSE]
    g <- g[!is.na(g$rse), , drop = FALSE]
    g <- g[order(g$rse, g$method), , drop = FALSE]
    tibble::tibble(
      variable = v,
      best_method = if (nrow(g)) g$method[1] else NA_character_,
      rse = if (nrow(g)) g$rse[1] else NA_real_,
      runner_up = if (nrow(g) > 1L) g$method[2] else NA_character_,
      runner_up_rse = if (nrow(g) > 1L) g$rse[2] else NA_real_,
      beats_null = if (nrow(g)) g$rse[1] < 1 else NA,
      n_methods = nrow(g)
    )
  })
  dplyr::bind_rows(rows)
}

#' Assemble the Final Imputed Data From the Winning Methods
#'
#' Builds one data frame in which every variable comes from the imputation method that won for it
#' ([tbl_best_imputation_method()]).
#'
#' @param data The original data frame (with its missing values). Columns that are not in `best`
#'   are returned as they are.
#' @param imputations A **named list** of data frames, one per method, each the same size as `data`
#'   and in the same row order (this is not checked beyond the number of rows)
#'   with the columns imputed by that method. The names are the method names used in `best`.
#' @param best A [tbl_best_imputation_method()] result.
#' @param require_beats_null If `TRUE`, a variable whose best method does not beat the null baseline
#'   (RSE of 1 or more) is left with the `fallback` method instead of the winner.
#' @param fallback Method name (in `imputations`) used for variables without a winner and, with
#'   `require_beats_null`, for those that do not beat the null baseline. `NULL` (default) leaves such
#'   variables as in `data`.
#' @return A data frame like `data`, with attribute `"imputation_source"`: a tibble of `variable`
#'   and `method` (`NA` where the column came from `data`).
#' @export
build_best_imputed_data <- function(data, imputations, best, require_beats_null = FALSE, fallback = NULL) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!is.list(imputations) || is.null(names(imputations)) || any(!nzchar(names(imputations)))) {
    stop("`imputations` must be a named list of data frames, one per method.", call. = FALSE)
  }
  if (!is.data.frame(best) || !all(c("variable", "best_method") %in% names(best))) {
    stop("`best` must be the result of `tbl_best_imputation_method()`.", call. = FALSE)
  }
  if (isTRUE(require_beats_null) && !"beats_null" %in% names(best)) {
    stop("`require_beats_null = TRUE` needs a `beats_null` column in `best`.", call. = FALSE)
  }
  for (m in names(imputations)) {
    if (!is.data.frame(imputations[[m]]) || nrow(imputations[[m]]) != nrow(data)) {
      stop("`imputations$", m, "` must be a data frame with the same number of rows as `data`.", call. = FALSE)
    }
  }
  if (!is.null(fallback) && !fallback %in% names(imputations)) {
    stop("`fallback` \"", fallback, "\" is not in `imputations`.", call. = FALSE)
  }
  out <- data
  src <- character(nrow(best))
  for (i in seq_len(nrow(best))) {
    v <- best$variable[i]
    m <- best$best_method[i]
    if (!v %in% names(data)) stop("`best` names a variable that is not in `data`: ", v, ".", call. = FALSE)
    if (isTRUE(require_beats_null) && !isTRUE(best$beats_null[i])) m <- NA_character_
    if (is.na(m)) m <- if (is.null(fallback)) NA_character_ else fallback
    if (!is.na(m)) {
      if (!m %in% names(imputations)) stop("No imputation named \"", m, "\" in `imputations`.", call. = FALSE)
      if (!v %in% names(imputations[[m]])) {
        stop("`imputations$", m, "` has no column \"", v, "\".", call. = FALSE)
      }
      out[[v]] <- imputations[[m]][[v]]
    }
    src[i] <- m
  }
  attr(out, "imputation_source") <- tibble::tibble(variable = best$variable, method = src)
  out
}

#' Plot Imputation Performance Across Columns
#'
#' Dot plot of each method's relative squared error (or RMSE) by variable, with the null baseline
#' (RSE = 1) marked.
#'
#' @param rse_df A [compare_imputation_rse()] result or anything with `variable`, `method`, `rse`.
#' @param metric `"RSE"` (default) or `"RMSE"` (needs an `rmse` column).
#' @param log_scale Logical; plot RSE on a log axis (default `FALSE`).
#' @return A ggplot2 object.
#' @export
#' @examples
#' rse <- data.frame(variable = c("x", "x", "y", "y"),
#'                   method = c("mean", "knn", "mean", "knn"), rse = c(1, 0.4, 0.9, 1.2))
#' plot_rse_by_column(rse)
plot_rse_by_column <- function(rse_df, metric = c("RSE", "RMSE"), log_scale = FALSE) {
  metric <- match.arg(metric)
  .check_rse_df(rse_df)
  col <- if (metric == "RSE") "rse" else "rmse"
  if (!col %in% names(rse_df)) stop("`rse_df` has no `", col, "` column.", call. = FALSE)
  df <- as.data.frame(rse_df)
  df$.y <- factor(df$variable, levels = rev(unique(df$variable)))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[col]], y = .data$.y, color = .data$method)) +
    ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 0.5))
  if (metric == "RSE") {
    p <- p + ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "grey50")
    if (isTRUE(log_scale)) p <- p + ggplot2::scale_x_log10()
  }
  p +
    scale_color_cbe() +
    theme_cbe() +
    ggplot2::labs(
      x = if (metric == "RSE") "Relative squared error (1 = null baseline)" else "RMSE",
      y = NULL, color = "Method"
    )
}

#' Plot Imputation Error Against the Percent Missing
#'
#' Scatter of each method's relative squared error against how much of the variable is missing,
#' to see whether imputation gets worse as missingness grows.
#'
#' @param data The data frame with the missing values (used for the percent missing of each variable).
#' @param rse_df A [compare_imputation_rse()] result or anything with `variable`, `method`, `rse`.
#'   If it has a `pct_miss` column that is used instead of `data`.
#' @return A ggplot2 object. Variables of `rse_df` that are not columns of `data` are dropped with a
#'   warning.
#' @export
plot_rse_vs_missingness <- function(data, rse_df) {
  .check_rse_df(rse_df)
  df <- as.data.frame(rse_df)
  if (!"pct_miss" %in% names(df)) {
    if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
    gone <- setdiff(unique(df$variable), names(data))
    if (length(gone)) {
      warning("Variable(s) not in `data` were dropped: ", paste(gone, collapse = ", "), ".", call. = FALSE)
      df <- df[df$variable %in% names(data), , drop = FALSE]
    }
    pm <- vapply(df$variable, function(v) 100 * mean(is.na(data[[v]])), numeric(1))
    df$pct_miss <- unname(pm)
  }
  if (nrow(df) == 0L) stop("No variables to plot.", call. = FALSE)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$pct_miss, y = .data$rse, color = .data$method)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
    ggplot2::geom_point(size = 3) +
    scale_color_cbe() +
    theme_cbe() +
    ggplot2::labs(x = "Percent missing", y = "Relative squared error (1 = null baseline)", color = "Method")
}
