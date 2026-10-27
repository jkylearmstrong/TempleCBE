#' Group Trajectories with Empirical Quantile Ribbons
#'
#' Mean trajectory per group over time with a ribbon between two empirical quantiles of the
#' subjects' values at each time (not a confidence interval of the mean; the ribbon shows where
#' most subjects lie). For individual trajectories by outcome use [plot_dynamic_trajectory()].
#'
#' @param data A data frame in long format.
#' @param time_col,value_col,group_col Time, value and group columns.
#' @param id_col Subject column. Several rows of one subject at one time are averaged first, so
#'   each subject counts once per time.
#' @param ci_quantiles Lower and upper quantile of the ribbon.
#' @param facet One panel per group (`TRUE`) or all groups in one panel, distinguished by colour.
#' @param title,xlab,ylab Labels.
#' @return A [ggplot2::ggplot()]; the plotted summary is in `$data` (columns `group`, `time`,
#'   `n`, `mean`, `lower`, `upper`). Groups and times with a single subject have a ribbon of zero
#'   width.
#' @seealso [plot_dynamic_trajectory()]
#' @export
#' @examples
#' set.seed(1)
#' d <- expand.grid(id = 1:30, time = c(0, 5, 15, 30))
#' d$grp <- ifelse(d$id <= 15, "A", "B")
#' d$value <- rnorm(nrow(d), d$time / 10 + (d$grp == "B"))
#' plot_trajectory_group_ci(d, "time", "value", "grp", "id")
plot_trajectory_group_ci <- function(data, time_col = "time", value_col = "value",
                                     group_col = "new_group", id_col = "subject_id",
                                     ci_quantiles = c(0.025, 0.975), facet = TRUE,
                                     title = NULL, xlab = "Time", ylab = "Value") {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (v in c(time_col, value_col, group_col, id_col)) {
    if (length(v) != 1L || !is.character(v) || !v %in% names(data)) {
      stop("`", paste(v, collapse = ", "), "` is not a column of `data`.", call. = FALSE)
    }
  }
  if (!is.numeric(data[[value_col]]) || !is.numeric(data[[time_col]])) {
    stop("`time_col` and `value_col` must be numeric.", call. = FALSE)
  }
  if (!is.numeric(ci_quantiles) || length(ci_quantiles) != 2L || anyNA(ci_quantiles) ||
      ci_quantiles[1] >= ci_quantiles[2] || any(ci_quantiles < 0 | ci_quantiles > 1)) {
    stop("`ci_quantiles` must be two increasing probabilities.", call. = FALSE)
  }
  d <- data.frame(group = data[[group_col]], time = data[[time_col]], id = data[[id_col]],
                  value = data[[value_col]])
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) == 0L) stop("No complete rows to plot.", call. = FALSE)
  per <- stats::aggregate(value ~ group + time + id, data = d, FUN = mean)
  keys <- unique(per[c("group", "time")])
  keys <- keys[order(keys$group, keys$time), , drop = FALSE]
  summ <- do.call(rbind, lapply(seq_len(nrow(keys)), function(i) {
    v <- per$value[per$group == keys$group[i] & per$time == keys$time[i]]
    q <- stats::quantile(v, ci_quantiles, names = FALSE)
    data.frame(group = keys$group[i], time = keys$time[i], n = length(v), mean = mean(v),
               lower = q[1], upper = q[2])
  }))
  summ$group <- factor(summ$group)

  p <- ggplot2::ggplot(summ, ggplot2::aes(x = .data$time, y = .data$mean, colour = .data$group, fill = .data$group,
                                          group = .data$group)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), alpha = 0.2, colour = NA) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::labs(title = title, x = xlab, y = ylab, colour = NULL, fill = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = if (facet) "none" else "bottom")
  if (facet) p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$group))
  p
}
