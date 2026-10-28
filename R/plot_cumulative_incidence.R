#' Plot Cumulative Incidence Curves
#'
#' Step curves with shaded pointwise intervals for the output of [tbl_cumulative_incidence()]. Call
#' `tbl_cumulative_incidence()` with a fine grid of `times` (for example every unit of follow-up)
#' for smooth-looking steps; at a handful of times the curves only join those points.
#'
#' @param cif_tbl Result of [tbl_cumulative_incidence()].
#' @param facet Facet by `group` (when present) instead of colouring groups. With no `group`
#'   column, `facet = TRUE` facets by `cause`.
#' @param base_size Base font size passed to [cbe_theme_survival()].
#' @return A ggplot.
#' @seealso [tbl_cumulative_incidence()]
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(time = rexp(200, 0.1), type = sample(0:2, 200, TRUE, prob = c(0.3, 0.4, 0.3)),
#'                 arm = rep(c("A", "B"), 100))
#' cif <- tbl_cumulative_incidence(d, "time", "type", group_col = "arm", times = seq(0, 20, 1))
#' plot_cumulative_incidence(cif)
plot_cumulative_incidence <- function(cif_tbl, facet = FALSE, base_size = 12) {
  need <- c("cause", "time", "cif", "conf.low", "conf.high")
  if (!is.data.frame(cif_tbl) || !all(need %in% names(cif_tbl))) {
    stop("`cif_tbl` must come from tbl_cumulative_incidence() (columns ", paste(need, collapse = ", "), ").",
         call. = FALSE)
  }
  d <- as.data.frame(cif_tbl)
  d$cause <- as.character(d$cause)
  has_group <- "group" %in% names(d)
  if (has_group) {
    d$group <- as.character(d$group)
    d$.curve <- interaction(d$cause, d$group, drop = TRUE)
  } else {
    d$.curve <- d$cause
  }
  # Hold the interval flat between times so the band follows the steps of the curve.
  step_ribbon <- do.call(rbind, lapply(split(d, d$.curve), function(g) {
    g <- g[order(g$time), , drop = FALSE]
    n <- nrow(g)
    if (n < 2L) return(g)
    pos <- rep(seq_len(n), each = 2L)[-2L * n]
    out <- g[pos, , drop = FALSE]
    out$time <- c(g$time[1L], rep(g$time[-1L], each = 2L))
    out
  }))
  colour_var <- if (has_group && !isTRUE(facet)) "group" else "cause"
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$time, y = .data$cif, colour = .data[[colour_var]],
                                       fill = .data[[colour_var]], group = .data$.curve)) +
    ggplot2::geom_ribbon(data = step_ribbon, ggplot2::aes(ymin = .data$conf.low, ymax = .data$conf.high),
                         alpha = 0.15, colour = NA) +
    ggplot2::geom_step(linewidth = 0.8) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(v) paste0(100 * v, "%")) +
    ggplot2::labs(x = "Time", y = "Cumulative incidence", colour = NULL, fill = NULL) +
    cbe_theme_survival(base_size = base_size)
  if (isTRUE(facet)) {
    p <- p + ggplot2::facet_wrap(if (has_group) ggplot2::vars(.data$group) else ggplot2::vars(.data$cause))
  } else if (has_group) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$cause))
  }
  p
}
