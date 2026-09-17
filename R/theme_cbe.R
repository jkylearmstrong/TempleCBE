#' Temple University CBE ggplot2 Themes, Palettes, and Formatters
#'
#' Consistent institutional branding and clean publication/presentation
#' styling for Temple University Center for Biostatistics & Epidemiology.
#'
#' @name theme_cbe
NULL

#' Temple CBE Color Palette
#'
#' Official and complementary colors for CBE publications and slide decks.
#' @export
cbe_palette <- c(
  cherry       = "#9D2235", # Temple Cherry
  neutral_grey = "#6F6F6F", # Neutral Grey
  dark_grey    = "#2B2B2B",
  light_grey   = "#D9D9D9",
  sand         = "#C2B280",
  blue_grey    = "#4A6572",
  accent_gold  = "#D4AF37"
)

#' CBE ggplot2 Theme for Manuscripts & Reports
#'
#' A minimal, publication-ready ggplot2 theme with clean gridlines and bold strip text.
#'
#' @param base_size Base font size (default: 11)
#' @param base_family Base font family (default: "")
#' @return A ggplot2 theme object
#' @export
theme_cbe <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(color = "grey30", hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 0, color = "grey30", size = ggplot2::rel(0.85))
    )
}

#' CBE ggplot2 Theme for Presentation Decks
#'
#' Optimized for slides (PowerPoint/Keynote) with larger typography and bottom legend.
#'
#' @param base_size Base font size (default: 14)
#' @return A ggplot2 theme object
#' @export
theme_cbe_deck <- function(base_size = 14) {
  theme_cbe(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0, color = "grey30", size = ggplot2::rel(0.8)),
      legend.position = "bottom"
    )
}

#' CBE ggplot2 Theme for Survival Analysis Visualizations
#'
#' A variant of \code{theme_cbe()} tuned for Cox model and Kaplan-Meier plots
#' (forest plots, predicted/observed survival curves, marginal risk curves), giving a
#' single consistent look across \code{plot_cox_forest()}, \code{plot_cox_forest_multi()},
#' \code{plot_cox_survival()}, and \code{plot_cox_marginal()}.
#'
#' @param base_size Base font size (default: 12)
#' @param base_family Base font family (default: "")
#' @return A ggplot2 theme object
#' @seealso [theme_cbe()], [theme_cbe_deck()]
#' @export
cbe_theme_survival <- function(base_size = 12, base_family = "") {
  theme_cbe(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "grey90"),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.line.x        = ggplot2::element_line(color = "grey40"),
      legend.title        = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.9))
    )
}

#' Discrete Color Scale for Temple CBE
#'
#' @param ... Arguments passed to \code{ggplot2::scale_color_manual}
#' @export
scale_color_cbe <- function(...) {
  ggplot2::scale_color_manual(values = unname(cbe_palette), ...)
}

#' Discrete Fill Scale for Temple CBE
#'
#' @param ... Arguments passed to \code{ggplot2::scale_fill_manual}
#' @export
scale_fill_cbe <- function(...) {
  ggplot2::scale_fill_manual(values = unname(cbe_palette), ...)
}

# --- Formatting Helpers ---

#' Format a Percentage
#'
#' @param x Numeric proportion between 0 and 1
#' @param digits Number of decimal places (default: 1)
#' @return Formatted character string with '\%'
#' @export
fmt_pct <- function(x, digits = 1) {
  sprintf(paste0("%.", digits, "f%%"), 100 * x)
}

#' Format Numbers to Fixed Decimals
#'
#' @param x Numeric value
#' @param digits Number of decimal places (default: 2)
#' @return Formatted character string
#' @export
fmt_num <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits)
}

#' Format Significant Figures
#'
#' @param x Numeric value
#' @param digits Number of significant figures (default: 3)
#' @return Formatted character string
#' @export
fmt_sig <- function(x, digits = 3) {
  trimws(formatC(x, digits = digits, format = "fg"))
}

#' Format P-Values
#'
#' Formats p-values with institutional threshold (< 0.001).
#'
#' @param p Numeric p-value
#' @return Formatted string (e.g., "p = 0.034" or "p < 0.001")
#' @export
fmt_p <- function(p) {
  ifelse(is.na(p), "\u2014", ifelse(p < 0.001, "p < 0.001", sprintf("p = %.3f", p)))
}

#' Format Hazard Ratio and Confidence Interval
#'
#' @param hr Point estimate for Hazard Ratio
#' @param conf_low Lower 95\% confidence limit
#' @param conf_high Upper 95\% confidence limit
#' @param digits Number of decimal places (default: 2)
#' @return Formatted string (e.g., "HR 1.25 (95\% CI 1.05--1.48)")
#' @export
fmt_hr <- function(hr, conf_low, conf_high, digits = 2) {
  sprintf(
    "HR %s (95%% CI %s\u2013%s)",
    fmt_num(hr, digits),
    fmt_num(conf_low, digits),
    fmt_num(conf_high, digits)
  )
}

#' Combine Character Vector into Natural-Language Words
#'
#' @param x Character vector
#' @param none Fallback text when vector is empty (default: "none")
#' @return Character string (e.g., "A, B, and C")
#' @export
words <- function(x, none = "none") {
  if (length(x) == 0) return(none)
  if (requireNamespace("knitr", quietly = TRUE)) {
    as.character(knitr::combine_words(x))
  } else {
    n <- length(x)
    if (n == 1) {
      as.character(x)
    } else if (n == 2) {
      paste(x, collapse = " and ")
    } else {
      paste0(paste(x[-n], collapse = ", "), ", and ", x[n])
    }
  }
}
