#' Include a Figure on a Landscape Page (PDF Only)
#'
#' For a Quarto or R Markdown chunk with `results = "asis"`. When the document is rendered to PDF the
#' figure is placed on its own landscape page; in HTML, Word and other formats it is a normal figure and
#' no LaTeX is written, so nothing leaks into the output.
#'
#' @section PDF requirements:
#' The default `environment = "landscape"` uses the `landscape` environment of the LaTeX package
#' pdflscape, so the document must load that package (in Quarto, under `header-includes`).
#' With `environment = "blandscape"` the figure is wrapped in a begin/end macro pair named
#' blandscape and elandscape that your own preamble defines. The function does not add packages
#' to your document.
#'
#' @param path Path to the image file, relative to the document.
#' @param caption Optional caption text.
#' @param label Optional cross-reference id (for Quarto use a `"fig-"` prefix, for example `"fig-flow"`).
#' @param width Image width as a percentage or length (default `"100\%"`).
#' @param environment `"landscape"` (default) or `"blandscape"`; see PDF requirements.
#' @param format Output format override: `"latex"`, `"html"`, `"docx"` or `"other"`. By default it is read
#'   from knitr.
#' @return A string of class `knit_asis` (printed by knitr as raw markdown); with `format = NULL` outside
#'   knitr the format is `"other"`.
#' @export
#' @examples
#' cat(include_landscape_figure("fig/flow.png", caption = "Flow", label = "fig-flow", format = "latex"))
#' cat(include_landscape_figure("fig/flow.png", caption = "Flow", format = "html"))
include_landscape_figure <- function(path,
                                     caption = NULL,
                                     label = NULL,
                                     width = "100%",
                                     environment = c("landscape", "blandscape"),
                                     format = NULL) {
  environment <- match.arg(environment)
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be one file path.", call. = FALSE)
  }
  if (!is.null(caption) && (!is.character(caption) || length(caption) != 1L)) {
    stop("`caption` must be one string or NULL.", call. = FALSE)
  }
  if (!is.null(label) && (!is.character(label) || length(label) != 1L || !grepl("^[A-Za-z][A-Za-z0-9:_-]*$", label))) {
    stop("`label` must start with a letter and use only letters, digits, `-`, `_` and `:`.", call. = FALSE)
  }
  if (!is.character(width) || length(width) != 1L || !grepl("^[0-9.]+(%|in|cm|mm|px|pt)?$", width)) {
    stop("`width` must be a percentage or length such as \"100%\" or \"8in\".", call. = FALSE)
  }
  if (is.null(format)) {
    format <- if (requireNamespace("knitr", quietly = TRUE)) {
      if (isTRUE(knitr::is_latex_output())) {
        "latex"
      } else if (isTRUE(knitr::is_html_output())) {
        "html"
      } else if (identical(knitr::pandoc_to(), "docx")) {
        "docx"
      } else {
        "other"
      }
    } else {
      "other"
    }
  }
  format <- match.arg(format, c("latex", "html", "docx", "other"))

  attrs <- c(if (!is.null(label)) paste0("#", label), paste0("width=\"", width, "\""))
  img <- sprintf("![%s](%s){%s}", caption %||% "", path, paste(attrs, collapse = " "))
  txt <- if (format == "latex") {
    begin <- if (environment == "landscape") "\\begin{landscape}" else "\\blandscape"
    end <- if (environment == "landscape") "\\end{landscape}" else "\\elandscape"
    paste0("\n\n", begin, "\n\n", img, "\n\n", end, "\n\n")
  } else {
    paste0("\n\n", img, "\n\n")
  }
  if (requireNamespace("knitr", quietly = TRUE)) knitr::asis_output(txt) else structure(txt, class = "knit_asis")
}
