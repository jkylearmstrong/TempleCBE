#' Expand and Knit a Child Template Once per Row of a Parameter Grid
#'
#' The loop that recurs in reports with one section per feature: [knitr::knit_expand()] the child
#' template for each row, join the pieces and [knitr::knit_child()] them. Use it in a chunk with
#' `results = "asis"`.
#'
#' Each column of `param_grid` is a template variable: `{{feature}}` in the child is replaced by the
#' value in column `feature` (change the markers with `delim`). The rows are expanded in order.
#'
#' @param template_path Path of the child template.
#' @param param_grid A data frame (or named list of equal-length vectors), one row per child.
#' @param quiet Passed to [knitr::knit_child()] (default `TRUE`).
#' @param sep Text between the expanded children (default a blank line).
#' @param delim Template markers for [knitr::knit_expand()] (default `c("{{", "}}")`).
#' @param expand_only Return the expanded text without knitting it (default `FALSE`).
#' @param envir Environment to knit in (default: the caller's).
#' @return The knitted text, as `knitr::knit_child()` returns it (print it with `cat()`, or return
#'   it from a chunk with `results = "asis"`). With `expand_only = TRUE`, the expanded text.
#' @export
#' @examples
#' tmp <- tempfile(fileext = ".qmd")
#' writeLines("## {{label}}\n\nFeature `{{feature}}`.\n", tmp)
#' grid <- data.frame(feature = c("age", "bmi"), label = c("Age", "BMI"))
#' cat(expand_child_template(tmp, grid, expand_only = TRUE))
expand_child_template <- function(template_path,
                                  param_grid,
                                  quiet = TRUE,
                                  sep = "\n\n",
                                  delim = c("{{", "}}"),
                                  expand_only = FALSE,
                                  envir = parent.frame()) {
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Package \"knitr\" is required.", call. = FALSE)
  if (!is.character(template_path) || length(template_path) != 1L || !file.exists(template_path)) {
    stop("`template_path` must be an existing file.", call. = FALSE)
  }
  if (is.list(param_grid) && !is.data.frame(param_grid)) param_grid <- as.data.frame(param_grid, stringsAsFactors = FALSE)
  if (!is.data.frame(param_grid) || nrow(param_grid) == 0L || ncol(param_grid) == 0L ||
      is.null(names(param_grid)) || any(!nzchar(names(param_grid)))) {
    stop("`param_grid` must be a data frame with named columns and at least one row.", call. = FALSE)
  }
  if (!is.character(delim) || length(delim) != 2L) stop("`delim` must be two strings.", call. = FALSE)

  pieces <- vapply(seq_len(nrow(param_grid)), function(i) {
    row <- lapply(as.list(param_grid[i, , drop = FALSE]), function(x) if (is.factor(x)) as.character(x) else x)
    paste(do.call(knitr::knit_expand, c(list(file = template_path, delim = delim), row)), collapse = "\n")
  }, character(1))
  text <- paste(pieces, collapse = sep)
  if (isTRUE(expand_only)) return(text)
  knitr::knit_child(text = text, quiet = quiet, envir = envir)
}
