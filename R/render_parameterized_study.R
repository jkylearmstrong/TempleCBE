#' Render a Whole Folder of Reports Once per Parameter Set
#'
#' [render_parameterized_pair()] for a folder: every `.qmd`/`.Rmd` file in `source_dir` is rendered
#' once per entry of `param_list`, and the outputs of entry `i` go to `target_dirs[i]`. This replaces
#' keeping a mirrored copy of the folder (for example `compute_index/` and `compute_index_impute/`)
#' that differs only in a few parameters. The templates themselves are never copied or changed.
#'
#' Files whose name starts with `_` or `.` are skipped (Quarto includes and children). A document
#' that does not declare a parameter in its YAML header fails (see [render_variants()]); with
#' `on_error = "continue"` the others are still rendered.
#'
#' @param source_dir Folder with the templates.
#' @param target_dirs Output folders, one per parameter set (created if missing). Unnamed or named.
#' @param param_list A list of named parameter lists, the same length as `target_dirs`.
#' @param pattern Regular expression for the files to render (default `"\\.(qmd|Rmd)$"`, ignoring case).
#' @param recursive Also look in sub-folders (default `FALSE`).
#' @param formats Output formats (default `"html"`).
#' @param on_error `"continue"` (default; failures are reported in the result) or `"stop"`.
#' @param dry_run Plan only; nothing is rendered.
#' @param ... Further arguments for [render_variants()] (`self_contained`, `check_params`, `renderer`, `quiet`).
#' @return A tibble, one row per document, parameter set and format, with the columns of
#'   [render_variants()] plus `document` and `target_dir`.
#' @seealso [render_parameterized_pair()], [render_variants()]
#' @export
#' @examples
#' \dontrun{
#' render_parameterized_study(
#'   "analysis/compute_index",
#'   target_dirs = c("reports/compute_index", "reports/compute_index_impute"),
#'   param_list  = list(list(use_imputed = FALSE), list(use_imputed = TRUE))
#' )
#' }
render_parameterized_study <- function(source_dir,
                                       target_dirs,
                                       param_list,
                                       pattern = "\\.(qmd|Rmd)$",
                                       recursive = FALSE,
                                       formats = "html",
                                       on_error = c("continue", "stop"),
                                       dry_run = FALSE,
                                       ...) {
  on_error <- match.arg(on_error)
  if (!is.character(source_dir) || length(source_dir) != 1L || !dir.exists(source_dir)) {
    stop("`source_dir` must be an existing folder.", call. = FALSE)
  }
  if (!is.character(target_dirs) || length(target_dirs) < 1L || anyNA(target_dirs) || !all(nzchar(target_dirs))) {
    stop("`target_dirs` must be a character vector of folders.", call. = FALSE)
  }
  if (!is.list(param_list) || length(param_list) != length(target_dirs)) {
    stop("`param_list` must be a list with one parameter set per entry of `target_dirs`.", call. = FALSE)
  }
  norm <- normalizePath(target_dirs, winslash = "/", mustWork = FALSE)
  if (anyDuplicated(norm)) stop("`target_dirs` must be different folders.", call. = FALSE)
  for (i in seq_along(param_list)) {
    p <- param_list[[i]]
    if (!is.list(p) || (length(p) > 0L && (is.null(names(p)) || any(!nzchar(names(p)))))) {
      stop("Entry ", i, " of `param_list` must be a named list of parameters.", call. = FALSE)
    }
  }

  files <- list.files(source_dir, pattern = pattern, recursive = recursive, full.names = TRUE, ignore.case = TRUE)
  files <- files[!grepl("^[_.]", basename(files))]
  if (length(files) == 0L) stop("No document matching `pattern` in `source_dir`.", call. = FALSE)
  src_norm <- normalizePath(source_dir, winslash = "/", mustWork = TRUE)
  if (any(norm == src_norm) && !dry_run) {
    stop("A target folder is the source folder; outputs would mix with the templates.", call. = FALSE)
  }

  rows <- list()
  for (f in sort(files)) {
    for (i in seq_along(target_dirs)) {
      if (!dry_run) dir.create(target_dirs[i], recursive = TRUE, showWarnings = FALSE)
      res <- tryCatch(
        render_variants(
          f, variants = stats::setNames(list(param_list[[i]]), "study"), formats = formats,
          output_dir = target_dirs[i], stop_on_error = TRUE, dry_run = dry_run, ...
        ),
        error = function(e) {
          if (on_error == "stop") stop(e)
          tibble::tibble(variant = "study", format = formats, params = list(param_list[[i]]),
                         output = NA_character_, status = "failed", error = conditionMessage(e))
        }
      )
      res$document <- f
      res$target_dir <- target_dirs[i]
      rows[[length(rows) + 1L]] <- res
    }
  }
  out <- dplyr::bind_rows(rows)
  out[c("document", "target_dir", setdiff(names(out), c("document", "target_dir")))]
}
