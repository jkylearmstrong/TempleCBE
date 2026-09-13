#' Render Quarto Documents to Multiple Formats With Timing
#'
#' Renders one or more Quarto (\code{.qmd}) documents to multiple output
#' formats (by default both PDF and DOCX) sequentially or in parallel,
#' recording execution duration for each document/format combination. Useful
#' for workflows where reviewers need a Word (\code{.docx}) file for track
#' changes/edits while publication and archival copies need PDF.
#'
#' @param path Character string specifying a path to a \code{.qmd} file, or a
#'   directory containing \code{.qmd} files.
#' @param formats Character vector of output formats to generate. Defaults to
#'   \code{c("pdf", "docx")}.
#' @param pattern Optional regular expression to filter files when
#'   \code{path} is a directory. Defaults to \code{"\\.qmd$"}.
#' @param workers Integer specifying the number of multisession parallel
#'   workers when rendering multiple documents. Defaults to \code{NULL},
#'   which caps at available cores minus one. Ignored (rendering falls back
#'   to sequential) when the \pkg{future}/\pkg{furrr} packages are not
#'   installed.
#'
#' @return A data frame containing:
#'   \item{file}{Base filename of the rendered document}
#'   \item{output}{Output format (e.g., "pdf", "docx")}
#'   \item{duration}{Execution time in seconds}
#'   \item{status}{"success" or error condition message}
#' @export
#'
#' @examples
#' \dontrun{
#' # Render a single report to both PDF and DOCX
#' render_me(here::here("analysis", "report.qmd"))
#'
#' # Render all QMD reports in a directory in parallel
#' render_me("analysis/")
#' }
render_me <- function(path,
                       formats = c("pdf", "docx"),
                       pattern = "\\.qmd$",
                       workers = NULL) {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("Package 'quarto' is required by render_me(). Install it with install.packages(\"quarto\").", call. = FALSE)
  }

  # 1. Resolve files
  if (dir.exists(path)) {
    files <- list.files(path, pattern = pattern, full.names = TRUE)
  } else if (file.exists(path)) {
    files <- normalizePath(path, winslash = "/")
  } else {
    matched <- list.files(dirname(path), pattern = utils::glob2rx(basename(path)), full.names = TRUE)
    if (length(matched) == 0) {
      stop("File or directory not found: ", path, call. = FALSE)
    }
    files <- matched
  }

  if (length(files) == 0) {
    message("No files found to render matching pattern: ", pattern)
    return(invisible(data.frame()))
  }

  names(files) <- basename(files)

  # 2. Worker helper to render all formats for a single file
  render_single <- function(file_path) {
    purrr::map_dfr(formats, function(fmt) {
      message("Rendering '", basename(file_path), "' to ", toupper(fmt), "...")
      tic <- Sys.time()
      res <- tryCatch({
        quarto::quarto_render(file_path, output_format = fmt)
        "success"
      }, error = function(e) {
        warning("Error rendering '", basename(file_path), "' to ", fmt, ": ", conditionMessage(e), call. = FALSE)
        conditionMessage(e)
      })
      dur <- as.numeric(Sys.time() - tic, units = "secs")

      data.frame(
        file     = basename(file_path),
        output   = fmt,
        duration = round(dur, 2),
        status   = res,
        stringsAsFactors = FALSE
      )
    })
  }

  # 3. Execution: parallel if multiple files and future/furrr are available, sequential otherwise
  can_parallelize <- length(files) > 1 &&
    requireNamespace("future", quietly = TRUE) &&
    requireNamespace("furrr", quietly = TRUE)

  if (length(files) > 1 && !can_parallelize) {
    message("Packages 'future'/'furrr' not available; rendering sequentially.")
  }

  if (can_parallelize) {
    max_workers <- future::availableCores() - 1
    n_workers <- if (is.null(workers)) min(length(files), max_workers) else workers
    n_workers <- max(1, n_workers)

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = n_workers)

    times <- furrr::future_map_dfr(
      files,
      render_single,
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    times <- purrr::map_dfr(files, render_single)
  }

  times
}
