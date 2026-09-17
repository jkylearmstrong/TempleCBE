#' Render Quarto and R Markdown Documents to Multiple Formats With Timing
#'
#' Renders one or more Quarto (\code{.qmd}) or R Markdown (\code{.Rmd}) documents
#' to multiple output formats (by default both PDF and DOCX) sequentially or in
#' parallel, recording execution duration for each document/format combination. Useful
#' for workflows where reviewers need a Word (\code{.docx}) file for track
#' changes/edits while publication and archival copies need PDF.
#'
#' @param path Character string specifying a path to a \code{.qmd} or \code{.Rmd} file,
#'   or a directory containing such documents.
#' @param formats Character vector of output formats to generate. Defaults to
#'   \code{c("pdf", "docx")}. Standard shorthand formats (\code{"pdf"}, \code{"docx"},
#'   \code{"html"}, \code{"gfm"}) are automatically translated to R Markdown equivalents
#'   (\code{"pdf_document"}, \code{"word_document"}, \code{"html_document"}, \code{"github_document"})
#'   when rendering with \pkg{rmarkdown}.
#' @param pattern Optional regular expression to filter files when \code{path} is a directory.
#'   Defaults to \code{"\\.(qmd|Rmd|rmd)$"}.
#' @param engine Rendering engine: \code{"auto"} (default), \code{"quarto"}, or \code{"rmarkdown"}.
#'   When \code{"auto"}, \code{.qmd} files are rendered via \pkg{quarto}, and \code{.Rmd} files
#'   are rendered via \pkg{quarto} if available or \pkg{rmarkdown} otherwise.
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
#' # Render a single Quarto report to both PDF and DOCX
#' render_me(here::here("analysis", "report.qmd"))
#'
#' # Render a single R Markdown report to both PDF and Word
#' render_me(here::here("analysis", "report.Rmd"))
#'
#' # Render all QMD and Rmd reports in a directory in parallel
#' render_me("analysis/")
#' }
render_me <- function(path,
                      formats = c("pdf", "docx"),
                      pattern = "\\.(qmd|Rmd|rmd)$",
                      engine = c("auto", "quarto", "rmarkdown"),
                      workers = NULL) {
  engine <- match.arg(engine)

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

  # Check available engines
  has_quarto <- requireNamespace("quarto", quietly = TRUE)
  has_rmarkdown <- requireNamespace("rmarkdown", quietly = TRUE)

  has_qmd <- any(tolower(tools::file_ext(files)) == "qmd")

  if (has_qmd && !has_quarto) {
    stop("Package 'quarto' is required to render .qmd files. Install it with install.packages(\"quarto\").",
         call. = FALSE)
  }

  if (engine == "quarto" && !has_quarto) {
    stop("Package 'quarto' is required when engine = 'quarto'. Install it with install.packages(\"quarto\").",
         call. = FALSE)
  }

  if (engine == "rmarkdown" && !has_rmarkdown) {
    stop("Package 'rmarkdown' is required when engine = 'rmarkdown'. Install it with install.packages(\"rmarkdown\").",
         call. = FALSE)
  }

  if (!has_quarto && !has_rmarkdown) {
    stop("Either 'quarto' or 'rmarkdown' package is required by render_me().", call. = FALSE)
  }

  # Helper to map format shorthand for rmarkdown
  map_rmd_format <- function(fmt) {
    switch(tolower(fmt),
      "pdf"  = "pdf_document",
      "docx" = "word_document",
      "word" = "word_document",
      "html" = "html_document",
      "gfm"  = "github_document",
      "md"   = "md_document",
      "rtf"  = "rtf_document",
      fmt
    )
  }

  # 2. Worker helper to render all formats for a single file
  render_single <- function(file_path) {
    ext <- tolower(tools::file_ext(file_path))
    purrr::map_dfr(formats, function(fmt) {
      message("Rendering '", basename(file_path), "' to ", toupper(fmt), "...")
      tic <- Sys.time()

      res <- tryCatch({
        if (ext == "qmd") {
          quarto::quarto_render(file_path, output_format = fmt)
        } else if (engine == "rmarkdown" || (!has_quarto && has_rmarkdown)) {
          rmd_fmt <- map_rmd_format(fmt)
          rmarkdown::render(file_path, output_format = rmd_fmt, quiet = TRUE)
        } else {
          # engine is "auto" or "quarto" on .Rmd
          tryCatch({
            quarto::quarto_render(file_path, output_format = fmt)
          }, error = function(e) {
            if (has_rmarkdown) {
              rmd_fmt <- map_rmd_format(fmt)
              rmarkdown::render(file_path, output_format = rmd_fmt, quiet = TRUE)
            } else {
              stop(e)
            }
          })
        }
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
