#' Extract Output Formats From Document YAML Front Matter
#'
#' Inspects the YAML header of a Quarto (\code{.qmd}) or R Markdown (\code{.Rmd})
#' file and returns all declared output formats (e.g. \code{"html"}, \code{"pdf"},
#' \code{"docx"}, \code{"gfm"}).
#'
#' @param file_path Path to the \code{.qmd} or \code{.Rmd} document.
#' @return Character vector of lowercase format names, or empty character vector if none found.
#' @export
extract_yaml_formats <- function(file_path) {
  if (!file.exists(file_path)) return(character())
  fmts <- character()

  yaml_data <- tryCatch({
    if (requireNamespace("rmarkdown", quietly = TRUE)) {
      rmarkdown::yaml_front_matter(file_path)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(yaml_data)) {
    # 1. Quarto 'format'
    if (!is.null(yaml_data$format)) {
      if (is.character(yaml_data$format)) {
        fmts <- c(fmts, yaml_data$format)
      } else if (is.list(yaml_data$format)) {
        fmts <- c(fmts, names(yaml_data$format))
      }
    }
    # 2. R Markdown 'output'
    if (!is.null(yaml_data$output)) {
      raw_outs <- if (is.character(yaml_data$output)) {
        yaml_data$output
      } else if (is.list(yaml_data$output)) {
        names(yaml_data$output)
      } else {
        character()
      }
      for (ro in raw_outs) {
        clean <- sub("^rmarkdown::", "", ro)
        fmt_name <- switch(clean,
          "pdf_document"    = "pdf",
          "word_document"   = "docx",
          "html_document"   = "html",
          "html_vignette"   = "html",
          "github_document" = "gfm",
          "md_document"     = "md",
          "rtf_document"    = "rtf",
          "odt_document"    = "odt",
          sub("_document$", "", clean)
        )
        fmts <- c(fmts, fmt_name)
      }
    }
  }

  unique(tolower(fmts[nzchar(fmts)]))
}

#' Render Quarto and R Markdown Documents to Multiple Formats With Timing
#'
#' Renders one or more Quarto (\code{.qmd}) or R Markdown (\code{.Rmd}) documents
#' to multiple output formats (by default both PDF and DOCX, or formats defined in
#' YAML or compute graphs) sequentially or in parallel, recording execution duration
#' for each document/format combination. Useful for workflows where reviewers need a
#' Word (\code{.docx}) file for track changes/edits while publication and archival
#' copies need PDF.
#'
#' @param path Target document(s) to render. Can be:
#'   \itemize{
#'     \item A character string specifying a file path or directory.
#'     \item A character vector of file paths.
#'     \item A list of file paths (e.g. \code{list("report1.qmd", "report2.Rmd")}).
#'     \item An S4 compute graph object (\code{FileOutputs} or \code{FilePath}).
#'     \item A list of compute graph objects, such as a topological render plan
#'       returned by \code{\link{get_render_plan}} or a pipeline list from \code{\link{pipeline_config}}.
#'     \item A named list or list of lists specifying per-document paths and formats
#'       (e.g. \code{list("report1.qmd" = c("pdf", "docx"), "report2.Rmd" = "html")}).
#'   }
#' @param formats Output format(s) to generate. Defaults to
#'   \code{getOption("pipeline.default_formats", c("pdf", "docx"))}.
#'   Can be:
#'   \itemize{
#'     \item A character vector of formats (e.g. \code{c("pdf", "docx")}, \code{"html"}, \code{"gfm"}).
#'     \item \code{"yaml"} or \code{"auto"} (or \code{NULL}) to read and render the output formats
#'       declared in each document's own YAML frontmatter.
#'     \item Standard shorthand formats (\code{"pdf"}, \code{"docx"}, \code{"html"}, \code{"gfm"})
#'       are automatically translated to R Markdown equivalents (\code{"pdf_document"},
#'       \code{"word_document"}, \code{"html_document"}, \code{"github_document"}) when rendering with \pkg{rmarkdown}.
#'   }
#'   When a document in a compute graph defines its own deliverable output formats
#'   (via \code{\link{create_qmd_renderer}}), those document-level formats override
#'   the default \code{formats}.
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
#' @param ... Additional options passed directly to the rendering backend
#'   (\code{quarto::quarto_render} or \code{rmarkdown::render}), such as
#'   \code{params}, \code{execute_params}, \code{output_dir}, \code{clean}, or \code{quiet}.
#'
#' @return A data frame containing:
#'   \item{file}{Base filename of the rendered document}
#'   \item{output}{Output format (e.g., "pdf", "docx")}
#'   \item{duration}{Execution time in seconds}
#'   \item{status}{"success" or error condition message}
#' @export
#' @aliases render_me
#'
#' @examples
#' \dontrun{
#' # Render a single Quarto report to both PDF and DOCX
#' render(here::here("analysis", "report.qmd"))
#'
#' # Render a single R Markdown report to both PDF and Word
#' render(here::here("analysis", "report.Rmd"))
#'
#' # Render using formats declared in document's own YAML
#' render("analysis/report.qmd", formats = "yaml")
#'
#' # Render a compute graph render plan
#' # plan <- get_render_plan(pipeline)
#' # render(plan)
#'
#' # Render all QMD and Rmd reports in a directory in parallel
#' render("analysis/")
#' }
render <- function(path,
                   formats = getOption("pipeline.default_formats", c("pdf", "docx")),
                   pattern = "\\.(qmd|Rmd|rmd)$",
                   engine = c("auto", "quarto", "rmarkdown"),
                   workers = NULL,
                   ...) {
  engine <- match.arg(engine)
  formats_explicit <- !missing(formats)
  dots <- list(...)

  # 1. Resolve render targets from path argument
  targets <- list()
  doc_exts <- c("pdf", "docx", "html", "gfm", "md", "rtf", "odt", "pptx", "epub")

  add_target <- function(fpath, fmts = NULL) {
    if (is.null(fpath) || !nzchar(fpath)) return()

    resolved_fmts <- fmts
    if (is.null(resolved_fmts)) {
      if (is.null(formats) || identical(formats, "yaml") || identical(formats, "auto")) {
        yaml_f <- extract_yaml_formats(fpath)
        resolved_fmts <- if (length(yaml_f) > 0) yaml_f else c("pdf", "docx")
      } else if ("yaml" %in% formats) {
        yaml_f <- extract_yaml_formats(fpath)
        resolved_fmts <- unique(c(yaml_f, setdiff(formats, "yaml")))
      } else {
        resolved_fmts <- formats
      }
    } else if (identical(resolved_fmts, "yaml") || identical(resolved_fmts, "auto")) {
      yaml_f <- extract_yaml_formats(fpath)
      resolved_fmts <- if (length(yaml_f) > 0) yaml_f else formats
    }

    targets[[length(targets) + 1L]] <<- list(
      path = fpath,
      formats = resolved_fmts
    )
  }

  process_item <- function(item, item_name = NULL) {
    if (is(item, "FileOutputs")) {
      if (.hasSlot(item, "renders") && !isTRUE(item@renders)) return()

      # Extract deliverable report formats from item@output
      report_outs <- Filter(function(o) {
        if (.hasSlot(o, "artifact_role") && length(o@artifact_role) > 0 && o@artifact_role == "deliverable_report") return(TRUE)
        ext <- tolower(tools::file_ext(o@path))
        ext %in% doc_exts
      }, item@output)

      custom_fmts <- if (length(report_outs) > 0) {
        unique(tolower(tools::file_ext(vapply(report_outs, function(o) o@path, character(1)))))
      } else NULL

      effective_fmts <- if (formats_explicit) formats else custom_fmts
      add_target(item@path, effective_fmts)

    } else if (is(item, "FilePath")) {
      if (.hasSlot(item, "renders") && isTRUE(item@renders)) {
        add_target(item@path, if (formats_explicit) formats else NULL)
      }
    } else if (is.list(item) && "path" %in% names(item)) {
      item_f <- if ("formats" %in% names(item)) item$formats else if ("format" %in% names(item)) item$format else NULL
      effective_fmts <- if (!is.null(item_f)) item_f else formats
      add_target(item$path, effective_fmts)

    } else if (is.character(item)) {
      if (!is.null(item_name) && (file.exists(item_name) || grepl(pattern, item_name))) {
        add_target(item_name, item)
      } else {
        for (p in item) {
          if (dir.exists(p)) {
            matched <- list.files(p, pattern = pattern, full.names = TRUE)
            for (m in matched) add_target(normalizePath(m, winslash = "/"), if (formats_explicit) formats else NULL)
          } else if (file.exists(p)) {
            add_target(normalizePath(p, winslash = "/"), if (formats_explicit) formats else NULL)
          } else {
            matched <- list.files(dirname(p), pattern = utils::glob2rx(basename(p)), full.names = TRUE)
            if (length(matched) > 0) {
              for (m in matched) add_target(normalizePath(m, winslash = "/"), if (formats_explicit) formats else NULL)
            } else {
              stop("File or directory not found: ", p, call. = FALSE)
            }
          }
        }
      }
    }
  }

  if (is(path, "FileOutputs") || is(path, "FilePath")) {
    process_item(path)
  } else if (is.list(path)) {
    for (i in seq_along(path)) {
      nm <- if (!is.null(names(path)) && nzchar(names(path)[i])) names(path)[i] else NULL
      process_item(path[[i]], nm)
    }
  } else if (is.character(path)) {
    process_item(path)
  } else {
    stop("Unsupported 'path' argument type: ", class(path)[1], call. = FALSE)
  }

  if (length(targets) == 0) {
    message("No files found to render matching pattern: ", pattern)
    return(invisible(data.frame()))
  }

  # Check engine availability
  has_quarto <- requireNamespace("quarto", quietly = TRUE)
  has_rmarkdown <- requireNamespace("rmarkdown", quietly = TRUE)

  target_paths <- vapply(targets, function(t) t$path, character(1))
  has_qmd <- any(tolower(tools::file_ext(target_paths)) == "qmd")

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
    stop("Either 'quarto' or 'rmarkdown' package is required by render().", call. = FALSE)
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
      "odt"  = "odt_document",
      fmt
    )
  }

  # 2. Worker helper to render all formats for a single target
  render_single <- function(file_path, file_formats) {
    ext <- tolower(tools::file_ext(file_path))
    purrr::map_dfr(file_formats, function(fmt) {
      message("Rendering '", basename(file_path), "' to ", toupper(fmt), "...")
      tic <- Sys.time()

      res <- tryCatch({
        if (ext == "qmd") {
          q_formals <- names(formals(quarto::quarto_render))
          q_args <- list(input = file_path, output_format = fmt)
          q_dots <- dots
          if ("params" %in% names(q_dots) && !"execute_params" %in% names(q_dots)) {
            q_dots$execute_params <- q_dots$params
            q_dots$params <- NULL
          }
          valid_q_dots <- q_dots[names(q_dots) %in% q_formals]
          do.call(quarto::quarto_render, c(q_args, valid_q_dots))
        } else if (engine == "rmarkdown" || (!has_quarto && has_rmarkdown)) {
          rmd_fmt <- map_rmd_format(fmt)
          rmd_formals <- names(formals(rmarkdown::render))
          rmd_args <- list(input = file_path, output_format = rmd_fmt)
          rmd_dots <- dots
          if ("execute_params" %in% names(rmd_dots) && !"params" %in% names(rmd_dots)) {
            rmd_dots$params <- rmd_dots$execute_params
            rmd_dots$execute_params <- NULL
          }
          if (!"quiet" %in% names(rmd_dots)) {
            rmd_dots$quiet <- TRUE
          }
          valid_rmd_dots <- rmd_dots[names(rmd_dots) %in% rmd_formals]
          do.call(rmarkdown::render, c(rmd_args, valid_rmd_dots))
        } else {
          # engine is "auto" or "quarto" on .Rmd
          tryCatch({
            q_formals <- names(formals(quarto::quarto_render))
            q_args <- list(input = file_path, output_format = fmt)
            q_dots <- dots
            if ("params" %in% names(q_dots) && !"execute_params" %in% names(q_dots)) {
              q_dots$execute_params <- q_dots$params
              q_dots$params <- NULL
            }
            valid_q_dots <- q_dots[names(q_dots) %in% q_formals]
            do.call(quarto::quarto_render, c(q_args, valid_q_dots))
          }, error = function(e) {
            if (has_rmarkdown) {
              rmd_fmt <- map_rmd_format(fmt)
              rmd_formals <- names(formals(rmarkdown::render))
              rmd_args <- list(input = file_path, output_format = rmd_fmt)
              rmd_dots <- dots
              if ("execute_params" %in% names(rmd_dots) && !"params" %in% names(rmd_dots)) {
                rmd_dots$params <- rmd_dots$execute_params
                rmd_dots$execute_params <- NULL
              }
              if (!"quiet" %in% names(rmd_dots)) {
                rmd_dots$quiet <- TRUE
              }
              valid_rmd_dots <- rmd_dots[names(rmd_dots) %in% rmd_formals]
              do.call(rmarkdown::render, c(rmd_args, valid_rmd_dots))
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

  # 3. Execution: parallel if multiple targets and future/furrr are available, sequential otherwise
  can_parallelize <- length(targets) > 1 &&
    requireNamespace("future", quietly = TRUE) &&
    requireNamespace("furrr", quietly = TRUE)

  if (length(targets) > 1 && !can_parallelize) {
    message("Packages 'future'/'furrr' not available; rendering sequentially.")
  }

  if (can_parallelize) {
    max_workers <- future::availableCores() - 1
    n_workers <- if (is.null(workers)) min(length(targets), max_workers) else workers
    n_workers <- max(1, n_workers)

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = n_workers)

    times <- furrr::future_map_dfr(
      targets,
      function(tgt) render_single(tgt$path, tgt$formats),
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    times <- purrr::map_dfr(targets, function(tgt) render_single(tgt$path, tgt$formats))
  }

  times
}

#' @rdname render
#' @export
render_me <- render
