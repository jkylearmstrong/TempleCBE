#' Render One Report Template for Several Variants of Its Parameters
#'
#' Renders a single parameterized Quarto (or R Markdown) report once per variant, so
#' that analyses which differ only in a choice (for example the original data and the
#' imputed data, \code{use_imputed = FALSE} / \code{TRUE}) are produced from one
#' canonical template instead of from copies that drift apart. Each variant is a named
#' list of parameters; the output files are named
#' \code{<template>_<variant>.<extension>}.
#'
#' @details
#' \strong{Parameters are checked.} Every parameter a variant sets must be declared
#' under \code{params:} in the template's YAML header: a misspelled parameter is an
#' error before anything is rendered (Quarto would otherwise ignore it and silently
#' render the default). The keys are read from the header as written, so parameters
#' such as \code{n}, \code{y} or \code{on} are not mistaken for YAML booleans. A
#' parameter that no variant sets keeps the template's default. The check is skipped,
#' with a warning, when the header cannot be read; \code{check_params = FALSE} switches it off
#' (needed for a Jupyter notebook template whose parameters are declared in a
#' \code{parameters}-tagged cell).
#'
#' \strong{Your files are not touched.} Each rendering is written to a file with a
#' private name in the template's folder and then renamed or moved to its final
#' name, so a file that already exists next to the template (a hand-made
#' \code{analysis_final.html}, say) is never overwritten, moved or deleted. A file at the
#' final name \emph{in \code{output_dir}} is replaced. The private file is removed
#' if the rendering fails.
#'
#' \strong{HTML.} Quarto's default HTML needs a folder of support files named after the
#' template, shared by every variant and not moved with the output. \code{self_contained = TRUE}
#' (default) therefore asks for a single self-contained HTML file
#' (\code{embed-resources: true}); with \code{FALSE} you must keep the support folder
#' yourself and variants overwrite each other's figures. For an R Markdown template
#' the setting is not applied (\code{html_document} is self-contained by default).
#'
#' @param input Path to the \code{.qmd} or \code{.Rmd} template.
#' @param variants A named list; each element is a named list of parameters for
#'   one rendering, for example
#'   \code{list(original = list(use_imputed = FALSE), imputed = list(use_imputed = TRUE))}.
#'   Names must be unique and consist of letters, digits, \code{_}, \code{.} or
#'   \code{-}.
#' @param formats Output formats to render for each variant (default \code{"html"}),
#'   for example \code{c("html", "docx")}. Letters, digits, \code{_} and \code{-} only.
#' @param output_dir Folder for the outputs. Defaults to the folder of the template.
#' @param stop_on_error If \code{TRUE} (default), the first failed rendering stops
#'   the run with an error of class \code{render_variants_error} that carries the
#'   results so far in \code{$results}; if \code{FALSE}, failures are recorded in the
#'   result and the other variants are still rendered.
#' @param dry_run If \code{TRUE}, nothing is rendered; the plan is returned.
#' @param self_contained For \code{.qmd} HTML output, ask for one self-contained file;
#'   see Details. Default \code{TRUE}.
#' @param check_params If \code{TRUE} (default), check the variants' parameters against
#'   the template's \code{params:}; see Details.
#' @param renderer The function that renders one document, with the arguments of
#'   \code{quarto::quarto_render()} (\code{input}, \code{output_format},
#'   \code{output_file}, \code{execute_params}, \code{quiet}, \code{metadata}).
#'   Defaults to \code{quarto::quarto_render} for a \code{.qmd} and to
#'   \code{rmarkdown::render} (with \code{params}) for an \code{.Rmd}.
#' @param quiet Passed on to the renderer.
#' @return A tibble with one row per variant and format: \code{variant}, \code{format},
#'   \code{params} (a list column), \code{output} (the final file),
#'   \code{status} (\code{"rendered"}, \code{"failed"} or \code{"planned"}) and
#'   \code{error}.
#' @seealso [render_pipeline()], [extract_yaml_formats()]
#' @export
#' @examples
#' \dontrun{
#' render_variants(
#'   "analysis/single_var_mixed_models.qmd",
#'   variants = list(original = list(use_imputed = FALSE), imputed = list(use_imputed = TRUE)),
#'   formats = c("html", "docx"),
#'   output_dir = "reports"
#' )
#' }
render_variants <- function(input,
                            variants,
                            formats = "html",
                            output_dir = NULL,
                            stop_on_error = TRUE,
                            dry_run = FALSE,
                            self_contained = TRUE,
                            check_params = TRUE,
                            renderer = NULL,
                            quiet = TRUE) {
  if (!is.character(input) || length(input) != 1L || is.na(input) || !file.exists(input) || dir.exists(input)) {
    stop("`input` must be the path of an existing .qmd or .Rmd file.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(input))
  if (!ext %in% c("qmd", "rmd")) stop("`input` must be a .qmd or .Rmd file.", call. = FALSE)
  if (!is.list(variants) || length(variants) == 0L || is.null(names(variants)) || any(!nzchar(names(variants)))) {
    stop("`variants` must be a named list with at least one variant.", call. = FALSE)
  }
  if (anyDuplicated(tolower(names(variants)))) {
    stop("The names of `variants` must be unique (ignoring case): ", paste(names(variants), collapse = ", "), ".", call. = FALSE)
  }
  bad_names <- names(variants)[!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", names(variants))]
  if (length(bad_names)) {
    stop("Variant names may contain only letters, digits, `_`, `.` and `-`: ", paste(bad_names, collapse = ", "), ".", call. = FALSE)
  }
  for (nm in names(variants)) {
    v <- variants[[nm]]
    if (!is.list(v) || (length(v) > 0L && (is.null(names(v)) || any(!nzchar(names(v)))))) {
      stop("Variant '", nm, "' must be a named list of parameters.", call. = FALSE)
    }
  }
  if (!is.character(formats) || length(formats) == 0L || anyNA(formats) || any(!nzchar(formats))) {
    stop("`formats` must be a character vector of output formats.", call. = FALSE)
  }
  if (any(!grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", formats))) {
    stop("Format names may contain only letters, digits, `_` and `-`: ",
         paste(formats[!grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", formats)], collapse = ", "), ".", call. = FALSE)
  }
  formats <- unique(formats)
  for (flag in c("stop_on_error", "dry_run", "self_contained", "check_params")) {
    v <- get(flag)
    if (!is.logical(v) || length(v) != 1L || is.na(v)) stop("`", flag, "` must be TRUE or FALSE.", call. = FALSE)
  }
  if (anyNA(.format_extension(formats))) {
    warning("The file extension of format(s) ", paste(formats[is.na(.format_extension(formats))], collapse = ", "),
            " is not known; the format name is used.", call. = FALSE)
  }

  if (isTRUE(check_params)) {
    declared <- .rv_declared_params(input)
    if (is.null(declared)) {
      warning("The YAML header of the template could not be read, so the parameters of the variants were not ",
              "checked against it.", call. = FALSE)
    } else {
      for (nm in names(variants)) {
        unknown <- setdiff(names(variants[[nm]]), declared)
        if (length(unknown)) {
          stop(
            "Variant '", nm, "' sets parameter(s) the template does not declare under `params:`: ",
            paste(unknown, collapse = ", "),
            if (length(declared)) paste0(". Declared: ", paste(declared, collapse = ", "), ".") else ". The template declares none.",
            " (`check_params = FALSE` skips this check.)",
            call. = FALSE
          )
        }
      }
    }
  }

  input <- normalizePath(input, winslash = "/", mustWork = TRUE)
  stem <- tools::file_path_sans_ext(basename(input))
  in_dir <- dirname(input)
  if (is.null(output_dir)) output_dir <- in_dir
  if (file.exists(output_dir) && !dir.exists(output_dir)) stop("`output_dir` is a file, not a folder.", call. = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)

  plan <- do.call(rbind, lapply(names(variants), function(nm) {
    data.frame(variant = nm, format = formats, stringsAsFactors = FALSE)
  }))
  ext_of <- ifelse(is.na(.format_extension(plan$format)), tolower(plan$format), .format_extension(plan$format))
  plan$file <- paste0(stem, "_", plan$variant, ".", ext_of)
  out <- tibble::tibble(
    variant = plan$variant,
    format = plan$format,
    params = unname(variants[plan$variant]),
    output = file.path(output_dir, plan$file),
    status = "planned",
    error = NA_character_
  )
  if (anyDuplicated(tolower(out$output))) {
    stop("Two renderings would write the same file: ", paste(unique(out$output[duplicated(tolower(out$output))]), collapse = ", "),
         ". Use a distinct variant name or format for each.", call. = FALSE)
  }
  if (isTRUE(dry_run)) return(out)

  if (is.null(renderer)) renderer <- .rv_default_renderer(ext)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  run_id <- paste0("rv", format(Sys.time(), "%H%M%S"), "_", paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = ""))
  same_dir <- identical(tolower(output_dir), tolower(in_dir))
  for (i in seq_len(nrow(out))) {
    work_name <- paste0(run_id, "_", plan$file[i]) # private name in the template's folder
    work_path <- file.path(in_dir, work_name)
    if (file.exists(work_path)) stop("The private file ", work_name, " already exists; remove it and run again.", call. = FALSE)
    metadata <- if (identical(ext, "qmd") && isTRUE(self_contained) && tolower(out$format[i]) %in% c("html", "revealjs")) {
      list(`embed-resources` = TRUE)
    } else {
      NULL
    }
    res <- tryCatch({
      renderer(
        input = input, output_format = out$format[i], output_file = work_name,
        execute_params = out$params[[i]], quiet = quiet, metadata = metadata
      )
      if (!file.exists(work_path)) stop("the renderer did not write ", work_name, call. = FALSE)
      if (file.exists(out$output[i]) && !file.remove(out$output[i])) stop("could not replace ", out$output[i], call. = FALSE)
      moved <- file.rename(work_path, out$output[i])
      if (!moved) {
        if (!file.copy(work_path, out$output[i], overwrite = TRUE)) stop("could not move the output to ", output_dir, call. = FALSE)
      }
      NULL
    }, error = function(e) conditionMessage(e))
    unlink(work_path) # whatever is left of the private file, rendered or not
    if (is.null(res)) {
      out$status[i] <- "rendered"
    } else {
      out$status[i] <- "failed"
      out$error[i] <- res
      if (isTRUE(stop_on_error)) {
        stop(structure(
          class = c("render_variants_error", "error", "condition"),
          list(
            message = paste0("Rendering variant '", out$variant[i], "' (", out$format[i], ") failed: ", res),
            call = NULL, results = out
          )
        ))
      }
    }
  }
  if (any(out$status == "failed")) {
    warning(sum(out$status == "failed"), " rendering(s) failed; see the `error` column.", call. = FALSE)
  }
  out
}

# Names of the parameters declared under `params:` in the YAML header, read as written (so a
# key such as `n`, `y` or `on` stays a name). NULL when there is no readable header or the
# `params:` block is written as an inline map; character(0) when the header declares none.
.rv_declared_params <- function(input) {
  lines <- tryCatch(readLines(input, warn = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(lines) || length(lines) < 2L || !grepl("^---\\s*$", lines[1])) return(NULL)
  end <- which(grepl("^(---|\\.\\.\\.)\\s*$", lines[-1]))[1] + 1L
  if (is.na(end)) return(NULL)
  header <- lines[2:(end - 1L)]
  start <- grep("^params\\s*:", header)
  if (length(start) == 0L) return(character(0))
  if (!grepl("^params\\s*:\\s*(#.*)?$", header[start[1]])) return(NULL) # `params: {a: 1}`
  body <- header[-seq_len(start[1])]
  keep <- logical(length(body))
  for (i in seq_along(body)) {
    if (grepl("^\\S", body[i])) break # next top-level key
    keep[i] <- TRUE
  }
  body <- body[keep]
  body <- body[nzchar(trimws(body)) & !grepl("^\\s*#", body)]
  if (length(body) == 0L) return(character(0))
  indent <- nchar(sub("^(\\s*).*$", "\\1", body))
  top <- body[indent == min(indent)]
  keys <- sub("^\\s*[\"']?([^\"':]+)[\"']?\\s*:.*$", "\\1", top[grepl("^\\s*[\"']?[^\"':]+[\"']?\\s*:", top)])
  unique(trimws(keys))
}

.rv_default_renderer <- function(ext) {
  if (identical(ext, "qmd")) {
    if (!requireNamespace("quarto", quietly = TRUE)) stop("Package 'quarto' is required to render a .qmd.", call. = FALSE)
    return(function(input, output_format, output_file, execute_params, quiet, metadata = NULL) {
      args <- list(
        input = input, output_format = output_format, output_file = output_file,
        execute_params = execute_params, quiet = quiet
      )
      if (!is.null(metadata)) args$metadata <- metadata
      do.call(quarto::quarto_render, args)
    })
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("Package 'rmarkdown' is required to render an .Rmd.", call. = FALSE)
  function(input, output_format, output_file, execute_params, quiet, metadata = NULL) {
    fmt <- switch(tolower(output_format), html = "html_document", pdf = "pdf_document", docx = "word_document", output_format)
    rmarkdown::render(
      input = input, output_format = fmt, output_file = output_file,
      params = execute_params, quiet = quiet, envir = new.env(parent = globalenv())
    )
  }
}
