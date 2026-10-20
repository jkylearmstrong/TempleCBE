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
#' Every parameter a variant sets must be declared under \code{params:} in the
#' template's YAML header: a misspelled parameter is an error before anything is
#' rendered (Quarto would otherwise ignore it and silently render the default).
#' A parameter that no variant sets keeps the template's default.
#'
#' The template is rendered in its own folder, as Quarto does, and the outputs are
#' then moved to \code{output_dir}. A file of that name that already exists there is
#' replaced.
#'
#' @param input Path to the \code{.qmd} or \code{.Rmd} template.
#' @param variants A named list; each element is a named list of parameters for
#'   one rendering, for example
#'   \code{list(original = list(use_imputed = FALSE), imputed = list(use_imputed = TRUE))}.
#'   Names must be unique and consist of letters, digits, \code{_}, \code{.} or
#'   \code{-}.
#' @param formats Output formats to render for each variant (default \code{"html"}),
#'   for example \code{c("html", "docx")}.
#' @param output_dir Folder for the outputs. Defaults to the folder of the template.
#' @param stop_on_error If \code{TRUE} (default), the first failed rendering stops
#'   the run; if \code{FALSE}, failures are recorded in the result and the other
#'   variants are still rendered.
#' @param dry_run If \code{TRUE}, nothing is rendered; the plan is returned.
#' @param renderer The function that renders one document, with the arguments of
#'   \code{quarto::quarto_render()} (\code{input}, \code{output_format},
#'   \code{output_file}, \code{execute_params}, \code{quiet}). Defaults to
#'   \code{quarto::quarto_render} for a \code{.qmd} and to
#'   \code{rmarkdown::render} (with \code{params}) for an \code{.Rmd}.
#' @param quiet Passed on to the renderer.
#' @return A tibble with one row per variant and format: \code{variant}, \code{format},
#'   \code{params} (a list column), \code{output} (the file the rendering writes),
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
  formats <- unique(formats)
  if (anyNA(.format_extension(formats))) {
    warning("The file extension of format(s) ", paste(formats[is.na(.format_extension(formats))], collapse = ", "),
            " is not known; the format name is used.", call. = FALSE)
  }
  if (!is.logical(stop_on_error) || length(stop_on_error) != 1L || is.na(stop_on_error)) {
    stop("`stop_on_error` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(dry_run) || length(dry_run) != 1L || is.na(dry_run)) stop("`dry_run` must be TRUE or FALSE.", call. = FALSE)

  declared <- .rv_declared_params(input)
  if (!is.null(declared)) {
    for (nm in names(variants)) {
      unknown <- setdiff(names(variants[[nm]]), declared)
      if (length(unknown)) {
        stop(
          "Variant '", nm, "' sets parameter(s) the template does not declare under `params:`: ",
          paste(unknown, collapse = ", "),
          if (length(declared)) paste0(". Declared: ", paste(declared, collapse = ", "), ".") else ". The template declares none.",
          call. = FALSE
        )
      }
    }
  }

  input <- normalizePath(input, winslash = "/", mustWork = TRUE)
  stem <- tools::file_path_sans_ext(basename(input))
  if (is.null(output_dir)) output_dir <- dirname(input)
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
  if (anyDuplicated(out$output)) {
    stop("Two renderings would write the same file: ", paste(unique(out$output[duplicated(out$output)]), collapse = ", "),
         ". Use a distinct variant name or format for each.", call. = FALSE)
  }
  if (isTRUE(dry_run)) return(out)

  if (is.null(renderer)) renderer <- .rv_default_renderer(ext)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  in_dir <- dirname(input)
  for (i in seq_len(nrow(out))) {
    tmp_name <- plan$file[i]
    res <- tryCatch({
      renderer(
        input = input, output_format = out$format[i], output_file = tmp_name,
        execute_params = out$params[[i]], quiet = quiet
      )
      produced <- file.path(in_dir, tmp_name)
      if (!file.exists(produced)) stop("the renderer did not write ", tmp_name, call. = FALSE)
      if (!identical(normalizePath(dirname(produced), winslash = "/"), output_dir)) {
        if (!file.copy(produced, out$output[i], overwrite = TRUE)) stop("could not copy the output to ", output_dir, call. = FALSE)
        unlink(produced)
      }
      NULL
    }, error = function(e) conditionMessage(e))
    if (is.null(res)) {
      out$status[i] <- "rendered"
    } else {
      out$status[i] <- "failed"
      out$error[i] <- res
      if (isTRUE(stop_on_error)) {
        stop("Rendering variant '", out$variant[i], "' (", out$format[i], ") failed: ", res, call. = FALSE)
      }
    }
  }
  if (any(out$status == "failed")) {
    warning(sum(out$status == "failed"), " rendering(s) failed; see the `error` column.", call. = FALSE)
  }
  out
}

# Names of the parameters the template declares under `params:`; NULL when they cannot be read
.rv_declared_params <- function(input) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) return(NULL)
  yml <- tryCatch(rmarkdown::yaml_front_matter(input), error = function(e) NULL)
  if (is.null(yml)) return(NULL)
  names(yml$params) %||% character(0)
}

.rv_default_renderer <- function(ext) {
  if (identical(ext, "qmd")) {
    if (!requireNamespace("quarto", quietly = TRUE)) stop("Package 'quarto' is required to render a .qmd.", call. = FALSE)
    return(function(input, output_format, output_file, execute_params, quiet) {
      quarto::quarto_render(
        input = input, output_format = output_format, output_file = output_file,
        execute_params = execute_params, quiet = quiet
      )
    })
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("Package 'rmarkdown' is required to render an .Rmd.", call. = FALSE)
  function(input, output_format, output_file, execute_params, quiet) {
    fmt <- switch(tolower(output_format), html = "html_document", pdf = "pdf_document", docx = "word_document", output_format)
    rmarkdown::render(
      input = input, output_format = fmt, output_file = output_file,
      params = execute_params, quiet = quiet, envir = new.env(parent = globalenv())
    )
  }
}
