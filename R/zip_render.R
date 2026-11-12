#' Render a Quarto Document and Zip It With Its Dependencies
#'
#' Renders \code{input} in an isolated build directory, copies in any
#' resources it references (explicitly, or heuristically detected from
#' quoted paths / \code{here::here()} calls), and zips the outputs together
#' with the source and sidecar files.
#'
#' @param input Path to the input \code{.qmd} file.
#' @param formats Character vector of output formats (e.g. \code{c("html","pdf","docx")} or \code{"all"}).
#'   \code{"all"} renders the formats declared in the document's YAML front matter
#'   (or \code{"html"} when it declares none), as \code{quarto render --to all} does;
#'   it is resolved to those formats before rendering, so the rendered outputs
#'   are matched by exactly the extensions those formats write.
#' @param resources Optional character vector of extra files to include, absolute or project-relative.
#' @param detect \code{"heuristic"} (default; scans the \code{.qmd} for likely file paths) or \code{"none"}.
#' @details
#' The rendered outputs are the files in the build directory named
#' \verb{<input-stem>.<extension>} for the extensions of \code{formats} (the
#' case of the name is ignored). If there are none, for instance because the
#' format writes its output to another folder, a warning says so and the zip
#' holds the sources only.
#'
#' Heuristic detection inspects quoted file paths and here::here() calls in the document. For YAML front-matter resources, pass them explicitly via the `resources` argument or include them in the document's YAML; this function will attempt to parse YAML when present to pick up top-level resource lists.
#' @param build_dir Staging directory; defaults to a fresh temp directory. A
#'   relative path is relative to the working directory the function is called from.
#' @param zip_name Name of the resulting zip; defaults to \verb{<input-stem>.zip}.
#' @param copy_back_dir Where to copy the finished zip; defaults to \code{dirname(input)}.
#'   A relative path is relative to the working directory the function is called from.
#' @param include_sources Logical (default \code{TRUE}); include the \code{.qmd} and sidecar bib/tex/css files.
#' @param overwrite Logical (default \code{TRUE}); overwrite an existing zip at the destination.
#' @param verbose Logical (default \code{TRUE}); print progress messages.
#' @return Invisibly, a list with the build directory, detected/copied resources, render outputs, and final zip path.
#' @export
#' @examples
#' \dontrun{
#' zip_render("report.qmd", formats = c("html", "pdf"))
#' }
zip_render <- function(input, formats = c("html", "pdf", "docx"), resources = NULL,
                        detect = c("heuristic", "none"), build_dir = NULL, zip_name = NULL,
                        copy_back_dir = NULL, include_sources = TRUE, overwrite = TRUE, verbose = TRUE) {
  detect <- match.arg(detect)

  input <- normalizePath(input, winslash = "/", mustWork = TRUE)
  input_dir <- dirname(input)
  input_stem <- sub("\\.qmd$", "", basename(input), ignore.case = TRUE)

  formats <- .resolve_all_formats(formats, input)

  if (is.null(zip_name)) zip_name <- paste0(input_stem, ".zip")
  # a zip name is a file name, not a path: a separator would write the zip
  # outside copy_back_dir (or fail on Windows, where it is not a valid name)
  if (grepl("[/\\\\]", zip_name)) {
    stop("`zip_name` must be a file name, not a path: '", zip_name, "'.", call. = FALSE)
  }
  if (is.null(copy_back_dir)) copy_back_dir <- input_dir
  if (is.null(build_dir)) build_dir <- tempfile(paste0("qbuild-", input_stem, "-"))
  # The working directory moves to build_dir below, so a relative build_dir or
  # copy_back_dir has to be pinned to the caller's folder first: afterwards it
  # would be read relative to the build folder (the zip landing in a folder
  # that is thrown away, or the zip step failing). These folders may not exist yet.
  build_dir <- .canonical_path(build_dir)
  copy_back_dir <- .canonical_path(copy_back_dir)
  if (!dir.exists(build_dir)) dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)

  vcat <- function(...) if (isTRUE(verbose)) cat(..., "\n")

  copy_preserve <- function(src, root = NULL) {
    if (!file.exists(src)) return(character(0))
    src_norm <- normalizePath(src, winslash = "/", mustWork = TRUE)
    rel <- NA_character_
    if (!is.null(root) && dir.exists(root)) {
      root_norm <- normalizePath(root, winslash = "/", mustWork = TRUE)
      # the relative path is found by a prefix match, which is case-sensitive
      # as written: on a case-insensitive file system (Windows, macOS) a root
      # spelled with different case would miss and the file would land flat
      under_root <- if (.case_insensitive_fs()) {
        startsWith(tolower(src_norm), paste0(tolower(root_norm), "/"))
      } else {
        startsWith(src_norm, paste0(root_norm, "/"))
      }
      if (under_root) {
        rel <- substr(src_norm, nchar(root_norm) + 2L, nchar(src_norm))
      }
    }
    dest <- if (is.na(rel)) file.path(build_dir, basename(src_norm)) else file.path(build_dir, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (file.copy(src_norm, dest, overwrite = TRUE)) {
      dest
    } else {
      warning("Could not copy '", src, "' into the build directory; it is left out of the zip.", call. = FALSE)
      character(0)
    }
  }

  # Split a here::here(...) argument list on commas that are not inside
  # quotes: strsplit(h, ",") splits "data/a,b.csv" into two paths
  split_here_args <- function(h) {
    chars <- strsplit(h, "", fixed = TRUE)[[1]]
    parts <- character()
    cur <- ""
    quote <- ""
    for (ch in chars) {
      if (nzchar(quote)) {
        cur <- paste0(cur, ch)
        if (identical(ch, quote)) quote <- ""
      } else if (ch %in% c("'", '"')) {
        quote <- ch
        cur <- paste0(cur, ch)
      } else if (ch == ",") {
        parts <- c(parts, cur)
        cur <- ""
      } else {
        cur <- paste0(cur, ch)
      }
    }
    c(parts, cur)
  }

  detected <- character(0)
  if (detect == "heuristic") {
    qmd_lines <- readLines(input, warn = FALSE, encoding = "UTF-8")

    # Parse YAML front-matter if present (--- at top). A UTF-8 byte-order mark
    # on the first line, or trailing spaces after a fence, would hide it from
    # an exact "---" match.
    if (length(qmd_lines) > 0) qmd_lines[1] <- sub("^\uFEFF", "", qmd_lines[1])
    if (length(qmd_lines) > 0 && grepl("^---\\s*$", qmd_lines[1])) {
      fence <- which(grepl("^---\\s*$", qmd_lines))
      if (length(fence) >= 2) {
        yaml_block <- paste(qmd_lines[2:(fence[2] - 1)], collapse = "\n")
        if (requireNamespace("yaml", quietly = TRUE)) {
          y <- tryCatch(yaml::read_yaml(text = yaml_block), error = function(e) NULL)
          if (!is.null(y) && !is.null(y$resources)) {
            detected <- c(detected, unlist(y$resources))
          }
        }
      }
    }

    patt_files <- "\"([^\"]+\\.(rds|csv|tsv|xlsx|xls|png|jpg|jpeg|svg|gif|bib|tex|css|csl))\""
    cand1 <- unique(gsub("^\"|\"$", "", unlist(regmatches(qmd_lines, gregexpr(patt_files, qmd_lines, perl = TRUE))), perl = TRUE))

    patt_here <- "here::here\\(([^\\)]+)\\)"
    here_calls <- unique(unlist(regmatches(qmd_lines, gregexpr(patt_here, qmd_lines, perl = TRUE))))
    cand2 <- character(0)
    if (length(here_calls)) {
      inner <- sub("^here::here\\((.*)\\)$", "\\1", here_calls)
      for (h in inner) {
        parts <- trimws(gsub("^['\"]|['\"]$", "", split_here_args(h)))
        parts <- parts[nzchar(parts)]
        if (length(parts)) cand2 <- c(cand2, do.call(file.path, as.list(parts)))
      }
    }

    cands <- unique(c(detected, cand1, cand2))
    cands <- cands[nzchar(cands)]
    detected <- unique(cands[file.exists(file.path(input_dir, cands)) | file.exists(cands)])
    if (length(detected)) vcat("Heuristic detected resources:\n -", paste(detected, collapse = "\n - "))
  }

  resources_all <- unique(stats::na.omit(c(resources, detected)))

  copied_main <- file.path(build_dir, basename(input))
  dir.create(dirname(copied_main), recursive = TRUE, showWarnings = FALSE)
  # without the source in the build directory there is nothing to render
  if (!isTRUE(file.copy(input, copied_main, overwrite = TRUE))) {
    stop("Could not copy '", input, "' into the build directory '", build_dir, "'.", call. = FALSE)
  }

  sidecars <- character(0)
  if (include_sources) {
    likely_sidecars <- c("bib.bib", "grateful-refs.bib", "title.tex", "styles.css")
    present <- file.path(input_dir, likely_sidecars)
    sidecars <- present[file.exists(present)]
  }

  # Project metadata, brand, and extensions (e.g. `format: temple-pdf`) must sit
  # beside the .qmd for it to render as it does in place, so they are copied
  # relative to input_dir whether or not sources are zipped.
  project_files <- file.path(input_dir, c("_quarto.yml", "_quarto.yaml", "_brand.yml", "_brand.yaml", "_variables.yml"))
  project_files <- project_files[file.exists(project_files)]
  if (dir.exists(file.path(input_dir, "_extensions"))) {
    project_files <- c(project_files, list.files(file.path(input_dir, "_extensions"), recursive = TRUE,
                                                 full.names = TRUE, all.files = TRUE))
  }
  project_rel <- substring(project_files, nchar(input_dir) + 2L)
  for (i in seq_along(project_files)) {
    dest <- file.path(build_dir, project_rel[i])
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(file.copy(project_files[i], dest, overwrite = TRUE))) {
      warning("Could not copy '", project_files[i], "' into the build directory; the document may not render as it does in place.", call. = FALSE)
    }
  }

  project_root <- tryCatch(here::here(), error = function(e) NULL)
  if (is.null(project_root)) {
    rp <- list.files(path = input_dir, pattern = "\\.Rproj$", full.names = TRUE)
    if (length(rp)) project_root <- dirname(rp[[1]])
  }

  copied_resources <- character(0)
  for (r in unique(c(resources_all, sidecars))) {
    src <- r
    if (!file.exists(src)) {
      cand <- file.path(input_dir, r)
      if (file.exists(cand)) src <- cand
    }
    if (!file.exists(src)) next
    copied_resources <- c(copied_resources, copy_preserve(src, root = project_root))
  }
  copied_resources <- unique(copied_resources)

  vcat("Rendering in build dir:", build_dir)
  if (!requireNamespace("zip", quietly = TRUE)) vcat("Package 'zip' not available; will try base utils::zip().")
  if (!requireNamespace("withr", quietly = TRUE)) stop("Package 'withr' is required for safe directory scoping.")
  if (!requireNamespace("quarto", quietly = TRUE)) stop("Package 'quarto' is required to render the document.")

  withr::local_dir(build_dir)
  quarto::quarto_render(input = basename(copied_main), output_format = formats, execute_dir = build_dir)

  # The outputs are matched by name, not by a pattern built from the file stem:
  # "Results (final)", "a[1" or "x+y" are regular expressions of their own.
  rendered <- list.files(build_dir, full.names = TRUE)
  rendered <- rendered[!dir.exists(rendered)]
  output_names <- paste0(input_stem, ".", strsplit(output_format_extensions(formats), "|", fixed = TRUE)[[1]])
  out_glob <- rendered[tolower(basename(rendered)) %in% tolower(output_names)]
  if (!length(out_glob)) {
    warning("No rendered output (", paste(output_names, collapse = ", "), ") was found in the build folder '",
            build_dir, "', so the zip holds no output file. Check where the format writes its output, ",
            "for example an `output-dir` in _quarto.yml.", call. = FALSE)
  }
  include_in_zip <- unique(c(out_glob, if (include_sources) c(file.path(build_dir, basename(input)), copied_resources) else character(0)))
  include_in_zip <- include_in_zip[file.exists(include_in_zip)]

  zip_path_tmp <- file.path(build_dir, zip_name)
  # a zip name that is also a rendered output (zip_name = "report.pdf", say)
  # would delete that output just before zipping: refuse it instead
  if (tolower(basename(zip_path_tmp)) %in% tolower(basename(include_in_zip))) {
    stop("`zip_name` ('", zip_name, "') is the name of a rendered output; choose another name.", call. = FALSE)
  }
  if (file.exists(zip_path_tmp)) file.remove(zip_path_tmp)

  # Project files keep their relative paths (e.g. _extensions/<name>/...) so the
  # zipped sources still render; everything else is stored flat, as before.
  zip_project_rel <- if (include_sources) project_rel else character(0)
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(zipfile = zip_path_tmp, files = include_in_zip, recurse = FALSE)
    if (length(zip_project_rel)) {
      zip::zip_append(zip_path_tmp, files = zip_project_rel, root = build_dir, mode = "mirror")
    }
  } else {
    local_wd(build_dir)
    utils::zip(zipfile = zip_name, files = c(basename(include_in_zip), zip_project_rel), flags = "-r9Xq")
  }

  dest_zip <- file.path(copy_back_dir, basename(zip_path_tmp))
  if (file.exists(dest_zip) && !overwrite) stop("Zip exists and overwrite = FALSE: ", dest_zip)
  dir.create(copy_back_dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(zip_path_tmp, dest_zip, overwrite = TRUE)) stop("Failed to copy zip to destination: ", dest_zip)
  vcat("Created zip:", dest_zip)

  invisible(list(build_dir = build_dir, input = input, formats = formats,
                 copied_resources = copied_resources, outputs = out_glob, zip = dest_zip))
}

# "all" is a valid quarto_render value (it renders the formats the YAML
# declares), but the rendered outputs are matched below by the extensions
# of `formats`: leaving "all" there would expand to every known extension
# and count any build-dir file sharing the stem (a report.png figure, say)
# as a rendered output. Resolve it to the declared formats first.
#' @noRd
.resolve_all_formats <- function(formats, input) {
  if (!"all" %in% formats) return(formats)
  declared <- extract_yaml_formats(input)
  unique(c(setdiff(formats, "all"), if (length(declared) > 0L) declared else "html"))
}

#' Quarto Output Format Name -> File Extension
#'
#' Maps Quarto output format names to the file extension(s) they actually
#' produce (several formats share an extension, and some don't match their
#' own name, e.g. \code{revealjs} produces \code{.html} and \code{beamer}
#' produces \code{.pdf}). \code{"all"} expands to every known extension.
#'
#' @param formats Character vector of Quarto output format names.
#' @return A single regex-alternation string of file extensions (no dot),
#'   suitable for a \code{list.files()} pattern.
#' @keywords internal
#' @noRd
output_format_extensions <- function(formats) {
  # the mapping itself, also read by create_qmd_renderer(), is .format_ext_map in compute_graph.R
  if ("all" %in% formats) {
    exts <- unique(.format_ext_map)
  } else {
    # extension formats (titlepage-pdf, temple-html) produce the base format's file
    exts <- .format_extension(formats)
    exts <- unique(ifelse(is.na(exts), formats, exts))
  }
  paste(exts, collapse = "|")
}
