#' Render a Quarto Document and Zip It With Its Dependencies
#'
#' Renders \code{input} in an isolated build directory, copies in any
#' resources it references (explicitly, or heuristically detected from
#' quoted paths / \code{here::here()} calls), and zips the outputs together
#' with the source and sidecar files.
#'
#' @param input Path to the input \code{.qmd} file.
#' @param formats Character vector of output formats (e.g. \code{c("html","pdf","docx")} or \code{"all"}).
#' @param resources Optional character vector of extra files to include, absolute or project-relative.
#' @param detect \code{"heuristic"} (default; scans the \code{.qmd} for likely file paths) or \code{"none"}.
#' @details
#' Heuristic detection inspects quoted file paths and here::here() calls in the document. For YAML front-matter resources, pass them explicitly via the `resources` argument or include them in the document's YAML; this function will attempt to parse YAML when present to pick up top-level resource lists.
#' @param build_dir Staging directory; defaults to a fresh temp directory.
#' @param zip_name Name of the resulting zip; defaults to \verb{<input-stem>.zip}.
#' @param copy_back_dir Where to copy the finished zip; defaults to \code{dirname(input)}.
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

  if (is.null(zip_name)) zip_name <- paste0(input_stem, ".zip")
  if (is.null(copy_back_dir)) copy_back_dir <- input_dir
  if (is.null(build_dir)) build_dir <- file.path(tempdir(), paste0("qbuild-", input_stem, "-", as.integer(Sys.time())))
  if (!dir.exists(build_dir)) dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)

  vcat <- function(...) if (isTRUE(verbose)) cat(..., "\n")

  copy_preserve <- function(src, root = NULL) {
    if (!file.exists(src)) return(character(0))
    src_norm <- normalizePath(src, winslash = "/", mustWork = TRUE)
    rel <- NA_character_
    if (!is.null(root) && dir.exists(root)) {
      root_norm <- normalizePath(root, winslash = "/", mustWork = TRUE)
      if (startsWith(src_norm, paste0(root_norm, "/"))) {
        rel <- substr(src_norm, nchar(root_norm) + 2L, nchar(src_norm))
      }
    }
    dest <- if (is.na(rel)) file.path(build_dir, basename(src_norm)) else file.path(build_dir, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (file.copy(src_norm, dest, overwrite = TRUE)) dest else character(0)
  }

  detected <- character(0)
  if (detect == "heuristic") {
    qmd_lines <- readLines(input, warn = FALSE, encoding = "UTF-8")

    # Parse YAML front-matter if present (--- at top)
    if (length(qmd_lines) > 0 && grepl("^---$", qmd_lines[1])) {
      end_yaml <- which(qmd_lines == "---")
      if (length(end_yaml) >= 2) {
        yaml_block <- paste(qmd_lines[2:(end_yaml[2]-1)], collapse = "\n")
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
        parts <- trimws(gsub("^['\"]|['\"]$", "", strsplit(h, ",")[[1]]))
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
  file.copy(input, copied_main, overwrite = TRUE)

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
    file.copy(project_files[i], dest, overwrite = TRUE)
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

  out_glob <- list.files(build_dir, pattern = paste0("^", input_stem, "\\.(", output_format_extensions(formats), ")$"), full.names = TRUE, ignore.case = TRUE)
  include_in_zip <- unique(c(out_glob, if (include_sources) c(file.path(build_dir, basename(input)), copied_resources) else character(0)))
  include_in_zip <- include_in_zip[file.exists(include_in_zip)]

  zip_path_tmp <- file.path(build_dir, zip_name)
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
    old <- setwd(build_dir)
    on.exit(setwd(old), add = TRUE)
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
  format_ext_map <- c(
    html = "html", revealjs = "html", slidy = "html", s5 = "html",
    pdf = "pdf", beamer = "pdf", pdflatex = "pdf", latex = "pdf",
    docx = "docx", pptx = "pptx", odt = "odt", rtf = "rtf",
    epub = "epub", epub3 = "epub", ipynb = "ipynb",
    gfm = "md", md = "md", markdown = "md", commonmark = "md",
    typst = "pdf", context = "pdf", docbook = "xml"
  )

  if ("all" %in% formats) {
    exts <- unique(format_ext_map)
  } else {
    resolve <- function(fmt) {
      if (fmt %in% names(format_ext_map)) return(format_ext_map[[fmt]])
      # Extension formats are named <extension>-<base format>, e.g.
      # titlepage-pdf or temple-html, and produce the base format's file.
      base <- sub("^.*-", "", fmt)
      if (base %in% names(format_ext_map)) format_ext_map[[base]] else fmt
    }
    exts <- unique(vapply(formats, resolve, character(1), USE.NAMES = FALSE))
  }
  paste(exts, collapse = "|")
}
