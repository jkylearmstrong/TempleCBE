# PDF -> DOCX conversion backends
#
# Backend selection is: python (pdf2docx) -> libreoffice (headless) ->
# Word COM (Windows only). Discovery is explicit and verified; there is no
# bare `system2("python", ...)` PATH gamble anywhere in this file.
#
# PDF -> DOCX is lossy reconstruction. When a report can be rendered to DOCX
# directly (e.g. Quarto with a `reference-doc`), prefer that; this is for
# reports whose tables only render well to PDF.

# ---------------------------------------------------------------------------
# Low-level process helpers
# ---------------------------------------------------------------------------

# system2() signals a *warning*, not an error, when the command cannot be
# found, and returns status 127 (or 1 on some Windows shells). A bare
# tryCatch(error = ...) therefore never fires. Every external call in this file
# goes through these helpers so that missing-command, non-zero-exit and
# genuine R-level errors all collapse to a single integer status.
#
# Every external call in this file goes through safe_system2 so that
# missing executables are explicitly validated and failures are instrumented.
.run_status <- function(cmd, args, check = FALSE, log_failures = FALSE, ...) {
  tryCatch({
    res <- safe_system2(cmd, args, check = check, log_failures = log_failures, ...)
    status <- attr(res, "status")
    if (is.null(status)) 0L else as.integer(status)
  }, external_process_error = function(e) {
    if (isTRUE(check)) stop(e)
    if (identical(e$failure_mode, "missing_executable")) 127L else 1L
  }, error = function(e) {
    if (isTRUE(check)) stop(e)
    NA_integer_
  })
}

.run_output <- function(cmd, args, check = FALSE, log_failures = FALSE) {
  tryCatch({
    safe_system2(cmd, args, stdout = TRUE, stderr = TRUE, check = check, log_failures = log_failures)
  }, external_process_error = function(e) {
    if (isTRUE(check)) stop(e)
    NULL
  }, error = function(e) {
    if (isTRUE(check)) stop(e)
    NULL
  })
}

.status_ok <- function(st) {
  !is.na(st) && identical(st, 0L)
}

# ---------------------------------------------------------------------------
# Discovery
# ---------------------------------------------------------------------------

#' Locate a Python Interpreter That Can Import pdf2docx
#'
#' Search order (first verified hit wins):
#' \enumerate{
#'   \item \code{getOption("templecbe.python")}
#'   \item \code{Sys.getenv("TEMPLECBE_PYTHON")}
#'   \item \code{Sys.getenv("RETICULATE_PYTHON")} and \code{reticulate::py_exe()}
#'   \item \code{Sys.which("python3")}, \code{Sys.which("python")}
#'   \item Platform-specific well-known locations
#' }
#'
#' Each candidate is verified by actually running it, so Windows App Execution
#' Alias stubs (which resolve on PATH but do nothing) are rejected. When
#' \code{verify = TRUE} the candidate must additionally be able to
#' \code{import pdf2docx}. Install the pinned Python requirements with
#' \code{pip install -r} on
#' \code{system.file("python", "requirements.txt", package = "TempleCBE")}.
#'
#' @param verify If \code{TRUE}, require \code{import pdf2docx} to succeed.
#' @return Path to a usable interpreter, or \code{NULL}.
#' @seealso [check_docx_toolchain()]
#' @export
find_python <- function(verify = TRUE) {
  reticulate_py <- if (requireNamespace("reticulate", quietly = TRUE)) {
    tryCatch(reticulate::py_exe(), error = function(e) "")
  } else {
    ""
  }
  if (is.null(reticulate_py) || is.na(reticulate_py)) reticulate_py <- ""

  candidates <- c(
    getOption("templecbe.python", ""),
    Sys.getenv("TEMPLECBE_PYTHON", ""),
    Sys.getenv("RETICULATE_PYTHON", ""),
    reticulate_py,
    unname(Sys.which("python3")),
    unname(Sys.which("python"))
  )

  if (.Platform$OS.type == "windows") {
    localapp <- Sys.getenv("LOCALAPPDATA", "")
    userprof <- Sys.getenv("USERPROFILE", "")
    if (nzchar(localapp)) {
      candidates <- c(
        candidates,
        # Python Install Manager (pymanager) shim directory
        file.path(localapp, "Python", "bin", "python.exe"),
        Sys.glob(file.path(localapp, "Python", "pythoncore-*", "python.exe")),
        Sys.glob(file.path(localapp, "Programs", "Python", "Python3*", "python.exe"))
      )
    }
    if (nzchar(userprof)) {
      candidates <- c(
        candidates,
        Sys.glob(file.path(userprof, ".virtualenvs", "*", "Scripts", "python.exe")),
        Sys.glob(file.path(userprof, "Documents", ".virtualenvs", "*", "Scripts", "python.exe")),
        Sys.glob(file.path(userprof, "OneDrive", "Documents", ".virtualenvs", "*", "Scripts", "python.exe"))
      )
    }
    candidates <- c(candidates, Sys.glob("C:/Python3*/python.exe"))
  } else {
    candidates <- c(
      candidates,
      Sys.glob(file.path("~", ".virtualenvs", "*", "bin", "python")),
      "/usr/bin/python3",
      "/usr/local/bin/python3",
      "/opt/venv/bin/python"
    )
  }

  candidates <- unique(candidates[nzchar(candidates)])
  if (!length(candidates)) {
    return(NULL)
  }
  resolvable <- file.exists(candidates) | nzchar(Sys.which(candidates))
  candidates <- candidates[resolvable]

  for (py in candidates) {
    if (!.python_runs(py)) next
    if (!verify) {
      return(py)
    }
    if (.python_has(py, "pdf2docx")) {
      return(py)
    }
  }
  NULL
}

.python_runs <- function(py) {
  out <- .run_output(py, "--version")
  # A Windows Store alias stub prints nothing and/or sets a non-zero status.
  !is.null(out) &&
    length(out) > 0 &&
    is.null(attr(out, "status")) &&
    any(grepl("^Python 3", out))
}

.python_has <- function(py, module) {
  .status_ok(.run_status(
    py,
    c("-c", shQuote(paste0("import ", module))),
    stdout = FALSE,
    stderr = FALSE
  ))
}

#' Locate a LibreOffice Headless Binary
#'
#' Honours \code{getOption("templecbe.soffice")} and the
#' \code{TEMPLECBE_SOFFICE} environment variable before searching the
#' \code{PATH} and default install locations.
#'
#' @return Path to \code{soffice}/\code{libreoffice}, or \code{NULL}.
#' @seealso [check_docx_toolchain()]
#' @export
find_soffice <- function() {
  candidates <- c(
    getOption("templecbe.soffice", ""),
    Sys.getenv("TEMPLECBE_SOFFICE", ""),
    unname(Sys.which("soffice")),
    unname(Sys.which("libreoffice"))
  )
  if (.Platform$OS.type == "windows") {
    candidates <- c(
      candidates,
      "C:/Program Files/LibreOffice/program/soffice.exe",
      "C:/Program Files (x86)/LibreOffice/program/soffice.exe"
    )
  } else {
    candidates <- c(
      candidates,
      "/usr/bin/soffice",
      "/usr/bin/libreoffice",
      "/opt/libreoffice/program/soffice"
    )
  }
  candidates <- unique(candidates[nzchar(candidates)])
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) hit[1] else NULL
}

# The library RDCOMClient is installed in, or NULL. Windows only. RDCOMClient
# is not on CRAN, so it is often in the per-user library rather than on
# .libPaths(); it is only ever loaded inside the Word COM subprocess.
.rdcomclient_lib <- function() {
  if (.Platform$OS.type != "windows") {
    return(NULL)
  }
  r_version <- paste0(R.version$major, ".", substr(R.version$minor, 1, 1))
  user_lib <- file.path(Sys.getenv("USERPROFILE"), "AppData", "Local", "R", "win-library", r_version)
  libs <- unique(c(.libPaths(), user_lib, R.home("library")))
  hit <- libs[dir.exists(file.path(libs, "RDCOMClient"))]
  if (length(hit)) hit[1] else NULL
}

.word_com_available <- function() {
  !is.null(.rdcomclient_lib())
}

#' Report Which PDF -> DOCX Backends Are Usable on This Machine
#'
#' Safe to call at any time; performs discovery only and converts nothing.
#'
#' @param quiet Suppress the printed diagnosis.
#' @return Invisibly, a list with elements \code{python_with_pdf2docx},
#'   \code{python_any}, \code{soffice}, \code{word_com} and \code{backend}.
#' @seealso [convert_pdf_to_docx()], [convert_pdfs_to_docx()]
#' @export
#' @examples
#' \dontrun{
#' check_docx_toolchain()
#' }
check_docx_toolchain <- function(quiet = FALSE) {
  py <- find_python(verify = TRUE)
  py_any <- if (is.null(py)) find_python(verify = FALSE) else py
  soffice <- find_soffice()
  com_ok <- .word_com_available()

  res <- list(
    python_with_pdf2docx = py,
    python_any = py_any,
    soffice = soffice,
    word_com = com_ok,
    backend = if (!is.null(py)) {
      "python"
    } else if (!is.null(soffice)) {
      "libreoffice"
    } else if (com_ok) {
      "word_com"
    } else {
      NA_character_
    }
  )

  if (!quiet) {
    cat("PDF -> DOCX toolchain\n")
    cat("  platform          : ", .Platform$OS.type, " / ", R.version$arch, "\n", sep = "")
    cat("  python + pdf2docx : ", if (is.null(py)) "NOT FOUND" else py, "\n", sep = "")
    cat("  python (any)      : ", if (is.null(py_any)) "NOT FOUND" else py_any, "\n", sep = "")
    cat("  libreoffice       : ", if (is.null(soffice)) "NOT FOUND" else soffice, "\n", sep = "")
    cat("  word COM          : ",
        if (com_ok) "available" else "unavailable",
        if (.Platform$OS.type != "windows") " (Windows only)" else "",
        "\n",
        sep = ""
    )
    cat("  selected backend  : ", res$backend, "\n", sep = "")
  }
  invisible(res)
}

.toolchain_error <- function(state) {
  req <- system.file("python", "requirements.txt", package = "TempleCBE")
  py_hint <- if (is.null(state$python_any)) "<no python interpreter found>" else state$python_any
  stop(
    "No PDF -> DOCX backend is available on this machine.\n",
    "  platform          : ", .Platform$OS.type, " / ", R.version$arch, "\n",
    "  python + pdf2docx : ", if (is.null(state$python_with_pdf2docx)) "no" else "yes", "\n",
    "  python (any)      : ", py_hint, "\n",
    "  libreoffice       : ", if (is.null(state$soffice)) "no" else "yes", "\n",
    "  word COM          : ", if (state$word_com) "yes" else "no",
    if (.Platform$OS.type != "windows") " (Windows only)" else "", "\n\n",
    "Fix ONE of the following:\n",
    "  A) Install the Python toolchain into the interpreter R will call:\n",
    "       \"", py_hint, "\" -m pip install -r \"", req, "\"\n",
    "     Or point R at a specific interpreter:\n",
    "       options(templecbe.python = \"/path/to/python\")\n",
    "       # or set the TEMPLECBE_PYTHON environment variable\n",
    "  B) Install LibreOffice and ensure `soffice` is on PATH:\n",
    "       Debian/Ubuntu (incl. arm64): apt-get install -y libreoffice-writer\n",
    "       Or set options(templecbe.soffice = \"/path/to/soffice\")\n",
    "  C) Windows only: install Microsoft Word and the RDCOMClient package.\n\n",
    "Re-diagnose with: TempleCBE::check_docx_toolchain()\n",
    "To continue without DOCX output instead of failing, pass strict = FALSE.",
    call. = FALSE
  )
}

# ---------------------------------------------------------------------------
# Backends
# ---------------------------------------------------------------------------

.convert_python <- function(py, pdf, docx) {
  # Write a script file and pass paths as argv. Avoids the quoting fragility
  # of building a one-liner with sprintf() and embedded single quotes, which
  # breaks on paths containing spaces or apostrophes.
  script <- tempfile(fileext = ".py")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(
    "import sys",
    "from pdf2docx import Converter",
    "cv = Converter(sys.argv[1])",
    "try:",
    "    cv.convert(sys.argv[2])",
    "finally:",
    "    cv.close()"
  ), script)

  st <- .run_status(py, shQuote(c(script, pdf, docx)))
  .status_ok(st) && file.exists(docx)
}

.convert_soffice <- function(soffice, pdf, docx) {
  outdir <- dirname(docx)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  # LibreOffice needs a writable, private profile dir or concurrent/headless
  # runs collide with a desktop session.
  profile <- tempfile("lo_profile_")
  dir.create(profile, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(profile, recursive = TRUE), add = TRUE)
  # file:// URI construction differs by platform: a POSIX path already starts
  # with "/" (-> file:///tmp/x) whereas a Windows path starts with a drive
  # letter (-> file:///C:/Users/...). Naive paste0("file:///", path) yields
  # file:////tmp/x on Linux, which LibreOffice rejects.
  profile_path <- gsub("\\\\", "/", normalizePath(profile, winslash = "/", mustWork = FALSE))
  profile_uri <- paste0(
    "-env:UserInstallation=file://",
    if (startsWith(profile_path, "/")) profile_path else paste0("/", profile_path)
  )

  st <- .run_status(
    soffice,
    c(
      shQuote(profile_uri),
      "--headless", "--norestore", "--invisible", "--nolockcheck",
      "--convert-to", shQuote("docx:MS Word 2007 XML"),
      "--outdir", shQuote(outdir),
      shQuote(pdf)
    ),
    stdout = FALSE,
    stderr = FALSE
  )

  produced <- file.path(outdir, paste0(tools::file_path_sans_ext(basename(pdf)), ".docx"))
  if (!.status_ok(st) || !file.exists(produced)) {
    return(FALSE)
  }
  if (!identical(normalizePath(produced, mustWork = FALSE), normalizePath(docx, mustWork = FALSE))) {
    file.rename(produced, docx)
  }
  file.exists(docx)
}

.convert_word_com <- function(pdf, docx, timeout = 600) {
  # Hard platform gate: the COM path shells out to powershell + Rscript +
  # Word.Application and must never be attempted off Windows.
  if (.Platform$OS.type != "windows") {
    return(FALSE)
  }
  com_lib <- .rdcomclient_lib()
  if (is.null(com_lib)) {
    return(FALSE)
  }

  temp_r_script <- normalizePath(tempfile(fileext = ".R"), mustWork = FALSE)
  on.exit(unlink(temp_r_script), add = TRUE)

  writeLines(c(
    "args <- c(",
    paste0("  ", encodeString(normalizePath(pdf), quote = '"'), ","),
    paste0("  ", encodeString(normalizePath(docx, mustWork = FALSE), quote = '"')),
    ")",
    paste0("library(RDCOMClient, lib.loc = ", encodeString(com_lib, quote = '"'), ")"),
    "wordApp <- RDCOMClient::COMCreate('Word.Application')",
    "wordApp[['Visible']] <- FALSE",
    "wordApp[['DisplayAlerts']] <- FALSE",
    "try(wordApp[['Options']][['UpdateLinksAtOpen']] <- FALSE, silent = TRUE)",
    "try(wordApp[['AutomationSecurity']] <- 3, silent = TRUE)",
    "tryCatch({",
    "  doc <- wordApp[['Documents']]$Open(normalizePath(args[1]), ConfirmConversions = FALSE, ReadOnly = TRUE)",
    "  if (!is.null(doc)) {",
    "    doc$SaveAs2(normalizePath(args[2], mustWork = FALSE))",
    "    doc$Close(FALSE)",
    "  }",
    "}, finally = {",
    "  try(wordApp$Quit(), silent = TRUE)",
    "})"
  ), temp_r_script)

  # The subprocess runs this session's own Rscript, not whichever R is first
  # on PATH, so it sees the same library as the caller.
  rscript <- normalizePath(file.path(R.home("bin"), "Rscript.exe"), mustWork = FALSE)
  ps_args <- paste0(
    "& { ",
    "  $p = Start-Process '", rscript, "' -ArgumentList '--vanilla', '", temp_r_script, "' -NoNewWindow -PassThru; ",
    "  $res = $p | Wait-Process -Timeout ", timeout, " -ErrorAction SilentlyContinue; ",
    "  if (-not $p.HasExited) { ",
    "    $p | Stop-Process -Force; ",
    "    Stop-Process -Name WINWORD -Force -ErrorAction SilentlyContinue; ",
    "    exit 99; ",
    "  } ",
    "}"
  )

  st <- .run_status("powershell", c("-Command", shQuote(ps_args)))

  if (identical(st, 99L)) {
    warning("Word COM conversion of ", basename(pdf), " timed out after ",
            timeout, "s and was skipped.", call. = FALSE)
    return(FALSE)
  }
  .status_ok(st) && file.exists(docx)
}

# ---------------------------------------------------------------------------
# Public entry points
# ---------------------------------------------------------------------------

#' Convert PDFs to DOCX Using the Best Available Backend
#'
#' Backend preference is python (\code{pdf2docx}) -> LibreOffice -> Word COM
#' (Windows only). When the python backend fails on an individual file and
#' LibreOffice is present, that file is retried once with LibreOffice.
#'
#' @param conversions A data frame with character columns \code{src} and
#'   \code{dest}, and optionally \code{temp_dest}, a second location each
#'   converted file is copied to.
#' @param backend One of \code{"auto"}, \code{"python"}, \code{"libreoffice"},
#'   \code{"word_com"}.
#' @param strict If \code{TRUE} (default), error with an actionable message
#'   when no backend is available. Set \code{FALSE} to warn and skip.
#' @param timeout Seconds before the Word COM backend gives up on a file.
#' @return \code{conversions} with an added logical \code{converted} column.
#' @seealso [convert_pdf_to_docx()], [check_docx_toolchain()], and
#'   [zip_reports()], whose \code{docx_from_pdf} argument accepts
#'   \code{convert_pdf_to_docx}.
#' @export
convert_pdfs_to_docx <- function(conversions,
                                 backend = "auto",
                                 strict = TRUE,
                                 timeout = 600) {
  if (is.null(conversions) || !nrow(conversions)) {
    return(cbind(conversions, converted = logical(0)))
  }

  state <- check_docx_toolchain(quiet = TRUE)
  if (identical(backend, "auto")) {
    backend <- state$backend
  }

  if (is.na(backend)) {
    if (isTRUE(strict)) {
      .toolchain_error(state)
    }
    warning(
      "No PDF -> DOCX backend available; skipping ", nrow(conversions),
      " DOCX output(s). See TempleCBE::check_docx_toolchain().",
      call. = FALSE
    )
    return(cbind(conversions, converted = FALSE))
  }

  message("PDF -> DOCX backend: ", backend, " (", nrow(conversions), " file(s))")

  converted <- logical(nrow(conversions))

  for (i in seq_len(nrow(conversions))) {
    pdf <- normalizePath(conversions$src[i], mustWork = TRUE)
    docx <- normalizePath(conversions$dest[i], mustWork = FALSE)
    has_temp <- "temp_dest" %in% names(conversions) &&
      !is.na(conversions$temp_dest[i]) &&
      nzchar(conversions$temp_dest[i])
    temp_dest <- if (has_temp) conversions$temp_dest[i] else NULL

    dir.create(dirname(docx), recursive = TRUE, showWarnings = FALSE)
    if (has_temp) {
      dir.create(dirname(temp_dest), recursive = TRUE, showWarnings = FALSE)
    }

    message("  [", i, "/", nrow(conversions), "] ", basename(pdf))

    ok <- switch(backend,
      python      = .convert_python(state$python_with_pdf2docx, pdf, docx),
      libreoffice = .convert_soffice(state$soffice, pdf, docx),
      word_com    = .convert_word_com(pdf, docx, timeout = timeout),
      FALSE
    )

    # One documented fallback hop, not a silent cascade.
    if (!ok && identical(backend, "python") && !is.null(state$soffice)) {
      warning("pdf2docx failed on ", basename(pdf), "; retrying with LibreOffice.", call. = FALSE)
      ok <- .convert_soffice(state$soffice, pdf, docx)
    }

    if (!ok) {
      warning("Conversion failed: ", basename(pdf), call. = FALSE)
    } else if (file.exists(docx) && has_temp) {
      file.copy(from = docx, to = temp_dest, overwrite = TRUE)
    }
    converted[i] <- ok
  }

  if (!all(converted)) {
    warning(sum(!converted), " of ", length(converted), " PDF -> DOCX conversion(s) failed.", call. = FALSE)
  }

  cbind(conversions, converted = converted)
}

#' Convert a Single PDF to DOCX Using the Best Available Backend
#'
#' Convenience wrapper around [convert_pdfs_to_docx()] for one file. Its
#' \code{src, dest} signature fits the \code{docx_from_pdf} callback of
#' [zip_reports()].
#'
#' @param src Path to the input PDF file.
#' @param dest Path for the output DOCX file. Defaults to \code{src} with
#'   \code{.pdf} replaced by \code{.docx}.
#' @inheritParams convert_pdfs_to_docx
#' @return Invisibly, \code{dest} on success, or \code{FALSE} if conversion
#'   failed.
#' @export
#' @examples
#' \dontrun{
#' convert_pdf_to_docx("report.pdf")
#' zip_reports(reports, output_formats = c("pdf", "docx"), docx_from_pdf = convert_pdf_to_docx)
#' }
convert_pdf_to_docx <- function(src,
                                dest = sub("\\.pdf$", ".docx", src, ignore.case = TRUE),
                                backend = "auto",
                                strict = TRUE,
                                timeout = 600) {
  if (!is.character(src) || length(src) != 1L || is.na(src) || !nzchar(src)) {
    stop("'src' must be a single non-empty file path.", call. = FALSE)
  }

  res <- convert_pdfs_to_docx(
    conversions = data.frame(src = src, dest = dest, stringsAsFactors = FALSE),
    backend = backend,
    strict = strict,
    timeout = timeout
  )

  if (isTRUE(res$converted[1])) invisible(dest) else invisible(FALSE)
}
