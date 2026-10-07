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
    if (!is.null(status)) {
      as.integer(status)
    } else if (is.numeric(res) && length(res) == 1L) {
      # stdout = FALSE, stderr = FALSE: system2() returns the exit code itself.
      as.integer(res)
    } else {
      0L
    }
  }, external_process_error = function(e) {
    if (isTRUE(check)) stop(e)
    if (identical(e$failure_mode, "missing_executable")) 127L else 1L
  }, error = function(e) {
    if (isTRUE(check)) stop(e)
    NA_integer_
  })
}

.run_output <- function(cmd, args, check = FALSE, log_failures = FALSE, ...) {
  tryCatch({
    safe_system2(cmd, args, stdout = TRUE, stderr = TRUE, check = check, log_failures = log_failures, ...)
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

# What system2() reports when its `timeout` runs out (on every platform; the
# command is stopped and a warning, which safe_system2() mutes, is raised).
.timeout_status <- 124L

.timed_out <- function(st) {
  identical(st, .timeout_status)
}

# A timeout is a failure of its own kind, so it is said out loud instead of
# being folded into the generic "Conversion failed".
.report_timeout <- function(tool, pdf, timeout, note = NULL) {
  warning(tool, " conversion of ", basename(pdf), " timed out after ", timeout, "s.",
          if (!is.null(note)) paste0(" ", note), call. = FALSE)
  FALSE
}

# Longest a probe of a candidate interpreter (`--version`, `import <module>`)
# may take before the candidate is passed over. Importing pdf2docx on a cold
# disk takes several seconds; a probe that takes a minute is not a usable
# interpreter.
.probe_timeout <- function() 60

# ---------------------------------------------------------------------------
# Discovery
# ---------------------------------------------------------------------------

#' Locate a Python Interpreter That Can Import a Module
#'
#' Search order (first verified hit wins):
#' \enumerate{
#'   \item \code{getOption("templecbe.python")}
#'   \item \code{Sys.getenv("TEMPLECBE_PYTHON")}
#'   \item \code{Sys.getenv("RETICULATE_PYTHON")} and, only when reticulate has
#'     already started Python in this session, \code{reticulate::py_exe()}
#'   \item \code{Sys.which("python3")}, \code{Sys.which("python")}
#'   \item Platform-specific well-known locations
#' }
#'
#' Each candidate is verified by actually running it, so Windows App Execution
#' Alias stubs (which resolve on PATH but do nothing) are rejected. A probe
#' that takes longer than 60 seconds (running \code{--version}, or importing
#' \code{module}) counts as a failure, so one hung interpreter cannot block the
#' search. When
#' \code{verify = TRUE} the candidate must additionally be able to
#' \code{import} \code{module}. Candidates are probed in a separate process:
#' this function never starts Python inside R, and never starts reticulate's
#' Python, so it is safe to use as a guard before code that would (for example
#' \code{eval = !is.null(find_python(module = "numpy"))} on a vignette chunk).
#' Install the pinned Python requirements for PDF conversion with
#' \code{pip install -r} on
#' \code{system.file("python", "requirements.txt", package = "TempleCBE")}.
#'
#' The tests that run a real PDF to DOCX conversion (Python, LibreOffice or
#' Word) are opt-in: set the environment variable
#' \code{TEMPLECBE_RUN_PDF_TESTS=true} to run them. See also
#' \code{\link{find_sas}()} for \code{TEMPLECBE_RUN_SAS_TESTS}.
#'
#' @param verify If \code{TRUE}, require \code{import <module>} to succeed.
#' @param module Name of the Python module an interpreter must be able to
#'   import when \code{verify = TRUE}: a plain, possibly dotted, module name.
#'   Defaults to \code{"pdf2docx"}.
#' @return Path to a usable interpreter, or \code{NULL}.
#' @seealso [check_docx_toolchain()]
#' @export
find_python <- function(verify = TRUE, module = "pdf2docx") {
  check_python_module(module)
  # reticulate::py_exe() starts Python when it is not running yet, so ask
  # reticulate only when it has already started (RETICULATE_PYTHON is read below).
  reticulate_py <- if (requireNamespace("reticulate", quietly = TRUE) &&
    isTRUE(tryCatch(reticulate::py_available(initialize = FALSE), error = function(e) FALSE))) {
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
    if (.python_has(py, module)) {
      return(py)
    }
  }
  NULL
}

# `module` ends up in `python -c "import <module>"`, so accept only a plain
# (dotted) module name.
check_python_module <- function(module) {
  if (!is.character(module) || length(module) != 1L || is.na(module) ||
    !grepl("^[A-Za-z_][A-Za-z0-9_]*(\\.[A-Za-z_][A-Za-z0-9_]*)*$", module)) {
    stop("`module` must be a single Python module name, such as \"numpy\" or \"scipy.stats\".", call. = FALSE)
  }
  invisible(module)
}

.python_runs <- function(py) {
  out <- .run_output(py, "--version", timeout = .probe_timeout())
  # A Windows Store alias stub prints nothing and/or sets a non-zero status; a
  # probe that timed out has status 124.
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
    stderr = FALSE,
    timeout = .probe_timeout()
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

# A converter never writes to `docx` itself. It writes a fresh file next to it,
# and that file is moved over `docx` only when the tool succeeded and left a
# non-empty file (for pdf2docx and Word COM, succeeded means exit status 0; for
# LibreOffice see .convert_soffice()). A DOCX that was already at `docx` (an
# earlier run, a stale copy) or a half-written one from a crashed pdf2docx
# therefore never counts as a conversion, and a failed run leaves `docx`
# exactly as it was.
.scratch_docx <- function(docx, fileext = ".docx") {
  dir.create(dirname(docx), recursive = TRUE, showWarnings = FALSE)
  tempfile(".docx_part_", tmpdir = dirname(docx), fileext = fileext)
}

.install_docx <- function(scratch, docx) {
  if (!isTRUE(file.size(scratch) > 0)) {
    return(FALSE)
  }
  # file.rename() replaces an existing file on every platform R runs on.
  if (!isTRUE(suppressWarnings(file.rename(scratch, docx)))) {
    warning("A DOCX was produced but could not be moved to '", docx,
            "'; is the existing file open in another program?", call. = FALSE)
    return(FALSE)
  }
  TRUE
}

.convert_python <- function(py, pdf, docx, timeout = 600) {
  # Write a script file and pass paths as argv. Avoids the quoting fragility
  # of building a one-liner with sprintf() and embedded single quotes, which
  # breaks on paths containing spaces or apostrophes.
  script <- tempfile(fileext = ".py")
  scratch <- .scratch_docx(docx)
  on.exit(unlink(c(script, scratch)), add = TRUE)
  writeLines(c(
    "import sys",
    "from pdf2docx import Converter",
    "cv = Converter(sys.argv[1])",
    "try:",
    "    cv.convert(sys.argv[2])",
    "finally:",
    "    cv.close()"
  ), script)

  st <- .run_status(py, shQuote(c(script, pdf, scratch)), timeout = timeout)
  if (.timed_out(st)) {
    return(.report_timeout("pdf2docx", pdf, timeout))
  }
  .status_ok(st) && .install_docx(scratch, docx)
}

.convert_soffice <- function(soffice, pdf, docx, timeout = 600) {
  # LibreOffice chooses the output file name itself (<pdf stem>.docx), so it
  # gets a private, empty output folder next to `docx`: whatever it finds there
  # afterwards was written by this run.
  work <- .scratch_docx(docx, fileext = "")
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

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
      "--outdir", shQuote(work),
      shQuote(pdf)
    ),
    stdout = FALSE,
    stderr = FALSE,
    timeout = timeout
  )
  if (.timed_out(st)) {
    return(.report_timeout("LibreOffice", pdf, timeout))
  }

  # The output file decides, not LibreOffice's exit status: every run starts a
  # fresh profile, and a first start may exit non-zero after a good conversion.
  # Nothing but this run can have written into `work` (it was empty), so a
  # stale file at `dest` cannot pass for the result; a timeout, above, is the
  # one failure that is reported whatever was written.
  produced <- file.path(work, paste0(tools::file_path_sans_ext(basename(pdf)), ".docx"))
  .install_docx(produced, docx)
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
  scratch <- .scratch_docx(docx)
  on.exit(unlink(c(temp_r_script, scratch)), add = TRUE)

  writeLines(c(
    "args <- c(",
    paste0("  ", encodeString(normalizePath(pdf), quote = '"'), ","),
    paste0("  ", encodeString(normalizePath(scratch, mustWork = FALSE), quote = '"')),
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
  st <- .run_word_com(rscript, temp_r_script, timeout)

  if (identical(st, .word_com_timeout_status)) {
    return(.report_timeout(
      "Word COM", pdf, timeout,
      note = paste0(
        "The conversion process was stopped. Word itself was not touched, so if the WINWORD.exe it ",
        "started is still running in the background, close it from Task Manager."
      )
    ))
  }
  .status_ok(st) && .install_docx(scratch, docx)
}

# Exit status of the PowerShell launcher when the converter had to be stopped.
.word_com_timeout_status <- 99L

# A string as a PowerShell single-quoted literal. Inside one, an apostrophe has
# to be doubled, and PowerShell treats the typographic single quotes (U+2018,
# U+2019, U+201A, U+201B) as apostrophes too.
.ps_quote <- function(x) {
  # The typographic quotes by code point, so that this file stays ASCII.
  quotes <- paste0("'", paste(intToUtf8(c(0x2018, 0x2019, 0x201a, 0x201b), multiple = TRUE), collapse = ""))
  paste0("'", gsub(paste0("([", quotes, "])"), "\\1\\1", x), "'")
}

# The PowerShell command that runs `script` with `rscript` and waits at most
# `timeout` seconds. Its exit status is the script's, or 99 after a timeout.
#
# Start-Process joins the -ArgumentList items with spaces and does not quote
# them, so the script path carries its own double quotes (a folder with a space
# would otherwise reach Rscript cut in two); every item and the program path
# are single-quoted literals with apostrophes doubled (a user name such as
# O'Brien would otherwise end the string). On a timeout it stops the process
# that was started, by process id: never every Word the user has open (the old
# `Stop-Process -Name WINWORD`), whose unsaved documents would be lost.
.word_com_command <- function(rscript, script, timeout) {
  seconds <- as.integer(min(ceiling(timeout), .Machine$integer.max))
  arg_list <- paste(.ps_quote(c("--vanilla", paste0("\"", script, "\""))), collapse = ", ")
  paste0(
    "& { ",
    "$p = Start-Process -FilePath ", .ps_quote(rscript), " -ArgumentList ", arg_list, " -NoNewWindow -PassThru; ",
    "$null = $p.Handle; ", # without this, ExitCode can come back empty
    "$null = $p | Wait-Process -Timeout ", seconds, " -ErrorAction SilentlyContinue; ",
    "if (-not $p.HasExited) { ",
    "Stop-Process -Id $p.Id -Force -ErrorAction SilentlyContinue; ",
    "exit ", .word_com_timeout_status, " ",
    "}; ",
    "exit $p.ExitCode ",
    "}"
  )
}

.run_word_com <- function(rscript, script, timeout) {
  .run_status("powershell", c("-Command", shQuote(.word_com_command(rscript, script, timeout))))
}

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

# Checks every row of a `conversions` data frame (already known to have `src`
# and `dest` columns and at least one row) WITHOUT looking for a backend, so the
# outcome depends only on the caller's input and file system, never on which
# converters are installed. With strict = TRUE the first bad row stops the run;
# with strict = FALSE each bad row gets a warning and is marked not valid, so no
# backend is ever asked to convert it. A `src` that resolves to the same file as
# its `dest` or `temp_dest` (or `dest` the same as `temp_dest`) is a caller bug
# that would overwrite the PDF, so it stops the run in both modes. A `dest` or
# `temp_dest` with a .pdf extension is a bad row like any other.
#
# Returns list(valid, pdf, docx): `valid` is a logical per row, `pdf` and `docx`
# are the normalised paths (NA where the row is not valid).
.validate_conversions <- function(conversions, strict = TRUE) {
  n <- nrow(conversions)
  valid <- rep(FALSE, n)
  pdf <- rep(NA_character_, n)
  docx <- rep(NA_character_, n)

  # Report one bad row: stop when strict, otherwise warn and move on.
  reject <- function(problem) {
    if (isTRUE(strict)) {
      stop(problem, ".", call. = FALSE)
    }
    warning(problem, "; skipping conversion.", call. = FALSE)
  }

  for (i in seq_len(n)) {
    src_raw <- conversions$src[i]
    dest_raw <- conversions$dest[i]

    if (is.na(src_raw) || !is.character(src_raw) || !nzchar(trimws(src_raw)) || !file.exists(src_raw)) {
      reject(sprintf("Source PDF file '%s' does not exist", as.character(src_raw)))
      next
    }

    if (isTRUE(file.info(src_raw)$size == 0)) {
      reject(sprintf("Source PDF file '%s' is empty (0 bytes)", as.character(src_raw)))
      next
    }

    if (is.na(dest_raw) || !is.character(dest_raw) || !nzchar(trimws(dest_raw))) {
      reject(sprintf("Destination DOCX path for source PDF '%s' is missing or empty", as.character(src_raw)))
      next
    }

    pdf_i <- normalizePath(src_raw, winslash = "/", mustWork = TRUE)
    docx_i <- normalizePath(dest_raw, winslash = "/", mustWork = FALSE)

    if (same_file_path(pdf_i, docx_i)) {
      stop(sprintf("'src' and 'dest' cannot point to the same file: '%s'", pdf_i), call. = FALSE)
    }

    # The converted file is copied to `temp_dest` too, so it is a place the PDF
    # could be overwritten from: with the PDF itself (DOCX bytes in a .pdf
    # file), or with `dest` (a copy onto itself empties the file).
    temp_raw <- if ("temp_dest" %in% names(conversions)) as.character(conversions$temp_dest[i]) else NA_character_
    if (!is.na(temp_raw) && nzchar(trimws(temp_raw))) {
      if (same_file_path(pdf_i, temp_raw)) {
        stop(sprintf("'src' and 'temp_dest' cannot point to the same file: '%s'", pdf_i), call. = FALSE)
      }
      if (same_file_path(docx_i, temp_raw)) {
        stop(sprintf("'dest' and 'temp_dest' cannot point to the same file: '%s'", docx_i), call. = FALSE)
      }
      if (tolower(tools::file_ext(temp_raw)) == "pdf") {
        reject(sprintf("'temp_dest' '%s' for source PDF '%s' has a .pdf extension, and a DOCX is not a PDF",
                       temp_raw, as.character(src_raw)))
        next
      }
    }
    if (tolower(tools::file_ext(docx_i)) == "pdf") {
      reject(sprintf("Destination DOCX path '%s' for source PDF '%s' has a .pdf extension, and a DOCX is not a PDF",
                     docx_i, as.character(src_raw)))
      next
    }

    valid[i] <- TRUE
    pdf[i] <- pdf_i
    docx[i] <- docx_i
  }

  list(valid = valid, pdf = pdf, docx = docx)
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
#' @details
#' The rows of \code{conversions} are checked before any backend is looked for,
#' so a bad row is reported the same way on every machine, whether or not a
#' converter is installed. A row is bad when its \code{src} does not exist or
#' is empty (0 bytes), when its \code{dest} is missing, or when its \code{dest}
#' or \code{temp_dest} ends in \code{.pdf} (a DOCX is not a PDF, and the file
#' that is there would be overwritten). A \code{src} that resolves to the same
#' file as its \code{dest} or \code{temp_dest}, however the path is spelled
#' (\code{./a.pdf}, another case), and a \code{dest} that is the same file as
#' its \code{temp_dest}, are always errors, whatever \code{strict} is, because
#' converting would overwrite the PDF or empty the copy. Rows that pass are
#' then converted; if none pass, no backend is looked for at all.
#'
#' A converter writes into a fresh file next to \code{dest}, and that file
#' replaces \code{dest} only when the conversion succeeded and left a non-empty
#' file. For pdf2docx and Word COM, succeeded means that the converter exited
#' with status 0. LibreOffice's exit status is not looked at: it writes into a
#' private, empty folder, so whatever it leaves there was written by this run,
#' and a first start with a fresh profile may exit non-zero after a good
#' conversion. A time-out is a failure for every backend. A DOCX that was
#' already at \code{dest} (from an earlier run, say) is never mistaken for the
#' result: when the conversion fails, \code{dest} is left exactly as it was (an
#' old file is neither deleted nor reported as converted, and a half-written
#' file from a crashed converter is discarded), and the row comes back with
#' \code{converted = FALSE}.
#'
#' @param conversions A data frame with character columns \code{src} and
#'   \code{dest}, and optionally \code{temp_dest}, a second location each
#'   converted file is copied to.
#' @param backend One of \code{"auto"}, \code{"python"}, \code{"libreoffice"},
#'   \code{"word_com"}.
#' @param strict If \code{TRUE} (default), stop at the first bad row and, when
#'   every row is fine, error with an actionable message if no backend is
#'   available. Set \code{FALSE} to warn and skip instead; a skipped row comes
#'   back with \code{converted = FALSE}.
#' @param timeout Seconds a converter may take on one file before it is
#'   stopped; it applies to every backend (python, LibreOffice and Word COM).
#'   A file that times out gets a warning that says so and comes back with
#'   \code{converted = FALSE} (the python backend then retries it once with
#'   LibreOffice, which gets its own \code{timeout}). Stopping a converter
#'   stops the process that was started; any process that one started in turn
#'   (LibreOffice's \code{soffice.bin}, or the Word instance behind Word COM)
#'   may keep running and has to be closed by hand.
#' @return \code{conversions} with an added logical \code{converted} column.
#' @seealso [convert_pdf_to_docx()], [check_docx_toolchain()], and
#'   [zip_reports()], whose \code{docx_from_pdf} argument accepts
#'   \code{convert_pdf_to_docx}.
#' @export
convert_pdfs_to_docx <- function(conversions,
                                 backend = "auto",
                                 strict = TRUE,
                                 timeout = 600) {
  valid_backends <- c("auto", "python", "libreoffice", "word_com")
  if (!is.character(backend) || length(backend) != 1L || !(backend %in% valid_backends)) {
    stop(paste0("'backend' must be one of: ", paste(paste0('"', valid_backends, '"'), collapse = ", ")), call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) || timeout <= 0) {
    stop("'timeout' must be a single positive number of seconds.", call. = FALSE)
  }

  if (is.null(conversions)) {
    return(data.frame(src = character(0), dest = character(0), converted = logical(0), stringsAsFactors = FALSE))
  }
  if (!is.data.frame(conversions)) {
    stop("'conversions' must be a data frame with columns 'src' and 'dest'.", call. = FALSE)
  }
  if (!all(c("src", "dest") %in% names(conversions))) {
    stop("'conversions' data frame must contain 'src' and 'dest' columns.", call. = FALSE)
  }
  if (!nrow(conversions)) {
    res <- conversions
    res$converted <- logical(0)
    return(res)
  }

  # The caller's input is checked first, so a bad row is reported the same way
  # on every machine, whatever converters happen to be installed. Only rows
  # that pass are ever handed to a backend.
  checked <- .validate_conversions(conversions, strict = strict)
  todo <- which(checked$valid)
  converted <- logical(nrow(conversions))

  state <- NULL
  if (length(todo)) {
    state <- check_docx_toolchain(quiet = TRUE)
    if (identical(backend, "auto")) {
      backend <- state$backend
    }

    if (is.na(backend)) {
      if (isTRUE(strict)) {
        .toolchain_error(state)
      }
      warning(
        "No PDF -> DOCX backend available; skipping ", length(todo),
        " DOCX output(s). See TempleCBE::check_docx_toolchain().",
        call. = FALSE
      )
      return(cbind(conversions, converted = FALSE))
    }

    message("PDF -> DOCX backend: ", backend, " (", length(todo), " file(s))")
  }

  for (i in todo) {
    pdf <- checked$pdf[i]
    docx <- checked$docx[i]

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
      python      = .convert_python(state$python_with_pdf2docx, pdf, docx, timeout = timeout),
      libreoffice = .convert_soffice(state$soffice, pdf, docx, timeout = timeout),
      word_com    = .convert_word_com(pdf, docx, timeout = timeout),
      FALSE
    )

    # One documented fallback hop, not a silent cascade.
    if (!ok && identical(backend, "python") && !is.null(state$soffice)) {
      warning("pdf2docx failed on ", basename(pdf), "; retrying with LibreOffice.", call. = FALSE)
      ok <- .convert_soffice(state$soffice, pdf, docx, timeout = timeout)
    }

    if (!ok) {
      warning("Conversion failed: ", basename(pdf), call. = FALSE)
    } else if (has_temp && !isTRUE(file.copy(from = docx, to = temp_dest, overwrite = TRUE))) {
      warning("Converted ", basename(pdf), " but could not copy the DOCX to temp_dest '", temp_dest, "'.", call. = FALSE)
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
  if (!is.character(src) || length(src) != 1L || is.na(src) || !nzchar(trimws(src))) {
    stop("'src' must be a single non-empty file path.", call. = FALSE)
  }
  if (!is.character(dest) || length(dest) != 1L || is.na(dest) || !nzchar(trimws(dest))) {
    stop("'dest' must be a single non-empty file path.", call. = FALSE)
  }

  res <- convert_pdfs_to_docx(
    conversions = data.frame(src = src, dest = dest, stringsAsFactors = FALSE),
    backend = backend,
    strict = strict,
    timeout = timeout
  )

  if (isTRUE(res$converted[1])) invisible(dest) else invisible(FALSE)
}
