# Every test in this file runs the same way on a machine with no converter at
# all (GitHub's Linux and macOS runners) and on one that has Python, LibreOffice
# or Word. A test either needs no toolchain (it checks the caller's input, and
# a tripwire fails it if the toolchain is probed), or it mocks the toolchain and
# the backends, or it needs a real tool and is gated by skip_if_no_python(),
# skip_if_no_soffice() or skip_if_no_word_com() (the Tier 2 tests at the end).

# What check_docx_toolchain() reports on a machine with no converter.
no_backend_state <- function() {
  list(
    python_with_pdf2docx = NULL,
    python_any = NULL,
    soffice = NULL,
    word_com = FALSE,
    backend = NA_character_
  )
}

# What it reports when python + pdf2docx is usable. The paths are fake: a test
# that uses this state also mocks .convert_python() and .convert_soffice().
python_backend_state <- function(soffice = "fake_soffice") {
  list(
    python_with_pdf2docx = "fake_python",
    python_any = "fake_python",
    soffice = soffice,
    word_com = FALSE,
    backend = "python"
  )
}

# Make check_docx_toolchain() report `state` for the rest of the test.
local_toolchain <- function(state, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    check_docx_toolchain = function(...) state,
    .env = .env
  )
}

# Tripwire: fail the test if anything looks for a backend. Used by tests whose
# outcome must not depend on what is installed.
local_no_toolchain_probe <- function(.env = parent.frame()) {
  testthat::local_mocked_bindings(
    check_docx_toolchain = function(...) {
      stop("check_docx_toolchain() was called before the input was validated.", call. = FALSE)
    },
    .env = .env
  )
}

# A non-empty placeholder PDF in the test's own temp folder. The content is not
# a real PDF: only backends read it, and they are mocked.
local_fake_pdf <- function(name = "input.pdf", dir = withr::local_tempdir(.local_envir = .env), .env = parent.frame()) {
  path <- file.path(dir, name)
  writeLines("%PDF-1.4 placeholder", path)
  path
}

test_that("convert_pdfs_to_docx validates argument schemas and types", {
  local_no_toolchain_probe()

  expect_error(
    convert_pdfs_to_docx(data.frame(src = "a.pdf", dest = "a.docx"), backend = "unsupported"),
    "'backend' must be one of"
  )
  expect_error(
    convert_pdfs_to_docx(data.frame(src = "a.pdf", dest = "a.docx"), backend = 42),
    "'backend' must be one of"
  )
  expect_error(
    convert_pdfs_to_docx(data.frame(src = "a.pdf", dest = "a.docx"), timeout = 0),
    "'timeout' must be a single positive number of seconds."
  )
  expect_error(
    convert_pdfs_to_docx(data.frame(src = "a.pdf", dest = "a.docx"), timeout = -10),
    "'timeout' must be a single positive number of seconds."
  )
  expect_error(
    convert_pdfs_to_docx(data.frame(src = "a.pdf", dest = "a.docx"), timeout = "slow"),
    "'timeout' must be a single positive number of seconds."
  )
  expect_error(
    convert_pdfs_to_docx("not_a_df"),
    "'conversions' must be a data frame with columns 'src' and 'dest'."
  )
  expect_error(
    convert_pdfs_to_docx(data.frame(x = 1, y = 2)),
    "'conversions' data frame must contain 'src' and 'dest' columns."
  )

  # NULL conversions returns empty data frame schema with converted logical column
  empty_null <- convert_pdfs_to_docx(NULL)
  expect_s3_class(empty_null, "data.frame")
  expect_equal(nrow(empty_null), 0)
  expect_named(empty_null, c("src", "dest", "converted"))
  expect_type(empty_null$converted, "logical")
})

test_that("convert_pdf_to_docx validates destination and source inputs", {
  local_no_toolchain_probe()

  expect_error(convert_pdf_to_docx(character(0)), "'src' must be a single non-empty file path.")
  expect_error(convert_pdf_to_docx(c("a.pdf", "b.pdf")), "'src' must be a single non-empty file path.")
  expect_error(convert_pdf_to_docx(""), "'src' must be a single non-empty file path.")
  expect_error(convert_pdf_to_docx(NA_character_), "'src' must be a single non-empty file path.")
  expect_error(convert_pdf_to_docx("input.pdf", dest = character(0)), "'dest' must be a single non-empty file path.")
  expect_error(convert_pdf_to_docx("input.pdf", dest = ""), "'dest' must be a single non-empty file path.")
  expect_error(convert_pdf_to_docx("input.pdf", dest = NA_character_), "'dest' must be a single non-empty file path.")
})

test_that("convert_pdfs_to_docx rejects non-existent or empty source files", {
  local_no_toolchain_probe()
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "out.docx")
  missing_pdf <- file.path(dir, "non_existent_file.pdf")

  # Non-existent source
  expect_error(
    convert_pdfs_to_docx(data.frame(src = missing_pdf, dest = dest), strict = TRUE),
    paste0("Source PDF file '", missing_pdf, "' does not exist."),
    fixed = TRUE
  )
  w <- testthat::capture_warnings(
    res_missing <- convert_pdfs_to_docx(data.frame(src = missing_pdf, dest = dest), strict = FALSE)
  )
  expect_length(w, 2L)
  expect_match(w[1], paste0("Source PDF file '", missing_pdf, "' does not exist; skipping conversion."), fixed = TRUE)
  expect_match(w[2], "1 of 1 PDF -> DOCX conversion(s) failed.", fixed = TRUE)
  expect_false(res_missing$converted[1])

  # Empty (0-byte) source file
  empty_pdf <- file.path(dir, "empty.pdf")
  file.create(empty_pdf)
  expect_equal(file.size(empty_pdf), 0)

  expect_error(
    convert_pdfs_to_docx(data.frame(src = empty_pdf, dest = dest), strict = TRUE),
    "is empty \\(0 bytes\\)."
  )
  w <- testthat::capture_warnings(
    res_empty <- convert_pdfs_to_docx(data.frame(src = empty_pdf, dest = dest), strict = FALSE)
  )
  expect_length(w, 2L)
  expect_match(w[1], "is empty (0 bytes); skipping conversion.", fixed = TRUE)
  expect_match(w[2], "1 of 1 PDF -> DOCX conversion(s) failed.", fixed = TRUE)
  expect_false(res_empty$converted[1])
})

test_that("convert_pdfs_to_docx rejects a missing or empty destination", {
  local_no_toolchain_probe()
  pdf <- local_fake_pdf()

  for (bad_dest in list(NA_character_, "", "   ")) {
    conversions <- data.frame(src = pdf, dest = bad_dest, stringsAsFactors = FALSE)
    expect_error(
      convert_pdfs_to_docx(conversions, strict = TRUE),
      "Destination DOCX path for source PDF .* is missing or empty."
    )
    w <- testthat::capture_warnings(res <- convert_pdfs_to_docx(conversions, strict = FALSE))
    expect_match(w[1], "is missing or empty; skipping conversion.", fixed = TRUE)
    expect_false(res$converted[1])
  }
})

test_that("convert_pdfs_to_docx rejects identical source and destination paths", {
  local_no_toolchain_probe()
  pdf <- local_fake_pdf()

  expect_error(
    convert_pdfs_to_docx(data.frame(src = pdf, dest = pdf)),
    "'src' and 'dest' cannot point to the same file"
  )
  # Overwriting the PDF is a caller bug, not a file to skip: strict = FALSE stops too.
  expect_error(
    convert_pdfs_to_docx(data.frame(src = pdf, dest = pdf), strict = FALSE),
    "'src' and 'dest' cannot point to the same file"
  )
})

test_that("temp_dest can no longer overwrite the source PDF, however it is spelled", {
  # The check is part of the input validation, so no converter is involved.
  local_no_toolchain_probe()
  dir <- withr::local_tempdir()
  pdf <- local_fake_pdf(dir = dir)
  dest <- file.path(dir, "out.docx")
  original <- readLines(pdf)

  same_file_spellings <- list(
    pdf,
    file.path(dir, ".", basename(pdf)),
    file.path(dir, "sub", "..", basename(pdf)),
    file.path(dir, toupper(basename(pdf)))   # a different case: the same file on Windows and macOS
  )
  for (temp_dest in same_file_spellings) {
    conversions <- data.frame(src = pdf, dest = dest, temp_dest = temp_dest, stringsAsFactors = FALSE)
    # Refused in both modes (the PDF would be overwritten), before anything is converted.
    for (strict in c(TRUE, FALSE)) {
      err <- tryCatch(convert_pdfs_to_docx(conversions, strict = strict), error = function(e) conditionMessage(e))
      # On a case-sensitive file system the upper-case name is another file, but still a .pdf path.
      expect_match(err, "'src' and 'temp_dest' cannot point to the same file|has a .pdf extension", info = temp_dest)
    }
  }
  expect_equal(readLines(pdf), original)
  expect_false(file.exists(dest))

  # temp_dest is the same file as dest: the copy would empty the converted file.
  expect_error(
    convert_pdfs_to_docx(data.frame(src = pdf, dest = dest, temp_dest = file.path(dir, ".", "out.docx"), stringsAsFactors = FALSE)),
    "'dest' and 'temp_dest' cannot point to the same file"
  )
})

test_that("a dest or temp_dest that ends in .pdf is a bad row", {
  local_no_toolchain_probe()
  dir <- withr::local_tempdir()
  pdf <- local_fake_pdf(dir = dir)
  other <- local_fake_pdf("other.pdf", dir = dir)
  original <- readLines(other)
  good_dest <- file.path(dir, "out.docx")

  bad <- list(
    data.frame(src = pdf, dest = good_dest, temp_dest = other, stringsAsFactors = FALSE),
    data.frame(src = pdf, dest = other, stringsAsFactors = FALSE)
  )
  for (conversions in bad) {
    expect_error(convert_pdfs_to_docx(conversions), "has a .pdf extension")
    w <- testthat::capture_warnings(res <- convert_pdfs_to_docx(conversions, strict = FALSE))
    expect_match(w[1], "has a .pdf extension.*; skipping conversion.")
    expect_false(res$converted[1])
  }
  expect_equal(readLines(other), original)
})

test_that("a temp_dest copy that fails is reported, and the conversion still counts", {
  dir <- withr::local_tempdir()
  pdf <- local_fake_pdf(dir = dir)
  dest <- file.path(dir, "out.docx")
  not_a_folder <- file.path(dir, "plain_file")
  writeLines("x", not_a_folder)
  local_toolchain(python_backend_state())
  testthat::local_mocked_bindings(
    .convert_python = function(py, in_pdf, out_docx, timeout) {
      writeLines("fake docx", out_docx)
      TRUE
    }
  )

  w <- testthat::capture_warnings(
    res <- suppressMessages(convert_pdfs_to_docx(
      data.frame(src = pdf, dest = dest, temp_dest = file.path(not_a_folder, "copy.docx"), stringsAsFactors = FALSE)
    ))
  )
  expect_true(res$converted[1])
  expect_true(file.exists(dest))
  expect_match(w, "could not copy the DOCX to temp_dest", all = FALSE)
})

test_that("invalid input is reported even when no backend exists", {
  # The regression this guards: the toolchain used to be probed first, so on a
  # machine with no converter every one of these reported "No PDF -> DOCX
  # backend" instead of the problem with the input.
  local_toolchain(no_backend_state())
  dir <- withr::local_tempdir()
  good <- local_fake_pdf(dir = dir)
  dest <- file.path(dir, "out.docx")
  missing_pdf <- file.path(dir, "missing.pdf")
  empty_pdf <- file.path(dir, "empty.pdf")
  file.create(empty_pdf)

  expect_error(
    convert_pdfs_to_docx(data.frame(src = missing_pdf, dest = dest)),
    "does not exist"
  )
  expect_error(
    convert_pdfs_to_docx(data.frame(src = empty_pdf, dest = dest)),
    "is empty \\(0 bytes\\)"
  )
  expect_error(
    convert_pdfs_to_docx(data.frame(src = good, dest = good)),
    "cannot point to the same file"
  )
  expect_error(
    convert_pdfs_to_docx(data.frame(src = good, dest = NA_character_)),
    "is missing or empty"
  )
  # One bad row among good ones is still the input error, not the toolchain one.
  expect_error(
    convert_pdfs_to_docx(data.frame(src = c(good, missing_pdf), dest = c(dest, dest))),
    "does not exist"
  )

  # With strict = FALSE, rows that are all bad are skipped with their own
  # warnings and come back unconverted: no error, and no "no backend" warning.
  all_bad <- data.frame(src = c(missing_pdf, empty_pdf), dest = c(dest, dest), stringsAsFactors = FALSE)
  w <- testthat::capture_warnings(res <- convert_pdfs_to_docx(all_bad, strict = FALSE))
  expect_length(w, 3L)
  expect_match(w[1], "does not exist; skipping conversion.", fixed = TRUE)
  expect_match(w[2], "is empty (0 bytes); skipping conversion.", fixed = TRUE)
  expect_match(w[3], "2 of 2 PDF -> DOCX conversion(s) failed.", fixed = TRUE)
  expect_false(any(grepl("No PDF -> DOCX backend", w, fixed = TRUE)))
  expect_equal(res$converted, c(FALSE, FALSE))
  expect_equal(res$src, all_bad$src)

  # The single-file wrapper gets the same order.
  expect_error(convert_pdf_to_docx(missing_pdf, dest), "does not exist")
  w <- testthat::capture_warnings(ret <- convert_pdf_to_docx(missing_pdf, dest, strict = FALSE))
  expect_false(ret)
  expect_false(any(grepl("No PDF -> DOCX backend", w, fixed = TRUE)))
})

test_that("rows that are all skipped never look for a backend, whichever backend is asked for", {
  local_no_toolchain_probe()
  dir <- withr::local_tempdir()
  conversions <- data.frame(
    src = file.path(dir, c("a.pdf", "b.pdf")),
    dest = file.path(dir, c("a.docx", "b.docx")),
    stringsAsFactors = FALSE
  )

  for (backend in c("auto", "python", "libreoffice", "word_com")) {
    w <- testthat::capture_warnings(
      res <- convert_pdfs_to_docx(conversions, backend = backend, strict = FALSE)
    )
    expect_equal(res$converted, c(FALSE, FALSE), info = backend)
    expect_equal(nrow(res), 2L, info = backend)
  }
})

test_that("the whole table is checked before any file is converted", {
  dir <- withr::local_tempdir()
  good1 <- local_fake_pdf("good1.pdf", dir = dir)
  good2 <- local_fake_pdf("good2.pdf", dir = dir)
  missing_pdf <- file.path(dir, "missing.pdf")
  empty_pdf <- file.path(dir, "empty.pdf")
  file.create(empty_pdf)
  out <- function(name) file.path(dir, name)

  calls <- character()
  local_toolchain(python_backend_state())
  testthat::local_mocked_bindings(
    .convert_python = function(py, in_pdf, out_docx, timeout) {
      calls <<- c(calls, basename(in_pdf))
      writeLines("fake docx", out_docx)
      TRUE
    }
  )

  # strict = TRUE: a bad row at the end stops the run before row 1 is converted.
  expect_error(
    suppressMessages(convert_pdfs_to_docx(
      data.frame(src = c(good1, missing_pdf), dest = c(out("good1.docx"), out("missing.docx"))),
      strict = TRUE
    )),
    "does not exist"
  )
  expect_error(
    suppressMessages(convert_pdfs_to_docx(
      data.frame(src = c(good1, good2), dest = c(out("good1.docx"), good2)),
      strict = FALSE
    )),
    "cannot point to the same file"
  )
  expect_length(calls, 0L)
  expect_false(file.exists(out("good1.docx")))

  # strict = FALSE: bad rows are skipped, good rows are converted, order is kept.
  conversions <- data.frame(
    src = c(good1, missing_pdf, empty_pdf, good2),
    dest = out(c("good1.docx", "missing.docx", "empty.docx", "good2.docx")),
    stringsAsFactors = FALSE
  )
  w <- testthat::capture_warnings(res <- suppressMessages(convert_pdfs_to_docx(conversions, strict = FALSE)))
  expect_equal(res$converted, c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(calls, c("good1.pdf", "good2.pdf"))
  expect_true(all(file.exists(out(c("good1.docx", "good2.docx")))))
  expect_false(any(file.exists(out(c("missing.docx", "empty.docx")))))
  expect_length(w, 3L)
  expect_match(w[3], "2 of 4 PDF -> DOCX conversion(s) failed.", fixed = TRUE)
})

test_that("convert_pdfs_to_docx handles missing toolchain correctly in strict vs non-strict mode", {
  pdf <- local_fake_pdf()
  dest <- withr::local_tempfile(fileext = ".docx")
  local_toolchain(no_backend_state())

  # strict = TRUE errors with toolchain installation advice
  expect_error(
    convert_pdfs_to_docx(data.frame(src = pdf, dest = dest), strict = TRUE),
    "No PDF -> DOCX backend is available on this machine."
  )

  # strict = FALSE warns and returns converted = FALSE
  expect_warning(
    res <- convert_pdfs_to_docx(data.frame(src = pdf, dest = dest), strict = FALSE),
    "No PDF -> DOCX backend available; skipping 1 DOCX output\\(s\\)."
  )
  expect_false(res$converted[1])

  # The single-file wrapper does the same.
  expect_error(convert_pdf_to_docx(pdf, dest), "No PDF -> DOCX backend is available on this machine.")
  expect_warning(
    ret <- convert_pdf_to_docx(pdf, dest, strict = FALSE),
    "No PDF -> DOCX backend available"
  )
  expect_false(ret)
})

test_that("a run with some bad rows and no backend reports both, and converts nothing", {
  dir <- withr::local_tempdir()
  good <- local_fake_pdf(dir = dir)
  missing_pdf <- file.path(dir, "missing.pdf")
  local_toolchain(no_backend_state())

  conversions <- data.frame(
    src = c(good, missing_pdf),
    dest = file.path(dir, c("good.docx", "missing.docx")),
    stringsAsFactors = FALSE
  )
  w <- testthat::capture_warnings(res <- convert_pdfs_to_docx(conversions, strict = FALSE))
  expect_length(w, 2L)
  expect_match(w[1], "does not exist; skipping conversion.", fixed = TRUE)
  # Only the row that was a candidate for conversion is counted as skipped here.
  expect_match(w[2], "skipping 1 DOCX output(s).", fixed = TRUE)
  expect_equal(res$converted, c(FALSE, FALSE))
  expect_equal(nrow(res), 2L)
})

test_that("mocked backend execution handles success, temp_dest copy, and single wrapper", {
  pdf <- local_fake_pdf()
  dest <- withr::local_tempfile(fileext = ".docx")
  temp_dest <- withr::local_tempfile(fileext = ".docx")

  local_toolchain(python_backend_state())
  testthat::local_mocked_bindings(
    .convert_python = function(py, in_pdf, out_docx, timeout) {
      writeLines("fake docx generated by python", out_docx)
      TRUE
    }
  )

  conversions <- data.frame(src = pdf, dest = dest, temp_dest = temp_dest, stringsAsFactors = FALSE)
  res <- suppressMessages(convert_pdfs_to_docx(conversions))

  expect_true(res$converted[1])
  expect_true(file.exists(dest))
  expect_true(file.exists(temp_dest))

  # Single wrapper success returns destination path
  single_dest <- withr::local_tempfile(fileext = ".docx")
  ret <- suppressMessages(convert_pdf_to_docx(pdf, dest = single_dest))
  expect_equal(ret, single_dest)
})

test_that("mocked backend execution retries with LibreOffice fallback when Python fails", {
  pdf <- local_fake_pdf()
  dest <- withr::local_tempfile(fileext = ".docx")

  local_toolchain(python_backend_state())
  testthat::local_mocked_bindings(
    .convert_python = function(py, in_pdf, out_docx, timeout) FALSE,
    .convert_soffice = function(soffice, in_pdf, out_docx, timeout) {
      writeLines("fake docx generated by libreoffice", out_docx)
      TRUE
    }
  )

  # Python fails -> warning about retry with LibreOffice -> succeeds
  expect_warning(
    res <- suppressMessages(convert_pdfs_to_docx(data.frame(src = pdf, dest = dest))),
    "pdf2docx failed on .* retrying with LibreOffice."
  )
  expect_true(res$converted[1])
  expect_true(file.exists(dest))
})

test_that("mocked backend failure emits warnings and returns converted = FALSE", {
  pdf <- local_fake_pdf()
  dest <- withr::local_tempfile(fileext = ".docx")

  local_toolchain(python_backend_state(soffice = NULL))
  testthat::local_mocked_bindings(
    .convert_python = function(py, in_pdf, out_docx, timeout) FALSE
  )

  # Python fails without LibreOffice fallback available
  expect_warning(
    expect_warning(
      res <- suppressMessages(convert_pdfs_to_docx(data.frame(src = pdf, dest = dest))),
      "Conversion failed:"
    ),
    "1 of 1 PDF -> DOCX conversion\\(s\\) failed."
  )
  expect_false(res$converted[1])

  # Single wrapper returns invisible(FALSE) on failure
  expect_warning(
    expect_warning(
      single_ret <- suppressMessages(convert_pdf_to_docx(pdf, dest = dest)),
      "Conversion failed:"
    ),
    "1 of 1 PDF -> DOCX conversion\\(s\\) failed."
  )
  expect_false(single_ret)
})

test_that("mocked Word COM backend can be explicitly selected", {
  pdf <- local_fake_pdf()
  dest <- withr::local_tempfile(fileext = ".docx")

  local_toolchain(list(
    python_with_pdf2docx = NULL,
    python_any = NULL,
    soffice = NULL,
    word_com = TRUE,
    backend = "word_com"
  ))
  testthat::local_mocked_bindings(
    .convert_word_com = function(in_pdf, out_docx, timeout) {
      writeLines("word com docx", out_docx)
      TRUE
    }
  )

  res <- suppressMessages(convert_pdfs_to_docx(data.frame(src = pdf, dest = dest), backend = "word_com"))
  expect_true(res$converted[1])
  expect_true(file.exists(dest))
})

test_that("convert_pdfs_to_docx returns an empty result for no conversions", {
  local_no_toolchain_probe()
  res <- convert_pdfs_to_docx(data.frame(src = character(0), dest = character(0)))
  expect_equal(nrow(res), 0)
  expect_true("converted" %in% names(res))
})

test_that("check_docx_toolchain reports every backend without converting", {
  # Real discovery, so the answer depends on the machine; the shape must not.
  state <- check_docx_toolchain(quiet = TRUE)
  expect_named(state, c("python_with_pdf2docx", "python_any", "soffice", "word_com", "backend"))
  expect_type(state$word_com, "logical")
  if (.Platform$OS.type != "windows") expect_false(state$word_com)
  expect_true(is.na(state$backend) || state$backend %in% c("python", "libreoffice", "word_com"))
  # A backend is reported only when the tool behind it was found.
  if (identical(state$backend, "python")) expect_false(is.null(state$python_with_pdf2docx))
  if (identical(state$backend, "libreoffice")) expect_false(is.null(state$soffice))
  if (identical(state$backend, "word_com")) expect_true(state$word_com)
  if (is.na(state$backend)) {
    expect_null(state$python_with_pdf2docx)
    expect_null(state$soffice)
    expect_false(state$word_com)
  }
})

test_that("check_docx_toolchain prefers python, then LibreOffice, then Word COM", {
  # Discovery is mocked, so this holds on any machine.
  found <- new.env()
  testthat::local_mocked_bindings(
    find_python = function(verify = TRUE, module = "pdf2docx") if (verify) found$py else found$py_any,
    find_soffice = function() found$soffice,
    .word_com_available = function() found$com
  )
  backend_when <- function(py = NULL, py_any = py, soffice = NULL, com = FALSE) {
    found$py <- py
    found$py_any <- py_any
    found$soffice <- soffice
    found$com <- com
    check_docx_toolchain(quiet = TRUE)$backend
  }

  expect_equal(backend_when(py = "py", soffice = "soffice", com = TRUE), "python")
  # Python exists but cannot import pdf2docx: LibreOffice is next.
  expect_equal(backend_when(py = NULL, py_any = "py", soffice = "soffice", com = TRUE), "libreoffice")
  expect_equal(backend_when(com = TRUE), "word_com")
  expect_true(is.na(backend_when()))
})

test_that("find_soffice honours the templecbe.soffice option", {
  soffice <- withr::local_tempfile()
  file.create(soffice)
  withr::local_options(templecbe.soffice = soffice)
  expect_equal(find_soffice(), soffice)
})

# A stand-in interpreter that is Python 3 for `--version` and can import every
# module except `unimportable`.
fake_python <- function(unimportable, envir = parent.frame()) {
  windows <- .Platform$OS.type == "windows"
  py <- withr::local_tempfile(fileext = if (windows) ".bat" else ".sh", .local_envir = envir)
  writeLines(
    if (windows) {
      c(
        "@echo off",
        "if \"%~1\"==\"--version\" (echo Python 3.11.0& exit /b 0)",
        paste0("echo %* | findstr /c:\"", unimportable, "\" >nul && exit /b 1"),
        "exit /b 0"
      )
    } else {
      c(
        "#!/bin/sh",
        "case \"$1\" in --version) echo \"Python 3.11.0\"; exit 0;; esac",
        paste0("case \"$*\" in *", unimportable, "*) exit 1;; esac"),
        "exit 0"
      )
    },
    py
  )
  Sys.chmod(py, "0755")
  py
}

test_that("find_python(module =) returns an interpreter that can import that module", {
  py <- fake_python(unimportable = "templecbe_absent_module")
  withr::local_options(templecbe.python = py)
  expect_equal(find_python(module = "templecbe_present_module"), py)
  expect_equal(find_python(verify = FALSE), py)
  # Falls through to whatever else is installed, none of which has the module.
  expect_null(find_python(module = "templecbe_absent_module"))
})

test_that("find_python only takes a plain module name", {
  expect_error(find_python(module = "os; import sys"), "single Python module name")
  expect_error(find_python(module = c("numpy", "scipy")), "single Python module name")
  expect_error(find_python(module = NA_character_), "single Python module name")
  expect_error(find_python(module = ""), "single Python module name")
})

# A stand-in for the pdf2docx interpreter (kind = "python": it is called as
# `<py> script.py in.pdf out.docx`) or for soffice (kind = "soffice": it is
# called with `--outdir <dir>` and the PDF last, and writes <pdf stem>.docx into
# the folder). `write` is what it leaves at the output path: a file ("content"),
# an empty file ("empty") or nothing ("none"); `exit` is its exit status;
# `sleep` makes it do nothing but wait that many seconds. Real executables, so
# the whole process path (quoting, exit status, timeout) runs, but no converter
# is needed.
fake_converter <- function(kind = c("python", "soffice"),
                           write = c("content", "none", "empty"),
                           exit = 0L,
                           sleep = 0,
                           envir = parent.frame()) {
  kind <- match.arg(kind)
  write <- match.arg(write)
  windows <- .Platform$OS.type == "windows"
  path <- withr::local_tempfile(fileext = if (windows) ".cmd" else ".sh", .local_envir = envir)

  if (windows) {
    out <- if (kind == "python") '"%~3"' else '"%~6\\%~n7.docx"'
    lines <- c(
      "@echo off",
      if (kind == "soffice") c("shift", "shift", "shift"),
      if (sleep > 0) paste0("ping -n ", ceiling(sleep) + 1, " 127.0.0.1 >nul"),
      if (sleep == 0 && write == "content") paste0("echo FAKE-DOCX>", out),
      if (sleep == 0 && write == "empty") paste0("type nul>", out),
      paste0("exit /b ", exit)
    )
  } else {
    out <- if (kind == "python") '"$3"' else '"$outdir/$stem.docx"'
    lines <- c(
      "#!/bin/sh",
      if (kind == "soffice") c(
        "outdir=''; prev=''",
        "for a in \"$@\"; do",
        "  if [ \"$prev\" = \"--outdir\" ]; then outdir=\"$a\"; fi",
        "  prev=\"$a\"; last=\"$a\"",
        "done",
        "stem=$(basename \"$last\" .pdf)"
      ),
      if (sleep > 0) paste0("exec sleep ", sleep),
      if (sleep == 0 && write == "content") paste0("printf 'FAKE-DOCX\\n' > ", out),
      if (sleep == 0 && write == "empty") paste0(": > ", out),
      paste0("exit ", exit)
    )
  }
  writeLines(lines, path)
  Sys.chmod(path, "0755")
  path
}

# What is in `dir`, hidden files included.
dir_listing <- function(dir) list.files(dir, all.files = TRUE, no.. = TRUE)

test_that("a failed python conversion leaves dest alone and a stale DOCX is never reported as converted", {
  dir <- withr::local_tempdir()
  pdf <- local_fake_pdf(dir = dir)
  dest <- file.path(dir, "out.docx")
  writeLines("STALE-OLD-DOCX", dest)
  untouched <- c(basename(pdf), "out.docx")

  # Exits non-zero and writes nothing.
  expect_false(.convert_python(fake_converter("python", write = "none", exit = 1L), pdf, dest))
  # Exits 0 but writes nothing, or only an empty file: dest still exists, but it is not a conversion.
  expect_false(.convert_python(fake_converter("python", write = "none"), pdf, dest))
  expect_false(.convert_python(fake_converter("python", write = "empty"), pdf, dest))
  # Crashes after writing a (partial) file: exit status counts, and the partial file is discarded.
  expect_false(.convert_python(fake_converter("python", write = "content", exit = 1L), pdf, dest))

  expect_equal(readLines(dest), "STALE-OLD-DOCX")
  expect_setequal(dir_listing(dir), untouched)

  # With no DOCX at dest to begin with, the crash leaves nothing behind either.
  unlink(dest)
  expect_false(.convert_python(fake_converter("python", write = "content", exit = 1L), pdf, dest))
  expect_equal(dir_listing(dir), basename(pdf))
})

test_that("a successful python conversion replaces an older DOCX and leaves no scratch file", {
  dir <- withr::local_tempdir()
  pdf <- local_fake_pdf(dir = dir)
  dest <- file.path(dir, "out.docx")
  writeLines("STALE-OLD-DOCX", dest)

  expect_true(.convert_python(fake_converter("python"), pdf, dest))
  expect_equal(readLines(dest), "FAKE-DOCX")
  expect_setequal(dir_listing(dir), c(basename(pdf), "out.docx"))

  # The destination folder is created when it is missing.
  nested <- file.path(dir, "new", "folder", "out.docx")
  expect_true(.convert_python(fake_converter("python"), pdf, nested))
  expect_equal(readLines(nested), "FAKE-DOCX")
})

test_that("a failed LibreOffice conversion is not satisfied by the DOCX that is already at dest", {
  dir <- withr::local_tempdir()
  pdf <- local_fake_pdf("report.pdf", dir = dir)
  # Same stem as the PDF: the very file LibreOffice would write.
  dest <- file.path(dir, "report.docx")
  writeLines("STALE-OLD-DOCX", dest)
  untouched <- c("report.pdf", "report.docx")

  # Writes nothing, with either exit status: dest exists, but it is not the result.
  expect_false(.convert_soffice(fake_converter("soffice", write = "none", exit = 1L), pdf, dest))
  expect_false(.convert_soffice(fake_converter("soffice", write = "none"), pdf, dest))
  expect_false(.convert_soffice(fake_converter("soffice", write = "empty"), pdf, dest))

  expect_equal(readLines(dest), "STALE-OLD-DOCX")
  expect_setequal(dir_listing(dir), untouched)

  unlink(dest)
  expect_false(.convert_soffice(fake_converter("soffice", write = "none", exit = 1L), pdf, dest))
  expect_equal(dir_listing(dir), "report.pdf")
})

test_that("LibreOffice's exit status does not decide: a good file counts, nothing written does not", {
  # A first start with a fresh profile may exit non-zero after a good conversion.
  dir <- withr::local_tempdir()
  pdf <- local_fake_pdf("report.pdf", dir = dir)
  dest <- file.path(dir, "report.docx")
  writeLines("STALE-OLD-DOCX", dest)

  # Exits non-zero but wrote a good file into its private folder: converted, dest replaced.
  expect_true(.convert_soffice(fake_converter("soffice", write = "content", exit = 81L), pdf, dest))
  expect_equal(readLines(dest), "FAKE-DOCX")
  expect_setequal(dir_listing(dir), c("report.pdf", "report.docx"))

  # Exits 0 but wrote nothing: not converted, dest (the file from the run above) untouched.
  expect_false(.convert_soffice(fake_converter("soffice", write = "none", exit = 0L), pdf, dest))
  expect_equal(readLines(dest), "FAKE-DOCX")

  # With no dest to begin with, the non-zero exit still gives a conversion.
  unlink(dest)
  expect_true(.convert_soffice(fake_converter("soffice", write = "content", exit = 1L), pdf, dest))
  expect_equal(readLines(dest), "FAKE-DOCX")
  expect_setequal(dir_listing(dir), c("report.pdf", "report.docx"))
})

test_that("pdf2docx still has to exit 0, and a LibreOffice timeout is still a reported failure", {
  dir <- withr::local_tempdir()
  pdf <- local_fake_pdf("report.pdf", dir = dir)
  dest <- file.path(dir, "report.docx")

  # pdf2docx: a non-zero exit after writing is the partial-file case, not a conversion.
  expect_false(.convert_python(fake_converter("python", write = "content", exit = 1L), pdf, dest))
  expect_false(file.exists(dest))

  # LibreOffice: the exit status is ignored, but a timeout (status 124) is not.
  testthat::local_mocked_bindings(
    .run_status = function(cmd, args, ...) {
      # What a LibreOffice that was stopped by the time-out leaves: a file in its output folder and status 124.
      outdir <- args[which(args == "--outdir") + 1]
      writeLines("PARTIAL", file.path(gsub("[\"']", "", outdir), "report.docx"))
      124L
    }
  )
  w <- character()
  withCallingHandlers(
    ok <- .convert_soffice("fake_soffice", pdf, dest, timeout = 7),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") }
  )
  expect_false(ok)
  expect_match(w, "LibreOffice conversion of report.pdf timed out after 7s", fixed = TRUE)
  expect_false(file.exists(dest))
})

test_that("a successful LibreOffice conversion lands at dest, whatever dest is called", {
  dir <- withr::local_tempdir()
  pdf <- local_fake_pdf("report.pdf", dir = dir)

  same_stem <- file.path(dir, "report.docx")
  writeLines("STALE-OLD-DOCX", same_stem)
  expect_true(.convert_soffice(fake_converter("soffice"), pdf, same_stem))
  expect_equal(readLines(same_stem), "FAKE-DOCX")

  renamed <- file.path(dir, "sub", "client copy.docx")
  expect_true(.convert_soffice(fake_converter("soffice"), pdf, renamed))
  expect_equal(readLines(renamed), "FAKE-DOCX")
  # LibreOffice's own <stem>.docx was moved, not copied, and nothing else is left.
  expect_setequal(dir_listing(dir), c("report.pdf", "report.docx", "sub"))
  expect_equal(dir_listing(file.path(dir, "sub")), "client copy.docx")
})

test_that("a DOCX that cannot be moved into place is reported, not counted", {
  dir <- withr::local_tempdir()
  scratch <- file.path(dir, "scratch.docx")
  writeLines("FAKE-DOCX", scratch)
  blocked <- file.path(dir, "blocked.docx")
  dir.create(blocked)   # a directory cannot be replaced by a file

  expect_warning(ok <- .install_docx(scratch, blocked), "could not be moved")
  expect_false(ok)
  expect_true(dir.exists(blocked))
})

test_that("python crash plus a failing LibreOffice fallback is a failed row with nothing at dest", {
  dir <- withr::local_tempdir()
  pdf <- local_fake_pdf(dir = dir)
  dest <- file.path(dir, "out.docx")
  py <- fake_converter("python", write = "content", exit = 1L)
  soffice <- fake_converter("soffice", write = "none", exit = 1L)
  local_toolchain(list(
    python_with_pdf2docx = py, python_any = py, soffice = soffice,
    word_com = FALSE, backend = "python"
  ))

  w <- testthat::capture_warnings(
    res <- suppressMessages(convert_pdfs_to_docx(data.frame(src = pdf, dest = dest, stringsAsFactors = FALSE)))
  )
  expect_match(w[1], "pdf2docx failed on .* retrying with LibreOffice", all = FALSE)
  expect_match(w, "Conversion failed", all = FALSE)
  expect_false(res$converted[1])
  expect_false(file.exists(dest))
  expect_equal(dir_listing(dir), basename(pdf))
})

test_that("a stale DOCX at dest does not turn a failing LibreOffice run into converted = TRUE", {
  dir <- withr::local_tempdir()
  pdf <- local_fake_pdf("report.pdf", dir = dir)
  dest <- file.path(dir, "report.docx")
  writeLines("STALE-OLD-DOCX", dest)
  local_toolchain(list(
    python_with_pdf2docx = NULL, python_any = NULL,
    soffice = fake_converter("soffice", write = "none", exit = 1L),
    word_com = FALSE, backend = "libreoffice"
  ))

  w <- testthat::capture_warnings(
    ret <- suppressMessages(convert_pdf_to_docx(pdf, dest, backend = "libreoffice"))
  )
  expect_false(ret)
  expect_match(w, "Conversion failed", all = FALSE)
  expect_equal(readLines(dest), "STALE-OLD-DOCX")
})

# Seconds `expr` takes to run.
elapsed_secs <- function(expr) {
  t0 <- Sys.time()
  force(expr)
  as.numeric(difftime(Sys.time(), t0, units = "secs"))
}

test_that("timeout stops a hung pdf2docx and says so, leaving dest alone", {
  dir <- withr::local_tempdir()
  pdf <- local_fake_pdf(dir = dir)
  dest <- file.path(dir, "out.docx")
  writeLines("STALE-OLD-DOCX", dest)
  py <- fake_converter("python", sleep = 5)   # takes 5 s; the limit is 1 s

  w <- character()
  secs <- elapsed_secs(withCallingHandlers(
    ok <- .convert_python(py, pdf, dest, timeout = 1),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") }
  ))
  expect_false(ok)
  expect_lt(secs, 4)
  expect_match(w, "pdf2docx conversion of input.pdf timed out after 1s", fixed = TRUE)
  expect_equal(readLines(dest), "STALE-OLD-DOCX")
  expect_setequal(dir_listing(dir), c(basename(pdf), "out.docx"))
})

test_that("timeout stops a hung LibreOffice (it used to run to the end whatever timeout said)", {
  dir <- withr::local_tempdir()
  pdf <- local_fake_pdf("report.pdf", dir = dir)
  dest <- file.path(dir, "report.docx")
  soffice <- fake_converter("soffice", sleep = 5)

  w <- character()
  secs <- elapsed_secs(withCallingHandlers(
    ok <- .convert_soffice(soffice, pdf, dest, timeout = 1),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") }
  ))
  expect_false(ok)
  expect_lt(secs, 4)
  expect_match(w, "LibreOffice conversion of report.pdf timed out after 1s", fixed = TRUE)
  expect_equal(dir_listing(dir), "report.pdf")
})

test_that("convert_pdfs_to_docx passes its timeout to every backend and keeps going after a timeout", {
  dir <- withr::local_tempdir()
  hung <- local_fake_pdf("hung.pdf", dir = dir)
  fine <- local_fake_pdf("fine.pdf", dir = dir)
  # Both the interpreter and LibreOffice hang on every file.
  py <- fake_converter("python", sleep = 5)
  soffice_hangs <- fake_converter("soffice", sleep = 5)
  local_toolchain(list(
    python_with_pdf2docx = py, python_any = py, soffice = soffice_hangs,
    word_com = FALSE, backend = "python"
  ))

  conversions <- data.frame(src = c(hung, fine), dest = file.path(dir, c("hung.docx", "fine.docx")), stringsAsFactors = FALSE)
  w <- character()
  secs <- elapsed_secs(withCallingHandlers(
    res <- suppressMessages(convert_pdfs_to_docx(conversions, timeout = 1)),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") }
  ))
  # 2 files x (pdf2docx + LibreOffice retry) x 1 s, not 4 x 5 s.
  expect_lt(secs, 12)
  expect_equal(res$converted, c(FALSE, FALSE))
  expect_equal(sum(grepl("pdf2docx conversion of .* timed out after 1s", w)), 2L)
  expect_equal(sum(grepl("LibreOffice conversion of .* timed out after 1s", w)), 2L)
  # A timeout is told apart from the other failures, which keep their own messages.
  expect_match(w, "Conversion failed: hung.pdf", all = FALSE)
  expect_false(any(file.exists(file.path(dir, c("hung.docx", "fine.docx")))))
})

test_that("a hung interpreter probe is passed over instead of blocking find_python", {
  windows <- .Platform$OS.type == "windows"
  py <- withr::local_tempfile(fileext = if (windows) ".cmd" else ".sh")
  writeLines(
    if (windows) {
      c("@echo off",
        "if \"%~1\"==\"--version\" (echo Python 3.11.0& exit /b 0)",
        "ping -n 7 127.0.0.1 >nul",
        "exit /b 0")
    } else {
      c("#!/bin/sh",
        "case \"$1\" in --version) echo \"Python 3.11.0\"; exit 0;; esac",
        "exec sleep 6")
    },
    py
  )
  Sys.chmod(py, "0755")
  withr::local_options(templecbe.python = py)
  testthat::local_mocked_bindings(.probe_timeout = function() 1)

  found <- NULL
  secs <- elapsed_secs(found <- find_python(module = "templecbe_hangs_on_import"))
  # `import <module>` never finishes within the limit, so this interpreter is
  # not a candidate (it used to be reported as usable after the 6 s wait).
  expect_false(identical(found, py))
  expect_lt(secs, 5)
})

test_that("the Word COM launcher quotes every path and never kills Word by name", {
  rscript <- "C:/Program Files/R/R-4.6.0/bin/Rscript.exe"
  script <- "C:/Users/O'Brien/My Documents/run.R"
  cmd <- .word_com_command(rscript, script, 600)

  # A space in the script path: the item carries its own double quotes. An
  # apostrophe is doubled, so it cannot end the single-quoted literal.
  expect_match(cmd, "-ArgumentList '--vanilla', '\"C:/Users/O''Brien/My Documents/run.R\"'", fixed = TRUE)
  expect_match(cmd, "-FilePath 'C:/Program Files/R/R-4.6.0/bin/Rscript.exe'", fixed = TRUE)
  # PowerShell reads the typographic single quotes as apostrophes too.
  curly <- .word_com_command(rscript, "C:/Users/O\u2019Brien/run.R", 600)
  expect_match(curly, "O\u2019\u2019Brien", fixed = TRUE)
  expect_equal(.ps_quote("it's"), "'it''s'")

  # On a timeout only the process id that was started is stopped. The command
  # never names Word, and has no Stop-Process by name.
  expect_match(cmd, "Stop-Process -Id $p.Id -Force", fixed = TRUE)
  expect_false(grepl("WINWORD", cmd, ignore.case = TRUE))
  expect_false(grepl("-Name", cmd, fixed = TRUE))

  # Wait-Process takes whole seconds.
  expect_match(.word_com_command(rscript, script, 0.2), "-Timeout 1 ", fixed = TRUE)
  expect_match(.word_com_command(rscript, script, 90), "-Timeout 90 ", fixed = TRUE)
})

# Whether a process with this id is running (Windows).
pid_alive <- function(pid) {
  out <- suppressWarnings(system2("tasklist", c("/FI", shQuote(paste0("PID eq ", pid)), "/NH"), stdout = TRUE, stderr = TRUE))
  any(grepl(paste0("\\b", pid, "\\b"), out))
}

# Waits up to `secs` for `file` to exist and returns its first line.
wait_for_line <- function(file, secs = 30) {
  deadline <- Sys.time() + secs
  while (!file.exists(file) && Sys.time() < deadline) Sys.sleep(0.2)
  if (file.exists(file)) readLines(file, n = 1, warn = FALSE) else NA_character_
}

test_that("the Word COM launcher runs a script whatever its folder is called (real PowerShell and Rscript, no Word)", {
  skip_on_os(c("mac", "linux", "solaris"))
  rscript <- file.path(R.home("bin"), "Rscript.exe")
  skip_if_not(file.exists(rscript))
  skip_if(!nzchar(Sys.which("powershell")), "powershell not found")
  root <- withr::local_tempdir()

  for (name in c("plain", "with space", "it's", "a&b", "dollar$sign", "back`tick", "percent%TEMP%", "O\u2019Brien")) {
    dir <- file.path(root, name)
    expect_true(dir.create(dir), info = name)
    marker <- file.path(dir, "ran.txt")
    script <- file.path(dir, "run.R")
    writeLines(paste0("writeLines('RAN', ", encodeString(marker, quote = "\""), ")"), script)

    st <- .run_word_com(rscript, normalizePath(script), timeout = 60)
    expect_equal(st, 0L, info = name)
    expect_true(file.exists(marker), info = name)
  }
})

test_that("the Word COM launcher passes the script's own exit status on", {
  skip_on_os(c("mac", "linux", "solaris"))
  rscript <- file.path(R.home("bin"), "Rscript.exe")
  skip_if_not(file.exists(rscript))
  skip_if(!nzchar(Sys.which("powershell")), "powershell not found")
  dir <- withr::local_tempdir()

  script <- file.path(dir, "fail.R")
  writeLines("quit(status = 3L)", script)
  expect_equal(.run_word_com(rscript, normalizePath(script), timeout = 60), 3L)
  writeLines("stop('boom')", script)
  expect_equal(.run_word_com(rscript, normalizePath(script), timeout = 60), 1L)
})

test_that("a Word COM timeout stops the process that was started and nothing else", {
  skip_on_os(c("mac", "linux", "solaris"))
  rscript <- file.path(R.home("bin"), "Rscript.exe")
  skip_if_not(file.exists(rscript))
  skip_if(!nzchar(Sys.which("powershell")), "powershell not found")
  dir <- withr::local_tempdir()

  # Two scripts that record their process id and then sleep: the one the
  # launcher starts, and a bystander that is the same program (as a Word the
  # user has open is the same program as the one the conversion starts).
  sleeper <- function(name) {
    pidfile <- file.path(dir, paste0(name, ".pid"))
    script <- file.path(dir, paste0(name, ".R"))
    writeLines(c(paste0("writeLines(as.character(Sys.getpid()), ", encodeString(pidfile, quote = "\""), ")"),
                 "Sys.sleep(120)"), script)
    list(script = normalizePath(script), pidfile = pidfile)
  }
  victim <- sleeper("victim")
  bystander <- sleeper("bystander")

  system2(rscript, c("--vanilla", shQuote(bystander$script)), wait = FALSE, stdout = FALSE, stderr = FALSE)
  bystander_pid <- wait_for_line(bystander$pidfile)
  withr::defer(suppressWarnings(system2("taskkill", c("/PID", bystander_pid, "/T", "/F"), stdout = FALSE, stderr = FALSE)))
  expect_false(is.na(bystander_pid))

  secs <- elapsed_secs(st <- .run_word_com(rscript, victim$script, timeout = 6))
  expect_equal(st, 99L)
  expect_lt(secs, 40)

  victim_pid <- wait_for_line(victim$pidfile, secs = 1)
  expect_false(is.na(victim_pid))
  # Stop-Process returns before the process is gone for good: give it a moment.
  deadline <- Sys.time() + 10
  while (pid_alive(victim_pid) && Sys.time() < deadline) Sys.sleep(0.3)
  # The R process the launcher started is gone, ...
  expect_false(pid_alive(victim_pid))
  # ... and the unrelated one, the same program, is still there.
  expect_true(pid_alive(bystander_pid))
})

# Tier 2: a real conversion per backend. Each test needs its backend and
# TEMPLECBE_RUN_PDF_TESTS=true, and is skipped otherwise.
sample_pdf <- function(path, text) {
  grDevices::pdf(path, width = 6, height = 3)
  on.exit(grDevices::dev.off())
  graphics::plot.new()
  graphics::text(0.5, 0.5, text, cex = 2)
  path
}

docx_text <- function(docx) {
  dir <- withr::local_tempdir()
  utils::unzip(docx, files = "word/document.xml", exdir = dir)
  xml <- xml2::read_xml(file.path(dir, "word", "document.xml"))
  paste(xml2::xml_text(xml2::xml_find_all(xml, "//w:t", xml2::xml_ns(xml))), collapse = "")
}

expect_converts_with <- function(backend) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  pdf <- sample_pdf(file.path(dir, "sample.pdf"), "TempleCBE2026")
  docx <- file.path(dir, "sample.docx")
  # A warning here would mean the backend failed and another one took over.
  expect_no_warning(
    res <- suppressMessages(convert_pdf_to_docx(pdf, docx, backend = backend, timeout = 120))
  )
  expect_equal(res, docx)
  expect_gt(file.size(docx), 0)
  expect_match(gsub("\\s+", "", docx_text(docx)), "TempleCBE2026", fixed = TRUE)
}

test_that("the python backend converts a real PDF", {
  skip_if_no_python("pdf2docx")
  expect_converts_with("python")
})

test_that("the LibreOffice backend converts a real PDF", {
  skip_if_no_soffice()
  expect_converts_with("libreoffice")
})

test_that("the Word COM backend converts a real PDF", {
  skip_if_no_word_com()
  expect_converts_with("word_com")
})
