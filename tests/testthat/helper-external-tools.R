# Skips for tests that need software this package does not install.
#
# The suite has two tiers.
#   Tier 1 needs only R. Where a claim is "R agrees with SAS", the test compares
#     R with numbers SAS produced once, stored in tests/testthat/reference/.
#   Tier 2 runs the real tool and compares its output with the same reference.
#     It needs the tool AND an explicit opt-in, so a machine that happens to have
#     SAS (or LibreOffice, or Word) does not start slow jobs by accident:
#       TEMPLECBE_RUN_SAS_TESTS=true   live SAS runs           skip_if_no_sas()
#       TEMPLECBE_RUN_PDF_TESTS=true   real PDF -> DOCX runs   skip_if_no_python(),
#                                      skip_if_no_soffice(), skip_if_no_word_com()
# Every helper checks the opt-in first, then looks for the tool. None of them
# starts Python inside R: interpreters are probed in a separate process.

skip_unless_opted_in <- function(var) {
  if (!identical(tolower(Sys.getenv(var, "")), "true")) {
    testthat::skip(paste0("Set ", var, "=true to run tests that need external software"))
  }
}

skip_if_no_sas <- function() {
  skip_unless_opted_in("TEMPLECBE_RUN_SAS_TESTS")
  sas <- find_sas()
  if (is.null(sas)) {
    testthat::skip("No SAS executable found")
  }
  invisible(sas)
}

# `module` (a Python module name) must import in the interpreter that is
# returned; with `module = NULL` any working Python 3 will do. Tests that use
# the interpreter through reticulate should pin it with reticulate::use_python().
skip_if_no_python <- function(module = NULL) {
  skip_unless_opted_in("TEMPLECBE_RUN_PDF_TESTS")
  py <- if (is.null(module)) find_python(verify = FALSE) else find_python(module = module)
  if (is.null(py)) {
    testthat::skip(if (is.null(module)) {
      "No Python 3 interpreter found"
    } else {
      paste0("No Python interpreter that can import '", module, "' found")
    })
  }
  invisible(py)
}

skip_if_no_soffice <- function() {
  skip_unless_opted_in("TEMPLECBE_RUN_PDF_TESTS")
  soffice <- find_soffice()
  if (is.null(soffice)) {
    testthat::skip("No LibreOffice (soffice) found")
  }
  invisible(soffice)
}

skip_if_no_word_com <- function() {
  skip_unless_opted_in("TEMPLECBE_RUN_PDF_TESTS")
  if (!.word_com_available()) {
    testthat::skip("Word COM needs Windows and the RDCOMClient package")
  }
  # RDCOMClient is often installed without Word; ask the registry for the server.
  registered <- suppressWarnings(
    system2("reg", c("query", shQuote("HKCR\\Word.Application")), stdout = FALSE, stderr = FALSE)
  )
  if (!identical(registered, 0L)) {
    testthat::skip("Microsoft Word is not registered as a COM server")
  }
}
