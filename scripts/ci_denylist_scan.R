# Denylist scan of the tracked files, for CI (R-CMD-check.yaml, job sanity-check).
#
# TempleCBE is public: no tracked file may name the private downstream study or
# PI. The terms cannot be committed, so on a runner they come from the Actions
# secret TEMPLECBE_DENYLIST_TERMS (one term per line), passed in the environment
# variable of the same name. The same scan runs locally from test-denylist.R,
# which reads `.git/info/denylist`.
#
# Without the secret (a fork's pull request, or a repository where it was never
# set) there is nothing to scan with. The script then writes a warning
# annotation and exits 0, unless TEMPLECBE_REQUIRE_DENYLIST is "true" (the
# repository variable of that name), in which case it fails.
#
# Run from the repository root: Rscript scripts/ci_denylist_scan.R

source(file.path("tests", "testthat", "helper-denylist.R"))

terms <- strsplit(Sys.getenv("TEMPLECBE_DENYLIST_TERMS", unset = ""), "\r?\n")[[1]]
terms <- trimws(terms)
terms <- terms[nzchar(terms) & !startsWith(terms, "#")]
required <- identical(tolower(Sys.getenv("TEMPLECBE_REQUIRE_DENYLIST", unset = "")), "true")

if (length(terms) == 0) {
  msg <- "Denylist scan NOT run: the secret TEMPLECBE_DENYLIST_TERMS is empty or not available to this run."
  if (required) {
    cat("::error::", msg, "\n", sep = "")
    quit(status = 1)
  }
  cat("::warning::", msg, "\n", sep = "")
  quit(status = 0)
}

root <- package_repo_root()
if (is.na(root)) {
  cat("::error::Denylist scan: not inside a TempleCBE git checkout (is the workspace a git safe.directory?)\n")
  quit(status = 1)
}

hits <- scan_tracked_files(terms, root)
if (requireNamespace("pdftools", quietly = TRUE)) {
  hits <- sort(unique(c(hits, scan_tracked_pdfs(terms, root))))
} else {
  cat("::warning::Denylist scan: pdftools is not installed, the text of tracked PDFs was not scanned.\n")
}

# Paths only: the matched terms must not be echoed into the log.
if (length(hits) > 0) {
  cat(sprintf("::error::%d tracked file(s) match a denylist term: %s\n", length(hits), paste(hits, collapse = ", ")))
  quit(status = 1)
}
cat(sprintf("Denylist scan passed: %d term(s), no tracked file matches.\n", length(terms)))
