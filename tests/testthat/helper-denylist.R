# Helpers for the tracked-file denylist scan in test-denylist.R.
#
# TempleCBE is a public repository, so the terms that must never appear in it
# (a downstream study or PI name, a study-specific identifier column, ...) can
# not be written down in the repository either. They are read from an UNTRACKED
# file, one term per line (blank lines and lines starting with `#` are ignored;
# matching is case-insensitive and literal):
#
#   1. the file named by the TEMPLECBE_DENYLIST environment variable, else
#   2. `info/denylist` inside the git directory (`.git/info/denylist`), which git
#      never tracks or publishes.
#
# When neither exists the scan is skipped.

denylist_path <- function() {
  env <- Sys.getenv("TEMPLECBE_DENYLIST", unset = "")
  if (nzchar(env)) {
    return(if (file.exists(env)) normalizePath(env, winslash = "/") else NA_character_)
  }
  info <- git_output(c("rev-parse", "--git-path", "info/denylist"))
  if (length(info) == 1 && file.exists(info)) {
    return(normalizePath(info, winslash = "/"))
  }
  NA_character_
}

read_denylist <- function(path) {
  terms <- trimws(readLines(path, warn = FALSE, encoding = "UTF-8"))
  terms[nzchar(terms) & !startsWith(terms, "#")]
}

# stdout lines of a git command, or character(0) if git fails
git_output <- function(args, dir = ".") {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  out <- suppressWarnings(tryCatch(
    system2("git", args, stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  ))
  if (!is.null(attr(out, "status"))) character(0) else out
}

# Top-level directory of the git checkout holding this package, else NA
package_repo_root <- function() {
  root <- git_output(c("rev-parse", "--show-toplevel"))
  if (length(root) != 1) return(NA_character_)
  desc <- file.path(root, "DESCRIPTION")
  if (!file.exists(desc)) return(NA_character_)
  if (!identical(unname(read.dcf(desc, fields = "Package")[1, 1]), "TempleCBE")) return(NA_character_)
  root
}

# Tracked files (paths relative to `repo`) whose name or contents contain any of
# `terms`. Binary files are scanned as raw bytes, so text inside compressed
# containers (docx, xlsx, pdf streams) is not inspected; see scan_tracked_pdfs().
scan_tracked_files <- function(terms, repo) {
  terms <- terms[nzchar(terms)]
  if (length(terms) == 0) return(character(0))

  tracked <- git_output(c("-c", "core.quotepath=off", "ls-files"), dir = repo)
  by_name <- Reduce(
    `|`,
    lapply(terms, function(t) grepl(tolower(t), tolower(tracked), fixed = TRUE))
  )

  # Binary connection: on Windows a text-mode write would emit CRLF and git
  # would then treat the carriage return as part of every pattern.
  pattern_file <- tempfile("denylist_patterns_")
  on.exit(unlink(pattern_file), add = TRUE)
  con <- file(pattern_file, "wb")
  writeLines(enc2utf8(terms), con, sep = "\n", useBytes = TRUE)
  close(con)

  old <- setwd(repo)
  on.exit(setwd(old), add = TRUE)
  by_content <- suppressWarnings(system2(
    "git",
    c("grep", "-l", "-i", "-a", "-F", "-f", shQuote(pattern_file)),
    stdout = TRUE, stderr = FALSE
  ))
  status <- attr(by_content, "status")
  # git grep: 0 = matches found, 1 = none, anything else = it failed
  if (!is.null(status) && status != 1L) {
    stop("`git grep` failed with status ", status, " while scanning tracked files.", call. = FALSE)
  }

  sort(unique(c(tracked[by_name], by_content)))
}

# Tracked PDFs whose extracted text contains any of `terms`. PDF text streams are
# compressed, so `git grep` (scan_tracked_files) can not see inside them.
# Needs pdftools; unreadable PDFs are treated as containing nothing.
scan_tracked_pdfs <- function(terms, repo) {
  terms <- tolower(terms[nzchar(terms)])
  pdfs <- git_output(c("-c", "core.quotepath=off", "ls-files", "*.pdf"), dir = repo)
  if (length(terms) == 0 || length(pdfs) == 0) return(character(0))

  has_term <- vapply(pdfs, function(p) {
    txt <- tryCatch(
      tolower(paste(pdftools::pdf_text(file.path(repo, p)), collapse = "\n")),
      error = function(e) ""
    )
    any(vapply(terms, function(t) grepl(t, txt, fixed = TRUE), logical(1)))
  }, logical(1))
  sort(unname(pdfs[has_term]))
}
