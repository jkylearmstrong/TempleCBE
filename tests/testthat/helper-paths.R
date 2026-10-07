# Assert that `path` sits inside the directory `dir`, comparing canonical spellings.
#
# normalizePath() resolves symlinks (macOS /var -> /private/var, a doubled
# separator from a TMPDIR with a trailing slash) and Windows 8.3 short names
# (RUNNER~1) only for paths that exist. A file that has not been created yet keeps
# the spelling it was given while an existing directory is rewritten, so a plain
# startsWith() on the two fails on those systems and passes on Linux. Resolve the
# directory that holds `path` instead; `mustWork = TRUE` turns a directory that is
# missing into an error here rather than a silent mismatch.
expect_path_inside <- function(path, dir) {
  parent <- normalizePath(dirname(path), winslash = "/", mustWork = TRUE)
  root <- sub("/$", "", normalizePath(dir, winslash = "/", mustWork = TRUE))
  testthat::expect_true(
    startsWith(paste0(parent, "/"), paste0(root, "/")),
    info = paste0("'", parent, "' is not inside '", root, "'")
  )
}
