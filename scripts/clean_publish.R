#!/usr/bin/env Rscript
# ==============================================================================
# scripts/clean_publish.R
# ------------------------------------------------------------------------------
# CLI entrypoint for TempleCBE::clean_publish() -- a thin wrapper. The command
# line itself (the options, the exit statuses, every check before a push) is the
# function clean_publish_cli() in R/clean_publish.R, which is installed with the
# package and tested in-process; this file is build-ignored, so nothing in it
# could be tested under R CMD check. It loads the dev copy of the package (the
# one this script sits in, wherever it is started from) so this always runs
# against the current working tree, not whatever is installed.
#
# Squashes the tree of HEAD into one parentless commit on the publish branch
# (standalone mode) or into a fast-forward commit on the remote tip (--snapshot),
# while preserving complete history on the local "private-history" branch. This
# is a DRY RUN unless --push is given: only then is the clean commit pushed to a
# remote, which must be named with --remote (there is deliberately no default:
# in a clone of the private repository "origin" is the private repository).
#
# Usage: Rscript scripts/clean_publish.R [options]   (--help lists them)
# Exit status: 0 success, 1 the run stopped (error, or a warning before the
# push), 2 bad command line.
# ==============================================================================

if (sys.nframe() == 0L && !interactive()) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Package 'pkgload' (installed with 'devtools') is required to run this script.")
  }
  script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
  pkg_root <- if (is.na(script_file)) "." else dirname(dirname(normalizePath(script_file, winslash = "/")))
  pkgload::load_all(pkg_root, quiet = TRUE)
  quit(save = "no", status = clean_publish_cli())
}
