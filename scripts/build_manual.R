#!/usr/bin/env Rscript
# ==============================================================================
# scripts/build_manual.R
# ------------------------------------------------------------------------------
# CLI entrypoint for TempleCBE::build_manual_versioned() -- see R/build_manual_versioned.R
# for the actual implementation. Loads the dev copy of the package so this
# always builds from the current working tree, not whatever is installed.
#
# Run manually before a real release (not on every automated datetime-stamped
# build from deploy_release.R), since each call adds a new versioned PDF that
# stays in the repo permanently.
#
# Usage: Rscript scripts/build_manual.R
# ==============================================================================

if (sys.nframe() == 0L && !interactive()) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Package 'devtools' is required to run this script.")
  }
  devtools::load_all(quiet = TRUE)
  build_manual_versioned()
}
