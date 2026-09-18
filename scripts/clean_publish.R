#!/usr/bin/env Rscript
# ==============================================================================
# scripts/clean_publish.R
# ------------------------------------------------------------------------------
# CLI entrypoint for TempleCBE::clean_publish() -- see R/clean_publish.R for
# the actual implementation. Loads the dev copy of the package so this always
# runs against the current working tree, not whatever is installed.
#
# Squashes the full local history into a single commit and force-pushes it to
# a remote (default "origin"), while preserving complete history on the local
# "private-history" branch. Pass --remote to target a different configured
# remote (e.g. a second MED-CBE-hosted mirror) without duplicating this logic.
#
# Usage: Rscript scripts/clean_publish.R [options]
# ==============================================================================

if (sys.nframe() == 0L && !interactive()) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Package 'devtools' is required to run this script.")
  }
  devtools::load_all(quiet = TRUE)

  args <- commandArgs(trailingOnly = TRUE)

  if ("--help" %in% args || "-h" %in% args) {
    cat("Usage: Rscript scripts/clean_publish.R [options]\n\n",
        "Options:\n",
        "  --branch <NAME>       Branch to overwrite with the clean commit\n",
        "                        (default: current branch, or master/main)\n",
        "  --private <NAME>      Local branch retaining full history (default: private-history)\n",
        "  --remote <NAME>       Remote to force-push to (default: origin)\n",
        "  -m, --message <MSG>   Commit message for the clean commit\n",
        "  --no-push             Rewrite branches locally but skip the force-push\n",
        "  -h, --help            Show this help message\n\n")
    quit(status = 0)
  }

  get_arg_val <- function(flag) {
    idx <- match(flag, args)
    if (!is.na(idx) && idx < length(args)) args[idx + 1] else NULL
  }

  cli_branch <- get_arg_val("--branch")
  cli_private <- get_arg_val("--private")
  cli_remote <- get_arg_val("--remote")
  cli_msg <- get_arg_val("--message")
  if (is.null(cli_msg)) cli_msg <- get_arg_val("-m")

  clean_publish(
    publish_branch = cli_branch,
    private_branch = if (is.null(cli_private)) "private-history" else cli_private,
    remote = if (is.null(cli_remote)) "origin" else cli_remote,
    commit_msg = if (is.null(cli_msg)) "Initial clean commit" else cli_msg,
    push = !("--no-push" %in% args)
  )
}
