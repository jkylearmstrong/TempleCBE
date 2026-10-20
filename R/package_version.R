#' Installed TempleCBE version
#'
#' Single source of truth for the package version recorded in run metadata,
#' reports and checkpoint provenance. Reads the installed (or `load_all()`ed)
#' package description, so it never needs editing at release time.
#'
#' @return A length-one character string, or `"unknown"` if the version cannot
#'   be resolved.
#' @noRd
templecbe_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("TempleCBE")),
    error = function(e) "unknown"
  )
}

#' Checkpoint schema version
#'
#' Part of every checkpoint fingerprint in place of the package version. Bump it
#' by hand only when a change alters what a cached checkpoint contains or means
#' (new result structure, changed algorithm or default that affects numbers).
#' Releases that do not change computed results leave it alone, so cached
#' long-running work stays valid across docs, API-alias and tooling releases.
#' The package version is still recorded in checkpoint metadata for provenance.
#'
#' @return A length-one character string.
#' @noRd
checkpoint_schema_version <- function() {
  "1"
}
