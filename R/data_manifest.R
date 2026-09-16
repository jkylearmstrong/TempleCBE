#' Data Snapshot Manifest and Verification Utilities
#'
#' Tools to track, snapshot, and verify analytical dataset copies against upstream
#' pipeline sources using cryptographic MD5 checksums and in-memory object identity.
#' Ensures downstream reports and presentation decks never execute on stale or desynchronized data.
#'
#' @keywords internal
#' @name data_manifest
NULL

#' Read Data Manifest File
#'
#' Reads a CSV manifest file listing frozen dataset copies and their repo-relative sources.
#' Expected columns: \code{file} (filename in destination folder) and \code{source} (repo-relative path).
#'
#' @param dir Directory containing the manifest file (default: current directory).
#' @param manifest_file Name of the manifest CSV file (default: \code{"manifest.csv"}).
#' @return A data frame containing the manifest entries.
#' @export
read_data_manifest <- function(dir = ".", manifest_file = "manifest.csv") {
  path <- file.path(dir, manifest_file)
  if (!file.exists(path)) {
    stop(sprintf("Manifest file not found: %s", path), call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

#' Copy Datasets to Snapshot Directory
#'
#' Copies files listed in a manifest from their source paths to a destination directory,
#' preserving timestamps and validating contents immediately upon copy.
#'
#' @param dir Destination directory for frozen copies.
#' @param manifest Data frame with columns \code{file} and \code{source}. If NULL,
#'   read from \code{file.path(dir, "manifest.csv")}.
#' @param project_root Absolute or relative root directory for resolving \code{source} paths
#'   (default: \code{here::here()}).
#' @param overwrite Logical; whether to overwrite existing destination files (default: TRUE).
#' @return A validation tibble produced by \code{\link{validate_data_manifest}}.
#' @export
copy_data_manifest <- function(dir = ".",
                               manifest = NULL,
                               project_root = here::here(),
                               overwrite = TRUE) {
  if (is.null(manifest)) {
    manifest <- read_data_manifest(dir)
  }

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  src_paths <- file.path(project_root, manifest$source)
  dst_paths <- file.path(dir, manifest$file)

  missing_src <- src_paths[!file.exists(src_paths)]
  if (length(missing_src) > 0) {
    stop("Source file(s) not found:\n", paste(missing_src, collapse = "\n"), call. = FALSE)
  }

  file.copy(src_paths, dst_paths, overwrite = overwrite, copy.date = TRUE)
  validate_data_manifest(dir = dir, manifest = manifest, project_root = project_root)
}

#' Validate Data Snapshot Copies Against Upstream Sources
#'
#' Compares destination files against upstream source files checking:
#' 1. Existence of both source and copy.
#' 2. Cryptographic MD5 hash equality.
#' 3. Object-level equivalence via \code{identical(readRDS(), readRDS())} for \code{.rds} files.
#'
#' @param dir Destination directory containing the snapshot copies.
#' @param manifest Data frame with columns \code{file} and \code{source}.
#' @param project_root Root directory for resolving \code{source} paths (default: \code{here::here()}).
#' @return A tibble with validation status per file.
#' @export
validate_data_manifest <- function(dir = ".",
                                   manifest = NULL,
                                   project_root = here::here()) {
  if (is.null(manifest)) {
    manifest <- read_data_manifest(dir)
  }

  purrr::pmap_dfr(manifest, function(file, source, ...) {
    src <- file.path(project_root, source)
    dst <- file.path(dir, file)

    source_exists <- file.exists(src)
    copy_exists <- file.exists(dst)
    both <- source_exists && copy_exists

    md5_match <- both && unname(tools::md5sum(src) == tools::md5sum(dst))

    same_content <- if (both && grepl("\\.rds$", file, ignore.case = TRUE)) {
      identical(readRDS(src), readRDS(dst))
    } else {
      md5_match
    }

    tibble::tibble(
      file = file,
      source = source,
      source_exists = source_exists,
      copy_exists = copy_exists,
      md5_match = md5_match,
      same_content = same_content,
      valid = both && md5_match && same_content
    )
  })
}

#' Stop Execution if Data Copies are Invalid or Stale
#'
#' Halts report execution with an actionable error message if any snapshot copy
#' does not match its upstream source.
#'
#' @param validation Validation tibble returned by \code{\link{validate_data_manifest}}.
#' @param dir Destination directory name for reporting.
#' @return Invisibly returns \code{validation} if all files are valid.
#' @export
stop_if_invalid_manifest <- function(validation, dir = ".") {
  bad <- validation$file[!validation$valid]
  if (length(bad) > 0) {
    stop(
      sprintf(
        "Data copies in '%s' do not match their upstream sources: %s. Re-run copy_data_manifest() after updating sources.",
        dir, paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(validation)
}
