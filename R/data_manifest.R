#' Data Snapshot Manifest and Verification Utilities
#'
#' Tools to track, snapshot, and verify analytical dataset copies against upstream
#' pipeline sources using cryptographic MD5 checksums and in-memory object identity.
#' Ensures downstream reports and presentation decks never execute on stale or desynchronized data.
#'
#' @keywords internal
#' @name data_manifest
NULL

.assert_within_root <- function(paths, root, label = "path") {
  # Both sides are resolved by .canonical_path(): normalizePath() resolves a
  # symbolic link or an 8.3 short name (macOS /var -> /private/var, a Windows
  # runner's RUNNER~1) only for a path that exists, so the folder `root` and a
  # file in it that is not there yet would otherwise be spelled differently.
  norm_root <- tolower(.canonical_path(root))
  root_slash <- if (endsWith(norm_root, "/")) norm_root else paste0(norm_root, "/")

  for (p in paths) {
    if (is.na(p) || !nzchar(p)) {
      stop(sprintf("Manifest %s cannot be empty or NA", label), call. = FALSE)
    }
    if (grepl("^[~/]|^[A-Za-z]:", p)) {
      stop(sprintf("Manifest %s must be a relative path, got absolute: '%s'", label, p), call. = FALSE)
    }
    if (grepl("(?:^|[/\\\\])\\.\\.(?:[/\\\\]|$)", p)) {
      stop(sprintf("Manifest %s must not contain '..' path segments: '%s'", label, p), call. = FALSE)
    }
    full_path <- file.path(root, p)
    norm_path <- tolower(.canonical_path(full_path))
    if (!startsWith(norm_path, root_slash) && norm_path != norm_root) {
      stop(sprintf("Manifest %s escapes allowed directory '%s': '%s'", label, root, p), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Read Data Manifest File
#'
#' Reads a CSV manifest file listing frozen dataset copies and their repo-relative sources.
#' Expected columns: \code{file} (filename in destination folder) and \code{source} (repo-relative path).
#' Handles UTF-8 BOM headers created by spreadsheet applications like Microsoft Excel.
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
  df <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
  names(df) <- sub("^\ufeff", "", names(df))
  names(df) <- sub("^X\\.U\\.FEFF\\.", "", names(df))
  df
}

#' Copy Datasets to Snapshot Directory
#'
#' Copies files listed in a manifest from their source paths to a destination directory,
#' preserving timestamps and validating contents immediately upon copy.
#' Manifests must come from a trusted source. Source and destination paths are confined
#' to their respective directories and must not contain parent-directory traversals.
#'
#' @param dir Destination directory for frozen copies.
#' @param manifest Data frame with columns \code{file} and \code{source}. If NULL,
#'   read from \code{file.path(dir, "manifest.csv")}.
#' @param project_root Absolute or relative root directory for resolving \code{source} paths
#'   (default: \code{here::here()}).
#' @param overwrite Logical; whether to overwrite existing destination files (default: FALSE).
#' @return A validation tibble produced by \code{\link{validate_data_manifest}}.
#' @export
copy_data_manifest <- function(dir = ".",
                               manifest = NULL,
                               project_root = here::here(),
                               overwrite = FALSE) {
  if (is.null(manifest)) {
    manifest <- read_data_manifest(dir)
  }

  if (!is.data.frame(manifest) || !all(c("file", "source") %in% names(manifest))) {
    stop("manifest must be a data frame containing 'file' and 'source' columns", call. = FALSE)
  }

  if (nrow(manifest) == 0) {
    return(validate_data_manifest(dir = dir, manifest = manifest, project_root = project_root))
  }

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  .assert_within_root(manifest$source, project_root, label = "source")
  .assert_within_root(manifest$file, dir, label = "file")

  src_paths <- file.path(project_root, manifest$source)
  dst_paths <- file.path(dir, manifest$file)

  missing_src <- src_paths[!file.exists(src_paths)]
  if (length(missing_src) > 0) {
    stop("Source file(s) not found:\n", paste(missing_src, collapse = "\n"), call. = FALSE)
  }

  if (!isTRUE(overwrite)) {
    already_exist <- dst_paths[file.exists(dst_paths)]
    if (length(already_exist) > 0) {
      stop(
        "Destination file(s) already exist and overwrite is FALSE:\n",
        paste(already_exist, collapse = "\n"),
        call. = FALSE
      )
    }
  }

  file.copy(src_paths, dst_paths, overwrite = overwrite, copy.date = TRUE)
  validate_data_manifest(dir = dir, manifest = manifest, project_root = project_root)
}

#' Validate Data Snapshot Copies Against Upstream Sources
#'
#' Compares destination files against upstream source files checking:
#' 1. Existence of both source and copy.
#' 2. Cryptographic MD5 hash equality.
#' 3. Object-level equivalence for \code{.rds} files when MD5 hashes match.
#'
#' Path entries in \code{manifest} are validated to ensure they are relative and do not escape
#' \code{dir} or \code{project_root}.
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

  if (!is.data.frame(manifest) || !all(c("file", "source") %in% names(manifest))) {
    stop("manifest must be a data frame containing 'file' and 'source' columns", call. = FALSE)
  }

  if (nrow(manifest) == 0) {
    return(tibble::tibble(
      file = character(0),
      source = character(0),
      source_exists = logical(0),
      copy_exists = logical(0),
      md5_match = logical(0),
      same_content = logical(0),
      valid = logical(0)
    ))
  }

  .assert_within_root(manifest$source, project_root, label = "source")
  .assert_within_root(manifest$file, dir, label = "file")

  purrr::pmap_dfr(manifest, function(file, source, ...) {
    src <- file.path(project_root, source)
    dst <- file.path(dir, file)

    source_exists <- file.exists(src) && !dir.exists(src)
    copy_exists <- file.exists(dst) && !dir.exists(dst)
    both <- source_exists && copy_exists

    md5_match <- both && isTRUE(unname(tools::md5sum(src) == tools::md5sum(dst)))

    same_content <- if (both && grepl("\\.rds$", file, ignore.case = TRUE)) {
      if (md5_match) {
        TRUE
      } else {
        tryCatch(
          identical(readRDS(src), readRDS(dst), ignore.environment = TRUE),
          error = function(e) FALSE
        )
      }
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
      valid = both && md5_match
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
  if (!is.data.frame(validation) || !"valid" %in% names(validation) || !"file" %in% names(validation)) {
    stop("validation must be a data frame returned by validate_data_manifest() containing 'valid' and 'file' columns", call. = FALSE)
  }
  if (nrow(validation) == 0) {
    return(invisible(validation))
  }
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
