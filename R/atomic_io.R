#' Atomic File I/O, Advisory Locking, and Secrets Validation
#'
#' Provides robust atomic file updates (write -> flush -> rename),
#' cross-platform advisory file locking, and strict directory permission checks.
#'
#' @name atomic_io
#' @keywords internal
NULL

#' Validate Secrets Directory Permissions and Scope
#'
#' Ensures that confidential directories (such as those storing PHI or identifier mappings)
#' are user-scoped, reside strictly outside the repository tree, and are not world-readable.
#'
#' @param dir Path to the secrets directory.
#' @return Invisibly, the normalized directory path. Throws an error if permissions are insecure or if the path is inside the git repository.
#' @keywords internal
#' @noRd
validate_secrets_dir <- function(dir) {
  if (!is.character(dir) || length(dir) != 1L || !nzchar(trimws(dir))) {
    stop("Secrets directory path must be a single non-empty character string.", call. = FALSE)
  }

  dir_norm <- tryCatch(
    normalizePath(dir, winslash = "/", mustWork = FALSE),
    error = function(e) dir
  )

  # Refuse to store secrets inside the git repository tree
  repo_root <- .find_repo_root(".")
  if (!is.null(repo_root)) {
    repo_root_norm <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)
    if (startsWith(dir_norm, repo_root_norm)) {
      stop(
        "Refusing to use repository path for secrets (", dir_norm, "). ",
        "Secrets must reside in a user-scoped directory outside the repository tree.",
        call. = FALSE
      )
    }
  }

  # Ensure directory exists with user-only permissions (0700)
  if (!dir.exists(dir_norm)) {
    dir.create(dir_norm, recursive = TRUE, showWarnings = FALSE)
    tryCatch(Sys.chmod(dir_norm, mode = "0700"), error = function(e) NULL)
  }

  # On POSIX systems, verify that permissions are non-world-readable and non-world-writable
  if (.Platform$OS.type != "windows") {
    info <- file.info(dir_norm)
    if (!is.na(info$mode)) {
      mode_int <- as.integer(info$mode)
      # Check world permissions: read (4), write (2), execute (1) -> octal 7
      world_bits <- bitwAnd(mode_int, 7L)
      if (world_bits != 0L) {
        # Attempt to restrict permissions to 0700
        tryCatch(Sys.chmod(dir_norm, mode = "0700"), error = function(e) NULL)
        info_after <- file.info(dir_norm)
        if (bitwAnd(as.integer(info_after$mode), 7L) != 0L) {
          stop(
            "Insecure permissions on secrets directory '", dir_norm,
            "' (mode: ", format(info$mode), "). Directory must not be world-readable or world-writable.",
            call. = FALSE
          )
        }
      }
    }
  }

  invisible(dir_norm)
}

#' Advisory File Locking
#'
#' Acquires an advisory file lock using an atomic directory creation strategy.
#' Supports timeout and automatic cleanup of stale locks.
#'
#' @param lock_path Path to the lock directory (e.g. \code{paste0(file_path, ".lock")}).
#' @param code Expression to evaluate while holding the lock.
#' @param timeout Numeric, maximum seconds to wait for lock acquisition (default 10).
#' @param retry_interval Numeric, seconds between lock acquisition attempts (default 0.05).
#' @param stale_age Numeric, seconds after which an existing lock is considered stale (default 30).
#' @return The result of evaluating \code{code}.
#' @keywords internal
#' @noRd
with_file_lock <- function(lock_path,
                           code,
                           timeout = 10,
                           retry_interval = 0.05,
                           stale_age = 30) {
  start_time <- Sys.time()
  acquired <- FALSE

  while (!acquired) {
    # Attempt atomic creation of lock directory
    if (dir.create(lock_path, recursive = FALSE, showWarnings = FALSE)) {
      acquired <- TRUE
      break
    }

    # Check for stale lock
    lock_info <- file.info(lock_path)
    if (!is.na(lock_info$mtime)) {
      age <- as.numeric(difftime(Sys.time(), lock_info$mtime, units = "secs"))
      if (age > stale_age) {
        warning("Removing stale advisory lock at '", lock_path, "' (age: ", round(age, 1), "s).", call. = FALSE)
        unlink(lock_path, recursive = TRUE, force = TRUE)
      }
    }

    # Check timeout
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed >= timeout) {
      stop(
        "Failed to acquire advisory file lock on '", lock_path,
        "' within ", timeout, " seconds. Concurrent operation timed out.",
        call. = FALSE
      )
    }

    Sys.sleep(retry_interval)
  }

  # Record lock owner metadata
  info_file <- file.path(lock_path, "lock_owner")
  tryCatch(
    writeLines(
      paste0("pid: ", Sys.getpid(), "\ntime: ", format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")),
      con = info_file
    ),
    error = function(e) NULL
  )

  # Ensure lock is unconditionally released on exit or error
  on.exit({
    if (dir.exists(lock_path)) {
      unlink(lock_path, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  # Execute the protected code block
  force(code)
}

#' Atomic File Update
#'
#' Writes content to a temporary file in the target directory, flushes data to disk,
#' and replaces the destination file via an atomic rename operation.
#'
#' @param content Character vector or raw vector to write.
#' @param target_path File path to update atomically.
#' @return Invisibly, \code{TRUE} on successful update.
#' @keywords internal
#' @noRd
atomic_write_file <- function(content, target_path) {
  target_dir <- dirname(target_path)
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }

  tmp_file <- tempfile(pattern = ".tmp_atomic_", tmpdir = target_dir)
  on.exit({
    if (file.exists(tmp_file)) {
      unlink(tmp_file, force = TRUE)
    }
  }, add = TRUE)

  con <- file(tmp_file, open = "wb")
  tryCatch({
    if (is.raw(content)) {
      writeBin(content, con)
    } else if (is.character(content)) {
      writeLines(content, con = con, useBytes = TRUE)
    } else {
      stop("Content must be character or raw vector.", call. = FALSE)
    }
    flush(con)
  }, finally = {
    close(con)
  })

  # Atomically rename temporary file to target path
  success <- file.rename(tmp_file, target_path)
  if (!success) {
    # Fallback in case of Windows file replacement lock
    copied <- file.copy(tmp_file, target_path, overwrite = TRUE)
    if (!copied) {
      stop("Failed to atomically update target file '", target_path, "'.", call. = FALSE)
    }
    unlink(tmp_file, force = TRUE)
  }

  invisible(TRUE)
}
