#' Process Utilities for Robust External Execution
#'
#' Unified process execution helpers providing explicit executable validation,
#' command and error instrumentation, and structured error propagation.
#'
#' @name process_utils
#' @keywords internal
NULL

#' Validate Executable Existence
#'
#' Validates that a command string points to an existing file on disk or
#' resolves to an executable on the system PATH.
#'
#' @param cmd Character string representing the command or executable name/path.
#' @return The normalized or resolved executable path invisibly. Throws an \code{external_process_error} if the executable cannot be located.
#' @keywords internal
#' @noRd
validate_executable <- function(cmd) {
  if (!is.character(cmd) || length(cmd) != 1L || is.na(cmd) || !nzchar(trimws(cmd))) {
    err <- structure(
      list(
        message = "Command must be a single non-empty character string.",
        cmd = cmd,
        args = character(),
        exit_code = NA_integer_,
        stderr = "Invalid command argument",
        failure_mode = "invalid_command"
      ),
      class = c("external_process_error", "error", "condition")
    )
    stop(err)
  }

  cmd_clean <- trimws(cmd)

  # Check if explicit path exists
  if (file.exists(cmd_clean)) {
    return(invisible(cmd_clean))
  }

  # Check PATH resolution
  resolved <- Sys.which(cmd_clean)
  if (nzchar(resolved) && file.exists(resolved)) {
    return(invisible(unname(resolved)))
  }

  # Command not found
  msg <- sprintf("Executable '%s' was not found on the system PATH or specified path.", cmd_clean)
  err <- structure(
    list(
      message = msg,
      cmd = cmd_clean,
      args = character(),
      exit_code = NA_integer_,
      stderr = "Executable not found",
      failure_mode = "missing_executable"
    ),
    class = c("external_process_error", "error", "condition")
  )
  stop(err)
}

#' Safe system2 Wrapper with Command Validation and Logging
#'
#' Wraps base \code{\link[base]{system2}} with pre-flight command existence checks,
#' logging of command execution and errors, and structured error propagation.
#'
#' @param command Character string, name or path of the executable.
#' @param args Character vector of arguments.
#' @param stdout Where output sent to stdout should be directed (default \code{TRUE}).
#' @param stderr Where output sent to stderr should be directed (default \code{TRUE}).
#' @param check Logical; if \code{TRUE} (default), raises an \code{external_process_error} on non-zero exit.
#' @param log_failures Logical; if \code{TRUE} (default), emits a diagnostic message on failure.
#' @param ... Additional arguments passed to \code{\link[base]{system2}}.
#' @return Output of \code{system2}. Raises an \code{external_process_error} if the executable is missing or if \code{check = TRUE} and exit code is non-zero.
#' @keywords internal
#' @noRd
safe_system2 <- function(command,
                         args = character(),
                         stdout = TRUE,
                         stderr = TRUE,
                         check = TRUE,
                         log_failures = TRUE,
                         ...) {
  # 1. Validate executable existence
  validate_executable(command)

  # 2. Invoke command safely
  res <- suppressWarnings(tryCatch(
    system2(command, args = args, stdout = stdout, stderr = stderr, ...),
    error = function(e) {
      msg <- sprintf("System execution error for '%s': %s", command, e$message)
      if (log_failures) {
        message(sprintf("[EXTERNAL_PROCESS_FAIL] %s", msg))
      }
      err <- structure(
        list(
          message = msg,
          cmd = command,
          args = args,
          exit_code = NA_integer_,
          stderr = e$message,
          failure_mode = "invocation_error"
        ),
        class = c("external_process_error", "error", "condition")
      )
      stop(err)
    }
  ))

  # 3. Determine exit status
  status <- attr(res, "status")
  exit_code <- if (is.null(status)) {
    if (is.numeric(res) && length(res) == 1L && isFALSE(stdout) && isFALSE(stderr)) {
      as.integer(res)
    } else {
      0L
    }
  } else {
    as.integer(status)
  }

  # 4. Handle non-zero exit codes
  if (exit_code != 0L) {
    stderr_lines <- if (is.character(res)) paste(res, collapse = "\n") else ""
    log_msg <- sprintf(
      "[EXTERNAL_PROCESS_FAIL] Command '%s' exited with code %d.\nArgs: %s\nStderr/Output: %s",
      command,
      exit_code,
      paste(args, collapse = " "),
      stderr_lines
    )

    if (log_failures) {
      message(log_msg)
    }

    if (isTRUE(check)) {
      err <- structure(
        list(
          message = sprintf("External process '%s' failed with exit code %d:\n%s", command, exit_code, stderr_lines),
          cmd = command,
          args = args,
          exit_code = exit_code,
          stderr = stderr_lines,
          failure_mode = "non_zero_exit"
        ),
        class = c("external_process_error", "error", "condition")
      )
      stop(err)
    }
  }

  res
}

#' Safe Shell Invocation
#'
#' Validates the platform shell and executes a command string safely.
#'
#' @param cmd Character string command to pass to the shell.
#' @param check Logical; whether to throw \code{external_process_error} on failure.
#' @param log_failures Logical; whether to emit diagnostics on failure.
#' @param ... Additional arguments.
#' @return Output or exit status of the shell invocation.
#' @keywords internal
#' @noRd
safe_shell <- function(cmd, check = TRUE, log_failures = TRUE, ...) {
  if (!is.character(cmd) || length(cmd) != 1L || !nzchar(trimws(cmd))) {
    stop("`cmd` must be a single non-empty character string.", call. = FALSE)
  }

  if (.Platform$OS.type == "windows") {
    shell_bin <- Sys.getenv("COMSPEC", "cmd.exe")
    args <- c("/c", cmd)
  } else {
    shell_bin <- "/bin/sh"
    # system2() pastes args unquoted into one command line, so without
    # shQuote() `sh -c echo hi` would run only `echo` with `hi` as $0
    args <- c("-c", shQuote(cmd))
  }

  safe_system2(
    command = shell_bin,
    args = args,
    stdout = TRUE,
    stderr = TRUE,
    check = check,
    log_failures = log_failures,
    ...
  )
}
