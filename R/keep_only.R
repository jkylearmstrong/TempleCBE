#' Keep Only Specified Objects in an Environment
#'
#' Removes every object in the caller's environment except the ones named in
#' \code{vector}. Prompts for confirmation in interactive sessions unless
#' \code{.dontask = TRUE}; proceeds without prompting in non-interactive
#' sessions (scripts, \code{R CMD check}, knitr rendering), since
#' \code{readline()} would otherwise hang there.
#'
#' @param vector Character vector of object names to keep.
#' @param .dontask Logical (default \code{FALSE}); skip the confirmation prompt.
#' @return Invisibly, \code{NULL}.
#' @export
#' @examples
#' e <- new.env()
#' local({a <- 1; b <- 2; keep_only("a", .dontask = TRUE)}, envir = e)
#' ls(e)
keep_only <- function(vector, .dontask = FALSE) {
  all_objs <- ls(envir = parent.frame())
  rm_vector <- setdiff(all_objs, vector)
  if (length(rm_vector) == 0) return(invisible(NULL))

  message(paste0("Removing objects:\n", paste("\t", rm_vector, collapse = "\n")))

  if (interactive() && !.dontask) {
    confirm <- readline(prompt = "Proceed with removing these objects? (yes/no): ")
    if (!(tolower(confirm) %in% c("yes", "y"))) {
      message("Operation cancelled.")
      return(invisible(NULL))
    }
  }
  rm(list = rm_vector, envir = parent.frame())
  invisible(NULL)
}

#' Delete Stray 'nul' Files
#'
#' Windows-only. \code{knitr} occasionally leaves behind a file literally
#' named \code{nul} as a side effect of redirecting output to the Windows
#' \code{NUL} device. This deletes files whose *basename* is exactly
#' \code{"nul"} (case-insensitive) under \code{path}.
#'
#' @param path Directory to search, defaults to \code{here::here()}.
#' @param .dontask Logical (default \code{FALSE}); skip the confirmation prompt.
#' @param .verify_command Logical (default \code{FALSE}); if \code{TRUE},
#'   return the shell command(s) instead of running them.
#' @return Invisibly, the deleted file paths (or the commands, if
#'   \code{.verify_command = TRUE}).
#' @export
delete_nul_files <- function(path = here::here(), .dontask = FALSE, .verify_command = FALSE) {
  if (.Platform$OS.type != "windows") {
    stop("delete_nul_files() targets a Windows-specific artifact (the NUL device) and only runs on Windows.")
  }

  all_files <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  nul_files <- all_files[tolower(basename(all_files)) == "nul"]

  if (length(nul_files) == 0) {
    message("No stray 'nul' files found under ", path)
    return(invisible(character(0)))
  }

  message(paste0("Files to delete:\n", paste("\t", nul_files, collapse = "\n")))

  delete_commands <- nul_delete_commands(nul_files)

  if (isTRUE(.verify_command)) {
    return(delete_commands)
  }

  if (interactive() && !.dontask) {
    confirm <- readline(prompt = "Proceed with deleting these files? (yes/no): ")
    if (!(tolower(confirm) %in% c("yes", "y"))) {
      message("Operation cancelled.")
      return(invisible(NULL))
    }
  }

  for (cmd in delete_commands) shell(cmd)
  invisible(nul_files)
}

#' @keywords internal
#' @noRd
nul_delete_commands <- function(nul_files) {
  win_paths <- gsub("/", "\\\\", nul_files)
  paste0("del ", shQuote(paste0("\\\\.\\", win_paths), type = "cmd"))
}
