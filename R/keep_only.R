#' Keep Only Specified Objects in an Environment
#'
#' Removes every object in the caller's environment except the ones named in
#' \code{vector}. Prompts for confirmation in interactive sessions unless
#' \code{.dontask = TRUE}; proceeds without prompting in non-interactive
#' sessions (scripts, \code{R CMD check}, knitr rendering), since
#' \code{readline()} would otherwise hang there.
#'
#' @details
#' \code{vector} must be a character vector of object names, so the names are
#' quoted: \code{keep_only(c("a", "b"))}, not \code{keep_only(c(a, b))}. Every
#' name has to be an object of the calling environment itself (not of one
#' above it); otherwise the function stops before it removes anything. A
#' misspelt name used to pass unnoticed and remove every object, the one that
#' was meant to be kept included.
#'
#' Called from inside a function, \code{keep_only()} works on that function's
#' own environment, so the function's arguments are removed too unless they
#' are named in \code{vector}.
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
  env <- parent.frame()
  if (!is.character(vector) || anyNA(vector)) {
    stop("`vector` must be a character vector of object names to keep, such as keep_only(c(\"a\", \"b\")); ",
         "the names have to be quoted. Nothing was removed.", call. = FALSE)
  }
  not_found <- vector[!vapply(vector, exists, logical(1), envir = env, inherits = FALSE, USE.NAMES = FALSE)]
  if (length(not_found) > 0) {
    stop("Nothing was removed: ", paste0("'", unique(not_found), "'", collapse = ", "),
         if (length(unique(not_found)) == 1L) " is" else " are",
         " not in the calling environment. Is the name misspelt?", call. = FALSE)
  }

  all_objs <- ls(envir = env)
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
  rm(list = rm_vector, envir = env)
  invisible(NULL)
}

#' Delete Stray 'nul' Files
#'
#' Windows-only. \code{knitr} occasionally leaves behind a file literally
#' named \code{nul} as a side effect of redirecting output to the Windows
#' \code{NUL} device. This deletes files whose *basename* is exactly
#' \code{"nul"} (case-insensitive) under \code{path}.
#'
#' The files are deleted from R, through their Windows device-namespace path (a
#' plain path to such a file is taken for the \code{NUL} device itself), not by
#' handing a command to \code{cmd.exe}, so a folder name that holds percent
#' signs, such as \code{x\%TEMP\%y}, is safe. Folders that are reached through
#' a link (a junction or symbolic link inside \code{path} that points
#' elsewhere) are skipped, so nothing outside \code{path} is deleted.
#'
#' @param path Directory to search, defaults to \code{here::here()}.
#' @param .dontask Logical (default \code{FALSE}); skip the confirmation prompt.
#' @param .verify_command Logical (default \code{FALSE}); if \code{TRUE},
#'   return the paths that would be deleted, in device-namespace form, and
#'   delete nothing.
#' @return Invisibly, the deleted file paths (or, if \code{.verify_command =
#'   TRUE}, the paths that would be deleted). An error lists any file that
#'   could not be deleted.
#' @export
delete_nul_files <- function(path = here::here(), .dontask = FALSE, .verify_command = FALSE) {
  if (.Platform$OS.type != "windows") {
    stop("delete_nul_files() targets a Windows-specific artifact (the NUL device) and only runs on Windows.")
  }

  found <- find_nul_files(path)
  nul_files <- found$shown

  if (length(nul_files) == 0) {
    message("No stray 'nul' files found under ", path)
    return(invisible(character(0)))
  }

  message(paste0("Files to delete:\n", paste("\t", nul_files, collapse = "\n")))

  device_paths <- nul_device_path(found$full)

  if (isTRUE(.verify_command)) {
    return(device_paths)
  }

  if (interactive() && !.dontask) {
    confirm <- readline(prompt = "Proceed with deleting these files? (yes/no): ")
    if (!(tolower(confirm) %in% c("yes", "y"))) {
      message("Operation cancelled.")
      return(invisible(NULL))
    }
  }

  deleted <- vapply(device_paths, function(p) isTRUE(suppressWarnings(file.remove(p))), logical(1), USE.NAMES = FALSE)
  if (!all(deleted)) {
    stop("Could not delete: ", paste(nul_files[!deleted], collapse = ", "), call. = FALSE)
  }
  invisible(nul_files)
}

# The files called "nul" under `path` that really are under it. list.files()
# follows junctions and symbolic links, so a file listed under <path>/link/ may
# live in the folder the link points to; a folder whose resolved location is not
# the one its name under `path` says is such a link, and is skipped.
# Returns list(full = absolute paths with "/", shown = the paths as list.files()
# would give them for `path`).
#' @keywords internal
#' @noRd
find_nul_files <- function(path) {
  root <- normalizePath(path, winslash = "/", mustWork = FALSE)
  rel <- list.files(root, recursive = TRUE, all.files = TRUE)
  rel <- rel[tolower(basename(rel)) == "nul"]
  if (!length(rel)) {
    return(list(full = character(0), shown = character(0)))
  }

  dir_rel <- dirname(rel)
  expected_dir <- ifelse(dir_rel == ".", root, file.path(root, dir_rel))
  actual_dir <- normalizePath(expected_dir, winslash = "/", mustWork = FALSE)
  via_link <- tolower(actual_dir) != tolower(expected_dir)
  if (any(via_link)) {
    message("Skipping files reached through a link:\n", paste("\t", file.path(path, rel[via_link]), collapse = "\n"))
  }

  rel <- rel[!via_link]
  list(full = file.path(root, rel), shown = file.path(path, rel))
}

# The device-namespace spelling of a path, which is how Windows is told to open
# a file called "nul" rather than the NUL device: \\.\C:\dir\nul.
#' @keywords internal
#' @noRd
nul_device_path <- function(x) {
  if (!length(x)) {
    return(character(0))
  }
  unc <- startsWith(x, "//")
  win <- gsub("/", "\\\\", ifelse(unc, sub("^//", "", x), x))
  paste0("\\\\.\\", ifelse(unc, "UNC\\", ""), win)
}
