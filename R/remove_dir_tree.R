#' Is a Path a Directory Link (Symbolic Link or Windows Junction)?
#'
#' @param path A single path.
#' @return `TRUE` when `path` is itself a link, whatever it points at.
#' @keywords internal
#' @noRd
is_dir_link <- function(path) {
  isTRUE(tryCatch(unname(fs::is_link(path)), error = function(e) FALSE)) ||
    isTRUE(nzchar(Sys.readlink(path)))
}

#' Remove a Directory Link Without Touching Its Target
#'
#' `unlink(recursive = TRUE)` cannot delete a Windows junction ("mismatch
#' between the tag ... reparse point"). `rmdir` drops the junction itself and
#' leaves the directory it points at alone.
#'
#' @param link Path of the link.
#' @return `TRUE` invisibly when `link` no longer exists.
#' @keywords internal
#' @noRd
remove_dir_link <- function(link) {
  if (!file.exists(link) && !is_dir_link(link)) return(invisible(FALSE))
  if (.Platform$OS.type == "windows") {
    # normalizePath() on the link itself would resolve it to its target, and
    # rmdir would then be aimed at the target: normalize the parent only.
    dir_win <- gsub("/", "\\", normalizePath(dirname(link), winslash = "/", mustWork = FALSE), fixed = TRUE)
    to_win <- file.path(dir_win, basename(link), fsep = "\\")
    suppressWarnings(system2("cmd", c("/c", "rmdir", shQuote(to_win)), stdout = FALSE, stderr = FALSE))
  } else {
    suppressWarnings(unlink(link))
  }
  invisible(!dir.exists(link) && !is_dir_link(link))
}

#' Remove Directories Recursively, Safely Across Links and Junctions
#'
#' The package's replacement for `unlink(path, recursive = TRUE)` on
#' directories that a user may have set up: a cache, a checkpoint or a build
#' directory. A link (a symbolic link, or a junction on Windows) at `path` or
#' anywhere below it is removed as a link; the directory it points at is never
#' entered or emptied. On Windows `unlink(recursive = TRUE)` fails on a
#' junction and leaves the tree half deleted.
#'
#' @param path Character vector of directories (or files) to remove.
#' @return Invisibly, a logical vector: `TRUE` where the path no longer exists.
#' @keywords internal
#' @noRd
remove_dir_tree <- function(path) {
  path <- path[!is.na(path) & nzchar(path)]
  gone <- vapply(path, function(p) {
    if (is_dir_link(p)) {
      remove_dir_link(p)
    } else if (dir.exists(p)) {
      links <- tryCatch(
        as.character(fs::dir_ls(p, recurse = TRUE, all = TRUE, type = "symlink")),
        error = function(e) character()
      )
      # deepest first, so that a link inside a linked-to tree is never reached
      for (l in links[order(nchar(links), decreasing = TRUE)]) remove_dir_link(l)
      unlink(p, recursive = TRUE, force = TRUE)
    } else if (file.exists(p)) {
      unlink(p, force = TRUE)
    }
    !file.exists(p) && !is_dir_link(p)
  }, logical(1))
  invisible(unname(gone))
}
