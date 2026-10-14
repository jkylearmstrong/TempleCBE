# Make `link` a directory link that points at the existing directory `target`.
#
# A symbolic link where the platform allows one. Creating symbolic links on Windows
# needs a privilege that most accounts lack, so fall back to a junction, which
# resolves to the target the same way and needs none. Returns TRUE when `link`
# now resolves to a directory, so callers can skip when it does not.
make_dir_link <- function(target, link) {
  ok <- suppressWarnings(file.symlink(target, link))
  if (!isTRUE(ok) && .Platform$OS.type == "windows") {
    to_win <- function(p) gsub("/", "\\", normalizePath(p, winslash = "/", mustWork = FALSE), fixed = TRUE)
    suppressWarnings(system2(
      "cmd", c("/c", "mklink", "/J", shQuote(to_win(link)), shQuote(to_win(target))),
      stdout = FALSE, stderr = FALSE
    ))
  }
  dir.exists(link)
}

# Remove a directory link without touching what it points at. unlink(recursive = TRUE)
# cannot delete a Windows junction ("mismatch between the tag ... reparse point"), so
# use rmdir there, which drops the junction itself and leaves the target alone.
remove_dir_link <- function(link) {
  if (!file.exists(link) && is.na(Sys.readlink(link))) return(invisible(FALSE))
  if (.Platform$OS.type == "windows") {
    to_win <- gsub("/", "\\", normalizePath(link, winslash = "/", mustWork = FALSE), fixed = TRUE)
    suppressWarnings(system2("cmd", c("/c", "rmdir", shQuote(to_win)), stdout = FALSE, stderr = FALSE))
  } else {
    suppressWarnings(unlink(link))
  }
  invisible(!dir.exists(link))
}
