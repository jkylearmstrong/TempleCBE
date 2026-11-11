# Make `link` a directory link that points at the existing directory `target`.
#
# A symbolic link where the platform allows one. Creating symbolic links on Windows
# needs a privilege that most accounts lack, so fall back to a junction, which
# resolves to the target the same way and needs none. Returns TRUE when `link`
# now resolves to a directory, so callers can skip when it does not.
make_dir_link <- function(target, link) {
  ok <- suppressWarnings(file.symlink(target, link))
  if (!isTRUE(ok) && .Platform$OS.type == "windows") {
    to_win <- function(p) {
      dir_win <- gsub("/", "\\", normalizePath(dirname(p), winslash = "/", mustWork = FALSE), fixed = TRUE)
      file.path(dir_win, basename(p), fsep = "\\")
    }
    suppressWarnings(system2(
      "cmd", c("/c", "mklink", "/J", shQuote(to_win(link)), shQuote(to_win(target))),
      stdout = FALSE, stderr = FALSE
    ))
  }
  dir.exists(link)
}

# remove_dir_link() and remove_dir_tree(), which delete a link (or a tree that holds links)
# without touching what the links point at, are package functions: R/remove_dir_tree.R.
