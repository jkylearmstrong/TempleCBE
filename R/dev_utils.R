#' Install a Package, Bypassing an Existing Lock
#'
#' Calls \code{\link[utils]{install.packages}} with \code{INSTALL_opts = "--no-lock"},
#' for the case where an interrupted installation left a \code{00LOCK} directory
#' behind. The packages come from the repositories given in \code{repos} (by
#' default the session's \code{getOption("repos")}); nothing else about the
#' source is checked, so install only packages you would install with
#' \code{install.packages()}.
#'
#' @param packages Character vector of package name(s) to install. Each must be a
#'   valid package name (letters, digits and dots, starting with a letter): a
#'   path, a URL or an option string is refused.
#' @param repos Repositories to install from, passed to
#'   \code{\link[utils]{install.packages}}.
#' @return Invisibly, \code{NULL} (as per \code{\link[utils]{install.packages}}).
#' @export
#' @examples
#' \dontrun{
#' install_package_no_lock("dplyr")
#' }
install_package_no_lock <- function(packages, repos = getOption("repos")) {
  if (!is.character(packages) || length(packages) == 0L || anyNA(packages)) {
    stop("`packages` must be a character vector of package names.", call. = FALSE)
  }
  bad <- !grepl("^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$", packages)
  if (any(bad)) {
    stop(
      "Not valid package name(s): ", paste(encodeString(packages[bad], quote = "'"), collapse = ", "),
      ". `packages` takes names only, not paths, URLs or options.",
      call. = FALSE
    )
  }
  utils::install.packages(packages, repos = repos, INSTALL_opts = "--no-lock")
}
