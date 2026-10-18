#' Install a Package, Bypassing an Existing Lock
#'
#' @param packages Character vector of package name(s) to install.
#' @return Invisibly, \code{NULL} (as per \code{\link[utils]{install.packages}}).
#' @export
#' @examples
#' \dontrun{
#' install_package_no_lock("dplyr")
#' }
install_package_no_lock <- function(packages) {
  utils::install.packages(packages, INSTALL_opts = "--no-lock")
}

#' @rdname install_package_no_lock
#' @export
install.packages.no_lock <- function(...) {
  lifecycle::deprecate_warn("0.5.8", "install.packages.no_lock()", "install_package_no_lock()")
  install_package_no_lock(...)
}
