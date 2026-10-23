#' Install a Package, Bypassing an Existing Lock
#'
#' @param packages Character vector of package name(s) to install.
#' @param ... Passed to \code{install_package_no_lock()}.
#' @return Invisibly, \code{NULL} (as per \code{\link[utils]{install.packages}}).
#' @export
#' @examples
#' \dontrun{
#' install_package_no_lock("dplyr")
#' }
install_package_no_lock <- function(packages) {
  utils::install.packages(packages, INSTALL_opts = "--no-lock")
}
