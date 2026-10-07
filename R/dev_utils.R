#' Install a Package, Bypassing an Existing Lock
#'
#' @param packages Character vector of package name(s) to install.
#' @return Invisibly, \code{NULL} (as per \code{\link[utils]{install.packages}}).
#' @export
#' @examples
#' \dontrun{
#' install.packages.no_lock("dplyr")
#' }
install.packages.no_lock <- function(packages) {
  utils::install.packages(packages, INSTALL_opts = "--no-lock")
}
