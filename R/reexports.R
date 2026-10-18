#' Write Data Frame to Excel (.xlsx)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `write_xlsx()` in TempleCBE was a re-export of [writexl::write_xlsx()] and is now
#' deprecated to avoid masking conflicts when both packages are loaded.
#' Call [writexl::write_xlsx()] directly instead.
#'
#' @param ... Passed to [writexl::write_xlsx()].
#' @export
write_xlsx <- function(...) {
  lifecycle::deprecate_warn("0.5.8", "write_xlsx()", details = "Use writexl::write_xlsx() directly instead.")
  writexl::write_xlsx(...)
}

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

