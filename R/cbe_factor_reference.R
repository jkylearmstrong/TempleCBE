#' Relevel a Factor's Reference Level
#'
#' Standalone helper for choosing the reference (baseline) level of a factor before
#' passing it to [cbe_cox_single()] or [cbe_cox_multi()]. Wraps [stats::relevel()],
#' defaulting to the first level (matching the reference-row convention already used
#' by \code{cbe_cox_single()}/\code{cbe_cox_multi()}), and attaches a
#' \code{"cbe_reference_level"} attribute recording the chosen level.
#'
#' \code{cbe_cox_single()} and \code{cbe_cox_multi()} are not rewired to require this
#' helper; it is an optional convenience for callers who want to choose the reference
#' level explicitly before fitting.
#'
#' @param x A factor, or a vector coercible to one via \code{as.factor()}.
#' @param ref_level Character string naming the level to use as reference. Defaults
#'   to the first level of \code{x} (its current or natural ordering).
#' @return The releveled factor, with attribute \code{"cbe_reference_level"} set to
#'   the chosen reference level.
#' @seealso [cbe_cox_single()], [cbe_cox_multi()]
#' @export
cbe_factor_reference <- function(x, ref_level = NULL) {
  f <- if (is.factor(x)) x else as.factor(x)

  if (is.null(ref_level)) {
    ref_level <- levels(f)[1]
  } else if (!ref_level %in% levels(f)) {
    stop(sprintf("`ref_level` = '%s' is not a level of `x`. Levels are: %s", ref_level, paste(levels(f), collapse = ", ")), call. = FALSE)
  }

  f_releveled <- stats::relevel(f, ref = ref_level)
  attr(f_releveled, "cbe_reference_level") <- ref_level
  message(sprintf("cbe_factor_reference(): reference level set to '%s'.", ref_level))
  f_releveled
}
