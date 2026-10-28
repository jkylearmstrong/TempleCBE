#' Proportional-Hazards Audit of a Cox Model
#'
#' Tests the proportional-hazards assumption of a fitted Cox model with scaled Schoenfeld residuals
#' ([survival::cox.zph()]) and returns one row per model term plus the global test, with a flag for
#' the terms whose hazard ratio appears to change over follow-up.
#'
#' @section Reading the result:
#' A small p-value means the term's effect is not constant over time. With many terms or a large
#' sample some small departures will be flagged, so look at the Schoenfeld plot
#' (`plot(survival::cox.zph(fit))`) before changing the model. If a term is flagged, stratify on it,
#' interact it with a function of time (`survival::coxph(..., tt = )`) or split follow-up
#' (`survival::survSplit()`) and report a hazard ratio per period.
#'
#' @param fit A model fitted by [survival::coxph()].
#' @param transform Time transform passed to [survival::cox.zph()]: `"km"` (default), `"rank"`,
#'   `"identity"` or `"log"`.
#' @param alpha p-value below which a term is flagged.
#' @return A tibble with `term`, `chisq`, `df`, `p.value` and `non_proportional` (logical). The
#'   `GLOBAL` row is the joint test and is flagged like a term.
#' @seealso [cox_check()]
#' @export
#' @examples
#' fit <- survival::coxph(survival::Surv(time, status) ~ age + sex + ph.ecog, data = survival::lung)
#' tbl_schoenfeld_audit(fit)
tbl_schoenfeld_audit <- function(fit, transform = "km", alpha = 0.05) {
  if (!inherits(fit, "coxph")) stop("`fit` must be a model from survival::coxph().", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a number in (0, 1).", call. = FALSE)
  }
  zph <- survival::cox.zph(fit, transform = transform)
  tab <- as.data.frame(zph$table)
  out <- tibble::tibble(
    term = rownames(tab),
    chisq = tab[["chisq"]],
    df = tab[["df"]],
    p.value = tab[["p"]]
  )
  out$non_proportional <- !is.na(out$p.value) & out$p.value < alpha
  out
}
